## Migrate the `source:` block of every `data/*/metadata.yml` to the shape
## traits.build reads from 2.3.0 (traits.build #296, PR #298): the dataset's
## own reference is `dataset:` (was `primary:`), keyed by its `dataset_id`;
## `source:` (was `original_source:` / `original_NN:`) lists the studies a
## compilation's records were taken from; `secondary:` lists later publications
## that re-used the data. `source` and `secondary` each hold a list of
## references, one `- key:` item per reference, unnumbered.
##
## Three steps, each safe to run again on a file it has already changed:
##
##   1. `traits.build::metadata_convert_sources()` converts the shape. It edits
##      the files as text, so every line of every reference is kept; only the
##      headings are renamed and moved. It checks each file still holds exactly
##      the references it did.
##   2. `moves`: references that `source_id` values point at are sources, not
##      secondary references, so are moved from `secondary` to `source`.
##   3. `rekeys`: the dataset's own reference takes the `dataset_id` as its key,
##      since every record's `source_id` defaults to the `dataset_id`.
##
## The other fixes in this migration (the NHNSW `source_id` mappings, values
## recoded to the keys they meant in Cornwell_2018, Togashi_2015, Wenk_2023_2
## and Simpson_2021, `Unpublished` entries for Choat_2012 and Davis_2026, and
## `Unpublished` dataset references for Westoby_2014 and Williams_2011) were
## made by hand. So were the keys that named different papers in different
## datasets: Simpson_2021's `Cook_2005` became `Cook_2005_2`, Togashi_2015's
## `OGrady_2006` `OGrady_2006_2`, Prendergast_2026_2's `Prendergast_2021_2`
## `Prendergast_2021_4`, and Schmidt_2003's `Schmidt_1997` `Schmidt_1997_2`
## (with the `source_id` values naming them); and the compilations' copies of
## `Crowley_2007` (Nano_2011, Simpson_2021) and `Prior_2000` (Togashi_2015)
## were replaced by those datasets' own entries.
##
## Run from the repository root, with traits.build >= 2.2.0.9000 installed from
## the source-references branch:
##
##     Rscript scripts/migrate_sources_2026-10.R                 # every dataset
##     Rscript scripts/migrate_sources_2026-10.R Ottaviani_2015  # just one

suppressPackageStartupMessages(library(traits.build))

# dataset_id -> keys moved from `secondary` to `source`: the references that the
# dataset's `source_id` values point at, so records are taken from them
# (traits.build #296 audit, 2026-10-20). "*" moves every `secondary` reference.
moves <- list(
  SPRAT_2025_2       = "*",   # 40 floras and databases gap-filled from, named per record
  Togashi_2015       = "*",   # 28 published studies the compiled LA:SA values come from
  Elliott_2026       = "*",   # 17 floral-visitor studies, named per record
  Prendergast_2026_2 = "*",   # 11 of the compilation's sources that have entries
  Cornwell_2018      = "*",   # 4 studies some records come from
  Bell_1995          = "Dixon_1995",
  Li_1997            = "Li_1993",
  Tsujii_2023        = "Tsujii_2023_2",
  Davis_2026         = "Phillips_2015",
  RadfordSmith_2024  = "Harden_2018",
  Sauquet_2026       = "*",   # 27 pollination studies, named per record
  Prendergast_2026_3 = "*"    # the 3 of the compilation's sources that have entries
)

# Datasets whose own reference was keyed other than by the `dataset_id`
# (traits.build #296 review, 2026-11-10). Most were written by an earlier
# `metadata_add_source_bibtex()`, which title-cased and truncated the key;
# Bryant_2021_2/_3 and Stephens_2024_2 had taken another dataset's key for a
# different paper, so `sources` held only one of them. Gallagher_2011_1/_3 cite
# one paper and should probably be combined. Wenk_2025 was keyed `Wenk_2026`.
# Westoby_2014 (keyed `Gleason_2012`) and Williams_2011 (keyed `Duncan_2011`)
# are unpublished compilations, so each gained an `Unpublished` dataset
# reference by hand, the paper that was there moving to `secondary`.
rekeys <- c(
  "Bryant_2021_2", "Bryant_2021_3", "Stephens_2024_2", "McQuillan_2020",
  "Gallagher_2011_1", "Gallagher_2011_3", "Wenk_2025"
)

# The extent of the `source:` block, and its entries by heading
source_block <- function(lines) {
  start <- which(lines == "source:")
  later <- which(grepl("^[^[:space:]#]", lines) & seq_along(lines) > start)
  end <- if (length(later)) min(later) - 1 else length(lines)
  block <- lines[(start + 1):end]
  heading_i <- grep("^  [A-Za-z_0-9]+:", block)
  heading_end <- c(heading_i[-1] - 1, length(block))
  groups <- Map(function(s, e) block[s:e], heading_i, heading_end)
  names(groups) <- sub("^  ([A-Za-z_0-9]+):.*$", "\\1", block[heading_i])
  list(start = start, end = end, groups = groups)
}

# Move the list items with these keys from `secondary` to `source`, creating
# the `source` heading if needed. Keys already under `source` are left alone
move_keys <- function(lines, keys) {
  b <- source_block(lines)
  groups <- b$groups
  if (is.null(groups[["secondary"]])) return(lines)

  src <- groups[["secondary"]]
  item_i <- grep("^  - ", src)
  item_end <- c(item_i[-1] - 1, length(src))
  items <- Map(function(s, e) src[s:e], item_i, item_end)
  item_keys <- sub("^  - key:\\s*", "", src[item_i])

  moving <- item_keys %in% keys
  if (!any(moving)) return(lines)

  groups[["secondary"]] <- c(src[1], unlist(items[!moving]))
  if (length(items[!moving]) == 0) groups[["secondary"]] <- NULL
  if (is.null(groups[["source"]])) groups[["source"]] <- "  source:"
  groups[["source"]] <- c(groups[["source"]], unlist(items[moving]))

  order <- c(intersect(c("dataset", "source", "secondary"), names(groups)),
             setdiff(names(groups), c("dataset", "source", "secondary")))
  c(lines[seq_len(b$start)], unlist(groups[order], use.names = FALSE),
    if (b$end < length(lines)) lines[(b$end + 1):length(lines)])
}

# Key the dataset's own reference by its `dataset_id`
rekey <- function(lines, dataset_id) {
  i <- which(lines == "  dataset:")
  if (length(i) != 1 || !grepl("^    key: ", lines[i + 1])) {
    stop(dataset_id, ": no `dataset:` reference with `key` as its first field")
  }
  lines[i + 1] <- paste0("    key: ", dataset_id)
  lines
}

migrate <- function(dataset_id) {
  path <- file.path("data", dataset_id, "metadata.yml")

  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  new <- lines

  keys_to_move <- moves[[dataset_id]]
  if (identical(keys_to_move, "*")) {
    flat <- util_flatten_sources(yaml::read_yaml(text = paste(new, collapse = "\n"))[["source"]])
    keys_to_move <- flat$key[flat$type == "secondary"]
  }
  if (length(keys_to_move) > 0) new <- move_keys(new, keys_to_move)
  if (dataset_id %in% rekeys) new <- rekey(new, dataset_id)

  if (identical(new, lines)) return(invisible(FALSE))
  writeLines(new, path, useBytes = TRUE)
  # Strict read: the result is what the build will accept
  util_flatten_sources(read_metadata(path)[["source"]])
  invisible(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
dataset_ids <- if (length(args)) args else
  sort(basename(dirname(Sys.glob("data/*/metadata.yml"))))

# 1. Shape
metadata_convert_sources(dataset_ids)

# 2 and 3. Moves and rekeys
changed <- vapply(dataset_ids, migrate, logical(1))
message(sum(changed), " of ", length(dataset_ids), " datasets had references moved or rekeyed")

# What still needs a curator (traitecoevo/austraits.build#879)
issues <- metadata_convert_sources(dataset_ids)
issues <- issues[!is.na(issues$issues), ]
if (nrow(issues) > 0) {
  message("Still to resolve by hand:")
  for (i in seq_len(nrow(issues))) message("  ", issues$dataset_id[i], ": ", issues$issues[i])
}
