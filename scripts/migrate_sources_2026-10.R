## Migrate the `source:` block of every `data/*/metadata.yml` to the shape
## traits.build reads from 2.3.0 (traits.build #296, PR #298): `secondary:` and
## `original_source:` each hold a list of references, one `- key:` item per
## reference, in place of the numbered `secondary_01` / `original_01` entries,
## and `original` is renamed `original_source`.
##
## The files are edited as text, so nothing outside the `source:` block changes
## and the block keeps every line of every reference byte for byte; only the
## entry headers move. (`write_metadata()` would re-serialise the whole file.)
## Entries of one type that were interleaved with another are gathered under
## their heading in their original relative order.
##
## Run from the repository root, after installing traits.build >= 2.2.0.9000:
##
##     Rscript scripts/migrate_sources_2026-10.R                 # every dataset
##     Rscript scripts/migrate_sources_2026-10.R Ottaviani_2015  # just one
##
## `moves` below lists the references that `source_id` values point at and
## that therefore belong under `original_source`, not `secondary`; they are
## moved after the conversion. Each file is checked afterwards: read through
## `traits.build::util_flatten_sources()`, the new block must hold exactly the
## references the old one did, in the same order within each type.

suppressPackageStartupMessages(library(traits.build))

# dataset_id -> keys moved from `secondary` to `original_source`: the references
# that the dataset's `source_id` values point at, so records are taken from them
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

convert_source_block <- function(lines) {
  start <- which(lines == "source:")
  if (length(start) != 1) stop("expected one `source:` line, found ", length(start))
  later <- which(grepl("^[A-Za-z_]", lines) & seq_along(lines) > start)
  end <- if (length(later)) min(later) - 1 else length(lines)
  block <- lines[(start + 1):end]

  slot_i <- grep("^  [A-Za-z_0-9]+:\\s*$", block)
  slot_names <- sub("^  ([A-Za-z_0-9]+):\\s*$", "\\1", block[slot_i])
  entry_end <- c(slot_i[-1] - 1, length(block))
  entries <- Map(function(s, e) block[s:e], slot_i, entry_end)

  types <- ifelse(slot_names == "primary", "primary", sub("_[0-9]+$", "", slot_names))
  types[types == "original"] <- "original_source"
  is_list <- vapply(entries, function(e) length(e) > 1 && grepl("^  - ", e[2]), logical(1))

  out <- character()
  for (type in unique(types)) {
    idx <- which(types == type)
    if (type == "primary") {
      for (k in idx) out <- c(out, entries[[k]])
      next
    }
    out <- c(out, sprintf("  %s:", type))
    for (k in idx) {
      body <- entries[[k]][-1]
      if (!is_list[k]) {
        if (!grepl("^    key:", body[1])) stop("entry `", slot_names[k], "`: first field is not `key`")
        body[1] <- sub("^    ", "  - ", body[1])
      }
      out <- c(out, body)
    }
  }
  c(lines[seq_len(start)], out, if (end < length(lines)) lines[(end + 1):length(lines)])
}

# Move the list items with these keys from one heading to another, creating the
# destination heading at the end of the block if needed
move_keys <- function(lines, keys, from = "secondary", to = "original_source") {
  start <- which(lines == "source:")
  later <- which(grepl("^[A-Za-z_]", lines) & seq_along(lines) > start)
  end <- if (length(later)) min(later) - 1 else length(lines)
  block <- lines[(start + 1):end]

  heading_i <- grep("^  [A-Za-z_0-9]+:\\s*$", block)
  heading_names <- sub("^  ([A-Za-z_0-9]+):\\s*$", "\\1", block[heading_i])
  heading_end <- c(heading_i[-1] - 1, length(block))
  groups <- Map(function(s, e) block[s:e], heading_i, heading_end)
  names(groups) <- heading_names

  if (!from %in% names(groups)) stop("no `", from, ":` heading to move from")
  src <- groups[[from]]
  item_i <- grep("^  - ", src)
  item_end <- c(item_i[-1] - 1, length(src))
  items <- Map(function(s, e) src[s:e], item_i, item_end)
  item_keys <- sub("^  - key:\\s*", "", src[item_i])
  missing <- setdiff(keys, item_keys)
  if (length(missing)) stop("not under `", from, "`: ", paste(missing, collapse = ", "))

  moving <- item_keys %in% keys
  groups[[from]] <- c(src[1], unlist(items[!moving]))
  if (length(items[!moving]) == 0) groups[[from]] <- NULL
  if (!to %in% names(groups)) groups[[to]] <- sprintf("  %s:", to)
  groups[[to]] <- c(groups[[to]], unlist(items[moving]))

  # `primary` first, then the other headings in their existing order
  order <- c("primary", setdiff(names(groups), "primary"))
  c(lines[seq_len(start)], unlist(groups[order], use.names = FALSE),
    if (end < length(lines)) lines[(end + 1):length(lines)])
}

# The references a block holds, per type, read leniently so the old shape
# can be compared with the new
references <- function(path) {
  flat <- util_flatten_sources(yaml::read_yaml(path)[["source"]], strict = FALSE)
  split(flat$entry, flat$type)
}

migrate <- function(dataset_id) {
  path <- file.path("data", dataset_id, "metadata.yml")
  before <- references(path)
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  # Old shape: a numbered entry, `original`, or `secondary` as a single mapping
  sec <- which(lines == "  secondary:")
  old_shape <-
    any(grepl("^  (secondary|original|original_source)_[0-9]+:\\s*$", lines)) ||
    any(lines == "  original:") ||
    (length(sec) == 1 && !grepl("^  - ", lines[sec + 1]))
  if (!old_shape && is.null(moves[[dataset_id]])) return(invisible(FALSE))

  new <- if (old_shape) convert_source_block(lines) else lines
  keys_to_move <- moves[[dataset_id]]
  if (identical(keys_to_move, "*")) {
    flat <- util_flatten_sources(yaml::read_yaml(text = paste(new, collapse = "\n"))[["source"]], strict = FALSE)
    keys_to_move <- flat$key[flat$type == "secondary"]
    if (length(keys_to_move) == 0) return(invisible(FALSE))
  }
  if (!is.null(keys_to_move)) new <- move_keys(new, keys_to_move)
  writeLines(new, path, useBytes = TRUE)

  after <- references(path)
  expected <- before
  for (key in keys_to_move) {
    i <- which(vapply(expected$secondary, function(e) identical(e$key, key), logical(1)))
    if (length(i) != 1) stop(dataset_id, ": key to move not found once under secondary: ", key)
    expected$original_source <- c(expected$original_source, expected$secondary[i])
    expected$secondary <- expected$secondary[-i]
    if (length(expected$secondary) == 0) expected$secondary <- NULL
  }
  for (type in union(names(expected), names(after))) {
    if (!identical(expected[[type]], after[[type]])) {
      stop(dataset_id, ": `", type, "` references differ after conversion")
    }
  }
  # Strict read: the result is what the build will accept
  util_flatten_sources(yaml::read_yaml(path)[["source"]])
  invisible(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
dataset_ids <- if (length(args)) args else
  sort(basename(dirname(Sys.glob("data/*/metadata.yml"))))
converted <- vapply(dataset_ids, migrate, logical(1))
message(sum(converted), " of ", length(dataset_ids), " datasets converted")
