# Add a `source_id` column to data/Stephens_2023/data.csv and write the
# references it names to data/Stephens_2023/sources.yml (#888):
#
#     Rscript data/Stephens_2023/raw/add_source_id.R
#
# Each record cites its source in `reference` (an APA citation, a website, or
# "TRY- <databases>"). raw/source_lookup.csv holds one row per distinct
# citation with either
#
#   - `site`: an online flora or database, keyed by its code and the year the
#     record was entered (`entry_date`), as the other compilations do:
#     iNaturalist_2022, WAH_2022_2 (Florabase), POWO_2022, Efloras_2022,
#     ABRS_2022, ATRP_2022, Floramalesiana_2021; the entry is the site's
#     reference elsewhere in AusTraits, re-dated and without its access date; or
#   - `key` and the reference's fields: a published work, keyed first-author
#     surname + year. `origin` is `crossref` (CrossRef metadata for the
#     citation's DOI or title, with the authors read from the citation) or
#     `manual`. "TRY- BASECO" and "TRY- BiolFlor" take the database's paper
#     (Gachet_2005, Kuhn_2004); or
#   - nothing, `origin` "several works: cites the dataset": the citation names
#     several works (several TRY databases, or references joined by ";"). The
#     record cites the dataset and keeps the citation as `measurement_remarks`
#     (derived in custom_R_code).
#
# Every key is then checked against the references already in AusTraits
# (ignore/source_references.R): the same work takes its existing key and
# entry, and a key naming a different work takes the next free suffix.
#
# source_lookup.csv was drafted once from the citations (CrossRef queries and
# hand-written rows), then checked by hand; edit it directly to fix a reference.

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/Stephens_2023"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE) |>
  select(-any_of("source_id"))
lookup <- read_csv(file.path(dir, "raw/source_lookup.csv"), col_types = cols(.default = col_character()),
                   na = "", trim_ws = FALSE, show_col_types = FALSE)

index <- source_reference_index(exclude = "Stephens_2023")
entry_of <- function(key) {
  e <- index |> filter(key == !!key) |> arrange(type != "dataset") |> pull(entry)
  if (length(e) == 0) stop("no reference with key ", key)
  e[[1]]
}

# site code -> the entry other years copy
sites <- c(iNaturalist = "iNaturalist_2026", WAH = "WAH_2023_1", POWO = "POWO_2026",
           Efloras = "Efloras_2010", ABRS = "ABRS_2023", ATRP = "ATRP_2020",
           Floramalesiana = "Floramalesiana_2014")
site_key_recode <- c(WAH_2022 = "WAH_2022_2", WAH_2023 = "WAH_2023_1")
site_entry <- function(site, key, year) {
  e <- entry_of(sites[[site]])
  e$key <- key; e$year <- as.integer(year); e$note <- NULL
  source_clean_entry(e)
}

fields <- c("bibtype", "year", "author", "title", "booktitle", "editor", "journal", "volume",
            "number", "pages", "publisher", "place", "institution", "type", "doi", "url", "note")
work_entry <- function(r) {
  e <- as.list(r[intersect(fields, names(r))])
  e <- e[!is.na(unlist(e))]
  e$year <- as.integer(e$year)
  for (f in intersect(c("author", "title", "booktitle", "editor", "journal", "publisher"), names(e))) {
    e[[f]] <- source_clean_text(e[[f]])
  }
  if (!is.null(e$title)) e$title <- source_clean_title(e$title)
  source_clean_entry(c(list(key = r$key), e))
}

d <- d |>
  mutate(.ref_sq = str_squish(reference), .year = str_extract(entry_date, "[0-9]{4}$")) |>
  left_join(lookup |> transmute(.ref_sq = str_squish(reference), .site = site, .key = key,
                                .same_as = same_as, .origin = origin) |>
              distinct(.ref_sq, .keep_all = TRUE), by = ".ref_sq") |>
  mutate(.key = ifelse(!is.na(.site), paste0(.site, "_", .year), coalesce(.key, .same_as)),
         .key = coalesce(unname(site_key_recode[.key]), .key))

missing <- d |> filter(is.na(.key), !str_detect(coalesce(.origin, ""), "^several works"))
if (nrow(missing) > 0) { print(distinct(missing, reference)); stop("references with no row in raw/source_lookup.csv") }

site_keys <- d |> filter(!is.na(.site)) |> distinct(.key, .site, .year)
work_rows <- lookup |> filter(is.na(site), is.na(same_as), !is.na(key)) |>
  arrange(is.na(bibtype)) |> distinct(key, .keep_all = TRUE)
same_as_keys <- unique(na.omit(lookup$same_as))
entries <- c(
  pmap(site_keys, \(.key, .site, .year) site_entry(.site, .key, .year)),
  map(split(work_rows, seq_len(nrow(work_rows))), work_entry),
  map(same_as_keys, entry_of)
)
names(entries) <- map_chr(entries, "key")

# ---- Check every key against AusTraits ----
cand <- tibble(
  key = names(entries),
  year = map_chr(entries, \(e) as.character(e$year %||% NA)),
  title = map_chr(entries, \(e) as.character(e$title %||% NA)),
  doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
  volume = map_chr(entries, \(e) as.character(e$volume %||% NA))
)
resolved <- source_resolve_keys(cand, index, keep = site_keys$.key,
                                same_as = setNames(same_as_keys, same_as_keys))
existing_site <- resolved$key %in% site_keys$.key & resolved$key %in% index$key
resolved$action[existing_site] <- "copy"
resolved$key_final[existing_site] <- resolved$key[existing_site]
resolved$copy_from[existing_site] <- map_chr(resolved$key[existing_site], \(k)
  index |> filter(key == k) |> arrange(type != "dataset") |> pull(dataset_id) |> first())
resolved$note[existing_site] <- NA_character_

final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
final <- final[!duplicated(map_chr(final, "key"))]
final <- final[order(map_chr(final, "key"))]
problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

key_map <- setNames(resolved$key_final, resolved$key)
d <- d |> mutate(source_id = coalesce(unname(key_map[.key]), "")) |> select(-starts_with("."))

write_csv(d, file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note) |> arrange(source_id),
          file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action))
print(resolved |> filter(action != "new" & !key %in% site_keys$.key | key != key_final), n = Inf, width = 200)
cat(length(final), "references written;", sum(d$source_id == ""), "of", nrow(d), "rows cite the dataset\n")
