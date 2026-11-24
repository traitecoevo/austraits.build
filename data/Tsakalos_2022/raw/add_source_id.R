# Add `source_id` and `source_remark` columns to data/Tsakalos_2022/data.csv and
# write the references they name to data/Tsakalos_2022/sources.yml (#888):
#
#     Rscript data/Tsakalos_2022/raw/add_source_id.R
#
# `Reference` (Table S1 of the supplement) gives, per species, one or more of:
# a short citation ("D'Agui (2017)"), a Florabase profile, herbarium specimens
# ("Banksia elegans Meisn.; S.D. Hopper 6355 PERTH 09204008"), the authors'
# field collections ("Field collection LM180919/34"), a personal communication,
# or "Inferred from similar ...", joined by ";". Each record is then
#
#   - one work, or Florabase, alone: `source_id` is its key. The works are in
#     raw/references.csv (from the supplement's reference list, DOIs from
#     CrossRef; a work already in AusTraits gives only its key, `same_as`), and
#     raw/citation_lookup.csv maps each short citation to one. Florabase,
#     accessed in October 2021, is WAH_2021; the printed Flora of Australia
#     volumes are ABRS_1995 (16), ABRS_1999 (17B) and ABRS_2000 (17A);
#   - field collections only: the authors' own observations; cites the
#     dataset, no remark;
#   - inferred from a similar species: cites the dataset, no remark;
#   - anything else (herbarium specimens, a personal communication, or several
#     sources together): cites the dataset, with `Reference` as
#     `measurement_remarks` (column `source_remark`).

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr); library(tidyr)})
source("ignore/source_references.R")

dir <- "data/Tsakalos_2022"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE) |>
  select(-any_of(c("source_id", "source_remark")))
refs <- read_csv(file.path(dir, "raw/references.csv"), col_types = cols(.default = col_character()),
                 na = "", show_col_types = FALSE)
cites <- read_csv(file.path(dir, "raw/citation_lookup.csv"), col_types = cols(.default = col_character()),
                  na = "", show_col_types = FALSE)

norm_cit <- function(x) x |> str_replace_all("’", "'") |> str_replace_all(",\\s*\\(", " (") |>
  str_remove("\\.$") |> str_squish()
pieces <- d |> mutate(.row = row_number()) |> select(.row, Reference) |>
  separate_rows(Reference, sep = ";") |> mutate(p = norm_cit(Reference)) |> filter(p != "", p != "NA") |>
  mutate(kind = case_when(
    str_detect(p, "Florabase") ~ "site",
    p %in% cites$citation ~ "work",
    str_detect(p, "^Field collection") ~ "field",
    str_detect(p, "pers comm") ~ "pers comm",
    str_detect(p, "^Inferred from") ~ "inferred",
    str_detect(p, "\\((19|20)\\d\\d\\)") ~ "unmatched citation",
    TRUE ~ "specimen"),
    id = case_when(kind == "site" ~ "WAH", kind == "work" ~ cites$id[match(p, cites$citation)]))
if (any(pieces$kind == "unmatched citation")) {
  print(distinct(filter(pieces, kind == "unmatched citation"), p)); stop("citations not in raw/citation_lookup.csv")
}
rows <- pieces |> group_by(.row) |>
  summarise(ids = list(unique(na.omit(id))), kinds = list(unique(kind)), .groups = "drop") |>
  mutate(n_id = lengths(ids),
         class = case_when(map_lgl(kinds, ~ "inferred" %in% .x) ~ "inferred",
                           n_id == 1 & map_lgl(kinds, ~ all(.x %in% c("work", "site"))) ~ "one source",
                           map_lgl(kinds, ~ all(.x == "field")) ~ "field",
                           TRUE ~ "remark"),
         id = ifelse(class == "one source", map_chr(ids, ~ c(.x, NA_character_)[1]), NA_character_))
d <- d |> mutate(.row = row_number()) |> left_join(select(rows, .row, class, id), by = ".row") |>
  mutate(class = coalesce(class, "none"))

# ---- entries, checked against AusTraits ----
index <- source_reference_index(exclude = "Tsakalos_2022")
entry_of <- function(key) {
  e <- index |> filter(key == !!key) |> arrange(type != "dataset") |> pull(entry)
  if (length(e) == 0) stop("no reference with key ", key)
  e[[1]]
}
fields <- c("bibtype", "year", "author", "title", "booktitle", "editor", "journal", "volume", "number",
            "pages", "publisher", "place", "institution", "type", "isbn", "doi", "url", "note")
used <- refs |> filter(id %in% d$id)
own <- used |> filter(!is.na(key))
same <- unique(na.omit(used$same_as))
entries <- c(
  map(split(own, seq_len(nrow(own))), function(r) {
    e <- as.list(r[fields]); e <- e[!is.na(unlist(e))]; e$year <- as.integer(e$year)
    source_clean_entry(c(list(key = r$key), e))
  }),
  map(same, entry_of)
)
names(entries) <- map_chr(entries, "key")
cand <- tibble(key = names(entries),
               year = map_chr(entries, \(e) as.character(e$year %||% NA)),
               title = map_chr(entries, \(e) as.character(e$title %||% NA)),
               doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
               volume = map_chr(entries, \(e) as.character(e$volume %||% NA)))
site_keys <- if ("WAH" %in% d$id) "WAH_2021" else character()
resolved <- source_resolve_keys(cand, index, keep = c("ABRS_1995", "ABRS_1999", "ABRS_2000", "CPBR_2006"),
                                same_as = setNames(c(same, site_keys), c(same, site_keys)))
# Florabase is the WAH_2021 site entry, by convention
if (length(site_keys)) {
  entries$WAH_2021 <- entry_of("WAH_2021")
  resolved <- bind_rows(resolved, tibble(key = "WAH_2021", key_final = "WAH_2021", action = "copy",
                                         copy_from = index$dataset_id[match("WAH_2021", index$key)]))
}
key_map <- setNames(resolved$key_final, resolved$key)
final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
final <- final[!duplicated(map_chr(final, "key"))]
final <- final[order(map_chr(final, "key"))]
problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

id_key <- c(with(filter(refs, is.na(site)), setNames(coalesce(key, same_as), id)), WAH = "WAH_2021")
d <- d |> mutate(source_id = coalesce(unname(key_map[unname(id_key[id])]), ""),
                 source_remark = ifelse(class == "remark", str_squish(Reference), ""))

write_csv(d |> select(-.row, -class, -id), file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note) |> arrange(source_id),
          file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action))
print(resolved |> filter(action == "rename" | key != key_final) |> select(key, key_final, action, copy_from, note), width = 200)
print(count(d, class))
cat(length(final), "references written;", sum(d$source_id == ""), "of", nrow(d), "rows cite the dataset\n")
