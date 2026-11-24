# Add a `source_id` column to data/Gross_2005/data.csv and write the references
# it names to data/Gross_2005/sources.yml (#888):
#
#     Rscript data/Gross_2005/raw/add_source_id.R
#
# `Source` gives, per species, a short citation ("Hyland et al. (2003)",
# "Weston (1995)"), a herbarium voucher ("Gray 4407, QRS"), a personal
# communication, or a note ("estimated"). raw/source_lookup.csv classifies each
# distinct `Source` (`kind`):
#
#   - source: a work in raw/references.csv (`id`). Short citations that name
#     several works in the Literature Cited of the data supplement ("Weston
#     (1995)" for Hicksbeachia, Athertonia and Catalepidia; "Hewson (1985)" for
#     Burseraceae and Simaroubaceae) are resolved by the record's genus
#     (`genus`). Two of the supplement's labels are swapped in the data:
#     "Cayzer, Crisp & Telford (2000)" for Auranticarpa is the supplement's
#     2000b, and "(2000b)" for Pittosporum and Citriobatus is its 2000a.
#   - specimen, personal communication: unpublished; the record cites the
#     dataset and keeps `Source` as `measurement_remarks` (column
#     `source_remark`);
#   - several works, unidentified: likewise cites the dataset with `Source` as
#     a remark (`note` says why a citation is unidentified);
#   - inferred or unresolved: a value estimated from the genus or family, or not
#     resolved; cites the dataset, no remark.
#
# raw/references.csv holds the works, from the supplement's Literature Cited
# (raw/Gross 2005 Supp info ajb20907-sup-0001.pdf), with DOIs from CrossRef; a
# work already in AusTraits gives only its key (`same_as`). Each key is checked
# against AusTraits (ignore/source_references.R).

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/Gross_2005"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE) |>
  select(-any_of(c("source_id", "source_remark")))
refs <- read_csv(file.path(dir, "raw/references.csv"), col_types = cols(.default = col_character()),
                 na = "", show_col_types = FALSE)
lookup <- read_csv(file.path(dir, "raw/source_lookup.csv"), col_types = cols(.default = col_character()),
                   na = "", show_col_types = FALSE)

d <- d |> mutate(.cit = str_squish(Source), .genus = word(str_squish(Species), 1))
m <- d |>
  left_join(lookup |> filter(!is.na(genus)) |> select(.cit = citation, .genus = genus, .id = id, .kind = kind),
            by = c(".cit", ".genus")) |>
  left_join(lookup |> filter(is.na(genus)) |> select(.cit = citation, .id2 = id, .kind2 = kind), by = ".cit") |>
  mutate(.id = coalesce(.id, .id2), .kind = coalesce(.kind, .kind2))
unread <- m |> filter(.cit != "", is.na(.kind))
if (nrow(unread) > 0) { print(distinct(unread, .cit, .genus)); stop("citations not in raw/source_lookup.csv") }

# ---- entries, checked against AusTraits ----
index <- source_reference_index(exclude = "Gross_2005")
entry_of <- function(key) {
  e <- index |> filter(key == !!key) |> arrange(type != "dataset") |> pull(entry)
  if (length(e) == 0) stop("no reference with key ", key)
  e[[1]]
}
fields <- c("bibtype", "year", "author", "title", "booktitle", "editor", "journal", "volume",
            "number", "pages", "publisher", "place", "doi", "url", "note")
used_ids <- unique(na.omit(m$.id))
own <- refs |> filter(id %in% used_ids, !is.na(key))
same <- refs |> filter(id %in% used_ids, !is.na(same_as))
entries <- c(
  map(split(own, seq_len(nrow(own))), function(r) {
    e <- as.list(r[fields]); e <- e[!is.na(unlist(e))]; e$year <- as.integer(e$year)
    source_clean_entry(c(list(key = r$key), e))
  }),
  map(unique(same$same_as), entry_of)
)
names(entries) <- map_chr(entries, "key")
cand <- tibble(key = names(entries),
               year = map_chr(entries, \(e) as.character(e$year %||% NA)),
               title = map_chr(entries, \(e) as.character(e$title %||% NA)),
               doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
               volume = map_chr(entries, \(e) as.character(e$volume %||% NA)))
resolved <- source_resolve_keys(cand, index, same_as = setNames(unique(same$same_as), unique(same$same_as)))
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

id_key <- setNames(coalesce(refs$key, refs$same_as), refs$id)
d$source_id <- coalesce(unname(key_map[unname(id_key[m$.id])]), "")
remark_kinds <- c("specimen", "personal communication", "several works", "unidentified")
d$source_remark <- ifelse(m$.kind %in% remark_kinds, m$.cit, "")

write_csv(d |> select(-starts_with(".")), file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note) |> arrange(source_id),
          file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action))
print(resolved |> filter(action == "rename" | (action == "copy" & key != key_final)) |> select(key, key_final, action, copy_from, note), n = Inf, width = 200)
print(count(m, .kind))
cat(length(final), "references written;", sum(d$source_id == ""), "of", nrow(d), "rows cite the dataset\n")
