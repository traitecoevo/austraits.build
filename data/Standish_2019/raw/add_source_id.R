# Give Standish_2019's values their sources (#888):
#
#     Rscript data/Standish_2019/raw/add_source_id.R
#
# Each trait has its own source column (`FR data source`, `SS data source`,
# `SM data source`, `NAS data source`), already mapped to that trait's
# `measurement_remarks`. A value taken from one published work gets that work's
# key in a matching `*_source_id` column of data.csv, named by the trait's
# `source_id` in metadata.yml (NAS also serves `nitrogen_fixing`, which is derived
# from it). The rest cite the dataset, with the source column kept as a remark:
#
#   - values inferred for a genus or family, defaults and approximations
#     ("Burrows et al. 2008 for Genus", "Inferred using data for Family",
#     "Approximation: avg seed mass of ..."), and lists of several works -- the
#     literature is evidence for Standish's scoring, not the value's source;
#   - unpublished records: "Alcoa DB", "Weighed", "Nursery records",
#     "Standish et al. unpub data", "Anne Cochrane", "John Dwyer",
#     "Brundrett pers. comm.".
#
# raw/references.csv holds the works, from the "Notes and literature" sheet of
# the raw spreadsheet (AusTraits_jarrah forest.xlsx), with DOIs from CrossRef.
# "Kew SID" seed masses are removed by the raw R code (they are in Kew_2019_1).

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/Standish_2019"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE) |>
  select(-any_of(c("FR_source_id", "SS_source_id", "SM_source_id", "NAS_source_id")))
refs <- read_csv(file.path(dir, "raw/references.csv"), col_types = cols(.default = col_character()),
                 na = "", show_col_types = FALSE)

not_a_source <- regex(paste(
  "for (the )?Genus|for Family|nfer|Assum|Default|Approx|Appox|Avg of|Estimated|unpub|pers\\. ?comm",
  "Weighed|Nursery|Alcoa|Standish|Cochrane|Dwyer|^Kew SID$|;|/|not listed", sep = "|"), ignore_case = TRUE)
key_of_citation <- function(x) {
  x <- str_squish(str_remove(x, "^(NM: ?|Not root clusters; )"))
  ifelse(is.na(x) | x %in% c("", "NA") | str_detect(x, not_a_source), NA_character_,
         coalesce(refs$key, refs$same_as, ifelse(is.na(refs$site), NA, paste0(refs$site, "_", refs$year)))[match(x, refs$citation)])
}
cols <- c(FR_source_id = "FR data source", SS_source_id = "SS data source",
          SM_source_id = "SM data source", NAS_source_id = "NAS data source")
for (nm in names(cols)) d[[nm]] <- key_of_citation(d[[cols[[nm]]]])

# every citation is either a source or deliberately not one
unread <- unique(unlist(map(cols, \(cl) {
  x <- str_squish(str_remove(d[[cl]], "^(NM: ?|Not root clusters; )"))
  x[!is.na(x) & !x %in% c("", "NA") & !str_detect(x, not_a_source) & !x %in% refs$citation]
})))
if (length(unread) > 0) stop("citations not in raw/references.csv: ", paste(unread, collapse = " | "))

# ---- entries, checked against AusTraits ----
index <- source_reference_index(exclude = "Standish_2019")
fields <- c("bibtype", "year", "author", "title", "journal", "volume", "number", "pages",
            "institution", "type", "publisher", "doi", "url", "note")
own <- refs |> filter(!is.na(key))
entries <- map(split(own, seq_len(nrow(own))), function(r) {
  e <- as.list(r[fields]); e <- e[!is.na(unlist(e))]; e$year <- as.integer(e$year)
  source_clean_entry(c(list(key = r$key), e))
})
sites <- refs |> filter(!is.na(site))
site_entries <- map(seq_len(nrow(sites)), function(i) {
  e <- index |> filter(key == "WAH_2023_1") |> pull(entry) |> (\(x) x[[1]])()
  e$key <- paste0(sites$site[i], "_", sites$year[i]); e$year <- as.integer(sites$year[i]); e$note <- NULL
  source_clean_entry(e)
})
same <- unique(na.omit(refs$same_as))
entries <- c(entries, site_entries, map(same, \(k) index |> filter(key == k) |> pull(entry) |> (\(x) x[[1]])()))
names(entries) <- map_chr(entries, "key")
cand <- tibble(key = names(entries),
               year = map_chr(entries, \(e) as.character(e$year %||% NA)),
               title = map_chr(entries, \(e) as.character(e$title %||% NA)),
               doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
               volume = map_chr(entries, \(e) as.character(e$volume %||% NA)))
site_keys <- map_chr(site_entries, "key")
resolved <- source_resolve_keys(cand, index, keep = site_keys, same_as = setNames(same, same))
key_map <- setNames(resolved$key_final, resolved$key)
final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
for (nm in names(cols)) d[[nm]] <- unname(key_map[d[[nm]]])

# only references a kept value cites
if (!file.exists(file.path(dir, "sources.yml"))) writeLines("[]", file.path(dir, "sources.yml"))
m <- traits.build::read_metadata(file.path(dir, "metadata.yml"))
kept <- traits.build:::process_custom_code(m$dataset$custom_R_code)(
  d |> mutate(across(everything(), ~ na_if(na_if(.x, ""), "NA"))))
value_col <- c(FR_source_id = "Fire Response", SS_source_id = "Seed storage",
               SM_source_id = "Seed mass (mg)", NAS_source_id = "Nutrient-acquisition strategy")
used <- unique(na.omit(unlist(map(names(value_col), \(nm) kept[[nm]][!is.na(kept[[value_col[[nm]]]])]))))
final <- final[map_chr(final, "key") %in% used]
final <- final[order(map_chr(final, "key"))]
problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

write_csv(d, file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
print(resolved |> filter(action != "new" | key != key_final))
cat(length(final), "references written\n")
print(map_int(names(cols), \(nm) sum(!is.na(d[[nm]]))) |> setNames(names(cols)))
