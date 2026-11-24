# Build data/McCarthy_2017_2/data.csv and sources.yml:
#
#     Rscript data/McCarthy_2017_2/raw/make_data.R
#
# McCarthy_2017 submitted, besides his own measurements (McCarthy_2017), 5,492
# species values harvested from the literature
# (data/McCarthy_2017/raw/JMcCarthy_AustraitsSubmission_harvestedData.csv).
# Each non-height value was compared with everything AusTraits holds -- built
# values, excluded values, the unfiltered data.csv of every dataset with these
# traits, and datasets since deleted, as individual values and as species means
# (2,999 of 4,048 match, or match within 10%). The 237 values of native
# species that matched nothing were vetted by source in
# raw/harvested_candidates_vetted.csv (`decision`, `reason`). This dataset holds
# the `include` rows, each citing its source paper (raw/references.csv).
#
# Trait mapping (`trait_name` in the vetted table): seed mass to seed_dry_mass,
# except French (1991), whose values are fruit masses (fruit_dry_mass), and
# Clarke et al. (2001), a mangrove propagule (diaspore_dry_mass); SLA to
# leaf_mass_per_area; the Smith et al. (1991) timber-handbook densities are
# air-dry densities at 12% moisture content (wood_density_air_dry).
#
# The 487 timber-handbook densities (Cause 1989, Bootle 2005, Boland 2006,
# Poropat 2009 and 2013, Lake 2015 and others) are air-dry densities, about 1.2
# times basic density, and were vetted separately in
# raw/handbook_densities_vetted.csv (ignore/source_survey/mcc7f.R): a value is
# already in AusTraits if it is within 6.5% of a Kingston_1961 air-dry value or
# within 2% of 1.16 times a basic density in Ilic_2000, Zanne_2009 or
# Kooyman_2011 (81% of the handbook values sit at exactly that factor to an
# Ilic_2000 value: those compilations converted the handbook values). Each
# species' handbook values within 6.5% of one another are taken as one value
# (the handbooks copy and round one another): such a group is dropped if any of
# it is in AusTraits, otherwise its earliest source is kept.

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/McCarthy_2017_2"
v <- bind_rows(
  read_csv(file.path(dir, "raw/harvested_candidates_vetted.csv"), col_types = cols(.default = col_character()), show_col_types = FALSE),
  read_csv(file.path(dir, "raw/handbook_densities_vetted.csv"), col_types = cols(.default = col_character()), show_col_types = FALSE))
refs <- read_csv(file.path(dir, "raw/references.csv"), col_types = cols(.default = col_character()), na = "", show_col_types = FALSE)

d <- v |> filter(decision == "include") |>
  mutate(id = map_chr(Citation_full, \(cf) {
    i <- which(map_lgl(refs$citation, \(p) startsWith(str_squish(cf), str_squish(p))))
    if (length(i) != 1) stop("citation not matched once in raw/references.csv: ", cf)
    refs$id[i]
  }))

index <- source_reference_index(exclude = "McCarthy_2017_2")
fields <- c("bibtype", "year", "author", "title", "journal", "volume", "number", "pages", "publisher", "place", "doi")
own <- refs |> filter(id %in% d$id)
entries <- map(split(own, seq_len(nrow(own))), function(r) {
  e <- as.list(r[fields]); e <- e[!is.na(unlist(e))]; e$year <- as.integer(e$year)
  source_clean_entry(c(list(key = r$key), e))
})
names(entries) <- map_chr(entries, "key")
cand <- tibble(key = names(entries),
               year = map_chr(entries, \(e) as.character(e$year %||% NA)),
               title = map_chr(entries, \(e) as.character(e$title %||% NA)),
               doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
               volume = map_chr(entries, \(e) as.character(e$volume %||% NA)))
resolved <- source_resolve_keys(cand, index)
key_map <- setNames(resolved$key_final, resolved$key)
final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
final <- final[order(map_chr(final, "key"))]
problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

out <- d |>
  transmute(taxon_name = Species, trait_name, value = Value,
            source_id = unname(key_map[refs$key[match(id, refs$id)]]),
            wood_moisture = ifelse(trait_name == "wood_density_air_dry", "air-dry, 12% moisture content", NA_character_),
            citation = Citation_author, harvested_row = row) |>
  arrange(taxon_name, trait_name)
write_csv(out, file.path(dir, "data.csv"), na = "")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note), file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action)); print(resolved |> filter(action != "new" | key != key_final) |> select(key, key_final, action, copy_from))
cat(nrow(out), "values,", length(final), "references\n")
