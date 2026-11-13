# Lists seed_dry_mass values in the 2026 seed bank datasets that differ at least
# 10-fold from what other datasets report for the same species
# (traits.build::dataset_check_outlier_by_species), with each value's seed bank
# accession number, and writes them to data/<dataset_id>/raw/seed_dry_mass_outliers.csv.
# Builds only the datasets mapping seed_dry_mass, which are all a value is compared with.
# Run from the repository root.

library(traits.build)
library(dplyr)


study_string <- c("ANBG_2026","SA_Seed_Conservation_Centre_2026","Australian_PlantBank_2026",
  "Brisbane_Botanic_Gardens_2026","Greening_Australia_2026","Tas_Seed_Conservation_Centre_2026",
  "Victorian_Conservation_Seed_Bank_2026","WA_Seed_Centre_2026","Darwin_Botanic_Gardens_2026")

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
taxon_list <- read_csv_char("config/taxon_list.csv")

# Comparison pool: every dataset mapping seed_dry_mass, plus the seed bank datasets
ids <- list.files("data")
ids <- ids[file.exists(file.path("data", ids, "metadata.yml"))]
has_seed_mass <- vapply(ids, function(id)
  any(grepl("seed_dry_mass", readLines(file.path("data", id, "metadata.yml"), warn = FALSE))), logical(1))
ids <- union(ids[has_seed_mass], study_string)

built <- lapply(ids, function(id) {
  message("Building ", id)
  config <- dataset_configure(file.path("data", id, "metadata.yml"), definitions)
  raw <- dataset_process(file.path("data", id, "data.csv"), config, schema, resource_metadata, unit_conversions)
  dataset_update_taxonomy(raw, taxon_list)
})
austraits <- build_combine(d = built)

outliers <- dataset_check_outlier_by_species(austraits, study_string, trait_names = "seed_dry_mass")

# Accession numbers: the identifiers mapped from `Accession Number` (herbarium
# vouchers are catalogNumbers too, so match on the seed bank's institution_code)
accession_institutions <- bind_rows(lapply(study_string, function(id) {
  ids <- yaml::read_yaml(file.path("data", id, "metadata.yml"))$identifiers
  ids <- Filter(function(x) x$var_in == "Accession Number", ids)
  tibble(dataset_id = id, institution_code = ids[[1]]$institution_code)
}))

accessions <-
  austraits$identifiers %>%
  semi_join(accession_institutions, by = c("dataset_id", "institution_code")) %>%
  distinct(dataset_id, observation_id, accession_number = identifier_value) %>%
  group_by(dataset_id, observation_id) %>%
  summarise(accession_number = paste(accession_number, collapse = "; "), .groups = "drop")

records <-
  outliers$records %>%
  left_join(accessions, by = c("dataset_id", "observation_id")) %>%
  relocate(accession_number, .after = taxon_name) %>%
  select(dataset_id:n_other_datasets)

print(outliers$summary, n = Inf)
message(nrow(records), " flagged records; ", sum(is.na(records$accession_number)), " without an accession number")

# One file of flagged values per dataset, in its raw/ folder
for (id in unique(records$dataset_id)) {
  readr::write_csv(filter(records, dataset_id == id), file.path("data", id, "raw", "seed_dry_mass_outliers.csv"))
}
