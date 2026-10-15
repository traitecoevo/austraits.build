# Writes data/Judd_1996/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - entity_type: Table 2 ranges are for one region (south-west Western Australia), compiled
#     across several studies, so metapopulation rather than species.
#   - source_id: Table 10 is reproduced from Judd et al. (1991); everything else is this chapter.
#   - basis_of_record: field for native forest, captive_cultivated for plantations and
#     glasshouse experiments, field captive_cultivated where a range spans both (Table 16,
#     Australia); literature only for rows with no stated growing environment.
#   - life_stage: adult unless stated otherwise: 2-year-old plantations (Table 9) and young
#     trees (Table 14) are saplings, Table 14 glasshouse seedlings are seedlings.
#   - growing_environment: native forest / plantation / glasshouse, from the context column.
#   - plant_age: 2 years for Table 9.
#   - entity_measured: branch, stem wood or twig, for wood_*_per_dry_mass rows (Tables 5 and 17).
#   - replicates: n where the source gives one; unknown for population/metapopulation rows
#     without one; blank for species-level ranges.
#   - omit_duplicate: the dataset that already holds the value, for Judd rows listed in
#     raw/duplicates_with_Richards_2008.csv (values duplicated in Richards_2008, which holds the
#     original per-study data). Matched on taxon, trait, value type, growing environment and
#     value (the duplicates list is in mg/g after unit conversion, Judd values are in %);
#     custom_R_code keeps only rows where it is blank.

library(dplyr)
library(readr)

data <- read_csv("data/Judd_1996/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

duplicates <- read_csv("data/Judd_1996/raw/duplicates_with_Richards_2008.csv",
                       col_types = cols(.default = "c")) |>
  filter(dataset_id == "Judd_1996") |>
  transmute(taxon_name, trait = trait_name, value_type,
            growing_environment = `entity_context: growing environment`,
            value_mg_g = as.numeric(value),
            omit_duplicate_match = "Richards_2008") |>
  distinct()

data <- data |>
  mutate(
    table = sub(",.*", "", source_section),
    entity_type = if_else(table == "Table 2", "metapopulation", entity_type),
    source_id = if_else(table == "Table 10", "Judd_1991", "Judd_1996"),
    life_stage = case_when(
      table == "Table 9" ~ "sapling",
      table == "Table 14" & grepl("young trees", context) ~ "sapling",
      table == "Table 14" & grepl("seedling", context) ~ "seedling",
      TRUE ~ "adult"
    ),
    growing_environment = case_when(
      grepl("glasshouse", context) ~ "mostly glasshouse experiments",
      grepl("country or region: Australia", context) ~ "native forest and plantation",
      grepl("stand type: native forest", context) ~ "native forest",
      grepl("stand type: plantation", context) ~ "plantation",
      TRUE ~ ""
    ),
    basis_of_record = case_when(
      growing_environment == "native forest" ~ "field",
      growing_environment == "plantation" ~ "captive_cultivated",
      growing_environment == "mostly glasshouse experiments" ~ "captive_cultivated",
      growing_environment == "native forest and plantation" ~ "field captive_cultivated",
      TRUE ~ "literature"
    ),
    plant_age = if_else(table == "Table 9", "2 years", ""),
    entity_measured = if_else(grepl("entity_measured: ", context), sub(".*entity_measured: ([^;]+).*", "\\1", context), ""),
    replicates = case_when(
      n != "" ~ n,
      entity_type %in% c("population", "metapopulation") ~ "unknown",
      TRUE ~ ""
    )
  ) |>
  mutate(value_mg_g = if_else(unit == "%", round(as.numeric(trait_value_clean) * 10, 6), NA_real_)) |>
  left_join(duplicates, by = c("taxon_name", "trait", "value_type", "growing_environment", "value_mg_g")) |>
  mutate(omit_duplicate = coalesce(omit_duplicate_match, "")) |>
  select(-table, -value_mg_g, -omit_duplicate_match) |>
  relocate(source_id, omit_duplicate, .before = taxon_name) |>
  relocate(basis_of_record, life_stage, growing_environment, plant_age, .after = entity_context) |>
  relocate(entity_measured, .after = trait) |>
  relocate(replicates, .after = n)

stopifnot(sum(data$omit_duplicate != "") == nrow(duplicates))

write_csv(data, "data/Judd_1996/data.csv", na = "", eol = "\n")
