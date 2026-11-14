# Writes data/Bowen_2017/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - Table 3 genus summary rows (entity_type genus: means across the resprouter or
#     seeder species of a genus) stay in raw/trait_data.csv only; the single-species
#     Table 3 rows are kept, under their species.
#   - basis_of_record: anatomy and starch of species whose Table 1 collection location is
#     marked § were measured on glasshouse plants grown from seed (captive_cultivated);
#     all other rows, and the fire responses (scored on burnt plants in the wild), field.
#   - life_stage: anatomy and starch were measured on 2- to 4-year-old seedlings
#     (sapling); the fire responses describe the species' plants (adult).
#   - replicates: individuals per species (2) for the numeric rows.
#   - omit_duplicate: root starch assays (mg/g) that repeat the value Pate_1990 already
#     reports for the species (same lab, sites and assay; equal to rounding).
#   - value: the build value - mapped term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Bowen_2017/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

pate_1990_starch <- c("Banksia grandis", "Banksia hookeriana", "Banksia ilicifolia",
                      "Hakea obliqua", "Hakea prostrata", "Hakea varia",
                      "Stirlingia tenuifolia")

data <- data |>
  filter(entity_type != "genus") |>
  select(-fire_response_group) |>
  mutate(
    fire_trait = trait %in% c("resprouting_capacity", "post_fire_recruitment"),
    basis_of_record = if_else(growing_conditions == "glasshouse" & !fire_trait,
                              "captive_cultivated", "field"),
    life_stage = if_else(fire_trait, "adult", "sapling"),
    replicates = n,
    omit_duplicate = if_else(trait == "root_starch_per_mass" & taxon_name %in% pate_1990_starch,
                             "Pate_1990", ""),
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  select(-fire_trait) |>
  relocate(basis_of_record, life_stage, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n) |>
  relocate(omit_duplicate, .after = replicates)

write_csv(data, "data/Bowen_2017/data.csv", na = "", eol = "\n")
