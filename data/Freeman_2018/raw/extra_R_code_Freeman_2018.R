# Writes data/Freeman_2018/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: all observations are of plants in the Tiwi Carbon Study fire
#     experiment, so field.
#   - life_stage: Table 1 and the post-fire flowering statements describe resprouts and
#     saplings (<5 cm diameter), i.e. juveniles in size, even when reproductive; the
#     clonality statements describe the species.
#   - replicates: individuals surveyed per species x fire treatment (Table 1 N); blank
#     for single-individual and species-level rows.
#   - source_id: all rows are the primary source.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Freeman_2018/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = "field",
    life_stage = if_else(entity_type == "species", "adult", "sapling"),
    replicates = if_else(entity_type == "metapopulation", n, ""),
    source_id = "Freeman_2018",
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(basis_of_record, life_stage, source_id, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Freeman_2018/data.csv", na = "", eol = "\n")
