# Writes data/Freeman_2017/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - source_id: leaf phenology classes in Chapter 2 Table 4 "follow Williams et al. (1997)";
#     everything else is the thesis itself.
#   - basis_of_record: Williams et al. (1997) classes are literature; all other rows are
#     field observations or field-based model estimates from the Tiwi Carbon Study.
#   - life_stage: metapopulation rows describe tagged resprouts and saplings (<5 cm
#     diameter); species-level statements describe the species.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Freeman_2017/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    source_id = if_else(source_id == "", "Freeman_2017", source_id),
    basis_of_record = if_else(source_id == "Williams_1997", "literature", "field"),
    life_stage = if_else(entity_type == "species", "adult", "sapling"),
    replicates = n,
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(basis_of_record, life_stage, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Freeman_2017/data.csv", na = "", eol = "\n")
