# Writes data/Lang_2009/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - source_id: rows citing Davies (1995) keep that source; everything else,
#     including the two rows citing Ecological Associates (2006) (no reference available), is the
#     report itself.
#   - basis_of_record: species-level statements are literature; all other rows are field
#     observations from the 2004 and 2007 surveys.
#   - life_stage: the juvenile resprouting row describes pre-fire saplings; everything else adults.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.
#   - replicates: 1 for individual rows, the number of quadrats (n) or "unknown" for numeric
#     population and metapopulation rows; blank for categorical rows.

library(dplyr)
library(readr)

data <- read_csv("data/Lang_2009/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    source_id = if_else(source_id == "", "Lang_2009", source_id),
    basis_of_record = if_else(entity_type == "species", "literature", "field"),
    life_stage = if_else(trait == "resprouting_capacity_juvenile", "sapling", "adult"),
    value = if_else(value_type == "mode", trait_value, trait_value_clean),
    replicates = case_when(
      value_type == "mode" ~ "",
      entity_type == "individual" ~ "1",
      n != "" ~ n,
      TRUE ~ "unknown"
    )
  ) |>
  relocate(basis_of_record, life_stage, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Lang_2009/data.csv", na = "", eol = "\n")
