# Writes data/Baker_2005/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: seed-lot measurements (fill, viability, germination, dormancy,
#     embryo type) were made in the laboratory on field-collected seed; species-level
#     classifications (fire ephemeral class, life history, seedbank) describe field plants.
#   - replicates: the number of replicate dishes behind each numeric value (n); blank for
#     categorical rows.
#   - source_id: all rows are the primary source.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Baker_2005/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = if_else(entity_type == "population", "lab", "field"),
    replicates = if_else(value_type == "mode", "", n),
    source_id = "Baker_2005",
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(basis_of_record, source_id, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Baker_2005/data.csv", na = "", eol = "\n")
