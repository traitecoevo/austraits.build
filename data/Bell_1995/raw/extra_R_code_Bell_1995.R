# Writes data/Bell_1995/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: seed-lot measurements (viability, germination, seed mass,
#     pre-treatments) were made in the laboratory on field-collected seed; species-level
#     classifications (life form, fire response, seed store, ...) describe field plants.
#   - replicates: Table 1 values are means of 3 replicates; Table 3 values pool the
#     +GA3 and -GA3 trials, i.e. 2 x 3 replicate plates.
#   - source_id: smoke responses quoted from Dixon et al. (1995) in the Discussion.
#   - measurement_remarks: the source phrase for categorical rows.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Bell_1995/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = if_else(entity_type == "population", "lab", "field"),
    replicates = case_when(
      value_type == "mode" ~ "",
      source_section == "Table 3" ~ "6",
      TRUE ~ n
    ),
    source_id = if_else(grepl("Dixon et al\\. \\(1995", notes), "Dixon_1995", "Bell_1995"),
    measurement_remarks = if_else(value_type == "mode", trait_value_raw, ""),
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(basis_of_record, source_id, .after = entity_context) |>
  relocate(value, measurement_remarks, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Bell_1995/data.csv", na = "", eol = "\n")
