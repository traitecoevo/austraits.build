# Writes data/Fernando_2021/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: all three studies sampled mature trees in situ at Hattah Lakes.
#   - individual_id: the AJB study (Fernando_2021_2) resampled tree a (flooded, 2017-12-09)
#     after floodwaters subsided (2018-07-13); tree b is a single never-flooded tree.
#   - replicates: the source n where stated (Fernando_2018: 5 trees per location, 3 for
#     phenolics); 15 trees (5 trees x sites H1-H3) for the pooled Fernando_2021 means;
#     1 for the single-tree AJB samples (each a bulk of 10-20 leaves); blank for
#     species-level descriptions.
#   - measurement_remarks: the source phrase for categorical rows.

library(dplyr)
library(readr)

data <- read_csv("data/Fernando_2021/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = "field",
    individual_id = if_else(source_id == "Fernando_2021_2" & entity_type == "individual",
                            entity_context, ""),
    replicates = case_when(
      n != "" ~ n,
      source_id == "Fernando_2021" ~ "15",
      source_id == "Fernando_2021_2" & entity_type == "individual" ~ "1",
      TRUE ~ ""
    ),
    measurement_remarks = if_else(value_type == "mode", trait_value_raw, "")
  ) |>
  relocate(basis_of_record, individual_id, .after = entity_context) |>
  relocate(measurement_remarks, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Fernando_2021/data.csv", na = "", eol = "\n")
