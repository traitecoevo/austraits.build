# Writes data/Steart_2009/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - location_name: the extraction's location_code column, renamed to match metadata.yml
#     (Cumberland Creek for field-collected leaves; blank for species-level rows and for
#     the mesocosm experiment).
#   - source_id: Steart_2009 for all rows, except the Acacia auriculiformis and A. mangium
#     values the authors cite in Discussion 4.5 from Bernhard-Reversat (1993), measured in
#     plantations in Congo.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.

library(dplyr)
library(readr)

data <- read_csv("data/Steart_2009/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  rename(location_name = location_code) |>
  mutate(
    source_id = if_else(grepl("^Discussion 4.5", source_section), "BernhardReversat_1993", "Steart_2009"),
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(source_id, .after = entity_context) |>
  relocate(value, .after = trait_value_clean)

write_csv(data, "data/Steart_2009/data.csv", na = "", eol = "\n")
