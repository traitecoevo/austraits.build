# Writes data/Dorken_2020/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - location_name: full location names for the location_code values (metadata.yml locations).
#   - basis_of_record: the Perth common garden and Kings Park material was cultivated
#     (Table 2 'cult'); the other three species were collected from wild plants.
#   - plant_part: leaf or photosynthetic stem, for the two stomatal traits (from the context column).
#   - measurement_remarks: the source phrase for categorical rows.

library(dplyr)
library(readr)

data <- read_csv("data/Dorken_2020/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

location_names <- c(
  "OUY" = "near Ouyen",
  "PCG" = "Perth common garden",
  "KP" = "Kings Park and Botanic Garden",
  "TNS" = "Tetratheca nuda collection site",
  "GFS" = "Glischrocaryon flavescens collection site"
)

data <- data |>
  mutate(
    location_name = if_else(location_code == "", "", unname(location_names[location_code])),
    basis_of_record = case_when(
      location_code %in% c("PCG", "KP") ~ "captive_cultivated",
      location_code %in% c("OUY", "TNS", "GFS") ~ "field",
      TRUE ~ ""
    ),
    plant_part = if_else(trait %in% c("stomatal_position", "leaf_stomatal_distribution"), context, ""),
    measurement_remarks = if_else(value_type == "mode", trait_value_raw, "")
  ) |>
  relocate(location_name, .after = location_code) |>
  relocate(basis_of_record, .after = entity_context) |>
  relocate(plant_part, .after = context) |>
  relocate(measurement_remarks, .after = trait_value_clean)

write_csv(data, "data/Dorken_2020/data.csv", na = "", eol = "\n")
