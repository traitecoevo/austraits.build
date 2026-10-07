# Writes data/Fernando_2018_2/data.csv from raw/trait_data.csv. Run from the repo root.
#   - plant_height: average height of the sampled trees (G. grayi ~3 m, G. shepherdii
#     ~1 m), from entity_context; an entity_context, not a plant_height trait.
#   - leaf_sample: "lamina of galled leaves" for the paired gall/lamina comparison on 10
#     G. grayi trees (Table 1, leaf gall samples); blank for the standard leaf samples.
#   - replicates: number of trees (n) where stated as a number.

library(dplyr)
library(readr)
library(stringr)

data <- read_csv("data/Fernando_2018_2/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    plant_height = str_extract(entity_context, "~[0-9]+ m"),
    leaf_sample = case_when(
      context == "tissue: lamina of galled leaves" ~ "lamina of galled leaves",
      context == "tissue: leaf gall" ~ "leaf gall",
      TRUE ~ ""
    ),
    replicates = if_else(str_detect(n, "^[0-9]+$"), n, "")
  ) |>
  relocate(plant_height, .after = entity_context) |>
  relocate(leaf_sample, .after = leaf_age) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Fernando_2018_2/data.csv", na = "", eol = "\n")
