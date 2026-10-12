## Build data/DeSmedt_2018/data.csv from raw/trait_data.csv
## Run from the repository root: source("data/DeSmedt_2018/raw/extra_R_code_DeSmedt_2018.R")

library(dplyr)

raw <- read.csv("data/DeSmedt_2018/raw/trait_data.csv", colClasses = "character",
                check.names = FALSE, na.strings = NULL)

## Sizes of the sampled plants (habitat means) become entity contexts on every
## record of the same species x habitat, rather than plant_height / stem_diameter /
## plant_width traits
size <- raw |>
  filter(trait %in% c("plant_height", "stem_diameter", "plant_width"), habitat != "") |>
  select(taxon_name, habitat, trait, trait_value_clean) |>
  tidyr::pivot_wider(names_from = trait, values_from = trait_value_clean) |>
  rename(plant_height_m = plant_height,
         stem_diameter_20cm_cm = stem_diameter,
         canopy_diameter_m = plant_width)

data <- raw |>
  left_join(size, by = c("taxon_name", "habitat")) |>
  mutate(across(c(plant_height_m, stem_diameter_20cm_cm, canopy_diameter_m), ~ if_else(is.na(.x), "", .x)))

write.csv(data, "data/DeSmedt_2018/data.csv", row.names = FALSE, na = "")
