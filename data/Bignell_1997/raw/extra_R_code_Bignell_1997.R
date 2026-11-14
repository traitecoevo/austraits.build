# Writes data/Bignell_1997/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: trees in the Waite and Currency Creek arboreta are captive_cultivated;
#     natural stands in Western Australia (Toodyay and the sites collected by Dean Nicolle)
#     are field.
#   - life_stage: mature leaves picked from single trees, so adult throughout.
#   - leaf_1_8_cineole_per_dry_mass rows: oil yield (Table 1, wt% of dry leaf) multiplied by
#     the proportion of 1,8-cineole in the oil (Table 2), giving % of dry leaf mass. Not
#     calculated where the oil yield is printed as "tr." (trace).

library(dplyr)
library(readr)

data <- read_csv("data/Bignell_1997/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

cineole <- data |>
  filter(trait == "leaf_oil_1_8_cineole_percent") |>
  select(taxon_name, cineole_percent = trait_value_clean)

cineole_per_dry_mass <- data |>
  filter(trait == "leaf_essential_oil_per_dry_mass", trait_value_clean != "") |>
  inner_join(cineole, by = "taxon_name") |>
  mutate(
    notes = paste0("Calculated as oil yield (", trait_value_raw, "% dry weight) x 1,8-cineole proportion of the oil (",
                   cineole_percent, "%)."),
    trait = "leaf_1_8_cineole_per_dry_mass",
    trait_value_raw = "",
    trait_value_clean = as.character(signif(as.numeric(trait_value_clean) * as.numeric(cineole_percent) / 100, 3)),
    source_section = "Tables 1 and 2"
  ) |>
  select(-cineole_percent)

data <- bind_rows(data, cineole_per_dry_mass) |>
  mutate(
    basis_of_record = if_else(location_name %in% c("Waite Arboretum", "Currency Creek Arboretum"),
                              "captive_cultivated", "field"),
    life_stage = "adult"
  ) |>
  relocate(basis_of_record, life_stage, .after = voucher_herbarium)

write_csv(data, "data/Bignell_1997/data.csv", na = "", eol = "\n")
