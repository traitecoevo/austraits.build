# Writes data/Warwick_2017/data.csv from raw/trait_data.csv (Warwick_2017) and
# raw/trait_data_Whinder_2013.csv (Whinder_2013). Run from the repo root.
#   - The 12 temperate species in Warwick_2017 are the Whinder_2013 specimens (same
#     coordinates, identical Table 3 codes), so their Warwick_2017 rows are dropped and
#     Whinder_2013 supplies those species.
#   - source_id, collection_date: the source of each row and its sampling date.
#   - life_stage: sapling for Warwick_2017 Table 2 plant form "Sapling", otherwise adult.
#   - plant_form_raw: the plant form of the sampled plants (Warwick_2017 Table 2 or
#     Whinder_2013 Table 1), carried on every row as an entity context.
#   - trait_value: numeric rows take the number from trait_value_clean (the raw value
#     column holds "mean ± s.e." text); categorical rows keep the source term.
#   - measurement_remarks: the Table 3 column/row and code(s) for the qualitative wood
#     characters, the plant form for the growth form rows.

library(dplyr)
library(readr)

read_raw <- function(file) {
  read_csv(file, col_types = cols(.default = "c"), na = character(), trim_ws = FALSE)
}

warwick <- read_raw("data/Warwick_2017/raw/trait_data.csv")
whinder <- read_raw("data/Warwick_2017/raw/trait_data_Whinder_2013.csv")

whinder_species <- unique(whinder$taxon_name)

whinder_form <- whinder |>
  filter(trait == "plant_growth_form") |>
  select(taxon_name, plant_form_raw = trait_value_raw)

warwick <- warwick |>
  filter(!(climate_region == "Temperate" & taxon_name %in% whinder_species)) |>
  mutate(
    source_id = "Warwick_2017",
    collection_date = "2017",
    life_stage = if_else(grepl("^Sapling", plant_form_raw), "sapling", "adult"),
    measurement_remarks = case_when(
      grepl("^Table 3", source_section) ~
        paste0(sub(".*column '(.*)'$", "\\1", source_section), " code(s): ", trait_value_raw),
      TRUE ~ paste0("plant form: ", trait_value_raw, " m")
    )
  )

whinder <- whinder |>
  left_join(whinder_form, by = "taxon_name") |>
  mutate(
    source_id = "Whinder_2013",
    climate_region = "Temperate",
    collection_date = "2010-03/2010-05",
    life_stage = "adult",
    trait_value = if_else(value_type == "mode", trait_value, trait_value_clean),
    measurement_remarks = case_when(
      grepl("^Table 3", source_section) ~
        paste0(sub(".*row '(.*)'$", "\\1", source_section), " code(s): ", trait_value_raw),
      grepl("^Table 1", source_section) ~ paste0("growth form: ", trait_value_raw),
      TRUE ~ ""
    )
  )

data <- bind_rows(warwick, whinder) |>
  mutate(across(everything(), ~ coalesce(.x, ""))) |>
  relocate(source_id, collection_date, life_stage, .after = taxon_name) |>
  relocate(measurement_remarks, .after = trait_value_clean)

write_csv(data, "data/Warwick_2017/data.csv", na = "", eol = "\n")
