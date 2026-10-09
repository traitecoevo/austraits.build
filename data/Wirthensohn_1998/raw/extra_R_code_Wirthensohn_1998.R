# Writes data/Wirthensohn_1998/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - location_name: full location names for the location_code values (metadata.yml locations).
#   - leaf_type: juvenile vs adult leaves, from the context column (leaf traits only).
#   - leaf_age: for the epicuticular wax records (Chapters 5 and 7), the leaf age and part of
#     the leaf sampled: leaves aged 0, 16 and 30 days pooled (Table 5.1 and the per-species
#     wax descriptions), proximal or distal area of unfolding day 0 leaves (Table 5.2 and the
#     day 0 descriptions), 16- or 30-day-old control leaves (Table 5.4, wax not removed), and
#     third-node leaves (Table 7.1).
#   - replicates: plants or leaves behind each wax mean - three trees per species (Tables 5.1,
#     5.2), four leaves per species (Table 5.4 controls), and 15 glaucous or 13 green trees
#     (Table 7.1).
#   - leaf_colour_phenotype: the glaucous or green E. gunnii tree groups of Chapter 7, from
#     entity_context.
#   - basis_of_record: Chapter 2 species descriptions (adapted from Pryor & Johnson 1971 and
#     Chippendale 1988) are literature; everything else was observed on cultivated trees.
#   - life_stage: Waite W9 plantation trees were planted March 1994 and observed to 41 months.
#   - plant_height_16_months_cm, stem_diameter_16_months_mm, lignotuber_diameter_16_months_mm:
#     Table 3.3 means for the trees at 16 months, added as entity contexts to the Chapter 3
#     species-evaluation rows (Table 3.1 and Chapter 3 Results) of the same species.
#   - measurement_remarks: the source phrase for categorical rows; the percentage of trees
#     that flowered for reproductive_maturity.

library(dplyr)
library(readr)

data <- read_csv("data/Wirthensohn_1998/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

location_names <- c(
  "Waite W9" = "Waite Campus paddock W9 (Laidlaw Plantation)",
  "Waite Arboretum" = "Waite Arboretum",
  "Forest Range plantation" = "Forest Range plantation"
)

size_16_months <- data |>
  filter(source_section == "Table 3.3, PDF p.61 (printed p.37)",
         trait %in% c("plant_height", "stem_diameter", "lignotuber_diameter"),
         trait_value != "") |>
  mutate(trait = recode(trait,
                        plant_height = "plant_height_16_months_cm",
                        stem_diameter = "stem_diameter_16_months_mm",
                        lignotuber_diameter = "lignotuber_diameter_16_months_mm")) |>
  select(taxon_name, location_code, trait, trait_value) |>
  tidyr::pivot_wider(names_from = trait, values_from = trait_value) |>
  mutate(species_evaluation = TRUE)

wax_traits <- c("leaf_epicuticular_wax_crystal_form", "leaf_epicuticular_wax_density",
                "leaf_wax_tube_length", "leaf_wax_tube_diameter", "leaf_wax_surface_cover")

data <- data |>
  mutate(
    location_name = unname(location_names[location_code]),
    leaf_type = case_when(
      !grepl("^leaf_|^juvenile_leaf", trait) ~ "",
      grepl("^juvenile", context) ~ "juvenile leaves",
      grepl("^adult", context) ~ "adult leaves",
      TRUE ~ ""
    ),
    wax_trait = trait %in% wax_traits & grepl("^Table [57]|^Chapter 5", source_section),
    leaf_age = case_when(
      !wax_trait ~ "",
      grepl("day 0 \\(unfolding\\); proximal", context) ~ "unfolding leaves (day 0), proximal area",
      grepl("day 0 \\(unfolding\\); distal", context) ~ "unfolding leaves (day 0), distal area",
      grepl("leaf age at wax removal 16 days$", context) ~ "16-day-old leaves",
      grepl("leaf age at wax removal 30 days$", context) ~ "30-day-old leaves (full expansion)",
      grepl("node 3", context) ~ "third-node leaves",
      grepl("^Table 5\\.1|^Chapter 5", source_section) ~ "leaves aged 0, 16 and 30 days (pooled)",
      TRUE ~ ""
    ),
    replicates = case_when(
      !wax_trait | trait %in% c("leaf_epicuticular_wax_crystal_form", "leaf_epicuticular_wax_density") ~ "",
      grepl("^Table 5\\.[12]", source_section) ~ "3",
      grepl("^Table 5\\.4", source_section) & entity_context == "control (wax not removed)" ~ "4",
      entity_context == "glaucous phenotype" ~ "15",
      entity_context == "green phenotype" ~ "13",
      TRUE ~ ""
    ),
    leaf_colour_phenotype = if_else(grepl("phenotype$", entity_context), entity_context, ""),
    basis_of_record = if_else(entity_type == "species", "literature", "captive_cultivated"),
    life_stage = if_else(location_code == "Waite W9", "sapling", "adult"),
    species_evaluation = grepl("^Table 3\\.1|^Chapter 3 Results", source_section),
    measurement_remarks = case_when(
      trait == "reproductive_maturity" & grepl("% of trees flowered", notes) ~
        sub(".*?([0-9]+% of trees flowered).*", "\\1", notes),
      value_type == "mode" ~ trait_value_raw,
      TRUE ~ ""
    )
  ) |>
  left_join(size_16_months, by = c("taxon_name", "location_code", "species_evaluation")) |>
  select(-species_evaluation, -wax_trait) |>
  relocate(location_name, .after = location_code) |>
  relocate(leaf_type, leaf_age, .after = context) |>
  relocate(leaf_colour_phenotype, basis_of_record, life_stage, plant_height_16_months_cm, stem_diameter_16_months_mm,
           lignotuber_diameter_16_months_mm, .after = entity_context) |>
  relocate(measurement_remarks, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Wirthensohn_1998/data.csv", na = "", eol = "\n")
