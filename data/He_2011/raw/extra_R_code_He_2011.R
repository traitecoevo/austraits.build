# Writes data/He_2011/data.csv (wide) from raw/trait_data.csv. Run from the repo root.
#   - one row per taxon for the Fig. 1-4 tip states; one row per B. hookeriana population
#     (unburnt / burnt) for the Discussion field observations; one row per taxon for the
#     Discussion clone-longevity estimates.
#   - value: source term (trait_value) for categorical rows, parsed number
#     (trait_value_clean) for numeric rows.
#   - source_id: B. goodii clone longevity is cited from Drechsler et al. (1999).

library(dplyr)
library(readr)
library(tidyr)

data <- read_csv("data/He_2011/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

trait_order <- unique(data$trait)

data_wide <- data |>
  mutate(
    value = if_else(value_type == "mode", trait_value, trait_value_clean),
    source_id = if_else(taxon_name == "Banksia goodii" & trait == "lifespan", "Drechsler_1999", "He_2011"),
    row_group = case_when(
      entity_type == "species" ~ "phylogeny tip states",
      trait == "lifespan" ~ "clone longevity",
      TRUE ~ "drought vs fire observations"
    ),
    n = if_else(trait %in% c("seedling_mortality_fraction", "seed_release_cues", "lifespan"), "", n)
  ) |>
  group_by(taxon_name, row_group, location_name) |>
  mutate(n = max(n)) |>
  ungroup() |>
  select(taxon_name, taxon_name_figure, row_group, population = location_name, fire_history,
         collection_date, source_id, n, trait, value) |>
  group_by(taxon_name, row_group, population) |>
  mutate(taxon_name_figure = max(taxon_name_figure), source_id = max(source_id)) |>
  ungroup() |>
  pivot_wider(names_from = trait, values_from = value, values_fill = "") |>
  select(taxon_name, taxon_name_figure, row_group, population, fire_history, collection_date,
         source_id, n, all_of(trait_order))

write_csv(data_wide, "data/He_2011/data.csv", na = "", eol = "\n")
