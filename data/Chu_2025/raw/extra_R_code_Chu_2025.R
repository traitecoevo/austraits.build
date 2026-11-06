# Writes data/Chu_2025/data.csv from the authors' FigShare workbook
# (raw/Chu_2025_FigShare_data.xlsx, doi.org/10.26188/28723466). Run from the repo root.
# The workbook's sheet numbering follows an earlier manuscript version:
#   - "Figure 3": survival after coppicing (%) for all 77 taxa (paper Fig. 1)
#   - "Figure 5": raw pre-coppice trait medians and resprouting vigour for the 68 taxa
#     that resprouted (paper Fig. 3)
#   - "Figure 7": habitat climate (ALA occurrences x WorldClim) for 61 resprouting taxa
#     (paper Fig. S4)
# The workbook gives pre-coppice values for the 9 taxa that did not resprout only as
# standardised values ("Figure 4" sheet), so those taxa have survival data only.
# Some sheets separate words in species names with non-breaking spaces; these are
# replaced with plain spaces so the sheets join.
# Derived columns: cultivar (cultivar name or flower colour form, from the taxon name)
# and location_name.

library(dplyr)
library(readxl)
library(readr)
library(stringr)

f <- "data/Chu_2025/raw/Chu_2025_FigShare_data.xlsx"

read_block <- function(sheet, cols, skip, names) {
  read_excel(f, sheet = sheet, col_names = FALSE, skip = skip, .name_repair = "unique_quiet") |>
    select(all_of(cols)) |>
    setNames(names) |>
    filter(!is.na(species)) |>
    mutate(species = str_squish(str_replace_all(species, " ", " ")))
}

survival <- read_block("Figure 3", 2:4, 2, c("family", "species", "survival_after_coppicing_percent"))

pre_traits <- c(
  "pre_coppice_basal_area_mm2", "pre_coppice_stem_number", "pre_coppice_height_mm",
  "pre_coppice_total_biomass_g", "pre_coppice_leaf_area_ratio_cm2_per_g",
  "pre_coppice_specific_leaf_area_mm2_per_mg", "pre_coppice_root_starch_percent",
  "pre_coppice_root_starch_pool_g", "pre_coppice_root_shoot_g_per_g"
)

raw_values <- read_block(
  "Figure 5", 2:14, 3,
  c("family", "species", "resprout_basal_area_mm2", "resprout_shoot_dry_biomass_g", pre_traits)
) |>
  select(-family) |>
  mutate(across(-species, as.numeric))

climate <- read_block(
  "Figure 7", c(3, 6:8), 3,
  c("species", "habitat_MAP_mm", "habitat_heat_moisture_index", "habitat_precipitation_seasonality")
) |>
  mutate(across(-species, as.numeric))

stopifnot(
  nrow(survival) == 77, nrow(raw_values) == 68, nrow(climate) == 61,
  all(raw_values$species %in% survival$species),
  all(climate$species %in% raw_values$species)
)

data <- survival |>
  left_join(raw_values, by = "species") |>
  left_join(climate, by = "species") |>
  mutate(
    survival_after_coppicing_percent = as.numeric(survival_after_coppicing_percent),
    cultivar = coalesce(
      str_match(species, "[‘']([^’']+)[’']")[, 2],
      str_match(species, "\\(([^)]+)\\)")[, 2]
    ),
    location_name = "Burnley Campus field station"
  ) |>
  relocate(resprout_basal_area_mm2, resprout_shoot_dry_biomass_g, .after = pre_coppice_root_shoot_g_per_g)

write_csv(data, "data/Chu_2025/data.csv", na = "", eol = "\n")
