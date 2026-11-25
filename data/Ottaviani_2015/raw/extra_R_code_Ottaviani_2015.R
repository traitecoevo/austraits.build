# Builds data/Ottaviani_2015/data.csv from the author's individual-level spreadsheet
# ("Trait data_SW Australia granite outcrops_Ottaviani.xlsx", the version also deposited
# in TRY; one row per sampled plant) plus raw/species_level_traits.csv, which holds the
# species-level categorical traits (leaf phenology, life form, clonal spread) taken from
# the papers and from the author's responses to AusTraits queries (2026-10-13).
# The spreadsheet is read with readxl so values keep full precision (a csv export from
# Excel rounds to the displayed digits). Its first data row holds the units and is dropped;
# the units are recorded as unit_in in metadata.yml. "-" (no value, Leaf C/N) is read as
# missing. Species-level traits are appended as one extra row per species, with no site,
# so they are not repeated on every plant.
# Run from the repository root: source("data/Ottaviani_2015/raw/extra_R_code_Ottaviani_2015.R")

library(dplyr)

dir <- "data/Ottaviani_2015"

xlsx <- file.path(dir, "raw/Trait data_SW Australia granite outcrops_Ottaviani.xlsx")

header <- names(readxl::read_excel(xlsx, sheet = "Sheet1", n_max = 0, .name_repair = "minimal"))

plants <- readxl::read_excel(
  xlsx,
  sheet = "Sheet1",
  skip = 2,
  col_names = header,
  na = c("", "-"),
  .name_repair = "minimal"
) |>
  mutate(across(where(is.numeric), ~ as.character(signif(.x, 15)))) |>
  mutate(across(everything(), as.character))

species_traits <- readr::read_csv(
  file.path(dir, "raw/species_level_traits.csv"),
  col_types = readr::cols(.default = "c")
)

data <- bind_rows(plants, species_traits)

readr::write_csv(data, file.path(dir, "data.csv"), na = "")
