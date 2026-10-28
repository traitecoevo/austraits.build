# Writes data/Andres_2024/data.csv from the authors' OSF files (https://osf.io/5wc4q/) plus
# Table S1 of the Supplementary Information. Run from the repo root.
#   - Seed-lot rows (TREATMENT blank): one per species x provenance listed in Table S1
#     (raw/table_S1.csv; "2+1" expanded to the two provenances, future and regional), with
#     the seed morphology means from raw/seed_traits_no_se.csv joined on, and the Table S1
#     seed pre-treatment in `seed_pretreatment` ("none" where Table S1 lists no
#     pre-treatment). Seed lots without seed morphology data keep
#     the Table S1 species name; all others keep the name used in seed_traits_no_se.csv.
#   - Emergence rows (TREATMENT = Bare seed / Pellet): raw/emergence_traits.csv as supplied
#     (it omits the 17 species x provenance x treatment groups in which nothing emerged).
#     The pre-treatment is copied to `germination_pretreatment` ("none" where Table S1
#     lists no pre-treatment), and location_name is set to the greenhouse at Dural where
#     the emergence trial was run (seed-lot rows have no location).
#   - Joins use whitespace-trimmed species names; Table S1's Plantago gaudichaudii is
#     matched to the data files' "Plantago guadichaudii". Species names in data.csv are
#     otherwise left as the authors spelt them.
#   - All values are read and written as character, so nothing is rounded.

library(dplyr)
library(readr)
library(tidyr)

raw <- "data/Andres_2024/raw/"
read_chr <- function(f) {
  read_csv(paste0(raw, f), col_types = cols(.default = "c"), na = "", trim_ws = FALSE)
}

key <- function(x) {
  x |> trimws() |> sub(pattern = "Plantago gaudichaudii", replacement = "Plantago guadichaudii")
}

table_S1 <- read_chr("table_S1.csv") |>
  mutate(
    PROVENANCE = if_else(provenance == "2+1", "future;regional", NA_character_),
    key = key(species)
  ) |>
  separate_rows(PROVENANCE, sep = ";") |>
  select(key, species, PROVENANCE, family, seed_pretreatment)

seed_traits <- read_chr("seed_traits_no_se.csv") |>
  mutate(key = key(SPECIES))

seed_lots <- table_S1 |>
  left_join(seed_traits, by = c("key", "PROVENANCE")) |>
  mutate(
    SPECIES = coalesce(SPECIES, species),
    seed_pretreatment = coalesce(seed_pretreatment, "none"),
    source_file = if_else(is.na(seed_length_Mean), "table_S1.csv",
                          "table_S1.csv; seed_traits_no_se.csv")
  )
stopifnot(nrow(seed_lots) == 68, all(seed_traits$key %in% seed_lots$key))

emergence <- read_chr("emergence_traits.csv") |>
  mutate(key = key(SPECIES), source_file = "emergence_traits.csv")

emergence <- emergence |>
  left_join(distinct(table_S1, key, seed_pretreatment), by = "key") |>
  mutate(germination_pretreatment = coalesce(seed_pretreatment, "none")) |>
  select(-seed_pretreatment)
stopifnot(nrow(emergence) == 119)

data <- bind_rows(seed_lots, emergence) |>
  group_by(key) |>
  mutate(family = first(na.omit(family)), growth_form = first(na.omit(growth_form))) |>
  ungroup() |>
  arrange(key, PROVENANCE, !is.na(TREATMENT), TREATMENT) |>
  mutate(location_name = if_else(is.na(TREATMENT), NA_character_, "Dural greenhouse")) |>
  select(SPECIES, PROVENANCE, TREATMENT, location_name, family, growth_form,
         endosperm_area_Mean:eccentricity_index_Mean, seed_pretreatment,
         `Time_to_50%`:Time_spread, germination_pretreatment, source_file)

write_csv(data, "data/Andres_2024/data.csv", na = "", eol = "\n")
