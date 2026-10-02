# Subset Li et al. 2025 (New Phytologist) SI Table S1 to rows whose coordinates fall in Australia.
# Bounding box: lat -44 to -9, lon 112 to 155 (excludes New Zealand, PNG, New Caledonia).
# Appends source_id (key of the original source of each row) and collection_date
# (publication year of that source, or the sampling year where already known in AusTraits).
# Column names have the Greek mu replaced with "u" (non-ASCII characters fail dataset_test).

library(readxl)
library(dplyr)
library(readr)

f <- "data/Li_2025/raw/nph70397-sup-0002-TableS1-S1@Supporting Information Table S1.xlsx"

sources <- tribble(
  ~`source paper`,                       ~source_id,        ~collection_date,
  "Burrows, 2001",                       "Burrows_2001",    "2001",
  "Cunningham et al., 1999",             "Cunningham_1999", "1996",
  "De Lillis, 1991",                     "deLillis_1991",   "1991",
  "Duncan DH & Westoby M, unpublished",  "Duncan_1998",     "1998",
  "Edwards et al., 2000",                "Edwards_2000_2",  "1995",
  "Gras et al., 2005",                   "Gras_2005",       "2005",
  "Groom et al., 1994",                  "Groom_1994",      "1994",
  "Johnson, 1980",                       "Johnson_1980",    "1980",
  "Jordan GJ, unpublished",              "Jordan_2007",     "2007",
  "Medina et al., 1990",                 "Medina_1990",     "1990",
  "Peeters, 2002",                       "Peeters_2002",    "2002",
  "Ridge et al., 1984",                  "Ridge_1984",      "1984"
)

# location_name: re-uses the site names of the existing AusTraits datasets for Burrows 2001
# (Burrows_2001) and Edwards et al. 2000 (Edwards_2000, which splits Wilson's Prom into forest
# and heath sites). Banksia marginata was sampled at both Edwards sites; ID 1752 (leaf size
# 1.67 cm2) matches the forest and ID 1753 (1.06 cm2) the heath collection.
edwards_sites <- read_csv("data/Edwards_2000/data.csv", show_col_types = FALSE) %>%
  mutate(Species = trimws(sub(" (forest|heath)$", "", name_original))) %>%
  distinct(Species, edwards_site = site) %>%
  filter(Species != "Banksia marginata")

# omit_<trait> columns: "x" marks values already in AusTraits under the dataset of the
# original source (species x trait matched against the compiled database, allowing for
# name variants); these are set to NA by custom_R_code in metadata.yml. Rows from
# Cunningham_1999, Duncan_1998, Johnson_1980, Jordan_2007 and Peeters_2002 keep no values,
# so they get no location. (Leaf size, LMA and leaf density are not mapped at all.)
# Medina_1990 (one Banksia aemula row "near Perth", outside the species' range, from a
# study of Venezuelan rain forests) is excluded the same way.
in_austraits_all <- c("Cunningham_1999", "Duncan_1998", "Johnson_1980", "Jordan_2007", "Peeters_2002")
excluded <- c(in_austraits_all, "Medina_1990")
burrows_edwards_new <- c("Dodonaea viscosa", "Bedfordia arborescens")

read_excel(f, sheet = "CT dataset Li et al") %>%
  filter(Latitude > -44, Latitude < -9, Longitude > 112, Longitude < 155) %>%
  left_join(sources, by = "source paper") %>%
  left_join(edwards_sites, by = "Species") %>%
  mutate(
    location_name = case_when(
      source_id == "Burrows_2001" ~ "The Rock",
      ID == 1752 ~ "ReadWilsonsProm_forest",
      ID == 1753 ~ "ReadWilsonsProm_heath",
      source_id == "Edwards_2000_2" ~ edwards_site,
      source_id %in% excluded ~ NA_character_,
      TRUE ~ `dataset and location`
    ),
    omit_CTupper = if_else(source_id %in% excluded, "x", NA_character_),
    omit_CTlower = omit_CTupper,
    omit_leaf_thickness = if_else(source_id %in% excluded |
      (source_id %in% c("Burrows_2001", "Edwards_2000_2") & !Species %in% burrows_edwards_new), "x", NA_character_)
  ) %>%
  select(-edwards_site) %>%
  rename_with(~ gsub("\u03bc", "u", .x)) %>%
  write_csv("data/Li_2025/data.csv", na = "")
