# Writes data/Myrans_2025/data.csv from the contributor's raw data file (raw xlsx, downloaded from
# Monash Bridges, doi.org/10.26180/28406183), one row per plant, adding derived columns so their
# values sit alongside the data. Run from the repo root.
# Values are read from the xlsx at full precision (a Save As csv from Excel rounds them to the
# digits displayed).
#   - taxon_name: full species name from the "Accession information" sheet.
#   - individual_id: accession and tub (one plant per accession per tub).
#   - location_name: the seed provenance site of each accession, as named in metadata.yml;
#     blank for the S. bicolor line BTx623, which has no provenance site.
#   - accession_name, AGG_ID, accession_doi: from the "Accession information" sheet.
#   - cultivar_status: "cultivar" for the domesticated S. bicolor line BTx623, "wild type" for
#     the wild accessions.
# The "VWC data" sheet (pot soil volumetric water content through the experiment) is not
# included. Trait columns are renamed to plain-ASCII names; values are unchanged.

library(dplyr)
library(readr)
library(readxl)

file <- "data/Myrans_2025/raw/Data%20for%20Wild%20Sorghum%20species%20exhibit%20greater%20drought%20tolerance%20but%20less%20plasticity%20than%20domesticated%20sorghum.xlsx"

data <- read_excel(file, sheet = "Phenotype data")

names(data) <- c("treatment", "tub", "accession", "biomass_g", "root_shoot_ratio",
                 "leaf_chlorophyll_mg_per_g", "chlorophyll_a_b_ratio",
                 "leaf_HCNp_ug_per_g", "root_HCNp_ug_per_g", "sheath_HCNp_ug_per_g",
                 "leaf_phenolics_mg_per_g")

species_names <- c("S. plumosum" = "Sorghum plumosum",
                   "S. stipoideum" = "Sorghum stipoideum",
                   "S. timorense" = "Sorghum timorense",
                   "S. bicolor" = "Sorghum bicolor")

accessions <- read_excel(file, sheet = "Accession information") |>
  filter(!is.na(`Accession code`)) |>
  select(accession = `Accession code`, species = Species, AGG_ID = `Accession number`,
         accession_doi = DOI, accession_name = `Accession name`) |>
  mutate(
    taxon_name = unname(species_names[species]),
    across(c(AGG_ID, accession_doi), ~ na_if(.x, "N/A"))
  ) |>
  select(-species)

data <- data |>
  left_join(accessions, by = "accession") |>
  mutate(
    individual_id = paste(accession, tub, sep = "_"),
    location_name = if_else(is.na(AGG_ID), NA_character_,
                            paste0(accession, " (", gsub(" ", "", accession_name), ")")),
    cultivar_status = if_else(taxon_name == "Sorghum bicolor", "cultivar", "wild type")
  ) |>
  select(taxon_name, accession, treatment, tub, individual_id, location_name,
         accession_name, AGG_ID, accession_doi, cultivar_status, everything())

write_csv(data, "data/Myrans_2025/data.csv", na = "")
