# Writes data/Myrans_2024/data.csv from the contributor's raw data file (raw xlsx), one row per
# plant, adding derived columns so their values sit alongside the data. Run from the repo root.
# Values are read from the xlsx at full precision (a Save As csv from Excel rounds them to the
# digits displayed).
#   - taxon_name: from the sheet name (one sheet per species).
#   - individual_id: accession and block number (one plant per accession per block).
#   - location_name: the seed provenance site of each accession, as named in metadata.yml.
#   - accession_name, AGG_ID, accession_doi: from the "Accession information" sheet.
#   - life_history: three species-level rows, from the Introduction of the primary reference.
# Trait columns are renamed to plain-ASCII names; values are unchanged.

library(dplyr)
library(readr)
library(readxl)

file <- "data/Myrans_2024/raw/Raw data Myrans_2024.xlsx"

species_sheets <- c("S. plumosum" = "Sorghum plumosum",
                    "S. stipoideum" = "Sorghum stipoideum",
                    "S. timorense" = "Sorghum timorense")

data <- bind_rows(lapply(names(species_sheets), function(s)
  read_excel(file, sheet = s) |> mutate(taxon_name = species_sheets[[s]])))

names(data) <- c("accession", "block_number",
                 "leaf_phenolics_mg_per_g", "sheath_phenolics_mg_per_g", "root_phenolics_mg_per_g",
                 "leaf_Si_percent", "root_Si_percent", "sheath_Si_percent",
                 "SLA_cm2_per_g", "root_shoot_ratio", "Fv_Fm", "taxon_name")

accessions <- read_excel(file, sheet = "Accession information") |>
  select(accession = Accession, accession_name = `Accession name`,
         AGG_ID = `Australian Grains Genebank ID`, accession_doi = doi)

data <- data |>
  left_join(accessions, by = "accession") |>
  mutate(
    individual_id = paste(accession, block_number, sep = "_"),
    location_name = paste0(accession, " (", gsub(" ", "", accession_name), ")")
  ) |>
  select(taxon_name, accession, block_number, individual_id, location_name,
         accession_name, AGG_ID, accession_doi, everything()) |>
  bind_rows(tibble(
    taxon_name = unname(species_sheets),
    life_history = c("perennial", "annual", "annual")
  ))

write_csv(data, "data/Myrans_2024/data.csv", na = "")
