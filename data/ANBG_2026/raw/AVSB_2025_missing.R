# Find trait values in the Feb 2025 AVSB website downloads (the 2025 data.csv files, read from
# commit 245b581a on branch seedbank_datasets) that are no longer in the Nov 2026 API
# download, and write them out:
#   - data/<id>/raw/2025_missing_values.csv: the 2025 rows, relabelled to the 2026 data.csv
#     headers, with `Data Version` = "2025 download". AVSB_split_by_seedbank.R appends these
#     to data.csv. Only the missing values are kept; values still in the 2026 download
#     (unchanged or revised by the seed bank) are blanked so nothing is entered twice.
#   - ignore/AVSB_download/AVSB_2025_values_missing_from_2026.csv: one row per missing value,
#     for following up with the seed banks.
# Germination counts (Number Germinated) only count as a germination value on rows without an
# adjusted germination percentage. A 2025 value counts as present if the same accession (or, failing that, the same taxon) has
# the same value for that field in 2026; as revised if the accession still has that field
# with another value; otherwise as missing.

library(dplyr)
library(readr)
library(tidyr)

dl <- "ignore/AVSB_download"
data_dir <- "../austraits.build-seedbank/data"

seed_banks <- c(
  "National Seed Bank" = "ANBG_2026",
  "Australian PlantBank" = "Australian_PlantBank_2026",
  "Brisbane Botanic Gardens" = "Brisbane_Botanic_Gardens_2026",
  "Greening Australia" = "Greening_Australia_2026",
  "South Australian Seed Conservation Centre" = "SA_Seed_Conservation_Centre_2026",
  "Tasmanian Seed Conservation Centre" = "Tas_Seed_Conservation_Centre_2026",
  "Victorian Conservation Seed Bank" = "Victorian_Conservation_Seed_Bank_2026",
  "The Western Australian Seed Centre (Kings Park)" = "WA_Seed_Centre_2026"
)

# 2025 column -> AVSB field; these are the columns that feed traits
trait_fields <- c(
  "Thousand Seed Weight" = "thousandSeedWeight",
  "Seed/gm" = "seedPerGram",
  "Adjusted Germination" = "adjustedGerminationPercentage",
  "Viability" = "viabilityPercentage",
  "Number Germinated" = "numberGerminated"
)
renames_2026 <- c("Adjusted Germination %" = "Adjusted Germination", "Viability %" = "Viability")
date_cols <- c("Collection Date", "Storage Date", "Date Tested")

new_acc <- read_csv(file.path(dl, "AVSB_accessions.csv"), col_types = cols(.default = "c"))
new_trial <- read_csv(file.path(dl, "AVSB_trials.csv"), col_types = cols(.default = "c"))

new_long <- bind_rows(
  new_acc |> mutate(key = coalesce(accessionNumber, eventID)),
  new_trial |>
    left_join(select(new_acc, eventID, acc_key = accessionNumber), by = c("parentEventID" = "eventID")) |>
    mutate(key = coalesce(accessionNumber, acc_key, parentEventID))
) |>
  select(datasetTitle, key, taxon = `Taxon Matched Name`, any_of(unname(trait_fields))) |>
  pivot_longer(any_of(unname(trait_fields)), names_to = "field", values_to = "value", values_drop_na = TRUE) |>
  mutate(value = as.character(signif(as.numeric(value), 4)))
new_keys <- new_acc |> transmute(datasetTitle, key = coalesce(accessionNumber, eventID))

value_status <- function(title, key, taxon, field, value) {
  v <- as.character(signif(as.numeric(value), 4))
  same <- new_long$datasetTitle == title & new_long$field == field
  case_when(
    any(same & new_long$key == key & new_long$value == v) ~ "present",
    any(same & new_long$taxon %in% taxon & new_long$value == v) ~ "present",
    any(same & new_long$key == key) ~ "revised",
    any(new_keys$key == key & new_keys$datasetTitle != title) ~ "accession moved to another seed bank",
    any(new_keys$key == key) ~ "accession in 2026 download, but not this value",
    .default = "accession not in 2026 download"
  )
}
missing_status <- c("accession in 2026 download, but not this value", "accession not in 2026 download")

all_missing <- list()
for (title in names(seed_banks)) {
  id <- seed_banks[[title]]
  old_csv <- system2("git", c("-C", data_dir, "show", paste0("245b581a:data/", sub("_2026$", "_2025", id), "/data.csv")),
                     stdout = TRUE)
  old <- read_csv(I(paste(old_csv, collapse = "\n")), col_types = cols(.default = "c")) |>
    distinct() |>
    mutate(row = row_number(), key = coalesce(`Accession Number`, sub("-trial.*$", "", `Event ID`)))
  checks <- old |>
    select(row, key, `Dataset Title`, `Scientific Name`, any_of(names(trait_fields))) |>
    pivot_longer(any_of(names(trait_fields)), names_to = "column", values_to = "value", values_drop_na = TRUE) |>
    filter(!is.na(suppressWarnings(as.numeric(value)))) |>
    filter(!(column == "Number Germinated" & row %in% old$row[!is.na(old$`Adjusted Germination`)])) |>
    rowwise() |>
    mutate(status = value_status(`Dataset Title`, key, `Scientific Name`, trait_fields[[column]], value)) |>
    ungroup() |>
    filter(status %in% missing_status)
  if (nrow(checks) == 0) next

  missing_cols <- checks |> summarise(cols = list(column), reason = paste(paste0(column, ": ", status), collapse = "; "), .by = row)
  rows <- old |>
    inner_join(missing_cols, by = "row") |>
    rowwise() |>
    mutate(
      across(any_of(names(trait_fields)), \(x) if (cur_column() %in% cols) x else NA_character_),
      `Number Tested` = if (any(c("Adjusted Germination", "Viability", "Number Germinated") %in% cols)) `Number Tested` else NA_character_
    ) |>
    ungroup() |>
    mutate(
      `Event Type` = ifelse(is.na(`Thousand Seed Weight`) & is.na(`Seed/gm`), "Trial", "Accession"),
      across(any_of(date_cols), \(x) as.character(lubridate::mdy(x, quiet = TRUE))),
      `Data Version` = "2025 download",
      `Missing from 2026 download` = reason
    ) |>
    rename(any_of(renames_2026))
  if (id == "ANBG_2026") rows <- rename(rows, any_of(c("Collection fill / X-ray" = "Collection Fill Rate / X-Ray")))

  # state from coordinates, for the locations block
  pts <- rows |> filter(!is.na(`Decimal Latitude`)) |> distinct(`Decimal Latitude`, `Decimal Longitude`)
  if (nrow(pts) > 0) {
    states <- sf::st_as_sf(pts, coords = c("Decimal Longitude", "Decimal Latitude"), crs = 4283, remove = FALSE) |>
      sf::st_join(sf::st_transform(ozmaps::ozmap_states, 4283)) |>
      sf::st_drop_geometry() |>
      select(`Decimal Latitude`, `Decimal Longitude`, State = NAME)
    rows <- left_join(rows, states, by = c("Decimal Latitude", "Decimal Longitude"))
  }

  current_cols <- names(read_csv(file.path(data_dir, id, "data.csv"), n_max = 0, col_types = cols(.default = "c")))
  out <- rows |> select(any_of(current_cols), `Data Version`, `Missing from 2026 download`)
  write_csv(out, file.path(data_dir, id, "raw/2025_missing_values.csv"), na = "")
  message(id, ": ", nrow(out), " rows, ", nrow(checks), " missing values")

  all_missing[[id]] <- checks |>
    left_join(select(old, row, `Event ID`, `Accession Number`, Locality, `Decimal Latitude`,
                     `Decimal Longitude`, `Collection Date`, `Date Tested`, `Number Tested`), by = "row") |>
    transmute(`Seed bank` = `Dataset Title`, dataset_id = id, `Accession Number` = key,
              `Event ID (2025)` = `Event ID`, `Scientific Name`, Field = column, `Value (2025)` = value,
              Status = status, `Number Tested`, `Collection Date`, `Date Tested`, Locality,
              `Decimal Latitude`, `Decimal Longitude`)
}

bind_rows(all_missing) |>
  arrange(`Seed bank`, Status, Field, `Accession Number`) |>
  write_csv(file.path(dl, "AVSB_2025_values_missing_from_2026.csv"), na = "")
