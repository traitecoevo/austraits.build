# Build one data.csv per seed bank from the Australian Virtual Seed Bank download
# (made by AVSB_download.R, saved outside the repo in `dl`).
# Rows are stacked: one row per accession (seed lot: collection, seed mass, storage) and one
# row per germination trial (with its treatment, plus the parent accession's taxon, locality,
# coordinates and dates). Column labels follow the AVSB website's own csv downloads; extra
# measurementOrFact fields keep their AVSB names.

library(dplyr)
library(readr)
library(purrr)

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
  "The Western Australian Seed Centre (Kings Park)" = "WA_Seed_Centre_2026",
  "George Brown Darwin Botanic Gardens Seed Bank" = "Darwin_Botanic_Gardens_2026"
)
# Not built: the WA Seed Centre (Kensington) and Alice Springs Desert Park records hold no
# seed mass, germination or other trait values (only names, stock and population counts).

# AVSB field -> website download label
accession_labels <- c(
  "Event ID" = "eventID", "Accession Number" = "accessionNumber",
  "Herbarium Voucher" = "herbariumVoucher", "Scientific Name" = "Taxon Matched Name",
  "Supplied Name" = "Taxon Supplied Name", "Taxon ID" = "Taxon Matched ID",
  "Dataset Title" = "datasetTitle", "Locality" = "locality",
  "Decimal Latitude" = "decimalLatitude", "Decimal Longitude" = "decimalLongitude",
  "State" = "stateProvince", "Seed/gm" = "seedPerGram", "Quantity (count)" = "quantityCount",
  "Quantity (g)" = "quantityInGrams", "Number of Plants Sampled" = "numberPlantsSampled",
  "Storage Behaviour" = "storageBehaviour", "Thousand Seed Weight" = "thousandSeedWeight",
  "Storage Relative Humidity" = "storageRelativeHumidityPercentage", "Purity" = "purityPercentage",
  "Storage Temperature" = "storageTemperatureInCelsius", "Collection Date" = "dateCollected",
  "Storage Date" = "dateInStorage", "Storage Seed Bank" = "primaryStorageSeedBank",
  "Collector" = "primaryCollector", "Pre-Storage Treatment/History" = "preStorageTreatment",
  "Plant Form" = "plantForm", "Form in Storage" = "formInStorage",
  "Cultivated" = "degreeOfEstablishment", "Duplicates / Replicates" = "duplicatesReplicates",
  "Permit Number" = "collectionPermitNumber", "Collection Fill Rate / X-Ray" = "collectionFill",
  "Publication DOI" = "publicationDOI", "ES Ratio" = "esRatio", "Dormancy Class" = "dormancyClass",
  "Population Code" = "populationCode", "Event Remarks" = "eventRemarks"
)
trial_labels <- c(
  "Event ID" = "eventID", "Parent Event ID" = "parentEventID",
  "Accession Number" = "accessionNumber", "Herbarium Voucher" = "herbariumVoucher",
  "Scientific Name" = "Taxon Matched Name", "Supplied Name" = "Taxon Supplied Name",
  "Taxon ID" = "Taxon Matched ID", "Dataset Title" = "datasetTitle",
  "Date Tested" = "testDateStarted", "Test Length" = "testLengthInDays",
  "Number Germinated" = "numberGerminated", "Number Full" = "numberFull",
  "Number Empty" = "numberEmpty", "Number Not Viable" = "numberNotViable",
  "Number Tested" = "numberTested", "Germination Rate" = "germinationRateInDays",
  "Adjusted Germination %" = "adjustedGerminationPercentage", "Viability %" = "viabilityPercentage",
  "Pre-test Processing Notes" = "preTestProcessingNotes", "Event Remarks" = "eventRemarks"
)
treatment_labels <- c(
  "Treatment Media/Substrate" = "mediaSubstrate", "Treatment Day Temp" = "dayTemperatureInCelsius",
  "Treatment Night Temp" = "nightTemperatureInCelsius", "Treatment Light Hours" = "lightHours",
  "Treatment Dark Hours" = "darkHours", "Treatment Pretreatment" = "pretreatment",
  "Treatment Remarks" = "eventRemarks"
)
# accession fields copied onto each trial row
parent_fields <- c("Locality", "Decimal Latitude", "Decimal Longitude", "State",
                   "Collection Date", "Storage Date", "Collector", "Cultivated")

read_dl <- function(lvl) read_csv(file.path(dl, paste0("AVSB_", lvl, ".csv")), col_types = cols(.default = "c"))
accessions <- read_dl("accessions")
trials <- read_dl("trials")
treatments <- read_dl("treatments")

core_fields <- c(accession_labels, trial_labels, treatment_labels, names(treatment_labels),
                 "eventHierarchyJoined", "datasetKey", "year", "month", "day", "locationID", "country")

# rename AVSB fields to labels; measurementOrFact columns keep their names, unless the name
# clashes with a label (SA's "Cultivated"), then suffixed " (AVSB measurement)"
relabel <- function(x, labels) {
  extra <- setdiff(names(x), core_fields)
  clash <- intersect(extra, names(labels))
  if (length(clash) > 0) x <- rename_with(x, ~ paste(.x, "(AVSB measurement)"), all_of(clash))
  x |>
    select(any_of(labels), all_of(setdiff(names(x), core_fields))) |>
    select(-any_of(c("Taxon Supplied Name", "Taxon Matched Name", "Taxon Matched ID")))
}

accession_rows <- accessions |>
  relabel(accession_labels) |>
  mutate(`Event Type` = "Accession", .after = `Event ID`)

trial_rows <- trials |>
  left_join(
    treatments |>
      select(trial_eventID = parentEventID, any_of(unname(treatment_labels))) |>
      rename(any_of(treatment_labels)),
    by = c("eventID" = "trial_eventID")
  ) |>
  relabel(c(trial_labels, setNames(names(treatment_labels), names(treatment_labels)))) |>
  left_join(
    accession_rows |> select(`Parent Event ID` = `Event ID`, all_of(parent_fields),
                             parent_accession = `Accession Number`, parent_name = `Scientific Name`,
                             parent_supplied = `Supplied Name`, parent_taxon_id = `Taxon ID`),
    by = "Parent Event ID"
  ) |>
  mutate(
    `Accession Number` = coalesce(`Accession Number`, parent_accession),
    `Scientific Name` = coalesce(`Scientific Name`, parent_name),
    `Supplied Name` = coalesce(`Supplied Name`, parent_supplied),
    `Taxon ID` = coalesce(`Taxon ID`, parent_taxon_id),
    `Event Type` = "Trial", .after = `Event ID`
  ) |>
  select(-starts_with("parent_"))

# NSB accessions whose seed mass is already in ANBG_2019 (its item_ID is the NSB accession event ID)
anbg_2019_seed_mass <- read_csv(file.path(data_dir, "ANBG_2019/data.csv"), col_types = cols(.default = "c")) |>
  filter(trait == "Seed Sample", trait_2 == "TSW") |>
  pull(item_ID) |>
  unique()

# numeric fields were written from doubles with float noise (132.89940000000001)
numeric_cols <- c("Decimal Latitude", "Decimal Longitude", "Seed/gm", "Quantity (count)",
                  "Quantity (g)", "Thousand Seed Weight", "Storage Relative Humidity", "Purity",
                  "Storage Temperature", "Test Length", "Number Germinated", "Number Full",
                  "Number Empty", "Number Not Viable", "Number Tested", "Germination Rate",
                  "Adjusted Germination %", "Viability %", "Treatment Day Temp",
                  "Treatment Night Temp", "Treatment Light Hours", "Treatment Dark Hours")
all_rows <- bind_rows(accession_rows, trial_rows) |>
  mutate(across(any_of(numeric_cols), ~ as.character(as.numeric(.x))))
lead_cols <- c("Event ID", "Event Type", "Parent Event ID", names(accession_labels)[-1],
               names(trial_labels), names(treatment_labels))

for (title in names(seed_banks)) {
  id <- seed_banks[[title]]
  out <- all_rows |>
    filter(`Dataset Title` == title)
  # 2025 values no longer in AVSB (from AVSB_2025_missing.R), flagged `Data Version` = "2025 download"
  missing_2025 <- file.path(data_dir, id, "raw/2025_missing_values.csv")
  if (file.exists(missing_2025)) {
    out <- bind_rows(
      out,
      read_csv(missing_2025, col_types = cols(.default = "c")) |>
        mutate(across(any_of(numeric_cols), ~ as.character(as.numeric(.x))))
    )
  }
  out <- out |>
    arrange(coalesce(`Parent Event ID`, `Event ID`), `Event Type`, `Event ID`) |>
    select(any_of(lead_cols), everything()) |>
    select(where(~ !all(is.na(.x))))
  if (id == "ANBG_2026") {
    out <- out |>
      mutate(omit_duplicate = ifelse(`Event Type` == "Accession" & !is.na(`Thousand Seed Weight`) &
                                       `Event ID` %in% anbg_2019_seed_mass, "ANBG_2019", NA))
  }
  dir.create(file.path(data_dir, id, "raw"), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, file.path(data_dir, id, "data.csv"), na = "")
  message(id, ": ", sum(out$`Event Type` == "Accession"), " accessions, ",
          sum(out$`Event Type` == "Trial"), " trials, ", ncol(out), " columns")
}
