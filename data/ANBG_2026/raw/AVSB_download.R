# Download all Australian Virtual Seed Bank data (https://seedbank.test.ala.org.au)
# from the ALA events GraphQL API that the website itself queries.
# Three linked event levels: Accession -> Trial -> Treatment (joined by parentEventID).

library(httr2)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

out_dir <- "ignore/AVSB_download"
endpoint <- "https://events-databox.ala.org.au/graphql"
page_size <- 3000

# the 11 seed bank data resources hard-coded in the website (PRED_DATA_RESOURCE)
data_resources <- c("dr21622", "dr21623", "dr21624", "dr21625", "dr21626", "dr21636",
                    "dr21627", "dr21656", "dr21970", "dr21980", "dr22870")

seedbank_fields <- c(
  "accessionNumber", "herbariumVoucher", "populationCode", "degreeOfEstablishment",
  "seedPerGram", "formInStorage", "quantityInGrams", "quantityCount", "purityPercentage",
  "collectionFill", "dateCollected", "dateInStorage", "storageTemperatureInCelsius",
  "storageRelativeHumidityPercentage", "publicationDOI", "preStorageTreatment",
  "primaryStorageSeedBank", "primaryCollector", "plantForm", "duplicatesReplicates",
  "collectionPermitNumber", "thousandSeedWeight", "numberPlantsSampled", "storageBehaviour",
  "esRatio", "dormancyClass", "testDateStarted", "testLengthInDays", "numberGerminated",
  "germinationRateInDays", "adjustedGerminationPercentage", "viabilityPercentage",
  "numberFull", "numberEmpty", "numberTested", "numberNotViable", "preTestProcessingNotes",
  "pretreatment", "mediaSubstrate", "nightTemperatureInCelsius", "dayTemperatureInCelsius",
  "darkHours", "lightHours"
)

event_fields <- c(
  "eventID", "parentEventID", "eventHierarchyJoined", "datasetKey", "datasetTitle",
  "eventDate", "year", "month", "day", "locality", "locationID", "stateProvince", "country",
  "decimalLatitude", "decimalLongitude", "eventRemarks"
)

query <- paste0(
  "query list($predicate: Predicate, $size: Int, $from: Int) {
    eventSearch(size: $size, from: $from, predicate: $predicate) {
      documents {
        total
        results {
          ", paste(event_fields, collapse = " "), "
          measurementOrFacts { measurementType measurementValue measurementUnit }
          extensions { seedbank { ", paste(seedbank_fields, collapse = " "), " } }
        }
      }
    }
  }"
)

gql <- function(variables) {
  request(endpoint) |>
    req_body_json(list(query = query, variables = variables)) |>
    req_retry(max_tries = 5) |>
    req_timeout(300) |>
    req_perform() |>
    resp_body_json()
}

# one row per event; NULLs -> NA
flatten_event <- function(x) {
  sb <- x$extensions$seedbank
  core <- map(set_names(event_fields), ~ x[[.x]] %||% NA)
  sb <- map(set_names(seedbank_fields), ~ sb[[.x]] %||% NA)
  as_tibble(c(core, sb))
}

flatten_mof <- function(x) {
  mof <- x$measurementOrFacts
  if (length(mof) == 0) return(NULL)
  tibble(
    eventID = x$eventID,
    measurementType = map_chr(mof, ~ .x$measurementType %||% NA_character_),
    measurementValue = map_chr(mof, ~ as.character(.x$measurementValue %||% NA)),
    measurementUnit = map_chr(mof, ~ .x$measurementUnit %||% NA_character_)
  )
}

fetch_level <- function(event_type) {
  predicate <- list(type = "and", predicates = list(
    list(type = "in", key = "datasetKey", values = as.list(data_resources)),
    list(type = "equals", key = "eventType", value = event_type)
  ))
  first <- gql(list(predicate = predicate, size = page_size, from = 0))
  total <- first$data$eventSearch$documents$total
  results <- first$data$eventSearch$documents$results
  starts <- seq(page_size, total - 1, by = page_size)
  for (from in starts[starts < total]) {
    message(event_type, ": ", from, " / ", total)
    page <- gql(list(predicate = predicate, size = page_size, from = from))
    results <- c(results, page$data$eventSearch$documents$results)
  }
  stopifnot(length(results) == total)
  message(event_type, ": ", total, " records")

  events <- map(results, flatten_event) |>
    list_rbind() |>
    mutate(across(any_of(c("dateCollected", "dateInStorage", "testDateStarted")),
                  ~ as.Date(as.POSIXct(as.numeric(.x) / 1000, origin = "1970-01-01", tz = "UTC")))) |>
    select(where(~ !all(is.na(.x))))
  mof <- map(results, flatten_mof) |> list_rbind()

  list(events = events, mof = mof)
}

# measurementOrFacts to wide: one column per measurementType (repeats joined with " | ")
mof_wide <- function(mof) {
  if (is.null(mof) || nrow(mof) == 0) return(NULL)
  mof |>
    summarise(measurementValue = paste(unique(measurementValue), collapse = " | "),
              .by = c(eventID, measurementType)) |>
    pivot_wider(names_from = measurementType, values_from = measurementValue)
}

levels <- c(accessions = "Accession", trials = "Trial", treatments = "Treatment")
downloads <- map(levels, fetch_level)

for (lvl in names(levels)) {
  events <- downloads[[lvl]]$events
  wide <- mof_wide(downloads[[lvl]]$mof)
  if (!is.null(wide)) events <- left_join(events, wide, by = "eventID")
  write_csv(events, file.path(out_dir, paste0("AVSB_", lvl, ".csv")), na = "")
  if (nrow(downloads[[lvl]]$mof) > 0)
    write_csv(downloads[[lvl]]$mof, file.path(out_dir, paste0("AVSB_", lvl, "_measurementOrFacts.csv")), na = "")
}

# combined: one row per accession x trial x treatment (accessions without trials kept)
read_level <- function(lvl, prefix) {
  read_csv(file.path(out_dir, paste0("AVSB_", lvl, ".csv")), col_types = cols(.default = "c")) |>
    rename_with(~ paste0(prefix, .x))
}
accessions <- read_level("accessions", "accession_")
trials <- read_level("trials", "trial_")
treatments <- read_level("treatments", "treatment_")

combined <- accessions |>
  left_join(trials, by = c("accession_eventID" = "trial_parentEventID"), keep = TRUE) |>
  left_join(treatments, by = c("trial_eventID" = "treatment_parentEventID"), keep = TRUE)

write_csv(combined, file.path(out_dir, "AVSB_combined.csv"), na = "")
message("combined: ", nrow(combined), " rows")
