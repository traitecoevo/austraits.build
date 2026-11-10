# Write the `locations:` block of each seed bank dataset's metadata.yml from its data.csv:
# one location per coordinate pair, named as in custom_R_code
# ("seeds_collected_at_<lat>_lat_and_<long>_long"). Accessions sharing coordinates but
# recorded with different locality text have those localities joined with "; ".
# Only the locations: section of metadata.yml is replaced.

library(dplyr)
library(readr)

data_dir <- "../austraits.build-seedbank/data"
dataset_ids <- c("ANBG_2026", "Australian_PlantBank_2026", "Brisbane_Botanic_Gardens_2026",
                 "Greening_Australia_2026", "SA_Seed_Conservation_Centre_2026",
                 "Tas_Seed_Conservation_Centre_2026", "Victorian_Conservation_Seed_Bank_2026",
                 "WA_Seed_Centre_2026", "Darwin_Botanic_Gardens_2026")

state_codes <- c("New South Wales" = "NSW", "Victoria" = "Vic", "Queensland" = "Qld",
                 "South Australia" = "SA", "Western Australia" = "WA", "Tasmania" = "Tas",
                 "Northern Territory" = "NT", "Australian Capital Territory" = "ACT")

# data.csv column -> location property (Darwin site descriptions)
site_properties <- c("Land Form" = "landform", "Soil" = "soil type",
                     "Geology" = "geology (parent material)", "Slope" = "slope angle (degrees)",
                     "Aspect" = "aspect", "Land Use" = "land use",
                     "Habitat Modification" = "disturbance")

# external territories have no stateProvince in AVSB, and Macquarie Island is given as
# Tasmania; APC codes from coordinates
territory_code <- function(lat, lon) {
  case_when(
    between(lat, -55, -54.4) & between(lon, 158.7, 159) ~ "MI",
    between(lat, -11, -10) & between(lon, 105, 106) ~ "ChI",
    between(lat, -30, -28.5) & between(lon, 167.5, 168.5) ~ "NI",
    between(lat, -24, -12) & between(lon, 147.5, 158) ~ "CSI",
    between(lat, -54, -52.5) & between(lon, 72.5, 74) ~ "HI",
    .default = NA_character_
  )
}

collapse_unique <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- unique(stringr::str_squish(x[!is.na(x) & x != ""]))
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

locations_yaml <- function(id) {
  data <- read_csv(file.path(data_dir, id, "data.csv"), col_types = cols(.default = "c"))
  extra <- intersect(names(site_properties), names(data))
  for (col in setdiff(c("Locality", "State"), names(data))) data[[col]] <- NA_character_
  locs <- data |>
    filter(!is.na(`Scientific Name`), !is.na(`Decimal Latitude`), !is.na(`Decimal Longitude`)) |>
    mutate(location_name = paste0("seeds_collected_at_", `Decimal Latitude`, "_lat_and_",
                                  `Decimal Longitude`, "_long")) |>
    summarise(
      `latitude (deg)` = as.numeric(first(`Decimal Latitude`)),
      `longitude (deg)` = as.numeric(first(`Decimal Longitude`)),
      locality = collapse_unique(Locality),
      across(all_of(extra), collapse_unique),
      state = collapse_unique(coalesce(territory_code(`latitude (deg)`, `longitude (deg)`),
                                       state_codes[State])),
      .by = location_name
    ) |>
    rename(any_of(setNames(names(site_properties), site_properties))) |>
    arrange(location_name)
  if (nrow(locs) == 0) return("locations: .na\n")
  loc_list <- lapply(seq_len(nrow(locs)), \(i) {
    row <- as.list(locs[i, -1])
    row[!is.na(row)]
  })
  names(loc_list) <- locs$location_name
  yaml::as.yaml(list(locations = loc_list), precision = 10, indent.mapping.sequence = TRUE)
}

replace_locations <- function(id) {
  path <- file.path(data_dir, id, "metadata.yml")
  lines <- readLines(path)
  start <- grep("^locations:", lines)
  end <- grep("^[a-z_]+:", lines)
  end <- end[end > start][1] - 1
  new <- strsplit(locations_yaml(id), "\n")[[1]]
  writeLines(c(lines[seq_len(start - 1)], new, lines[(end + 1):length(lines)]), path)
  message(id, ": ", sum(grepl("^  [^ ]", new)), " locations")
}

existing <- dataset_ids[file.exists(file.path(data_dir, dataset_ids, "metadata.yml"))]
purrr::walk(existing, replace_locations)
