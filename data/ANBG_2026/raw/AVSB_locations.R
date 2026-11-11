# Write each seed bank dataset's locations.csv (read via `locations: locations.csv` in
# metadata.yml) from its data.csv: one location per coordinate pair, named as in
# custom_R_code ("seeds_collected_at_<lat>_lat_and_<long>_long"). Accessions sharing
# coordinates but recorded with different locality text have those localities joined
# with "; ".

library(dplyr)
library(readr)

data_dir <- "data"
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
  x <- stringr::str_remove(x, "^~\\s*")
  x <- unique(stringr::str_squish(x[!is.na(x) & x != ""]))
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

latitude_sign_errors <- c("CANB 916974.2", "CANB 916974.3", "CANB 916974.4", "CANB 916974.5")
latitude_corrections <- c("CANB 960384.2" = "-35.8331")

write_locations <- function(id) {
  data <- read_csv(file.path(data_dir, id, "data.csv"), col_types = cols(.default = "c"))
  extra <- intersect(names(site_properties), names(data))
  for (col in setdiff(c("Locality", "State"), names(data))) data[[col]] <- NA_character_
  locs <- data |>
    filter(!is.na(`Scientific Name`), !is.na(`Decimal Latitude`), !is.na(`Decimal Longitude`)) |>
    # latitude errors, also corrected in that dataset's custom_R_code
    mutate(`Decimal Latitude` = ifelse(`Accession Number` %in% latitude_sign_errors,
                                       as.character(-abs(as.numeric(`Decimal Latitude`))),
                                       `Decimal Latitude`),
           `Decimal Latitude` = dplyr::coalesce(latitude_corrections[`Accession Number`], `Decimal Latitude`)) |>
    mutate(location_name = paste0("seeds_collected_at_", `Decimal Latitude`, "_lat_and_",
                                  `Decimal Longitude`, "_long")) |>
    summarise(
      `latitude (deg)` = first(`Decimal Latitude`),
      `longitude (deg)` = first(`Decimal Longitude`),
      locality = collapse_unique(Locality),
      across(all_of(extra), collapse_unique),
      state = collapse_unique(coalesce(territory_code(as.numeric(`latitude (deg)`), as.numeric(`longitude (deg)`)),
                                       state_codes[State])),
      .by = location_name
    ) |>
    rename(any_of(setNames(names(site_properties), site_properties))) |>
    select(where(~ !all(is.na(.x)))) |>
    relocate(any_of("state"), .after = last_col()) |>
    arrange(location_name)
  write_csv(locs, file.path(data_dir, id, "locations.csv"), na = "")
  message(id, ": ", nrow(locs), " locations")
}

purrr::walk(dataset_ids, write_locations)
