# Builds data/Paroissien_2026_2/data.csv from the Dryad file
# (https://doi.org/10.5061/dryad.sf7m0cgj9): one row per species x site x depth
# (leaf litter, soil) for every species that emerged from the soil seed bank
# samples at that site. Each site has a single fire severity and fire interval
# class, so these are carried through as columns. Days to first emergence are
# counted from the experiment set-up date, 10 February 2023 (Supporting
# Information, Figure S1). Run from the repository root.

raw <- read.csv(
  "data/Paroissien_2026_2/raw/Data_for__The_post-fire_recovery_of_soil_seed_banks_along_a_fire_severity_gradient_in_an_Australian_threatened_mesic_forest_.csv",
  check.names = FALSE, colClasses = "character", na.strings = "NA"
)

extant <- raw |>
  dplyr::filter(ExtantorSSB == "Extant vegetation") |>
  dplyr::distinct(Site, Species) |>
  dplyr::mutate(in_extant_vegetation = "yes")

raw |>
  dplyr::filter(ExtantorSSB == "Soil seed bank") |>
  dplyr::mutate(
    Count = as.numeric(Count),
    DateFirstEmergence = as.Date(DateFirstEmergence, format = "%d/%m/%Y")
  ) |>
  dplyr::group_by(Species, Site, Severity, FireIntervalGroups, Depth) |>
  dplyr::summarise(
    FunctionalGroup = dplyr::first(FunctionalGroup),
    seedbank_location = "soil_seedbank",
    plots_with_seedlings = dplyr::n_distinct(Quadrat),
    seedlings_emerged = sum(Count),
    first_emergence_date = format(min(DateFirstEmergence)),
    last_first_emergence_date = format(max(DateFirstEmergence)),
    days_to_first_emergence = as.numeric(min(DateFirstEmergence) - as.Date("2023-02-10")),
    .groups = "drop"
  ) |>
  dplyr::left_join(extant, by = c("Site", "Species")) |>
  dplyr::mutate(in_extant_vegetation = dplyr::coalesce(in_extant_vegetation, "no")) |>
  dplyr::arrange(Severity, Site, Species, Depth) |>
  readr::write_csv("data/Paroissien_2026_2/data.csv", na = "")
