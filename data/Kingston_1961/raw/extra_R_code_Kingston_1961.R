# Builds data/Kingston_1961/data.csv from the spreadsheet transcription of
# Kingston & Risdon (1961) Table 1 ("Kingston and Risdon.xls") plus
# raw/table1_symbols.csv, which records information from Table 1 that the
# spreadsheet dropped: the footnote symbols (* density from different methods
# only; double dagger: basic density partly from a different method, with the
# bracketed air-dry densities calculated from basic density; dagger:
# plantation-grown material), tree ages, and states omitted by the spreadsheet
# (Eucalyptus propinqua, Toona australis). Cryptocarya alleniana is New Guinea
# material in Table 1 (the spreadsheet says Qld) and is marked non-Australian.
# Run from the repository root: source("data/Kingston_1961/raw/extra_R_code_Kingston_1961.R")

library(dplyr)

dir <- "data/Kingston_1961"

xls <- readxl::read_excel(
  file.path(dir, "raw/Kingston and Risdon.xls"),
  .name_repair = "minimal"
)

# Spreadsheet columns, in order, renamed to unique snake_case names
names(xls) <- c(
  "scientific_name", "genus", "species", "sub_species", "name_notes",
  "age_of_timber_notes", "common_name",
  "NSW", "Vic", "Qld", "SA", "Tas", "WA", "ref_within_WA", "NT", "ACT",
  "Australia", "New_Zealand", "Papua_New_Guinea", "New_Guinea",
  "Solomon_Islands", "Borneo", "Malaysia", "Malaya", "Fiji",
  "basic_density", "basic_density_n", "basic_density_SE",
  "basic_density_CI_lower", "basic_density_CI_upper", "basic_density_CI",
  "air_dry_density_BR", "air_dry_density_BR_n", "air_dry_density_BR_SE",
  "air_dry_density_BR_CI_lower", "air_dry_density_BR_CI_upper", "air_dry_density_BR_CI",
  "air_dry_density_AR", "air_dry_density_AR_n", "air_dry_density_AR_SE",
  "air_dry_density_AR_CI_lower", "air_dry_density_AR_CI_upper", "air_dry_density_AR_CI",
  "unit_shrinkage_tangential", "unit_shrinkage_tangential_n",
  "unit_shrinkage_tangential_SE", "unit_shrinkage_tangential_CI",
  "unit_shrinkage_radial", "unit_shrinkage_radial_n",
  "unit_shrinkage_radial_SE", "unit_shrinkage_radial_CI",
  "shrinkage_12MC_BR_tangential", "shrinkage_12MC_BR_tangential_n",
  "shrinkage_12MC_BR_tangential_SE", "shrinkage_12MC_BR_tangential_CI",
  "shrinkage_12MC_BR_radial", "shrinkage_12MC_BR_radial_n",
  "shrinkage_12MC_BR_radial_SE", "shrinkage_12MC_BR_radial_CI",
  "shrinkage_12MC_AR_tangential", "shrinkage_12MC_AR_tangential_n",
  "shrinkage_12MC_AR_tangential_SE", "shrinkage_12MC_AR_tangential_CI",
  "shrinkage_12MC_AR_radial", "shrinkage_12MC_AR_radial_n",
  "shrinkage_12MC_AR_radial_SE", "shrinkage_12MC_AR_radial_CI"
)

# Format numbers without floating-point noise (0.36 not 0.35999999999999999)
fmt <- function(x) {
  if (is.numeric(x)) {
    out <- format(signif(x, 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    out[is.na(x)] <- NA_character_
    out
  } else {
    x
  }
}

xls <- xls |>
  mutate(across(everything(), fmt)) |>
  mutate(row_source = "Kingston and Risdon.xls")

# Pinus taeda (6 years old, plantation, Qld) is in Table 1 (p. 43) but missing
# from the spreadsheet; values transcribed from the PDF
taeda_6 <- tibble(
  scientific_name = "Pinus taeda", genus = "Pinus", species = "taeda",
  common_name = "PINE, LOBLOLLY", Qld = "1",
  basic_density = "362", basic_density_n = "70", basic_density_SE = "3.4",
  basic_density_CI = "290-442",
  air_dry_density_BR = "426", air_dry_density_BR_n = "70", air_dry_density_BR_SE = "4.2",
  air_dry_density_BR_CI = "336-525",
  air_dry_density_AR = "420", air_dry_density_AR_n = "51", air_dry_density_AR_SE = "4.6",
  air_dry_density_AR_CI = "338-509",
  shrinkage_12MC_BR_tangential = "3", shrinkage_12MC_BR_tangential_n = "70",
  shrinkage_12MC_BR_tangential_SE = "0.06", shrinkage_12MC_BR_tangential_CI = "1.6-4.6",
  shrinkage_12MC_BR_radial = "1.8", shrinkage_12MC_BR_radial_n = "70",
  shrinkage_12MC_BR_radial_SE = "0.04", shrinkage_12MC_BR_radial_CI = "1.1-2.5",
  shrinkage_12MC_AR_tangential = "2.8", shrinkage_12MC_AR_tangential_n = "51",
  shrinkage_12MC_AR_tangential_SE = "0.07", shrinkage_12MC_AR_tangential_CI = "1.5-4.1",
  shrinkage_12MC_AR_radial = "1.9", shrinkage_12MC_AR_radial_n = "51",
  shrinkage_12MC_AR_radial_SE = "0.05", shrinkage_12MC_AR_radial_CI = "1.1-2.6",
  row_source = "Kingston_1961 Table 1 p. 43 (missing from spreadsheet)"
)

data <- bind_rows(xls, taeda_6)

# Numbers of trees missing from the spreadsheet, transcribed from Table 1
data <- data |>
  mutate(
    basic_density_n = case_when(
      scientific_name == "Eucalyptus propinqua" ~ "38",
      scientific_name == "Melicope sp." ~ "3",
      TRUE ~ basic_density_n
    ),
    air_dry_density_BR_n = if_else(scientific_name == "Eucalyptus propinqua", "15", air_dry_density_BR_n),
    air_dry_density_AR_n = if_else(scientific_name == "Eucalyptus propinqua", "15", air_dry_density_AR_n)
  )

symbols <- read.csv(file.path(dir, "raw/table1_symbols.csv"),
                    colClasses = "character", encoding = "UTF-8")

# Basic density as kg/m3 integer string, used to key rows of repeated taxa
bd_key <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  as.character(round(ifelse(v < 5, v * 1000, v)))
}

data <- data |>
  mutate(.key = bd_key(basic_density), .row = row_number())

matched <- symbols |>
  rowwise() |>
  mutate(.row = list(data$.row[
    data$scientific_name == scientific_name &
      (basic_density_key == "" | data$.key == basic_density_key) &
      (xls_age_key == "" | data$age_of_timber_notes %in% xls_age_key)
  ])) |>
  ungroup()

stopifnot(all(lengths(matched$.row) == 1))

matched <- matched |>
  mutate(.row = unlist(.row)) |>
  select(.row, basic_density_symbol, air_dry_BR_symbol, plantation, air_dry_calculated, tree_age_pdf,
         states_pdf, australian_material_override)

aus <- c(NSW = "NSW", Vic = "Vic", Qld = "Qld", SA = "SA", Tas = "Tas",
         WA = "WA", NT = "NT", ACT = "ACT")
other <- c(New_Zealand = "New Zealand", New_Guinea = "New Guinea",
           Solomon_Islands = "Solomon Islands", Borneo = "Borneo",
           Malaya = "Malaya", Fiji = "Fiji")

region_string <- function(df, lookup) {
  apply(df[, names(lookup)], 1, function(r) {
    x <- unname(lookup[!is.na(r)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  })
}

data <- data |>
  left_join(matched, by = ".row") |>
  mutate(
    taxon_name = if_else(
      !is.na(sub_species) & grepl("^(ssp\\.|var\\.)", sub_species),
      paste(scientific_name, sub_species),
      scientific_name
    ),
    taxon_name = gsub("\\s+", " ", taxon_name),
    plantation = coalesce(plantation, "FALSE"),
    air_dry_calculated = coalesce(air_dry_calculated, "FALSE"),
    basis_of_record = if_else(plantation == "TRUE", "captive_cultivated", "field"),
    tree_age = tree_age_pdf,
    basic_density_method = case_when(
      basic_density_symbol == "*" ~ "earlier methods",
      basic_density_symbol == "‡" ~ "partly earlier methods",
      TRUE ~ NA_character_
    ),
    air_dry_density_BR_method = if_else(air_dry_BR_symbol %in% "*", "earlier methods", NA_character_),
    states = region_string(pick(everything()), aus),
    states = if_else(!is.na(states_pdf) & states_pdf != "", states_pdf, states),
    other_regions = region_string(pick(everything()), other),
    location_name = case_when(
      !is.na(states) & !is.na(other_regions) ~ paste(states, other_regions, sep = "; "),
      !is.na(states) ~ states,
      TRUE ~ other_regions
    ),
    australian_material = case_when(
      !is.na(australian_material_override) & australian_material_override != "" ~ australian_material_override,
      is.na(states) ~ "FALSE",
      TRUE ~ "TRUE"
    )
  ) |>
  select(-.key, -.row, -tree_age_pdf, -states_pdf, -australian_material_override)

write.csv(data, file.path(dir, "data.csv"), row.names = FALSE, na = "")
