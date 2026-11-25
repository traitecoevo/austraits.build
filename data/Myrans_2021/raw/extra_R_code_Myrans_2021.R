# Writes data/Myrans_2021/data.csv from the contributor's raw data file (raw xlsx), one row per
# plant, adding derived columns so their values sit alongside the data. Run from the repo root.
# Values are read from the xlsx at full precision (a Save As csv from Excel rounds them to the
# digits displayed).
#   - taxon_name: from the sheet name (one sheet per species).
#   - individual_id: species code and plant ID (plant IDs are numbered within each species).
#   - location_name: the seed provenance site of each wild accession, as named in metadata.yml;
#     blank for the S. bicolor line BTx623, which has no provenance site.
#   - AGG_ID, accession_name: from Table 2 of the primary reference.
#   - cultivar_status: "cultivar" for the domesticated S. bicolor line BTx623, "wild type" for
#     the wild accessions.
# The S. bicolor sheet has an extra flower head biomass column. Trait columns are renamed to
# plain-ASCII names; values are unchanged.
# Silicon concentrations are not in the raw data file. Species means (and SE) for the HN
# treatment are added as six extra rows, one per species, from raw/Fig4_Si_digitised.csv:
# values digitised from Fig. 4 of the primary reference by locating bar and error-bar tops
# to the pixel against the y-axis ticks (1 pixel = 0.0026 % Si).

library(dplyr)
library(readr)
library(readxl)

file <- "data/Myrans_2021/raw/Myrans_2021 raw data.xlsx"

accessions <- tribble(
  ~sheet,            ~taxon_name,             ~species_code, ~AGG_ID,           ~accession_name, ~location_name,
  "S. bicolor",      "Sorghum bicolor",       "bic",         NA,                "BTx623",        NA,
  "S. brachypodum",  "Sorghum brachypodum",   "bra",         "AGG 302480 WSOR", "JC 2125",       "Kakadu (JC2125)",
  "S. laxiflorum",   "Sorghum laxiflorum",    "lax",         "AGG 302510 WSOR", "JC 2174",       "Nicholson (JC2174)",
  "S. leiocladum",   "Sorghum leiocladum",    "lei",         "AGG 300170 WSOR", "ATCGRC 0062",   "Drake (ATCGRC0062)",
  "S. macrospermum", "Sorghum macrospermum",  "mac",         "AGG 302367 WSOR", "JC 2253",       "Katherine (JC2253)",
  "S. matarankense", "Sorghum matarankense",  "mat",         "AGG 302517 WSOR", "JC 2183",       "Borroloola (JC2183)"
)

column_names <- c(
  "Plant ID" = "plant_id", "Treatment" = "nitrogen_treatment", "Fv/Fm" = "Fv_Fm",
  "Net photosynthesis rate" = "net_photosynthesis_umol_per_m2_per_s",
  "Stem length" = "stem_length_cm", "Total leaf area" = "total_leaf_area_cm2",
  "Leaf count" = "leaf_count", "Biomass green leaf" = "biomass_green_leaf_g",
  "Biomass dead leaf" = "biomass_dead_leaf_g", "Biomass sheath" = "biomass_sheath_g",
  "Biomass root" = "biomass_root_g", "Biomass flower head" = "biomass_flower_head_g",
  "Biomass total" = "biomass_total_g", "RGR" = "RGR_g_per_g_per_day", "SLA" = "SLA_cm2_per_g",
  "HCNp leaf" = "leaf_HCNp_mg_per_g", "HCNp sheath" = "sheath_HCNp_mg_per_g",
  "HCNp root" = "root_HCNp_mg_per_g", "[Nitrate] leaf" = "leaf_nitrate_mg_per_g",
  "[Nitrate] sheath" = "sheath_nitrate_mg_per_g", "[Nitrate] root" = "root_nitrate_mg_per_g",
  "[Phenolics] leaf" = "leaf_phenolics_mg_per_g", "[Phenolics] sheath" = "sheath_phenolics_mg_per_g",
  "[Phenolics] root" = "root_phenolics_mg_per_g",
  "ANplant" = "N_productivity_g_per_day_per_mgN", "Plant HCN-N/N" = "plant_HCN_N_percent_of_N",
  "N% leaf" = "leaf_N_percent", "N% sheath" = "sheath_N_percent", "N% root" = "root_N_percent",
  "Days grown" = "days_grown"
)

data <- bind_rows(lapply(accessions$sheet, function(s) {
  d <- read_excel(file, sheet = s)
  names(d) <- unname(column_names[names(d)])
  mutate(d, sheet = s)
}))

data <- data |>
  left_join(accessions, by = "sheet") |>
  mutate(
    individual_id = paste(species_code, plant_id, sep = "_"),
    cultivar_status = if_else(taxon_name == "Sorghum bicolor", "cultivar", "wild type")
  ) |>
  select(taxon_name, individual_id, plant_id, nitrogen_treatment, location_name, AGG_ID,
         accession_name, cultivar_status, days_grown, any_of(unname(column_names)))

silicon <- read_csv("data/Myrans_2021/raw/Fig4_Si_digitised.csv", show_col_types = FALSE) |>
  tidyr::pivot_wider(names_from = organ, values_from = c(Si_percent, Si_percent_SE),
                     names_glue = "{organ}_{.value}") |>
  left_join(accessions, by = "taxon_name") |>
  mutate(
    nitrogen_treatment = "HN",
    cultivar_status = if_else(taxon_name == "Sorghum bicolor", "cultivar", "wild type")
  ) |>
  select(taxon_name, nitrogen_treatment, location_name, AGG_ID, accession_name,
         cultivar_status, leaf_Si_percent, leaf_Si_percent_SE, sheath_Si_percent,
         sheath_Si_percent_SE, root_Si_percent, root_Si_percent_SE)

data <- bind_rows(data, silicon)

write_csv(data, "data/Myrans_2021/data.csv", na = "")
