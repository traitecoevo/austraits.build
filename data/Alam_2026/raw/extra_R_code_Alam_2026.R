# Writes data/Alam_2026/data.csv from the authors' Dryad files (doi:10.5061/dryad.zgmsbccst).
# Run from the repo root. Three files are stacked; source_file names the file each row came from.
#   - NT_NQ_leaf_data.csv: every row. NE-QLD-Cheesman rows are new data (source_id Alam_2026).
#     The 81 NAT-Cernusak leaves are the Cernusak et al. (2011) data already in AusTraits
#     (same Leaf_id, values identical apart from rounding): source_id Cernusak_2011.
#   - Transects_data.csv: VIC-Givnish rows only (source_id Givnish_2014; all 20 sites are in
#     the Givnish_2014 dataset). The other transects are site means across several species, from
#     studies already in AusTraits. d13C is renamed d13C_leaf_equivalent, as the authors applied
#     a -1.5 per mil offset to these stem wood values.
#   - Common_gardens.csv: every row (source_id Anderson_1996 or Cernusak_2020). MAP is renamed
#     MAP_origin, as it is the MAP at seed origin or of the species range, not at a sampling site.
#     The Anderson et al. (1996) rows are also in the Anderson_1996 dataset (all 17 populations
#     of its Table 1).
# Every row keeps its source_id; custom_R_code in metadata.yml drops the sources whose data are
# already in AusTraits as their own datasets (Cernusak_2011, Givnish_2014, Anderson_1996).
# Original columns and values are otherwise copied unchanged (read as character, so no rounding).
# Appended columns: source_file, source_id, taxon_name, location_name, basis_of_record,
# collection_date, fire_history, life_stage.
#   taxon_name - NE-QLD-Cheesman rows give Species only as "eucalypt", and Transects_data.csv has
#     no species column. NE-QLD species are from Cheesman & Cernusak (2017) Table 2, matched to the
#     STN sites by coordinates (Table 1; all within 0.1 km, and station LMA means and SDs match).
#     Cheesman station numbers run in the opposite direction to the STN codes (station 1 = STN_11).
#     Alam et al. (2026) Table S1 lists the species in Cheesman station order against the STN
#     codes, so its NE-QLD species are not used. VIC species are from Table S1 (one species per
#     site). NAT-Cernusak abbreviations (E., C.) are written out in full.
#   fire_history - Cheesman & Cernusak (2017) note recent (<1 year) burning at station 4 (STN_8).
#   collection_date - sampling date (NE-QLD April 2015 from Cheesman & Cernusak 2017, NAT-Cernusak
#     2008, VIC Year column = 2011); Cernusak (2020) seedlings measured in 2009 (shade house on the
#     Charles Darwin University campus, Fig. 1 caption, now location_name); Anderson publication year.
#   life_stage - adult for field trees; seedling for the Cernusak (2020) shade-house plants;
#     sapling for the Anderson et al. (1996) plantation (as in Anderson_1996).

library(readr)
library(dplyr)

read_raw <- function(file) {
  read_csv(file.path("data/Alam_2026/raw", file), col_types = cols(.default = "c"),
           na = character(), trim_ws = FALSE)
}

leaf <- read_raw("NT_NQ_leaf_data.csv")
transects <- read_raw("Transects_data.csv")
gardens <- read_raw("Common_gardens.csv")

cheesman_NEQLD <- tibble::tribble(
  ~Site_num, ~cheesman_station, ~taxon_S1,
  "1",  "6",  "Corymbia trachyphloia",
  "2",  "7",  "Eucalyptus coolabah",
  "3",  "8",  "Eucalyptus coolabah",
  "4",  "9",  "Eucalyptus normantonensis",
  "5",  "10", "Eucalyptus coolabah",
  "6",  "11", "Eucalyptus coolabah",
  "7",  "5",  "Eucalyptus crebra",
  "8",  "4",  "Corymbia erythrophloia",
  "9",  "3",  "Corymbia dallachiana",
  "10", "2",  "Eucalyptus sp.",
  "11", "1",  "Eucalyptus crebra"
)

table_S1_VIC <- tibble::tribble(
  ~Site_Code, ~taxon_name,
  "1",  "Eucalyptus regnans",
  "2",  "Eucalyptus regnans",
  "5",  "Eucalyptus dives",
  "6",  "Eucalyptus macrorhyncha",
  "7",  "Eucalyptus viminalis",
  "8",  "Eucalyptus viminalis",
  "9",  "Eucalyptus regnans",
  "10", "Eucalyptus obliqua",
  "13", "Eucalyptus goniocalyx",
  "15", "Eucalyptus sideroxylon",
  "16", "Eucalyptus sideroxylon",
  "17", "Eucalyptus macrorhyncha",
  "18", "Eucalyptus arenacea",
  "19", "Eucalyptus incrassata",
  "20", "Eucalyptus dumosa",
  "21", "Eucalyptus dumosa",
  "O",  "Eucalyptus regnans",
  "A",  "Eucalyptus regnans",
  "K",  "Eucalyptus regnans",
  "X",  "Eucalyptus macrorhyncha"
)

leaf_out <- leaf |>
  left_join(cheesman_NEQLD, by = "Site_num") |>
  mutate(
    source_file = "NT_NQ_leaf_data.csv",
    source_id = if_else(Transect == "NE-QLD-Cheesman", "Alam_2026", "Cernusak_2011"),
    taxon_name = case_when(
      Transect == "NE-QLD-Cheesman" ~ taxon_S1,
      TRUE ~ Species |>
        stringr::str_replace("^E\\. ", "Eucalyptus ") |>
        stringr::str_replace("^C\\. ", "Corymbia ") |>
        stringr::str_remove(" \\(TR\\)$")
    ),
    location_name = if_else(Transect == "NE-QLD-Cheesman", paste0("STN_", Site_num), Site),
    basis_of_record = "field",
    life_stage = "adult",
    collection_date = if_else(Transect == "NE-QLD-Cheesman", "2015-04", "2008"),
    fire_history = case_when(
      Transect == "NE-QLD-Cheesman" & Site_num == "8" ~ "recently burnt",
      Transect == "NE-QLD-Cheesman" ~ "fire history not known",
      TRUE ~ ""
    )
  ) |>
  select(-taxon_S1, -cheesman_station)

vic <- transects |>
  filter(Source == "VIC-Givnish")

vic_out <- vic |>
  rename(d13C_leaf_equivalent = d13C) |>
  left_join(table_S1_VIC, by = "Site_Code") |>
  mutate(
    source_file = "Transects_data.csv",
    source_id = "Givnish_2014",
    location_name = paste("Victorian transect site", Site_Code),
    basis_of_record = "field",
    life_stage = "adult",
    collection_date = Year
  )

gardens_out <- gardens |>
  rename(MAP_origin = MAP) |>
  mutate(
    source_file = "Common_gardens.csv",
    source_id = if_else(Study == "Anderson", "Anderson_1996", "Cernusak_2020"),
    taxon_name = Species,
    location_name = if_else(Study == "Anderson", "", "Charles Darwin University shade house"),
    basis_of_record = "captive_cultivated",
    life_stage = if_else(Study == "Anderson", "sapling", "seedling"),
    collection_date = if_else(Study == "Anderson", "1996", "2009")
  )

stopifnot(
  !any(is.na(c(leaf_out$taxon_name, vic_out$taxon_name))),
  nrow(vic_out) == 20,
  identical(as.data.frame(leaf_out[names(leaf)]), as.data.frame(leaf))
)

appended <- c("source_file", "source_id", "taxon_name", "location_name", "basis_of_record",
              "collection_date", "fire_history", "life_stage")

out <- bind_rows(leaf_out, vic_out, gardens_out) |>
  relocate(all_of(appended), .after = last_col())

write_csv(out, "data/Alam_2026/data.csv", na = "", eol = "\n")
