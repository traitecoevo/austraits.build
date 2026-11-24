# Writes data/Britton_2026/data.csv from two author files. Run from the repo root.
#   - raw/CurrencyCreekArboretum_euc_traits_tree-level_AusTraits_v2.csv: tree-level LMA, Huber
#     value, leaf thickness, leaf area, LDMC and wood density, supplied by Travis Britton
#     (October 2026). Wood density measured for PerezMartinez_2025 is omitted from this file.
#     One row per tree per campaign (data_level = "tree"); all columns copied unchanged.
#   - raw/NPH-MS-2025-52475_DATA_CCA_only.csv (Dryad): taxon-level values. Only the foliar
#     N, d13C, ci/ca and Narea values (one bulked sample per taxon, not measured per tree) and
#     the seed provenance coordinates and climate are kept, as data_level = "taxon" rows.
# Added columns, looked up in the Currency Creek Arboretum planting records supplied with
# PerezMartinez_2025 (data/PerezMartinez_2025/raw/), so trees sampled by both studies share
# identifiers:
#   - CCA_unique_ID, CCA_row: the arboretum's accession number and row for each tree, matched
#     on CCA_ID + Tree_ID (or on taxon + Tree_ID when CCA_ID is blank). When an accession was
#     planted in two rows, the row matching most of that accession's sampled trees is used,
#     then the row whose tree was alive in 2016; still-ambiguous trees are left blank.
#   - seed_collector, seed_provenance: the accession's seed collector and provenance locality.
#   - MAP, MAT: seed provenance climate from the Dryad file, matched on the seed coordinates.

library(dplyr)
library(readr)
library(stringr)

trees <- read_csv("data/Britton_2026/raw/CurrencyCreekArboretum_euc_traits_tree-level_AusTraits_v2.csv",
                  col_types = cols(.default = "c"))
taxa <- read_csv("data/Britton_2026/raw/NPH-MS-2025-52475_DATA_CCA_only.csv",
                 col_types = cols(.default = "c"))
cca <- read_csv("data/PerezMartinez_2025/raw/CCAThornhillTraitsretrievedplussampling&ClimateFinal.csv",
                col_types = cols(.default = "c"), locale = locale(encoding = "latin1")) |>
  transmute(CCA_unique_ID = UniqueID, CCA_row = Row, Tree_ID = Tree,
            cca_taxon = word(str_squish(Taxonsimplified_DN), 1, 2),
            seed_collector = Collector, seed_provenance = str_squish(Provenance.x),
            alive_2016 = StatusFeb2016 == "Alive")

trees <- trees |> mutate(tree_row = row_number(), taxon_key = word(species, 1, 2))

candidates <- bind_rows(
  trees |> filter(!is.na(CCA_ID)) |>
    inner_join(cca, by = c("CCA_ID" = "CCA_unique_ID", "Tree_ID"), relationship = "many-to-many") |>
    mutate(CCA_unique_ID = CCA_ID),
  trees |> filter(is.na(CCA_ID), !is.na(Tree_ID)) |>
    inner_join(cca, by = c("taxon_key" = "cca_taxon", "Tree_ID"), relationship = "many-to-many")
)

matched <- candidates |>
  add_count(CCA_unique_ID, CCA_row, name = "trees_in_row") |>
  group_by(tree_row) |>
  filter(trees_in_row == max(trees_in_row)) |>
  filter(n() == 1 | alive_2016 %in% TRUE) |>
  filter(n() == 1) |>
  ungroup() |>
  select(tree_row, CCA_unique_ID, CCA_row, seed_collector, seed_provenance)

accessions <- cca |> distinct(CCA_unique_ID, .keep_all = TRUE) |>
  select(CCA_unique_ID, seed_collector, seed_provenance)

climate <- taxa |>
  transmute(lat = round(as.numeric(Latitude), 3), lon = round(as.numeric(Longitude), 3),
            taxon_key = word(Taxon_ALA, 1, 2), MAP, MAT)

tree_rows <- trees |>
  left_join(matched, by = "tree_row") |>
  left_join(accessions |> rename(acc_collector = seed_collector, acc_provenance = seed_provenance),
            by = c("CCA_ID" = "CCA_unique_ID")) |>
  mutate(CCA_unique_ID = coalesce(CCA_unique_ID, if_else(is.na(acc_provenance), NA, CCA_ID)),
         seed_collector = coalesce(seed_collector, acc_collector),
         seed_provenance = coalesce(seed_provenance, acc_provenance)) |>
  mutate(lat = round(as.numeric(seed_lat), 3), lon = round(as.numeric(seed_lon), 3)) |>
  left_join(climate, by = c("lat", "lon"), relationship = "many-to-many", suffix = c("", "_clim")) |>
  group_by(tree_row) |>
  arrange(desc(taxon_key == taxon_key_clim), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  arrange(tree_row) |>
  mutate(data_level = "tree") |>
  select(all_of(names(trees)[!names(trees) %in% c("tree_row", "taxon_key")]),
         CCA_unique_ID, CCA_row, seed_collector, seed_provenance, MAP, MAT, data_level)

provenances <- tree_rows |>
  filter(!is.na(seed_provenance)) |>
  transmute(lat = round(as.numeric(seed_lat), 3), lon = round(as.numeric(seed_lon), 3),
            seed_collector, seed_provenance) |>
  distinct(lat, lon, .keep_all = TRUE)

taxon_rows <- taxa |>
  transmute(species = Taxon_ALA, seed_lat = Latitude, seed_lon = Longitude,
            N_percent, dC13, delta, cica, Narea, MAP, MAT,
            lat = round(as.numeric(Latitude), 3), lon = round(as.numeric(Longitude), 3)) |>
  left_join(provenances, by = c("lat", "lon")) |>
  mutate(data_level = "taxon") |>
  select(-lat, -lon)

data <- bind_rows(tree_rows, taxon_rows) |>
  relocate(N_percent, dC13, delta, cica, Narea, .after = seed_lon)

write_csv(data, "data/Britton_2026/data.csv", na = "", eol = "\n")
