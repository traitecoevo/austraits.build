## Builds data/Gurung_2025/data.csv from the authors' four trait files
## (https://github.com/BennyWag/ashtraits): sla.csv, op.csv, sd.csv and gmin.csv,
## plus the provenance details in climate_origin.csv. Run from the repo root.
## Values are aggregated to one value per tree/leaf, as in the paper:
##   - stomatal density: mean of the 6 images per tree (2 leaves x 3 images)
##   - gmin: mean of the last four time points (240-330 min) of each leaf's
##     bench-drying curve; this reproduces the authors' published provenance means
##     (all_avg.csv) exactly.
## gmin.csv has no tree column (one leaf per tree, one tree per provenance per
## measurement day), so gmin values are kept on their own rows/individual_ids.

library(dplyr)

raw <- "data/Gurung_2025/raw/"

provenance <- read.csv(paste0(raw, "climate_origin.csv")) |>
  distinct(Provenance, .keep_all = TRUE) |>
  select(prov = Provenance, State, Seedlot, Name, Seedlot_Name)

sla <- read.csv(paste0(raw, "sla.csv"), check.names = FALSE) |>
  rename(prov = Prov, tree = trees)

op <- read.csv(paste0(raw, "op.csv"))

sd <- read.csv(paste0(raw, "sd.csv")) |>
  group_by(prov, tree) |>
  summarise(
    stomata_images = n(),
    density = mean(density),
    .groups = "drop"
  )

gmin <- read.csv(paste0(raw, "gmin.csv")) |>
  filter(time_min >= 240) |>
  group_by(prov, day) |>
  summarise(gminvalue = mean(gminvalue), .groups = "drop")

trees <- sla |>
  full_join(op, by = c("prov", "tree")) |>
  full_join(sd, by = c("prov", "tree")) |>
  mutate(individual_id = paste0("prov", prov, "_tree", tree))

gmin_leaves <- gmin |>
  mutate(individual_id = paste0("prov", prov, "_gmin_leaf", day))

data <- bind_rows(trees, gmin_leaves) |>
  left_join(provenance, by = "prov") |>
  mutate(
    taxon_name = "Eucalyptus delegatensis",
    location_name = "University of Melbourne Burnley Campus common garden",
    seed_provenance = paste0("provenance ", prov, ": ", Name, " (", State, ")")
  ) |>
  arrange(prov, !is.na(day), tree, day) |>
  select(
    taxon_name, location_name, prov, State, Seedlot, Name, Seedlot_Name,
    seed_provenance, tree, day, individual_id,
    `avg leaf area`, `avg dry wt`, avg_SLA,
    stomata_images, density, osmolality, gminvalue
  )

write.csv(data, "data/Gurung_2025/data.csv", row.names = FALSE, na = "")
