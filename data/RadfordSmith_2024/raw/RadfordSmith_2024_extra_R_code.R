library(tidyverse)

conduit_Alison <- read_csv("data/RadfordSmith_2024/raw/Alison_conduit_reinforcement_revised_29_08_2023.csv") %>%
  mutate(
    collector_conduit = "Alison",
    max_lumen_width = max_lumen_width/1000,
    min_lumen_width = min_lumen_width/1000,
    wall_thickness_1 = wall_thickness_1/1000,
    wall_thickness_2 = wall_thickness_2/1000,
    wall_thickness_3 = wall_thickness_3/1000
    )

conduit_Ella <- read_csv("data/RadfordSmith_2024/raw/Ella_conduit_reinforcement.csv") %>%
  mutate(collector_conduit = "Ella")

conduit_Julian <- read_csv("data/RadfordSmith_2024/raw/Julian_conduit_reinforcement_29_08_2023.csv") %>%
  mutate(collector_conduit = "Julian")

conduit_data <- conduit_Alison %>%
  bind_rows(conduit_Ella, conduit_Julian) %>%
  mutate(wall_thickness = (wall_thickness_1 + wall_thickness_2 + wall_thickness_3)/3) %>%
  group_by(abbrev, individual_id) %>%
  summarise(
    max_lumen_width = max(max_lumen_width),
    min_lumen_width = min(min_lumen_width),
    wall_thickness = mean(wall_thickness),
    replicates_conduit = n(),
    collector_conduit = first(collector_conduit)
    ) %>%
  ungroup() %>%
  mutate(individual_id = paste0(individual_id,"_conduit"))

leaf_Alison <- read_csv("data/RadfordSmith_2024/raw/Alison_leaf_traits.csv") %>%
  mutate(collector_leaf = "Alison")

leaf_Julian <- read_csv("data/RadfordSmith_2024/raw/Julian_leaf_traits_22_May_2023.csv") %>%
  mutate(collector_leaf = "Julian")

leaf_data <- leaf_Alison %>%
  bind_rows(leaf_Julian) %>%
  mutate(
    leaf_mass_per_area_g_m2 = (leaf_dry_mass_g / leaf_area_mm2)*1000000,
    leaf_type = ifelse(is.na(leaflet_id), "leaf", "leaflet")
    ) %>% 
  group_by(abbrev, individual_id) %>%
  summarise(
    leaf_area_mm2  = mean(leaf_area_mm2),
    leaf_fresh_mass_g  = mean(leaf_fresh_mass_g),
    leaf_dry_mass_g  = mean(leaf_dry_mass_g),
    leaf_mass_per_area_g_m2  = mean(leaf_mass_per_area_g_m2),
    leaf_dry_matter_content = leaf_dry_mass_g/leaf_fresh_mass_g,
    replicates_leaf = n(),
    collector_leaf = first(collector_leaf),
    leaf_type = first(leaf_type)
  ) %>%
  ungroup() %>%
  mutate(individual_id = paste0(individual_id,"_leaf"))

wood_Alison <- read_csv("data/RadfordSmith_2024/raw/Alison_wood_density.csv")%>%
  mutate(collector_wood = "Alison")

wood_Julian <- read_csv("data/RadfordSmith_2024/raw/Julian_wood_density_29_08_2023.csv")%>%
  mutate(collector_wood = "Julian") %>%
  mutate(individual_id = individual)

wood_data <- wood_Alison %>%
  bind_rows(wood_Julian) %>%
  mutate(wood_density = dry_mass/fresh_vol) %>% 
  group_by(abbrev, individual_id) %>%
  summarise(
    fresh_vol   = mean(fresh_vol),
    dry_mass   = mean(dry_mass),
    wood_density  = mean(wood_density),
    replicates_wood = n(),
    collector_wood = first(collector_wood)
  ) %>%
  ungroup() %>%
  mutate(individual_id = paste0(individual_id,"_wood"))

species_names <- read_csv("data/RadfordSmith_2024/raw/Species_names_max_heights_leaf_phenology.csv")

data <- conduit_data %>%
  bind_rows(wood_data) %>%
  bind_rows(leaf_data) %>%
  left_join(species_names)

data %>% write_csv("data/RadfordSmith_2024/data.csv", na="")
