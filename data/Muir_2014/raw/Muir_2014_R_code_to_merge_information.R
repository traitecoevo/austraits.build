read_csv("data/Muir_2014/species_names.csv") -> species_names

read_csv("data/Muir_2014/raw/EnsembleData.csv") %>%
  subset(Site=="Foothills") %>%
  left_join(species_names, by = "Sp") %>%
  write_csv("data/Muir_2014/data_individual.csv")

read_csv("data/Muir_2014/flower_counts.csv") %>%
  select(species_name, flower_count_maximum) -> flower_counts

read_csv("data/Muir_2014/data_individual.csv") %>%
  select(species_name, Site,site_name, years_since_fire, 
         `latitude (deg)`, `longitude (deg)`) %>%
  group_by(species_name) %>%
  summarise_all(.funs = last) -> sites_to_merge

read_csv("data/Muir_2014/data_individual.csv") %>%
  select(species_name, SDMASS, SLA, N) %>%
  group_by(species_name) %>%
  summarise_all(.funs = first) %>%
  ungroup() %>%
  left_join(flower_counts, by = "species_name") %>%
  left_join(sites_to_merge, by = "species_name") %>%
  write_csv("data/Muir_2014/data.csv")
