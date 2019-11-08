read_csv("data/Muir_2014/species_names.csv") -> species_names

read_csv("data/Muir_2014/raw/EnsembleData.csv") %>%
  subset(Site=="Foothills") %>%
  left_join(species_names, by = "Sp") %>%
  write_csv("data/Muir_2014/data.csv")


