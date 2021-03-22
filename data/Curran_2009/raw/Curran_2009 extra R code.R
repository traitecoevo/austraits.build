read_csv("data/Curran_2009/raw/species_match.csv") -> species

read_csv("data/Curran_2009/raw/leaves.csv") %>%
  mutate(`Sampling period` = "Jan 2003") -> leaves

read_csv("data/Curran_2009/raw/water_potential.csv") -> water

read_csv("data/Curran_2009/raw/water_potential.csv") %>%
  mutate(species = gsub("C.aust","E.aust",species)) %>%
  full_join(leaves, by = c("species", "Site", "Tree #", "Soil type","Sampling period")) %>% 
  full_join(species) %>% 
  write_csv("data/Curran_2009/data.csv")

read_csv("data/Curran_2009/raw/site_data.csv") %>%
  rename(description = `site descriptor from paper`) -> sites
