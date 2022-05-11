read_csv("data/Thomas_2017/raw/species_names.csv") -> species_names

read_csv("data/Thomas_2017/raw/FT_species_traits.csv") %>%
  select(SPP, SLA, SD, SD_MASS, N) %>%
  distinct(SPP, .keep_all = TRUE) -> species_mean

read_csv("data/Thomas_2017/raw/FT_species_traits.csv") %>%
  select(SPP, Yrs, Ht, Length, Width, Diam) %>%
  group_by(SPP,Yrs) %>%
  summarise_all(.funs=max) %>%
  ungroup() -> species_max

#added 2021.11.11 to include individual replicate values
read_csv("data/Thomas_2017/raw/FT_species_traits.csv") %>%
  group_by(SPP, Yrs) %>%
  summarise(count_plants = n()) -> replicate_counts

#using only max allometric traits
species_max %>%
  bind_rows(species_mean) %>%
  left_join(replicate_counts,by = c("SPP", "Yrs")) %>%
  rename(Abbreviation = SPP) %>%
  left_join(species_names,by = "Abbreviation") %>%
  mutate(species_binomial = paste(Genus,species,sep=" ")) %>%
  mutate(site = "MurraySunset") %>%
  write_csv("data/Thomas_2017/data.csv")

#old version
read_csv("data/Thomas_2017/raw/FT_species_traits.csv") %>%
  select(-X1, - SLA, -SD, -SD_MASS, -N) %>%
  bind_rows(species_mean) %>%
  rename(Abbreviation = SPP) %>%
  left_join(species_names,by = "Abbreviation") %>%
  mutate(species_binomial = paste(Genus,species,sep=" ")) %>%
  mutate(site = "MurraySunset") %>%
  write_csv("data/Thomas_2017/data.csv")

