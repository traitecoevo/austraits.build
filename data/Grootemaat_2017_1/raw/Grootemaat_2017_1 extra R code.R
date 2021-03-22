read_csv("data/Grootemaat_2017_1/raw/bark.csv") -> bark

read_csv("data/Grootemaat_2017_1/raw/species_match.csv") -> species_match

read_csv("data/Grootemaat_2017_1/raw/leaves.csv")%>%
  bind_rows(bark) %>% 
  rename(Abbreviation = Species) %>%
  full_join(species_match, by = "Abbreviation") %>% 
  write_csv("data/Grootemaat_2017_1/data.csv")


read_csv("data/Grootemaat_2017_1/data.csv") %>%
  left_join(species_match) %>% 
  write_csv("data/Grootemaat_2017_1/data.csv")