#archive a copy of the original data file
read_csv("data/Wright_2000/data.csv") %>%
  write_csv("data/Wright_2000/raw/old_data.csv") 

#read in additional data from Wright 2000 paper
read_csv("data/Wright_2000/raw/Clifford_traits.csv") %>%
  mutate(name_original = paste(genus,spp,sep = " ")) %>%
  select('name_original','crypto','cata','node','hypo','coty','emb') -> additional_data

#join two data files and save
read_csv("data/Wright_2000/data.csv") %>%
  select(-units) %>%
  spread(key = "trait", value = "raw_trait_value") %>% 
  rename('seed_volume_mm3' = 'volume seed') %>%
  full_join(by = "name_original", additional_data) %>% 
  write_csv("data/Wright_2000/data.csv") 