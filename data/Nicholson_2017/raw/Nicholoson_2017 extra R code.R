read_csv("data/Nicholson_2017/raw/data_raw.csv") %>% 
  mutate(site_name = "Clarke Island") %>% 
  mutate(dead = if_else(is.na(dead),0,dead)) %>% 
  mutate(resprouting = if_else(is.na(resprouting),0,resprouting)) %>% 
  mutate(fire_response = if_else(dead==(resprouting+dead),"fire_killed","resprouts")) %>% 
  mutate(resprouting_proportion = resprouting/(resprouting+dead)) %>% 
  write_csv("data/Nicholson_2017/data.csv")

#data.csv file then manually manipulated to add in data from the file 
#"Species by severity_Nicholson et al 2017.xls" in the raw data folder.


read_csv("data/Nicholson_2017/raw/indivdual_tree_data.csv")


read_csv("data/Nicholson_2017/raw/individual_tree_data.csv") %>%
  filter(species != "u") %>%
  group_by(sev,species) %>%
  rename(severity = sev, Species = species) %>%
  summarise(individuals_sampled = n()) -> replicates


read_csv("data/Nicholson_2017/data.csv") %>% left_join(replicates, by=c("Species","severity")) %>% write_csv("data/Nicholson_2017/data.csv")
