#filtering duplicates
subset(austraits_deduped$excluded_data,dataset_id=="Metcalfe_2009" & error !="Missing value" & trait_name == "seed_mass") %>%
  write_csv("data/Metcalfe_2009/raw/seed_mass_duplicates.csv") 

read_csv("data/Metcalfe_2009/raw/seed_mass_duplicates.csv") %>%
  select(error,original_name) %>%
  rename(seed_mass_duplicate = error, species_name = original_name) -> seed_mass_duplicates

subset(austraits_deduped$excluded_data,dataset_id=="Metcalfe_2009" & error !="Missing value" & trait_name == "plant_height")  %>%
  write_csv("data/Metcalfe_2009/raw/plant_height_duplicates.csv")

read_csv("data/Metcalfe_2009/raw/plant_height_duplicates.csv") %>%
  select(error,original_name) %>%
  rename(plant_height_duplicate = error, species_name = original_name) -> plant_height_duplicates

read_csv("data/Metcalfe_2009/data.csv") %>%
  full_join(seed_mass_duplicates,by="species_name") %>%
  mutate(`Seed_mass_filtered` = ifelse(is.na(seed_mass_duplicate),`Seed_mass`,NA)) %>% 
  write_csv("data/Metcalfe_2009/data.csv")