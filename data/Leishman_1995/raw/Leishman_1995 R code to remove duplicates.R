subset(austraits_deduped$excluded_data,dataset_id=="Leishman_1995" & error !="Missing value" & trait_name == "plant_height") %>%
  write_csv("data/Leishman_1995/raw/plant_height_duplicates.csv")
  
read_csv("data/Leishman_1995/raw/plant_height_duplicates.csv") %>%
  select(original_name,error) %>%
  rename(plant_height_duplicate = error, species = original_name) -> plant_height_duplicates

subset(austraits_deduped$excluded_data,dataset_id=="Leishman_1995" & error !="Missing value" & trait_name == "seed_mass") %>%
  write_csv("data/Leishman_1995/raw/seed_mass_duplicates.csv")

read_csv("data/Leishman_1995/raw/seed_mass_duplicates.csv") %>%
  select(original_name,error) %>%
  rename(seed_mass_duplicate = error, species = original_name) -> seed_mass_duplicates

read_csv("data/Leishman_1995/data.csv") %>%
  full_join(plant_height_duplicates,by="species") %>%
  full_join(seed_mass_duplicates,by="species") %>%
  mutate(`Height (cm)_filtered` = ifelse(is.na(plant_height_duplicate),`Height (cm)`,NA)) %>%
  mutate(`Seed mass (mg)_filtered` = ifelse(is.na(seed_mass_duplicate),`Seed mass (mg)`,NA)) %>%
  write_csv("data/Leishman_1995/data.csv")

