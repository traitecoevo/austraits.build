subset(austraits_deduped$excluded_data,dataset_id=="Leishman_1992" & error !="Missing value" & trait_name == "plant_height") %>%
  write_csv("data/Leishman_1992/raw/plant_height_duplicates.csv")
  
read_csv("data/Leishman_1992/raw/plant_height_duplicates.csv") %>%
  select(original_name,error) %>%
  rename(plant_height_duplicate = error, SPECIES = original_name) -> plant_height_duplicates

read_csv("data/Leishman_1992/data.csv") %>%
  full_join(plant_height_duplicates,by="SPECIES") %>%
  mutate(`height (cm)_filtered` = ifelse(is.na(plant_height_duplicate),`height (cm)`,NA)) %>%
  write_csv("data/Leishman_1992/data.csv")

