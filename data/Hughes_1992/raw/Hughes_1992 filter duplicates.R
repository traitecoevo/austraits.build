subset(austraits_deduped$excluded_data,dataset_id=="Hughes_1992" & error !="Missing value" & trait_name == "plant_height")  %>%
  write_csv("data/Hughes_1992/raw/plant_height_duplicates.csv")

read_csv("data/Hughes_1992/raw/plant_height_duplicates.csv") %>%
  select(error,original_name) %>%
  rename(plant_height_duplicate = error, `name original` = original_name) -> plant_height_duplicates


read_csv("data/Hughes_1992/data.csv") %>%
  full_join(plant_height_duplicates,by="name original") %>%
  mutate(`maximum height (m)_filtered` = ifelse(is.na(plant_height_duplicate),`maximum height (m)`,NA)) %>% 
  write_csv("data/Hughes_1992/data.csv")