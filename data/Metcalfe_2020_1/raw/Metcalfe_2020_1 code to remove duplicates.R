#code to add columns to filter out duplicates
subset(austraits$traits,trait_name=="plant_height" & dataset_id %in% 
         c("Metcalfe_2020_1","WAH_1998","RBGSYD_2014_2")) %>%
  select(species_name,value,dataset_id,original_name) %>%
  mutate(plant_height = as.numeric(value)) %>%
  select(species_name,dataset_id,plant_height,original_name) %>%
  group_by(species_name,dataset_id,original_name) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=plant_height) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_1)) %>% 
  rename(plant_height_Metcalfe_2020_1 = Metcalfe_2020_1,  
         plant_height_RBGSYD_2014 = RBGSYD_2014_2, plant_height_WAH_1998 = WAH_1998) %>%
  mutate(plant_height_to_keep = ifelse(plant_height_Metcalfe_2020_1 == plant_height_RBGSYD_2014 |
                                         plant_height_Metcalfe_2020_1 == plant_height_WAH_1998,
                                          "omit","keep")) %>%
  mutate(plant_height_to_keep = ifelse(is.na(plant_height_to_keep),"keep",plant_height_to_keep)) %>% View()
  write_csv("data/Metcalfe_2020_1/raw/plant_height_comparison.csv") %>%
  mutate(plant_height_unique = ifelse(plant_height_to_keep=="keep",plant_height_Metcalfe_2020_1,NA)) %>%
  select(original_name, plant_height_unique) %>%
  rename(`current full name without authority` = original_name) -> plant_height_comparison

read_csv("data/Metcalfe_2020_1/raw/data_with_duplicates.csv") %>%
  full_join(plant_height_comparison, by="current full name without authority") %>%
  write_csv("data/Metcalfe_2020_1/data.csv")