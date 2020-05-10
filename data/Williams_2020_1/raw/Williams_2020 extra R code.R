read_csv("data/Williams_2020_1/raw/Adelaide_data.csv") %>%
  mutate(Adelaide = "Adelaide") -> Adelaide

read_csv("data/Williams_2020_1/raw/Melbourne_data.csv") %>%
  mutate(SLA = as.numeric(SLA),Melbourne = "Melbourne") -> Melbourne

Melbourne %>%
  anti_join(Adelaide,by="Species") -> Melbourne_unique

Adelaide %>%
  anti_join(Melbourne,by="Species") -> Adelaide_unique
  
Adelaide %>%
  select(Species,Adelaide) -> Adelaide_names

Melbourne %>%
  inner_join(Adelaide_names,by="Species") %>%
  bind_rows(Melbourne_unique) %>%
  bind_rows(Adelaide_unique) %>% 
  write_csv("data/Williams_2020_1/data.csv")



#filtering duplicates
subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value") %>%
  subset(trait_name %in% c("specific_leaf_area")) %>%
  write_csv("data/Williams_2020_1/raw/SLA_duplicates.csv") 

read_csv("data/Williams_2020_1/raw/SLA_duplicates.csv") %>%
  select(trait_name,original_name) %>%
  rename(SLA_duplicate = trait_name, Species = original_name) -> SLA_duplicates

subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value") %>%
  subset(trait_name %in% c("seed_mass")) %>%
  write_csv("data/Williams_2020_1/raw/seed_mass_duplicates.csv")

#in excel remove duplicates from Jurado, Ooi, because not listed literature sources

read_csv("data/Williams_2020_1/raw/seed_mass_duplicates.csv") %>%
  select(trait_name,original_name) %>%
  rename(seed_mass_duplicate = trait_name, Species = original_name) -> seed_mass_duplicates

subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value") %>%
  subset(trait_name %in% c("plant_height")) %>%
  write_csv("data/Williams_2020_1/raw/plant_height_duplicates.csv")

#in excel remove duplicates from a number of studies with 1-3 overlapping values, that were not listed literature sources; 
#left in Catford_2014, with 8 overlapping values; not sure if both sourced from a different location or passed on
#left in Hughes_1992, which is also literature sourced, because any values - this is probably because we don't have a Victorian flora

read_csv("data/Williams_2020_1/raw/plant_height_duplicates_edited.csv") %>%
  select(trait_name,original_name) %>%
  rename(plant_height_duplicate = trait_name, Species = original_name) -> plant_height_duplicates

read_csv("data/Williams_2020_1/data.csv") %>%
  full_join(SLA_duplicates,by="Species") %>% 
  full_join(seed_mass_duplicates,by="Species") %>%
  full_join(plant_height_duplicates,by="Species") %>%
  mutate(SLA_filtered = ifelse(is.na(SLA_duplicate),SLA,NA),
         `Plant height_filtered` = ifelse(is.na(plant_height_duplicate),`Plant height`,NA),
         `Seed Mass_filtered` = ifelse(is.na(seed_mass_duplicate),`Seed Mass`,NA)) %>% 
  write_csv("data/Williams_2020_1/data.csv")

