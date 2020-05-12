subset(austraits_deduped$excluded_data,dataset_id==current_study & trait_name == "specific_leaf_area" &
         error !="Missing value") %>% 
  write_csv("data/Morgan_2011_2/raw/specific_leaf_area_duplicates.csv")

read_csv("data/Morgan_2011_2/raw/specific_leaf_area_duplicates.csv") %>%
  select(trait_name,original_name) %>%
  rename(SLA_duplicate = trait_name, `Scientific Name` = original_name) -> SLA_duplicates

subset(austraits_deduped$excluded_data,dataset_id==current_study & trait_name == "seed_mass" &
         error !="Missing value") %>% 
  write_csv("data/Morgan_2011_2/raw/seed_mass_dupicates.csv")

read_csv("data/Morgan_2011_2/raw/seed_mass_dupicates.csv") %>%
  select(trait_name,original_name) %>%
  rename(seed_mass_duplicate = trait_name, `Scientific Name` = original_name) -> seed_mass_duplicates

read_csv("data/Morgan_2011_2/data.csv") %>%
  full_join(SLA_duplicates,by="Scientific Name") %>% 
  full_join(seed_mass_duplicates,by="Scientific Name") %>%
  mutate(`SLA (mg/mm2)_filtered` = ifelse(is.na(SLA_duplicate),`SLA (mg/mm2)`,NA),
         `Seed mass (mg)_filtered` = ifelse(is.na(seed_mass_duplicate),`Seed mass (mg)`,NA)) %>%
  write_csv("data/Morgan_2011_2/data.csv")