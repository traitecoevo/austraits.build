subset(austraits_deduped$excluded_data,dataset_id==current_study & trait_name == "specific_leaf_area" &
         error !="Missing value") %>% 
  write_csv("data/Morgan_2011_2/raw/specific_leaf_area_dupicates.csv")


subset(austraits_deduped$excluded_data,dataset_id==current_study & trait_name == "seed_mass" &
         error !="Missing value") %>% 
  write_csv("data/Morgan_2011_2/raw/seed_mass_dupicates")