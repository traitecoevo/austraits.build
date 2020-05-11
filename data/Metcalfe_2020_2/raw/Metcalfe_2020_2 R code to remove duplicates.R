#code to add columns to filter out duplicates
SAH_2014, Wheeler_2002, GrassBase_2014: leaf_length, min, max
SAH_2014, Wheeler_2002, GrassBase_2014: leaf_width, min, max
RBGSYD, GrassBase, Wheeler,SAH: plant_height
Kew_2019_1, RBGK_2014: seed_mass 

subset(austraits$traits,trait_name=="seed_mass" & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> seed_mass_names
       
subset(austraits$traits,trait_name=="seed_mass" & dataset_id %in% 
         c("Metcalfe_2020_2","Kew_2019_1","RBGK_2014")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(seed_mass = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(seed_mass = max(seed_mass)) %>%
  spread(key=dataset_id,value=seed_mass) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(seed_mass_Metcalfe_2020_2 = Metcalfe_2020_2,  
         seed_mass_Kew_2019_1 = Kew_2019_1, seed_mass_RBGK_2014 = RBGK_2014) %>%
  mutate(seed_mass_to_keep = ifelse(seed_mass_Metcalfe_2020_2 == seed_mass_Kew_2019_1 |
                                         seed_mass_Metcalfe_2020_2 == seed_mass_RBGK_2014,
                                       "omit","keep")) %>%
  mutate(seed_mass_to_keep = ifelse(is.na(seed_mass_to_keep),"keep",seed_mass_to_keep)) %>%
  left_join(seed_mass_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/seed_mass_comparison.csv") %>%
  mutate(seed_mass_unique = ifelse(seed_mass_to_keep=="keep",seed_mass_Metcalfe_2020_2,NA)) %>%
  select(original_name, seed_mass_unique) %>%
  rename(`current full name without authority` = original_name) -> seed_mass_comparison

subset(austraits$traits,trait_name=="plant_height" & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> plant_height_names

subset(austraits$traits,trait_name=="plant_height" & dataset_id %in% 
         c("Metcalfe_2020_2","GrassBase_2014","RBGSYD_2014_2","SAH_2014","Wheeler_2002")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(plant_height = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(plant_height = max(plant_height)) %>%
  spread(key=dataset_id,value=plant_height) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(plant_height_Metcalfe_2020_2 = Metcalfe_2020_2,  
         plant_height_RBGSYD_2014 = RBGSYD_2014_2, plant_height_GrassBase_2014 = GrassBase_2014,
         plant_height_SAH_2014 = SAH_2014, plant_height_Wheeler_2002 = Wheeler_2002) %>%
  mutate(plant_height_to_keep = ifelse(plant_height_Metcalfe_2020_2 == plant_height_RBGSYD_2014 |
                                         plant_height_Metcalfe_2020_2 == plant_height_SAH_2014 |
                                         plant_height_Metcalfe_2020_2 == plant_height_Wheeler_2002 |
                                         plant_height_Metcalfe_2020_2 == plant_height_GrassBase_2014,
                                       "omit","keep")) %>%
  mutate(plant_height_to_keep = ifelse(is.na(plant_height_to_keep),"keep",plant_height_to_keep)) %>%
  left_join(plant_height_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/plant_height_comparison.csv") %>%
  mutate(plant_height_unique = ifelse(plant_height_to_keep=="keep",plant_height_Metcalfe_2020_2,NA)) %>%
  select(original_name, plant_height_unique) %>%
  rename(`current full name without authority` = original_name) -> plant_height_comparison

subset(austraits$traits,trait_name=="leaf_width" & value_type == "expert_min"  & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> leaf_width_min_names

subset(austraits$traits,trait_name=="leaf_width" & value_type == "expert_min" & dataset_id %in% 
         c("Metcalfe_2020_2","GrassBase_2014","SAH_2014","Wheeler_2002")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(leaf_width_min = as.numeric(value)) %>% 
  group_by(species_name,dataset_id) %>%
  summarise(leaf_width_min = min(leaf_width_min)) %>%
  spread(key=dataset_id,value=leaf_width_min) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(leaf_width_min_Metcalfe_2020_2 = Metcalfe_2020_2,  
         leaf_width_min_GrassBase_2014 = GrassBase_2014,
         leaf_width_min_SAH_2014 = SAH_2014, leaf_width_min_Wheeler_2002 = Wheeler_2002) %>%
  mutate(leaf_width_min_to_keep = ifelse(leaf_width_min_Metcalfe_2020_2 == leaf_width_min_SAH_2014 |
                                         leaf_width_min_Metcalfe_2020_2 == leaf_width_min_Wheeler_2002 |
                                         leaf_width_min_Metcalfe_2020_2 == leaf_width_min_GrassBase_2014,
                                       "omit","keep")) %>%
  mutate(leaf_width_min_to_keep = ifelse(is.na(leaf_width_min_to_keep),"keep",leaf_width_min_to_keep)) %>%
  left_join(leaf_width_min_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/leaf_width_min_comparison.csv") %>%
  mutate(leaf_width_min_unique = ifelse(leaf_width_min_to_keep=="keep",leaf_width_min_Metcalfe_2020_2,NA)) %>%
  select(original_name, leaf_width_min_unique) %>%
  rename(`current full name without authority` = original_name) -> leaf_width_min_comparison

subset(austraits$traits,trait_name=="leaf_width" & value_type == "expert_max"  & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> leaf_width_max_names

subset(austraits$traits,trait_name=="leaf_width" & value_type == "expert_max" & dataset_id %in% 
         c("Metcalfe_2020_2","GrassBase_2014","SAH_2014","Wheeler_2002")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(leaf_width_max = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(leaf_width_max = max(leaf_width_max)) %>%
  spread(key=dataset_id,value=leaf_width_max) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(leaf_width_max_Metcalfe_2020_2 = Metcalfe_2020_2,  
         leaf_width_max_GrassBase_2014 = GrassBase_2014,
         leaf_width_max_SAH_2014 = SAH_2014, leaf_width_max_Wheeler_2002 = Wheeler_2002) %>%
  mutate(leaf_width_max_to_keep = ifelse(leaf_width_max_Metcalfe_2020_2 == leaf_width_max_SAH_2014 |
                                       leaf_width_max_Metcalfe_2020_2 == leaf_width_max_Wheeler_2002 |
                                       leaf_width_max_Metcalfe_2020_2 == leaf_width_max_GrassBase_2014,
                                     "omit","keep")) %>%
  mutate(leaf_width_max_to_keep = ifelse(is.na(leaf_width_max_to_keep),"keep",leaf_width_max_to_keep)) %>%
  left_join(leaf_width_max_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/leaf_width_max_comparison.csv") %>%
  mutate(leaf_width_max_unique = ifelse(leaf_width_max_to_keep=="keep",leaf_width_max_Metcalfe_2020_2,NA)) %>%
  select(original_name, leaf_width_max_unique) %>%
  rename(`current full name without authority` = original_name) -> leaf_width_max_comparison

subset(austraits$traits,trait_name=="leaf_length" & value_type == "expert_min"  & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> leaf_length_min_names

subset(austraits$traits,trait_name=="leaf_length" & value_type == "expert_min" & dataset_id %in% 
         c("Metcalfe_2020_2","GrassBase_2014","SAH_2014","Wheeler_2002")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(leaf_length_min = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(leaf_length_min = min(leaf_length_min)) %>%
  spread(key=dataset_id,value=leaf_length_min) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(leaf_length_min_Metcalfe_2020_2 = Metcalfe_2020_2,  
         leaf_length_min_GrassBase_2014 = GrassBase_2014,
         leaf_length_min_SAH_2014 = SAH_2014, leaf_length_min_Wheeler_2002 = Wheeler_2002) %>%
  mutate(leaf_length_min_to_keep = ifelse(leaf_length_min_Metcalfe_2020_2 == leaf_length_min_SAH_2014 |
                                           leaf_length_min_Metcalfe_2020_2 == leaf_length_min_Wheeler_2002 |
                                           leaf_length_min_Metcalfe_2020_2 == leaf_length_min_GrassBase_2014,
                                         "omit","keep")) %>%
  mutate(leaf_length_min_to_keep = ifelse(is.na(leaf_length_min_to_keep),"keep",leaf_length_min_to_keep)) %>%
  left_join(leaf_length_min_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/leaf_length_min_comparison.csv") %>%
  mutate(leaf_length_min_unique = ifelse(leaf_length_min_to_keep=="keep",leaf_length_min_Metcalfe_2020_2,NA)) %>%
  select(original_name, leaf_length_min_unique) %>%
  rename(`current full name without authority` = original_name) -> leaf_length_min_comparison

subset(austraits$traits,trait_name=="leaf_length" & value_type == "expert_max"  & dataset_id=="Metcalfe_2020_2") %>%
  select(species_name,original_name) -> leaf_length_max_names

subset(austraits$traits,trait_name=="leaf_length" & value_type == "expert_max" & dataset_id %in% 
         c("Metcalfe_2020_2","GrassBase_2014","SAH_2014","Wheeler_2002")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(leaf_length_max = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(leaf_length_max = max(leaf_length_max)) %>%
  spread(key=dataset_id,value=leaf_length_max) %>%
  ungroup() %>%
  subset(!is.na(Metcalfe_2020_2)) %>% 
  rename(leaf_length_max_Metcalfe_2020_2 = Metcalfe_2020_2,  
         leaf_length_max_GrassBase_2014 = GrassBase_2014,
         leaf_length_max_SAH_2014 = SAH_2014, leaf_length_max_Wheeler_2002 = Wheeler_2002) %>%
  mutate(leaf_length_max_to_keep = ifelse(leaf_length_max_Metcalfe_2020_2 == leaf_length_max_SAH_2014 |
                                           leaf_length_max_Metcalfe_2020_2 == leaf_length_max_Wheeler_2002 |
                                           leaf_length_max_Metcalfe_2020_2 == leaf_length_max_GrassBase_2014,
                                         "omit","keep")) %>%
  mutate(leaf_length_max_to_keep = ifelse(is.na(leaf_length_max_to_keep),"keep",leaf_length_max_to_keep)) %>%
  left_join(leaf_length_max_names,by="species_name") %>%
  write_csv("data/Metcalfe_2020_2/raw/leaf_length_max_comparison.csv") %>%
  mutate(leaf_length_max_unique = ifelse(leaf_length_max_to_keep=="keep",leaf_length_max_Metcalfe_2020_2,NA)) %>%
  select(original_name, leaf_length_max_unique) %>%
  rename(`current full name without authority` = original_name) -> leaf_length_max_comparison

read_csv("data/Metcalfe_2020_2/raw/data_with_duplicates.csv") %>%
  full_join(plant_height_comparison, by="current full name without authority") %>%
  full_join(seed_mass_comparison, by="current full name without authority") %>%
  full_join(leaf_length_min_comparison, by="current full name without authority") %>%
  full_join(leaf_length_max_comparison, by="current full name without authority") %>%
  full_join(leaf_width_min_comparison, by="current full name without authority") %>%
  full_join(leaf_width_max_comparison, by="current full name without authority") %>%
  write_csv("data/Metcalfe_2020_2/data.csv")