austraits_deduped <- remove_suspected_duplicates(austraits)


subset(austraits_deduped$excluded_data,dataset_id=="Mokany_2015" & error !="Missing value") %>%
  subset(trait_name == "plant_height") %>%
  mutate(error = gsub("Duplicate of ","",error)) %>% 
  write_csv("data/Mokany_2015/raw/test.csv")
  
subset(austraits_deduped$excluded_data,dataset_id=="Mokany_2015" & error !="Missing value") %>%
  subset(trait_name == "seed_mass") %>%
  mutate(error = gsub("Duplicate of ","",error)) %>% 
  write_csv("data/Mokany_2015/raw/seed_mass_to_remove.csv")   

read_csv("data/Mokany_2015/raw/plant_height_to_remove.csv") %>%
  select(original_name) %>%
  mutate(plant_height_duplicates = "plant_height_duplicate")  %>%
  rename(`Spp Name` = original_name)-> plant_height_to_remove

read_csv("data/Mokany_2015/raw/seed_mass_to_remove.csv") %>%
  select(original_name) %>%
  mutate(seed_mass_duplicates = "seed_mass_duplicate") %>%
  rename(`Spp Name` = original_name) -> seed_mass_to_remove

read_csv("data/Mokany_2015/data.csv") %>%
  left_join(seed_mass_to_remove,by="Spp Name") %>%
  left_join(plant_height_to_remove,by="Spp Name") %>% 
  mutate(seed_mass_unique_values = ifelse(`Seed_Mass_interpolated_(1=Y,0=N)`==1,NA,Seed_Mass_g),
         seed_mass_unique_values = ifelse(is.na(seed_mass_duplicates),seed_mass_unique_values,NA),
         plant_height_unique_values = ifelse(`Plant_height_interpolated_(1=Y,0=N)`==1,NA,`Plant_height_(m)`),
         plant_height_unique_values = ifelse(is.na(plant_height_duplicates),plant_height_unique_values,NA)) %>%
  write_csv("data/Mokany_2015/data.csv")


###OLDER CODE

subset(austraits$traits,trait_name=="seed_mass" & dataset_id=="Mokany_2015") %>%
  select(species_name,original_name) %>%
  group_by(original_name) %>%
  summarise_all(.funs = first) %>%
  ungroup() -> seed_mass_names

subset(austraits$traits,trait_name=="seed_mass" & dataset_id %in% 
         c("Mokany_2015","Kew_2019_1","RBGK_2014")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(seed_mass = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(seed_mass = round(mean(seed_mass),2)) %>%
  spread(key=dataset_id,value=seed_mass) %>%
  ungroup() %>%
  subset(!is.na(Mokany_2015)) %>% 
  rename(seed_mass_mean_Mokany_2015 = Mokany_2015,  
         seed_mass_mean_Kew_2019_1 = Kew_2019_1,
         seed_mass_mean_RBGK_2014 = RBGK_2014
  ) %>%
  mutate(seed_mass_to_keep_mean = ifelse(seed_mass_mean_Mokany_2015 == seed_mass_mean_Kew_2019_1 |
                                    seed_mass_mean_Mokany_2015 == seed_mass_mean_RBGK_2014,
                                       "omit","keep")) %>%
  mutate(seed_mass_to_keep_mean = ifelse(is.na(seed_mass_to_keep_mean),"keep",seed_mass_to_keep_mean)) %>%
  left_join(seed_mass_names,by="species_name") -> seed_mass_means_comparison

subset(austraits$traits,trait_name=="seed_mass" & dataset_id %in% 
         c("Mokany_2015","Kew_2019_1","RBGK_2014")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(seed_mass = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(seed_mass = round(max(seed_mass),2)) %>%
  spread(key=dataset_id,value=seed_mass) %>%
  ungroup() %>%
  subset(!is.na(Mokany_2015)) %>% 
  rename(seed_mass_max_Mokany_2015 = Mokany_2015,  
         seed_mass_max_Kew_2019_1 = Kew_2019_1,
         seed_mass_max_RBGK_2014 = RBGK_2014
  ) %>%
  mutate(seed_mass_to_keep_max = ifelse(seed_mass_max_Mokany_2015 == seed_mass_max_Kew_2019_1 |
                                      seed_mass_max_Mokany_2015 == seed_mass_max_RBGK_2014,
                                    "omit","keep")) %>%
  mutate(seed_mass_to_keep_max = ifelse(is.na(seed_mass_to_keep_max),"keep",seed_mass_to_keep_max)) %>%
  full_join(seed_mass_means_comparison,by="species_name") %>%
  mutate(seed_mass_unique = ifelse(seed_mass_to_keep_max=="keep"|
                                     seed_mass_to_keep_mean=="keep","seed_mass_to_keep",NA)) %>%
  write_csv("data/Mokany_2015/raw/seed_mass_comparison.csv") %>%
  select(original_name,seed_mass_unique) %>%
  rename(`Spp Name` = original_name) -> seed_mass_all_comparison

subset(austraits$traits,trait_name=="plant_height" & dataset_id=="Mokany_2015") %>%
  select(species_name,original_name) %>%
  group_by(original_name) %>%
  summarise_all(.funs = first) %>%
  ungroup() -> plant_height_names

subset(austraits$traits,trait_name=="plant_height" & dataset_id %in% 
         c("Mokany_2015","Barlow_1981","CPBR_2002","GrassBase_2014","RBGSYD_2014_2","SAH_2014","TMAG_2009","WAH_1998")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(plant_height = as.numeric(value)) %>%
  group_by(species_name,dataset_id) %>%
  summarise(plant_height = round(max(plant_height),2)) %>%
  spread(key=dataset_id,value=plant_height) %>%
  ungroup() %>%
  subset(!is.na(Mokany_2015)) %>% 
  rename(plant_height_max_Mokany_2015 = Mokany_2015,  
         plant_height_max_Barlow_1981 = Barlow_1981,
         plant_height_max_CPBR_2002 = CPBR_2002,
         plant_height_max_GrassBase_2014 = GrassBase_2014,
         plant_height_max_RBGSYD_2014_2 = RBGSYD_2014_2,
         plant_height_max_SAH_2014 = SAH_2014,
         plant_height_max_TMAG_2009 = TMAG_2009,
         plant_height_max_WAH_1998 = WAH_1998
  ) %>%
  mutate(plant_height_to_keep_max = ifelse(plant_height_max_Mokany_2015 == plant_height_max_Barlow_1981 |
                                            plant_height_max_Mokany_2015 == plant_height_max_CPBR_2002| 
                                            plant_height_max_Mokany_2015 == plant_height_max_GrassBase_2014| 
                                            plant_height_max_Mokany_2015 == plant_height_max_RBGSYD_2014_2| 
                                            plant_height_max_Mokany_2015 == plant_height_max_SAH_2014| 
                                            plant_height_max_Mokany_2015 == plant_height_max_TMAG_2009| 
                                            plant_height_max_Mokany_2015 == plant_height_max_WAH_1998,
                                        "omit","keep")) %>%
  mutate(plant_height_to_keep_max = ifelse(is.na(plant_height_to_keep_max),"keep",plant_height_to_keep_max)) %>%
  mutate(plant_height_unique = ifelse(plant_height_to_keep_max=="keep","plant_height_to_keep",NA)) %>%
  left_join(plant_height_names,by="species_name") %>%
  write_csv("data/Mokany_2015/raw/plant_height_comparison.csv") %>%
  select(original_name,plant_height_unique) %>%
  rename(`Spp Name` = original_name) -> plant_height_all_comparison

read_csv("data/Mokany_2015/data.csv") %>%
  left_join(seed_mass_all_comparison,by="Spp Name") %>%
  left_join(plant_height_all_comparison,by="Spp Name") %>%
  mutate(seed_mass_unique_values = ifelse(`Seed_Mass_interpolated_(1=Y,0=N)`==1,NA,Seed_Mass_g),
         seed_mass_unique_values = ifelse(seed_mass_unique=="seed_mass_to_keep",seed_mass_unique_values,NA),
        plant_height_unique_values = ifelse(`Plant_height_interpolated_(1=Y,0=N)`==1,NA,`Plant_height_(m)`),
        plant_height_unique_values = ifelse(plant_height_unique=="plant_height_to_keep",plant_height_unique_values,NA)) %>%
  write_csv("data/Mokany_2015/data.csv")