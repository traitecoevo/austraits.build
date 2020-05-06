#seed mass

subset(austraits$traits,trait_name=="seed_mass" & dataset_id == "Kew_2019_1") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(value_rounded = round(value,digits = 2)) %>%
  rename(Kew = value, Kew_rounded = value_rounded) -> Kew

subset(austraits$traits,trait_name=="seed_mass" & dataset_id == "Tasmania_2020") %>%
  select(species_name, original_name, value) %>%
  mutate(value = as.numeric(value),
         value_rounded = round(value,digits = 2)) %>%
  rename(Tasmania = value, Tasmania_rounded = value_rounded) %>%
  left_join(Kew,by="species_name") %>%
  mutate(Kew_yes = if_else(Kew == Tasmania,1,0),
         Kew_rounded_yes = if_else(Kew_rounded == Tasmania_rounded,1,0)) %>% 
  mutate(sum_matches = rowSums(select(.,c(Kew_yes,Kew_rounded_yes)),na.rm = TRUE)) %>% 
  mutate(seed_mass_outcome = if_else(sum_matches == 0, "keep","omit")) %>%
  #subset(sum_matches == 0) %>% 
  write_csv("data/Tasmania_2020/raw/seed_values_to_check.csv") %>%
  select(original_name,seed_mass_outcome) %>%
  rename(`Spp Name` = original_name) -> seed_mass_outcome

#plant height

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "Barlow_1981") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(Barlow = value) -> Barlow

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "CPBR_2002") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(CPBR = value) -> CPBR

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "GrassBase_2014") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(GrassBase = value) -> GrassBase

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "RBGSYD_2014_2") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(RBGSYD = value) -> RBGSYD

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "SAH_2014") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(SAH = value) -> SAH

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "TMAG_2009") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(TMAG = value) -> TMAG

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "WAH_1998") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  #mutate(value_rounded = round(value,digits = 2)) %>%
  rename(WAH = value) -> WAH

subset(austraits$traits,trait_name=="plant_height" & dataset_id == "Tasmania_2020") %>%
  select(species_name, original_name, value) %>%
  mutate(value = as.numeric(value),
         value_rounded = round(value,digits = 2)) %>%
  rename(Tasmania = value, Tasmania_rounded = value_rounded) %>%
  left_join(Barlow,by="species_name") %>%
  left_join(CPBR,by="species_name") %>%
  left_join(GrassBase,by="species_name") %>%
  left_join(RBGSYD,by="species_name") %>%
  left_join(SAH,by="species_name") %>%
  left_join(TMAG,by="species_name") %>%
  left_join(WAH,by="species_name") %>%
  mutate(Barlow_yes = if_else(Barlow == Tasmania,1,0),
         CPBR_yes = if_else(CPBR == Tasmania,1,0),
         GrassBase_yes = if_else(GrassBase == Tasmania,1,0),
         RBGSYD_yes = if_else(RBGSYD == Tasmania,1,0),
         SAH_yes = if_else(SAH == Tasmania,1,0),
         TMAG_yes = if_else(TMAG == Tasmania,1,0),
         WAH_yes = if_else(WAH == Tasmania,1,0)) %>%
  mutate(sum_matches = rowSums(select(.,c(Barlow_yes,CPBR_yes,GrassBase_yes,RBGSYD_yes,SAH_yes,TMAG_yes,WAH_yes)),na.rm = TRUE)) %>% 
  mutate(plant_height_outcome = if_else(sum_matches == 0, "keep","omit")) %>%
  write_csv("data/Tasmania_2020/raw/height_values_to_check.csv")%>%
  select(original_name,plant_height_outcome) %>%
  rename(`Spp Name` = original_name) -> plant_height_outcome

### add to main file

read_csv("data/Tasmania_2020/raw/data_original.csv") %>%
  left_join(seed_mass_outcome, by="Spp Name") %>%
  left_join(plant_height_outcome, by="Spp Name") %>%
  write_csv("data/Tasmania_2020/data.csv")



read_csv("data/Tasmania_2020/data.csv") %>%
  mutate(Seed_Mass_g = ifelse(`Seed_Mass_interpolated_(1=Y,0=N)`==1,NA,Seed_Mass_g),
         `Plant_height_(m)` = ifelse(`Plant_height_interpolated_(1=Y,0=N)`==1,NA,`Plant_height_(m)`),
         Leaf_Area_cm2 = ifelse(`Leaf_Area_interpolated_(1=Y,0=N)`==1,NA,Leaf_Area_cm2),
         Seed_Mass_g = ifelse(`seed_mass_outcome`=="omit",NA,Seed_Mass_g),
         `Plant_height_(m)` = ifelse(`plant_height_outcome`=="omit",NA,`Plant_height_(m)`))
