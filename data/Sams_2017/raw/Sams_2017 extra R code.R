read_csv("data/Sams_2017/raw/Sams_et_al_2017_site_coords.csv") -> sites

read_csv("data/Sams_2017/raw/SLA_species_n_names_matched.csv")  %>%
  rename(species_name = species) -> SLA_n

#code to create data.csv file without filtering columns
read_csv("data/Sams_2017/raw/data_starting.csv") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>%
  select(species_name,`Hmax(m)`,WD, SLA, minseedlength, maxseedlength,`fruit size(min).mm2`,
         `fruit size(max).mm2`,Fruit_Type_2, dispersalmode, dispersal2,`Location.n`,`Rainforest.Type`) %>%
  left_join(sites,by="Location.n") %>%
  full_join(SLA_n,by="species_name") %>%
  distinct(species_name,.keep_all = TRUE) %>% 
  write_csv("data/Sams_2017/data.csv")

#code to add columns to filter out duplicates
read_csv("data/Sams_2017/raw/data_starting.csv") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>%
  select(species_name,`Hmax(m)`,WD, SLA, minseedlength, maxseedlength,`fruit size(min).mm2`,
         `fruit size(max).mm2`,Fruit_Type_2, dispersalmode, dispersal2,`Location.n`,`Rainforest.Type`) %>%
  left_join(sites,by="Location.n") %>%
  full_join(SLA_n,by="species_name") %>%
  distinct(species_name,.keep_all = TRUE) %>% 
  write_csv("data/Sams_2017/data.csv")

  
subset(austraits$traits,trait_name=="plant_height" & dataset_id %in% c("Metcalfe_2020_2","Cooper_2013","Kooyman_2011")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(plant_height = as.numeric(value)) %>%
  select(species_name,dataset_id,plant_height) %>%
  group_by(species_name,dataset_id) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=plant_height) %>%
  ungroup() -> plant_height_other_studies

subset(austraits$traits,trait_name=="wood_density" & dataset_id %in% c("Metcalfe_2020_2","Cooper_2013","Kooyman_2011","Zanne_2009")) %>%
  select(species_name,value,dataset_id) %>%
  mutate(wood_density = as.numeric(value)) %>%
  select(species_name,dataset_id,wood_density) %>%
  group_by(species_name,dataset_id) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=wood_density) %>%
  ungroup() -> wood_density_other_studies
  

  
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
read_csv("data/Tasmania_2020/raw/data_original.csv") %>%
  left_join(seed_mass_outcome, by="Spp Name") %>%
  write_csv("data/Tasmania_2020/data.csv")





#code used to check if indeed, single value for each species
Sams_to_use %>%
  select(-Fruit_Type_2, -dispersalmode, -dispersal2, -Location) %>%
  group_by(species_name) %>%
  summarise_all(.funs=max) %>%
  ungroup() -> Sams_max

Sams_to_use %>%
  select(-Fruit_Type_2, -dispersalmode, -dispersal2, -Location) %>%
  group_by(species_name) %>%
  summarise_all(.funs=mean) %>%
  ungroup() %>%
  rename(height_mean = `Hmax(m)`,WD_mean = WD, SLA_mean = SLA, seed_min_mean = minseedlength, 
         seed_max_mean = maxseedlength, fruit_min_mean = `fruit size(min).mm2`, 
         fruit_max_mean =`fruit size(max).mm2`) %>%
  full_join(Sams_max,by=c("species_name")) %>% 
  write_csv("data/Sams_2017/raw/test2.csv") 

#code to compare Metacalfe_2020_2, Kooyman_2011, Cooper_2013, Zanne_2009
subset(austraits$traits,dataset_id %in% c("Sams_2017","Metcalfe_2020_2","Cooper_2013","Kooyman_2011")) %>%
  subset(trait_name %in% c("wood_density","specific_leaf_area","plant_height","seed_length")) %>% 
  write_csv("data/Sams_2017/raw/comparing_across_studies.csv")
