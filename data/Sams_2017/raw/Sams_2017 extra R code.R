#code to create data.csv file without filtering columns
read_csv("data/Sams_2017/raw/Sams_et_al_2017_site_coords.csv") -> sites

read_csv("data/Sams_2017/raw/SLA_species_n_names_matched.csv")  %>%
  rename(species_name = species) -> SLA_n

read_csv("data/Sams_2017/raw/data_starting.csv") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>%
  select(species_name,`Hmax(m)`,WD, SLA, minseedlength, maxseedlength,`fruit size(min).mm2`,
         `fruit size(max).mm2`,Fruit_Type_2, dispersalmode, dispersal2,`Location.n`,`Rainforest.Type`) %>%
  left_join(sites,by="Location.n") %>%
  full_join(SLA_n,by="species_name") %>%
  distinct(species_name,.keep_all = TRUE) %>% 
  write_csv("data/Sams_2017/raw/data_with_duplicates.csv")

#code to add columns to filter out duplicates
subset(austraits$traits,trait_name=="seed_length" & value_type == "expert_min" & dataset_id %in% 
         c("Sams_2017","Cooper_2013","Kooyman_2011")) %>%
  select(species_name,value,dataset_id,original_name) %>%
  mutate(seed_length = as.numeric(value)) %>%
  select(species_name,dataset_id,seed_length,original_name) %>%
  group_by(species_name,dataset_id,original_name) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=seed_length) %>%
  ungroup() %>%
  subset(!is.na(Sams_2017)) %>% 
  rename(seed_length_min_Kooyman_2011 = Kooyman_2011,  
         seed_length_min_Cooper_2013 = Cooper_2013, seed_length_min_Sams_2017 = Sams_2017) %>%
  mutate(seed_length_min_to_keep = ifelse(seed_length_min_Sams_2017 == seed_length_min_Kooyman_2011 |
                                        seed_length_min_Sams_2017 == seed_length_min_Cooper_2013,
                                      "omit","keep")) %>%
  mutate(seed_length_min_to_keep = ifelse(is.na(seed_length_min_to_keep),"keep",seed_length_min_to_keep)) %>%
  write_csv("data/Sams_2017/raw/seed_length_min_comparison.csv") %>%
  mutate(seed_length_min_unique = ifelse(seed_length_min_to_keep=="keep",seed_length_min_Sams_2017,NA)) %>%
  select(original_name, seed_length_min_unique) %>%
  rename(species_name = original_name) -> seed_length_min_comparison

subset(austraits$traits,trait_name=="seed_length" & value_type == "expert_max" & dataset_id %in% c("Sams_2017","Cooper_2013","Kooyman_2011")) %>%
  select(species_name,value,dataset_id,original_name) %>%
  mutate(seed_length = as.numeric(value)) %>%
  select(species_name,dataset_id,seed_length,original_name) %>%
  group_by(species_name,dataset_id,original_name) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=seed_length) %>%
  ungroup() %>%
  subset(!is.na(Sams_2017)) %>% 
  rename(seed_length_max_Kooyman_2011 = Kooyman_2011, 
         seed_length_max_Cooper_2013 = Cooper_2013,seed_length_max_Sams_2017 = Sams_2017) %>%
  mutate(seed_length_max_to_keep = ifelse(seed_length_max_Sams_2017 == seed_length_max_Kooyman_2011 | 
                                            seed_length_max_Sams_2017 == seed_length_max_Cooper_2013,
                                          "omit","keep")) %>%
  mutate(seed_length_max_to_keep = ifelse(is.na(seed_length_max_to_keep),"keep",seed_length_max_to_keep)) %>%
  write_csv("data/Sams_2017/raw/seed_length_max_comparison.csv") %>%
  mutate(seed_length_max_unique = ifelse(seed_length_max_to_keep=="keep",seed_length_max_Sams_2017,NA)) %>%
  select(original_name, seed_length_max_unique) %>%
  rename(species_name = original_name) -> seed_length_max_comparison

subset(austraits$traits,trait_name=="plant_height" & dataset_id %in% c("Sams_2017","Metcalfe_2020_2","Cooper_2013","Kooyman_2011")) %>%
  select(species_name,value,dataset_id,original_name) %>%
  mutate(plant_height = as.numeric(value)) %>%
  select(species_name,dataset_id,plant_height,original_name) %>%
  group_by(species_name,dataset_id,original_name) %>%
  summarise_all(.funs=max,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=plant_height) %>%
  ungroup() %>%
  subset(!is.na(Sams_2017)) %>%
  rename(plant_height_Kooyman_2011 = Kooyman_2011, plant_height_Metcalfe_2020_2 = Metcalfe_2020_2,
         plant_height_Cooper_2013 = Cooper_2013,plant_height_Sams_2017 = Sams_2017) %>%
  mutate(plant_height_to_keep = ifelse(plant_height_Sams_2017 == plant_height_Kooyman_2011 |plant_height_Sams_2017 == plant_height_Metcalfe_2020_2 |
                                         plant_height_Sams_2017 == plant_height_Cooper_2013,"omit","keep")) %>%
  mutate(plant_height_to_keep = ifelse(is.na(plant_height_to_keep),"keep",plant_height_to_keep)) %>%
  write_csv("data/Sams_2017/raw/plant_height_comparison.csv") %>%
  mutate(plant_height_unique = ifelse(plant_height_to_keep=="keep",plant_height_Sams_2017,NA))%>%
  select(original_name, plant_height_unique) %>%
  rename(species_name = original_name)  ->  plant_height_comparison

subset(austraits$traits,trait_name=="wood_density" & dataset_id %in% c("Sams_2017","Kooyman_2011","Zanne_2009","Ilic_2000")) %>%
  select(species_name,value,dataset_id,original_name) %>%
  mutate(wood_density = as.numeric(value)) %>%
  select(species_name,dataset_id,wood_density,original_name) %>%
  group_by(species_name,dataset_id,original_name) %>%
  summarise_all(.funs=mean,.keep_all = TRUE) %>%
  spread(key=dataset_id,value=wood_density) %>%
  ungroup(unique_to_Sams_2017 = ifelse()) %>%
  subset(!is.na(Sams_2017)) %>%
  rename(wood_density_Sams_2017 = Sams_2017, wood_density_Kooyman_2011 = Kooyman_2011, wood_density_Zanne_2009 = Zanne_2009, 
         wood_density_Ilic_2000 = Ilic_2000) %>%
  mutate(wood_density_to_keep = ifelse(round(wood_density_Sams_2017,2) == round(wood_density_Kooyman_2011,2) |
                                         round(wood_density_Sams_2017,2) == round(wood_density_Zanne_2009,2) |
                                       round(wood_density_Sams_2017,2) == round(wood_density_Ilic_2000,2),
                                       "omit","keep")) %>%
  mutate(wood_density_to_keep = ifelse(is.na(wood_density_to_keep),"keep",wood_density_to_keep)) %>%
  mutate(wood_density_unique = ifelse(wood_density_to_keep=="keep",wood_density_Sams_2017,NA)) %>%
  select(original_name, wood_density_unique) %>%
  rename(species_name = original_name) -> wood_density_comparison

read_csv("data/Sams_2017/raw/data_before_filters.csv") %>%
  full_join(wood_density_comparison, by="species_name") %>%
  full_join(plant_height_comparison, by="species_name") %>%
  full_join(seed_length_min_comparison, by="species_name") %>%
  full_join(seed_length_max_comparison, by="species_name") %>%
  write_csv("data/Sams_2017/data.csv")