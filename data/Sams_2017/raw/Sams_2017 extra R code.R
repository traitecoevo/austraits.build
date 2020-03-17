read_csv("data/Sams_2017/raw/data_starting.csv") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>%
  select(species_name,`Hmax(m)`,WD, SLA, minseedlength, maxseedlength,`fruit size(min).mm2`,
         `fruit size(max).mm2`,Fruit_Type_2, dispersalmode, dispersal2) %>%
  distinct(species_name,.keep_all = TRUE) %>% 
  write_csv("data/Sams_2017/data.csv")
  
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

#code to compare Metacalfe_2020_2, Kooyman_2011, Cooper_2013
subset(austraits$traits,dataset_id %in% c("Sams_2017","Metcalfe_2020_2","Cooper_2013","Kooyman_2011")) %>%
  subset(trait_name %in% c("wood_density","specific_leaf_area","plant_height","seed_length")) %>% 
  write_csv("data/Sams_2017/raw/comparing_across_studies.csv")
