subset(austraits$traits,trait_name=="wood_density" & dataset_id == "Ilic_2000") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(value = round(value,digits = 2)) %>%
  rename(Ilic = value) -> Ilic

subset(austraits$traits,trait_name=="wood_density" & dataset_id == "Metcalfe_2009") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(value = round(value,digits = 2)) %>%
  rename(Metcalfe = value) -> Metcalfe

subset(austraits$traits,trait_name=="wood_density" & dataset_id == "Zanne_2009") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(value = round(value,digits = 2)) %>%
  rename(Zanne = value) -> Zanne

subset(austraits$traits,trait_name=="wood_density" & dataset_id == "Kooyman_2011") %>%
  select(species_name,value) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(species_name) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(value = round(value,digits = 2)) %>%
  rename(Kooyman = value) -> Kooyman

subset(austraits$traits,trait_name=="wood_density" & dataset_id == "Sams_2017") %>%
  select(species_name, original_name, value) %>%
  mutate(value = as.numeric(value),
         value = round(value,digits = 2)) %>%
  rename(Sams = value) %>%
  left_join(Ilic,by="species_name") %>%
  left_join(Zanne,by="species_name") %>%
  left_join(Metcalfe,by="species_name") %>%
  left_join(Kooyman,by="species_name") %>%
  mutate(mean_WD = rowMeans(select(.,c(Ilic,Zanne,Metcalfe,Kooyman)),na.rm = TRUE),
         mean_WD = round(mean_WD,digits = 2),
         Ilic_yes = if_else(Ilic == Sams,1,0),
         Metcalfe_yes = if_else(Metcalfe == Sams,1,0),
         Zanne_yes = if_else(Zanne == Sams,1,0),
         Kooyman_yes = if_else(Kooyman == Sams,1,0),
         mean_yes = if_else(mean_WD == Sams,1,0)) %>% 
  mutate(sum_matches = rowSums(select(.,c(Ilic_yes,Zanne_yes,Metcalfe_yes,Kooyman_yes,mean_yes)),na.rm = TRUE)) %>% 
  subset(sum_matches == 0) %>% 
  write_csv("data/Sams_2017/raw/WD_values_to_keep.csv")
