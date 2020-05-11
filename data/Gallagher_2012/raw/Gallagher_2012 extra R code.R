read_csv("data/Gallagher_2012b/raw/trait_values.csv") %>%
  rename(species_name = `Accepted taxon name in Plant List www.theplantlist.org`) -> trait_values

#extract SLA from Rachael Gallagher's collections, reference 15
trait_values %>%
  subset(`SLA source` == 15) %>%
  select(species_name, `SLA (mm2/mg)`) -> SLA_values

trait_values %>%
  subset(!is.na(`dispersal appendage/syndrome`)) %>%
  select(species_name, `dispersal appendage/syndrome`) -> dispersal_appendage_values

trait_values %>%
  subset(!is.na(`growth habit`)) %>%
  select(species_name, `growth habit`) -> growth_habit_values

trait_values %>%
  subset(!is.na(`climbing type`)) %>%
  select(species_name, `climbing type`) -> climbing_mechanism_values

trait_values %>%
  subset(`seed mass source` %in% c(48,49,50,52)) %>%
  select(species_name, `seed mass (mg)`) -> seed_mass_values

trait_values %>%
  subset(`leaf size source` %in% c(15)) %>%
  select(species_name, `leaf size (cm2)`) -> leaf_size_values

read_csv("data/Gallagher_2012b/raw/species_names.csv") %>%
  rename(species_name = "Accepted taxon name in Plant List www.theplantlist.org/") %>%
  select(species_name,"Original taxon in paper") %>%
  group_by(species_name) %>%
  summarise_all(.funs=first) %>%
  ungroup() -> names

read_csv("data/Gallagher_2012b/raw/data_SLA_from_TRY_raw.csv") %>%
  inner_join(names,by="species_name") %>%
  write_csv("data/Gallagher_2012b/raw/data_SLA_from_TRY_Australia_only.csv") 

subset(austraits$traits,trait_name=="specific_leaf_area") %>%
  select(species_name,dataset_id,value) %>%
  group_by(species_name,dataset_id) %>%
  summarise(value = mean(as.numeric(value))) %>%
  ungroup() %>%
  spread(key=dataset_id,value=value) %>%
  select(-Gallagher_2012b) %>%
  mutate(total_SLA = rowSums(.[2:82],na.rm = TRUE)) %>%
  select(species_name,total_SLA) -> test

subset(austraits$traits,trait_name=="specific_leaf_area"& dataset_id=="Gallagher_2012b") %>% 
  select(original_name,species_name,dataset_id,value) %>%
  left_join(test,by="species_name") %>%
  mutate(SLA_from_TRY = ifelse(total_SLA == 0,value,NA)) %>%
  subset(!is.na(SLA_from_TRY)) %>%
  select(species_name,SLA_from_TRY) %>%
  write_csv("data/Gallagher_2012b/raw/SLA_from_TRY_unique_to_use.csv") -> SLA_from_TRY

read_csv("data/Gallagher_2012b/raw/data_leaf_area_from_TRY_raw.csv") %>%
  inner_join(names,by="species_name") %>%
  write_csv("data/Gallagher_2012b/raw/data_leaf_area_from_TRY_Australia_only.csv") 

subset(austraits$traits,trait_name=="leaf_area") %>%
  select(species_name,dataset_id,value) %>%
  group_by(species_name,dataset_id) %>%
  summarise(value = mean(as.numeric(value))) %>%
  ungroup() %>%
  spread(key=dataset_id,value=value) %>%
  select(-Gallagher_2012b) %>% 
  mutate(total_leaf_area = rowSums(.[2:54],na.rm = TRUE)) %>%
  select(species_name,total_leaf_area) -> test

subset(austraits$traits,trait_name=="leaf_area"& dataset_id=="Gallagher_2012b") %>% 
  select(original_name,species_name,dataset_id,value) %>%
  left_join(test,by="species_name") %>% View()
  mutate(leaf_area_from_TRY = ifelse(total_leaf_area == 0,value,NA)) %>%
  subset(!is.na(leaf_area_from_TRY)) %>%
  select(species_name,leaf_area_from_TRY) %>%
  write_csv("data/Gallagher_2012b/raw/leaf_area_from_TRY_unique_to_use.csv") -> leaf_area_from_TRY

##no unique leaf area values from TRY
SLA_values %>%
  full_join(SLA_from_TRY,by=c("species_name")) %>%
  full_join(seed_mass_values,by=c("species_name")) %>%
  full_join(leaf_size_values,by=c("species_name")) %>%
  full_join(dispersal_appendage_values,by=c("species_name")) %>%
  full_join(growth_habit_values,by=c("species_name")) %>%
  full_join(climbing_mechanism_values,by=c("species_name")) %>%
  inner_join(names,by="species_name") %>%
  write_csv("data/Gallagher_2012b/data.csv") -> traits