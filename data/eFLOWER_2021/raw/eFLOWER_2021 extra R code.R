read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>% 
  rename(trait_name = Character) %>%
  distinct(Character_state,.keep_all = TRUE) %>% write_csv("data/eFLOWER_2021/raw/traits_values1.csv")

#taxon match
read_csv("config/taxon_list.csv") %>%
  mutate(taxa_to_keep = 1) %>%
  rename(Taxon = taxon_name) %>%
  select(Taxon, taxa_to_keep) -> taxa_in_Austraits

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Character_state)) %>% 
  mutate(trait_value = Character_state) -> character_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Value)) %>% 
  subset(is.na(Max)) %>%
  mutate(trait_value = Value) %>%
  mutate(trait_value = as.character(trait_value)) -> mean_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Min)) %>% 
  mutate(trait_value = Min) %>%
  mutate(Character = paste0(Character,"_min")) %>%
  mutate(trait_value = as.character(trait_value)) -> min_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Max)) %>% 
  mutate(trait_value = Max) %>%
  mutate(Character = paste0(Character,"_max")) %>%
  mutate(trait_value = as.character(trait_value)) -> max_values

character_values %>%
  bind_rows(mean_values) %>%
  bind_rows(min_values) %>%
  bind_rows(max_values) %>% 
  rename(Taxon = Species) %>%
  left_join(taxa_in_Austraits) %>%
  group_by(NDat,Taxon,Character,trait_value) %>%
  mutate_at(vars(trait_value),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  write_csv("data/eFLOWER_2021/data.csv")

#excluded data:
subset(austraits$excluded_data,dataset_id==current_study & error!="Missing value") %>%
  write_csv("data/eFLOWER_2021/raw/excluded_data.csv")

subset(austraits$traits,dataset_id==current_study) %>%
  write_csv("data/eFLOWER_2021/raw/data_in_Austraits.csv")

austraits$traits %>%
  subset(dataset_id == "eFLOWER_2021") -> eFLOWER

eFLOWER %>%
  distinct(taxon_name) %>%
  as.data.frame() %>%
  anti_join(taxa_in_Austraits) %>%
  write_csv("data/eFLOWER_2021/raw/eFLOWER_taxa_not_in_AusTraits.csv")

taxa_in_Austraits %>%
  rename(taxon_name = Taxon) -> taxa_in_Austraits


