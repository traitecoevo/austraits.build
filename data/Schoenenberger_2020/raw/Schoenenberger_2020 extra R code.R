read_csv("data/Schoenenberger_2020/raw/data_raw.csv") %>% 
  rename(trait_name = Character) %>%
  distinct(CharacterState,.keep_all = TRUE) %>% write_csv("data/Schoenenberger_2020/raw/traits_values1.csv")

read_csv("data/Schoenenberger_2020/raw/data_raw.csv") %>%
  subset(!is.na(CharacterState)) %>% 
  mutate(trait_value = CharacterState) -> character_values

read_csv("data/Schoenenberger_2020/raw/data_raw.csv") %>%
  subset(!is.na(Value)) %>% 
  mutate(trait_value = Value) %>%
  mutate(trait_value = as.character(trait_value)) -> mean_values

read_csv("data/Schoenenberger_2020/raw/data_raw.csv") %>%
  subset(!is.na(Min)) %>% 
  mutate(trait_value = Min) %>%
  mutate(Character = paste0(Character,"_min")) %>%
  mutate(trait_value = as.character(trait_value)) -> min_values

read_csv("data/Schoenenberger_2020/raw/data_raw.csv") %>%
  subset(!is.na(Max)) %>% 
  mutate(trait_value = Max) %>%
  mutate(Character = paste0(Character,"_max")) %>%
  mutate(trait_value = as.character(trait_value)) -> max_values

character_values %>%
  bind_rows(mean_values) %>%
  bind_rows(min_values) %>%
  bind_rows(max_values) %>%
  left_join(taxa_in_Austraits) %>%
  write_csv("data/Schoenenberger_2020/data.csv")

#excluded data:
subset(austraits$excluded_data,dataset_id==current_study & error!="Missing value") %>%
  write_csv("data/Schoenenberger_2020/raw/excluded_data.csv")


#taxon match
read_csv("config/taxon_list.csv") %>%
  mutate(taxa_to_keep = 1) %>%
  rename(Taxon = taxon_name) %>%
  select(Taxon, taxa_to_keep) -> taxa_in_Austraits



