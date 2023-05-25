read_csv("data/eFLOWER_Dun_2022/raw/qry_Lily_Data_AusTraits_2023-05-21.csv") %>%
  subset(!is.na(Character_state)) %>% 
  mutate(trait_value = Character_state) -> character_values

read_csv("data/eFLOWER_Dun_2022/raw/qry_Lily_Data_AusTraits_2023-05-21.csv") %>%
  subset(!is.na(Value)) %>% 
  subset(Value != Max | is.na(Max)) %>%
  mutate(trait_value = Value) %>%
  mutate(trait_value = as.character(trait_value)) -> mean_values

read_csv("data/eFLOWER_Dun_2022/raw/qry_Lily_Data_AusTraits_2023-05-21.csv") %>%
  subset(!is.na(Min)) %>% 
  mutate(trait_value = Min) %>%
  mutate(Character = paste0(Character,"_min")) %>%
  mutate(trait_value = as.character(trait_value)) -> min_values

read_csv("data/eFLOWER_Dun_2022/raw/qry_Lily_Data_AusTraits_2023-05-21.csv") %>%
  subset(!is.na(Max)) %>% 
  mutate(trait_value = Max) %>%
  mutate(Character = paste0(Character,"_max")) %>%
  mutate(trait_value = as.character(trait_value)) -> max_values

character_values %>%
  bind_rows(mean_values) %>%
  bind_rows(min_values) %>%
  bind_rows(max_values) %>% 
  rename(Taxon = Species) %>%
  group_by(NDat,Taxon,Character,trait_value) %>%
  mutate_at(vars(trait_value),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  write_csv("data/eFLOWER_Dun_2022/data.csv")
