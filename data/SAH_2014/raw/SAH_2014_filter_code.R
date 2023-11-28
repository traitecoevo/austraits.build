austraits$traits %>% 
  filter(dataset_id %in% c("SAH_2014")) %>%
  select(taxon_name, trait_name, value_type, value, unit, observation_id, original_name) %>% 
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() -> SAH_old

tmp <- austraits %>%
  austraits::extract_dataset(c("SAH_2022", "SAH_2023")) %>%
  austraits::join_contexts()
  
tmp$traits %>%
  select(taxon_name, trait_name, value_type, value, `entity measured`) %>%
  select(-`entity measured`) %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>%
  rename(value_new = value) -> SAH_new

SAH_old %>% 
  left_join(SAH_new) %>%
  filter(is.na(value_new)) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>% View()

SAH_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  distinct() %>%
  left_join(SAH_new) %>%
  filter(is.na(value_new)) %>%
  select(-value_new) -> SAH_2014_retain

SAH_2014_retain %>% write_csv("data/SAH_2014/data.csv")
