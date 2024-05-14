WAH_old <- austraits$traits %>% 
  filter(dataset_id %in% c("WAH_1998", "WAH_2016")) %>%
  select(dataset_id, taxon_name, trait_name, value_type, value, unit, original_name, observation_id) %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>% distinct() %>%
  ungroup()

tmp <- austraits %>%
  austraits::extract_dataset(c("WAH_2022_1", "WAH_2023_1", "WAH_2022_2", "WAH_2023_2")) %>%
  austraits::join_contexts()

WAH_new <- tmp$traits %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>%
  rename(value_new = value)

counts_diff <- WAH_old %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>% 
  left_join(WAH_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>%
  arrange(n_taxa)

retain_1 <- WAH_old %>%
  distinct(dataset_id, taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(WAH_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form"))

retain_missing_taxa <- retain_1 %>% filter(!taxon_name %in% WAH_new$taxon_name)

retain_2 <- retain_1 %>% anti_join(retain_missing_taxa)

excluded <- retain_2 %>% filter(trait_name == "plant_height")

WAH_retain <-
  retain_missing_taxa %>%
  bind_rows(retain_2 %>% filter(trait_name != "plant_height")) %>%
  select(-value_new)

WAH_retain %>% filter(dataset_id == "WAH_1998") %>% write_csv("data/WAH_1998/data.csv")

WAH_retain %>% filter(dataset_id == "WAH_2016") %>% write_csv("data/WAH_2016/data.csv")  
