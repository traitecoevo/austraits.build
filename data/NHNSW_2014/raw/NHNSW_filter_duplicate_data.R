NHNSW_old <- austraits$traits %>% 
  filter(dataset_id %in% c("NHNSW_2014", "NHNSW_2014_2", "NHNSW_2016")) %>%
  select(taxon_name, trait_name, value_type, value, unit, observation_id, original_name) %>% 
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup()

tmp <- austraits %>%
  austraits::extract_dataset(c("NHNSW_2022", "NHNSW_2023")) %>%
  austraits::join_contexts()

NHNSW_new <- tmp$traits %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>%
  rename(value_new = value)

counts_diff <- NHNSW_old %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(NHNSW_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>%
  arrange(n_taxa)

NHNSW_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(NHNSW_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  distinct(taxon_name) -> taxa_in_old

NHNSW_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(NHNSW_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form", "clonal_spread_mechanism", "leaf_width", "leaf_length", "plant_height")) %>%
  select(-value_new) -> NHNSW_retain

NHNSW_retain %>% write_csv("data/NHNSW_2014/data.csv")
