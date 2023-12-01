ABRS_old <- austraits$traits %>% 
  filter(dataset_id %in% c("ABRS_1981")) %>%
  select(taxon_name, trait_name, value_type, value, unit, original_name, observation_id) %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>% distinct() %>%
  ungroup()

tmp <- austraits %>%
  austraits::extract_dataset(c("ABRS_2022", "ABRS_2023")) %>%
  austraits::join_contexts()

ABRS_new <- tmp$traits %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>%
  rename(value_new = value)

counts_diff <- ABRS_old %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>% 
  left_join(ABRS_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>%
  arrange(n_taxa)

retain_1 <- ABRS_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(ABRS_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form"))

retain_missing_taxa <- retain_1 %>% filter(!taxon_name %in% ABRS_new$taxon_name)

retain_2 <- retain_1 %>% anti_join(retain_missing_taxa)

retain_trustworthy_traits <-
  retain_2 %>%
  filter(trait_name %in% c("leaf_phenology", "flowering_time", "leaf_compoundness", "dispersal_appendage", "plant_growth_substrate", "seed_shape"))

retain_seed_traits <-
  retain_2 %>%
  filter(trait_name %in% c("seed_length", "seed_width", "seed_height"))

excluded <- retain_2 %>% anti_join(retain_trustworthy_traits) %>% anti_join(retain_seed_traits)

ABRS_retain <-
  retain_missing_taxa %>%
  bind_rows(retain_trustworthy_traits) %>%
  bind_rows(retain_seed_traits)

ABRS_retain %>% select(-value_new) %>% write_csv("data/ABRS_1981/data.csv")
