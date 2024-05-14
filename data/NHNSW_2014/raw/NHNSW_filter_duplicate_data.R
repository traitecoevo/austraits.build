NHNSW_old <- austraits$traits %>% 
  filter(dataset_id %in% c("NHNSW_2014", "NHNSW_2014_2", "NHNSW_2016")) %>%
  select(dataset_id, taxon_name, trait_name, value_type, value, unit, original_name) %>% 
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(dataset_id, taxon_name, trait_name, value_type, original_name, unit) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>% distinct()

tmp <- austraits %>%
  austraits::extract_dataset(c("NHNSW_2022", "NHNSW_2023")) %>%
  austraits::join_contexts()

NHNSW_new <- tmp$traits %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>% distinct() %>%
  rename(value_new = value)

counts_diff <- NHNSW_old %>% 
  distinct(taxon_name, trait_name, value_type, value, .keep_all = TRUE) %>%
  left_join(NHNSW_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>%
  arrange(n_taxa)

retain_1 <- NHNSW_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, original_name, .keep_all = TRUE) %>%
  left_join(NHNSW_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form"))

retain_missing_taxa <- retain_1 %>% filter(!taxon_name %in% NHNSW_new$taxon_name)

retain_missing_taxa %>% select(taxon_name, original_name) %>% left_join(austraits$taxa %>% select(taxon_name, family)) %>% distinct() %>%  write_csv("data/NHNSW_2014/raw/taxa_missing_in_NHNSW_2023.csv")

retain_missing_taxa %>% select(taxon_name, original_name) %>% write_csv("data/NHNSW_2014/raw/taxa_data_missing_in_NHNSW_2023.csv")

retain_2 <- retain_1 %>% anti_join(retain_missing_taxa)

retain_trustworthy_traits <-
  retain_2 %>%
  filter(trait_name %in% c("leaf_phenology", "flowering_time", "leaf_compoundness", "dispersal_appendage", "plant_growth_substrate", "seed_shape",
"stem_growth_habit", "flower_colour", "leaf_margin", "leaf_phyllotaxis", "vegetative_reproduction_ability", "fruiting_time", 
"clonal_spread_mechanism", "leaf_type", "parasitic", "dispersal_syndrome"))

retain_3 <- retain_2 %>% anti_join(retain_trustworthy_traits)

retain_leaf_area <-retain_3 %>% filter(trait_name == "leaf_area")

retain_3 %>% filter(trait_name != "leaf_area") %>% write_csv("data/NHNSW_2014/raw/excluded_numeric_data.csv")

NHNSW_retain <- retain_missing_taxa %>%
  bind_rows(retain_trustworthy_traits) %>%
  bind_rows(retain_leaf_area)
 
NHNSW_retain %>% filter(dataset_id == "NHNSW_2014") %>% write_csv("data/NHNSW_2014/data.csv")
NHNSW_retain %>% filter(dataset_id == "NHNSW_2014_2") %>% write_csv("data/NHNSW_2014_2/data.csv")
NHNSW_retain %>% filter(dataset_id == "NHNSW_2016") %>% write_csv("data/NHNSW_2016/data.csv")
