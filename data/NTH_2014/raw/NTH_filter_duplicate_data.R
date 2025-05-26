NTH_old <- austraits$traits %>% 
  filter(dataset_id %in% c("NTH_2014")) %>%
  select(taxon_name, trait_name, value_type, value, unit, original_name, observation_id) %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  filter(!(trait_name == "plant_height" & value_type == "minimum")) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>% distinct() %>%
  ungroup()

tmp <- austraits %>%
  austraits::extract_dataset(c("NTH_2022", "NTH_2023")) %>%
  austraits::join_contexts()

NTH_new <- tmp$traits %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct() %>%
  group_by(taxon_name, trait_name, value_type) %>%
  mutate(value = paste(value, collapse = " ")) %>%
  ungroup() %>%
  rename(value_new = value)

counts_diff <- NTH_old %>% 
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>% 
  left_join(NTH_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  filter(trait_name %in% NTH_new$trait_name) %>%
  group_by(trait_name) %>% mutate(n_taxa = n()) %>% ungroup() %>% distinct(trait_name, n_taxa) %>%
  arrange(n_taxa)

retain_Trichosanthes_cucu_cucu <-
  austraits$traits %>% 
  filter(dataset_id == "NTH_2014") %>% 
  filter(taxon_name == "Trichosanthes cucumerina var. cucumerina") %>% 
  select(taxon_name, trait_name, value_type, value, unit, original_name, observation_id)


retain_Synostemon_glaucus <-
  austraits$traits %>% 
  filter(dataset_id == "NTH_2014") %>% 
  filter(taxon_name == "Synostemon glaucus") %>% 
  filter(trait_name == "flowering_time") %>%
  select(taxon_name, trait_name, value_type, value, unit, original_name, observation_id) 

retain_1 <- NTH_old %>%
  distinct(taxon_name, trait_name, value_type, value, unit, .keep_all = TRUE) %>%
  left_join(NTH_new) %>%
  filter(is.na(value_new)) %>%
  filter(!trait_name %in% c("woodiness", "woodiness_detailed", "life_history", "plant_growth_form")) %>%
  filter(taxon_name != "Trichosanthes cucumerina var. cucumerina")

retain_missing_taxa <- retain_1 %>% filter(!taxon_name %in% NTH_new$taxon_name)

retain_2 <- retain_1 %>% anti_join(retain_missing_taxa)

new_max_seed_widths <- 
  NTH_new %>% 
  filter(trait_name == "seed_width") %>%
  filter(value_type == "maximum") %>%
  select(-value_type) %>%
  rename(value_new_max = value_new)

retain_missing_seed_widths <- 
  retain_2 %>% 
  filter(trait_name == "seed_width") %>%
  left_join(new_max_seed_widths) %>%
  filter(is.na(value_new_max) | value != value_new_max) %>%
  arrange(taxon_name, value_type) %>%
  select(-value_new, -value_new_max) %>%
  distinct(taxon_name, trait_name, value, unit, observation_id, original_name, .keep_all = TRUE) #remove min = max

missing_seed_heights <-
  retain_2 %>%
  filter(trait_name == "seed_height")

retain_missing_seed_heights <- 
  NTH_old %>%
    filter(trait_name %in% c("seed_height", "seed_width")) %>%
    select(taxon_name, trait_name, value_type, value, unit, observation_id, original_name) %>%
    pivot_wider(names_from = trait_name, values_from = value) %>%
    filter(seed_height != seed_width) %>%
    filter(observation_id %in% missing_seed_heights$observation_id) %>%
    rename(value = seed_height) %>%
    select(-seed_width) %>%
    mutate(trait_name = "seed_height") %>%
    distinct(taxon_name, trait_name, value, unit, observation_id, original_name, .keep_all = TRUE) #remove min = max

retain_trustworthy_traits <-
  retain_2 %>%
  filter(trait_name %in% c("plant_photosynthetic_organ", "leaf_compoundness", "parasitic", "leaf_glaucousness"))

excluded_numeric <- 
  retain_2 %>%
    filter(trait_name %in% c("seed_length", "seed_width", "seed_height", "leaf_width", "leaf_length", "plant_height"))

excluded_numeric %>% write_csv("data/NTH_2014/raw/excluded_numeric_data.csv")

retain_3 <-
  retain_2 %>%
    filter(!trait_name %in% c("seed_length", "seed_width", "seed_height", "leaf_width", "leaf_length", "plant_height")) %>%
    filter(!trait_name %in% c("plant_photosynthetic_organ", "leaf_compoundness", "parasitic", "leaf_glaucousness"))

NTH_retain <-
  retain_3 %>% 
  bind_rows(retain_missing_seed_heights) %>%
  bind_rows(retain_missing_seed_widths) %>%
  bind_rows(retain_missing_taxa) %>%
  bind_rows(retain_trustworthy_traits) %>%
  bind_rows(NTH_old %>% filter(trait_name %in% c("seed_height", "seed_width")) %>% filter(taxon_name == "Acacia gonocarpa")) %>%
  bind_rows(NTH_old %>% filter(trait_name %in% c("leaf_length", "leaf_width")) %>% filter(taxon_name %in% c("Nelumbo nucifera", "Ficus carpentariensis"))) %>%
  bind_rows(retain_Trichosanthes_cucu_cucu) %>%
  bind_rows(retain_Synostemon_glaucus) %>%
  filter(value != "yyyyynnyyyyy yyyyynnnyyyy") %>%
  filter(!trait_name %in% c("life_history", "plant_growth_form"))

NTH_retain %>% select(-value_new) %>% write_csv("data/NTH_2014/data.csv")