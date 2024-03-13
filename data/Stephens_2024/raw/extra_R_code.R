# Juvenile leaf data from Euclid

juvenile_leaves <- 
  read_csv("data/Stephens_2024/raw/euclid_juvenile_leaf_dimensions_checked_matchedAPC.csv") %>%
  mutate(
    extra = stringr::str_replace_all(euclid_name, "_", " ") %>% stringr::str_to_sentence(),
    accepted_name = ifelse(is.na(accepted_name), extra, accepted_name)
  ) %>%
  select(-extra)

# Other trait data from Euclid

other_traits <- 
  read_csv("data/Stephens_2024/raw/RS_euc_traits_nosubsp.csv") %>% 
  rename(accepted_name = apc_nosubsp)

# Merge together tables

data_new <- other_traits %>%
  full_join(juvenile_leaves)

# Extract Euclid leaf dimension data that is already in AusTraits

Euclid_2002 <- austraits$traits %>%
  filter(dataset_id == "CPBR_2002") %>%
  filter(trait_name %in% c("leaf_length", "leaf_width")) %>%
  select(taxon_name, trait_name, value_type, value) %>%
  distinct(taxon_name, trait_name, value_type, .keep_all = TRUE) %>%
  mutate(name = paste0(trait_name, "_", value_type)) %>%
  select(-trait_name, -value_type) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  rename(accepted_name = taxon_name)
           
# Remove leaf dimension values that are also in CPBR_2002, previous Euclid extraction.

data_new %>%
  left_join(Euclid_2002) %>%
  mutate(
    leaflength_min2 = ifelse(!is.na(leaf_length_minimum) & leaf_length_minimum == 10*leaflength_min, 
                            NA, leaflength_min),
    leaflength_max2 = ifelse(!is.na(leaf_length_minimum) & leaf_length_maximum == 10*leaflength_max, 
                            NA, leaflength_max),
    leafwidth_min2 = ifelse(!is.na(leaf_width_minimum) & leaf_width_minimum == 10*leafwidth_min, 
                            NA, leafwidth_min),
    leafwidth_max2 = ifelse(!is.na(leaf_width_maximum) & leaf_width_maximum == 10*leafwidth_max, 
                            NA, leafwidth_max),
    ) %>%
  write_csv("data/Stephens_2024/data.csv")


read_csv("data/Stephens_2024/data.csv") %>% 
  mutate(
    euclid_name = stringr::str_replace_all(euclid_name, "_", " "),
    euclid_name = stringr::str_to_sentence(euclid_name)
    ) %>%
  filter(euclid_name != accepted_name) %>% 
  select(find = euclid_name, replace = accepted_name) %>%
  mutate(
    reason = "Align EUCLID name with APC accepted name.", 
    taxonomic_resolution = ifelse(stringr::str_count(replace, " ") == 1, "species", "subspecies")
  ) -> replacements
