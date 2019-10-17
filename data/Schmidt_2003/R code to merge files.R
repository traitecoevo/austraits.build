read_csv("data/Schmidt_2003_v2/data_original.csv") %>%
  rename(species_name = `gen spp`, phenology = `deciduous/evergreen`, plant_growth_form = `growth form`, site_name = site_text,
         leaf_N_per_dry_mass = `leaf Nmass`, leaf_P_per_dry_mass =`leaf Pmass`, leaf_delta13C = `delta 13C`, leaf_delta15N = `15-N`) -> original_data

read_csv("data/Schmidt_2003_v2/data_raw.csv") %>%
  rename(life_history = LIFE, plant_growth_form = FORM, root_structure = MYC, Nfixer = NFIX, 
         leaf_water_content = H20CONT, xylem_nitrate = NO3_xylem, nitrate_leaf = NO3_leaf, leaf_N_per_dry_mass = `%N`,
         leaf_delta15N = DEL_N, leaf_delta13C = DEL_C, leaf_C_per_dry_mass = `%C`, site_name = SITE) %>%
  mutate(species_name = ifelse(is.na(Species_name_guess),SPECIES,Species_name_guess)) %>%
  #mutate(species_name = Species_name_guess) %>%
  #drop_na(Species_name_guess) %>%
  bind_rows(original_data) %>%
  mutate(duplicate_check = paste(species_name,site_name,leaf_delta13C,sep="_")) %>%
  group_by(duplicate_check) %>%
  summarise_each(.,first) %>%
  write_csv("data/Schmidt_2003_v2/data.csv") -> data_test

read_csv("data/Schmidt_2003_v2/site_data.csv") -> site_data
