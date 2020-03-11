read_csv("data/Henery_2001/raw/Henery_2001_expanded_data.csv") %>%
  rename('name_original' = 'Species',seed_mass_mg ='Seed mass (mg)') -> expanded

read_csv("data/Henery_2001/raw/Henery_2001_original_data.csv") %>%
  full_join(expanded,by=c("name_original")) %>% 
  mutate(seed_mass_mg = ifelse(is.na(seed_mass_mg),`seed mass (mg)`,seed_mass_mg)) %>%
  select(-'reserve seed mass (mg)',-'seed mass (mg)') %>%
  write_csv("data/Henery_2001/data.csv")