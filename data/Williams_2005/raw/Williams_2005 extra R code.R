#in excel remove duplicates from Jurado, Ooi, because not listed literature sources
subset(austraits_deduped$excluded_data,dataset_id=="Williams_2021" & trait_name == "seed_mass" & error !="Missing value") %>%
  write_csv("data/Williams_2021/raw/seed_mass_duplicates_expanded.csv")

read_csv("data/Williams_2021/raw/seed_mass_duplicates_expanded.csv") %>%
  select(error,original_name) %>%
  rename(seed_mass_duplicate = error, Species = original_name) -> seed_mass_duplicates

read_csv("data/Williams_2021/data.csv") %>%
  full_join(seed_mass_duplicates,by="Species") %>%
  mutate(`Seed Weight (mg)_filtered` = ifelse(is.na(seed_mass_duplicate),`Seed Weight (mg)`,NA)) %>% 
  write_csv("data/Williams_2021/data.csv")