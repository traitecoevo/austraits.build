# Need to work out what data already in AusTraits as part of dataset_id "ANBG_2019"

(austraits %>% extract_dataset("ANBG_2019"))$traits %>% 
  select(`Scientific Name` = taxon_name, trait_name, value) %>% 
  filter(trait_name %in% c("seed_dry_mass", "seed_viability", "seed_germination")) %>%
  mutate(
    ANBG_2019 = "in_ANBG_2019",
    `Thousand Seed Weight` = ifelse(trait_name == "seed_dry_mass", as.numeric(value), NA),
    Viability = ifelse(trait_name == "seed_viability", as.numeric(value), NA),
    Adjusted_Germination_Calculated = ifelse(trait_name  == "seed_germination", as.numeric(value), NA)
  ) -> ANBG_2019

data_2025 <- read_csv("data/ANBG_2025/data.csv") %>%
  mutate(Adjusted_Germination_Calculated = 100*(`Number Germinated`/(`Number Germinated` + `Number Full`))) %>%
  left_join(ANBG_2019 %>% select(`Scientific Name`, trait_name, `Thousand Seed Weight`, ANBG_2019)) %>%
  select(1:5, `Thousand Seed Weight`, `Collection Date`, trait_name, ANBG_2019) %>% 
  filter(!is.na(`Thousand Seed Weight`)) %>% View()

  
  
(threatened_species %>% extract_dataset("ANBG_2025"))$traits -> ANBG_2025

ANBG_2025_2 %>% full_join(ANBG_2019_2 %>% filter(trait_name %in% traits_to_check$trait_name)) %>% View()

ANBG_2025$traits %>% View()
View(ANBG_2025)
