read_csv("data/Mokany_2008/raw/IndividualPlantBiomassTraits_2020-04-16.csv") %>%
  mutate(leaf_mass_fraction = ifelse(`Non-green shoot biomass (g)`== 0, NA, 
                                      (`Green shoot biomass (g)` / (`Total shoot biomass (g)` + `Root Biomass (g) 0-10cm depth`))),
         leaf_mass_to_stem_mass = ifelse(`Non-green shoot biomass (g)`== 0, NA,
                                         `Green shoot biomass (g)` / `Total shoot biomass (g)`)) -> plant_biomass

read_csv("data/Mokany_2008/raw/IndividualPlantTraits_2020-04-15.csv") %>%
  rename(Date_plant_traits = Date) -> plant_traits

plant_traits %>%
  select(Species, "Tap root (yes/no)")  %>%
  rename(tap_root = "Tap root (yes/no)") %>%
  mutate(tap_root2 = 0,
         tap_root2 = if_else(tap_root == "yes",1,0)) %>%
  select(Species,tap_root2) %>%
  group_by(Species) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  mutate(tap_root2 = if_else(tap_root2 < 1 & tap_root2 > 0,2,tap_root2)) -> tap_root

read_csv("data/Mokany_2008/raw/data_with_site_means.csv") %>%
  select(1:4,20:22, 24) %>%
  full_join(tap_root,by="Species") %>%
  mutate(`Specific Root Area (SRA) (m2.kg-1)` = ifelse(Species %in% c("Aira elegantissima","Aristida ramosa","Austrodanthonia caespitosa","Austrodanthonia carphoides",
                                                               "Bothriochloa macra","Hypochaeris radicata","Joycea pallida","Lomandra filiformis",
                                                               "Plantago varia","Poa labillardieri","Themeda australis","Trifolium dubium", 
                                                               "Trifolium subterraneum","Vulpia bromoides"),
                                                        NA,`Specific Root Area (SRA) (m2.kg-1)`),
         `Root Dry Matter Content (RDMC)` = ifelse(Species %in% c("Aira elegantissima","Aristida ramosa","Austrodanthonia caespitosa","Austrodanthonia carphoides",
                                                                      "Bothriochloa macra","Hypochaeris radicata","Joycea pallida","Lomandra filiformis",
                                                                      "Plantago varia","Poa labillardieri","Themeda australis","Trifolium dubium", 
                                                                      "Trifolium subterraneum","Vulpia bromoides"),
                                                       NA,`Root Dry Matter Content (RDMC)`))-> species_level_traits

read_csv("data/Mokany_2008/raw/IndividualRootTraits_2020-04-15.csv") -> root_traits

read_csv("data/Mokany_2008/raw/IndividualLeafTraits_2020-04-15.csv") %>%
  rename(Date_left_traits = Date) %>%
  full_join(plant_traits,by=c("Species","Replicate")) %>%
  full_join(root_traits,by=c("Species","Replicate")) %>% 
  full_join(plant_biomass,by=c("Species","Replicate")) %>%
  bind_rows(species_level_traits) %>%
  write_csv("data/Mokany_2008/data.csv") -> combined
