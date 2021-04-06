read_csv("data/Wells_2012/raw/individual_level_data.csv") %>%
  select(Site,Lat_decimal,Long_decimal) %>%
  distinct(Site,.keep_all = TRUE) %>%
  rename(`latitude (deg)` = Lat_decimal, `longitude (deg)` = Long_decimal) -> sites

read_csv("data/Wells_2012/data.csv", guess_max = 10000) %>%
  mutate(StrataBinary = gsub(1,"Strata_1",StrataBinary),
         StrataBinary = gsub(2,"Strata_2", StrataBinary)) %>% 
  write_csv("data/Wells_2012/data.csv")

subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value" & trait_name == "wood_density") %>%
  write_csv("data/Wells_2012/raw/wood_density_duplicates.csv")

subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value" & trait_name == "plant_height") %>%
  write_csv("data/Wells_2012/raw/plant_height_duplicates.csv")

#looking through this shows that all species in Zanne_2009 that are attributed to Wells_2009 are also in this Wells dataset 
#and therefore will now be removed from Zanne_2009. Note there are a number of species in Zanne_2009 (under Wells) that are genus indet. spp
#in each case these match with a phrase name in this dataset, presumably the accepted current identification of the "indet" species

read_csv("data/Zanne_2009/data.csv") %>%
  subset(secondary_ref == "Wells_2009") %>%
  select(name_original) %>%
  rename(Species = name_original) %>%
  distinct(Species) %>%
  mutate(Wells_in_Zanne = "Wells_in_Zanne") -> Wells_in_Zanne

read_csv("data/Wells_2012/data.csv") %>%
  distinct(Species) %>%
  mutate(Wells_2012_ind_level = "Wells_2012_ind_level") %>% 
  full_join(Wells_2012_species_level) %>%  
  left_join(Ilic_names) %>% 
  full_join(Wells_in_Zanne) %>% View()

#I filtered AusTraits to find whether the literature sourced data in Wells_2012 were already in AusTraits, but extracting all
#wood density values for the data points she flagged as literature sourced
#In Excel, I then compared values, once rounded to both 2 and 3 decimal places and compiled a list of data whose likely sources
#are already in AusTraits.
#These data were then filtered out.
read_csv("data/Wells_2012/data.csv") %>%
  subset(Site == "Lit") %>%
  select(Species,StemDensity) %>%
  rename(taxon_name = Species) %>%
  mutate(study = "Wells_2012") ->  Wells_2012_lit_data

austraits$traits %>%
  subset(trait_name == "wood_density") %>%
  right_join(Wells_2012_lit_data) %>% 
  write_csv("data/Wells_2012/raw/possible_wood_density_duplicates.csv")
  
read_csv("data/Wells_2012/raw/filtered_duplicates.csv") %>%
  distinct(taxon_name,StemDensity, .keep_all = TRUE) %>%
  select(taxon_name,StemDensity, study_duplicated) %>%
  rename(Species = taxon_name) -> Wells_wood_density_to_filter

read_csv("data/Wells_2012/data.csv") %>%
  full_join(Wells_wood_density_to_filter) %>% 
  mutate(StemDensity_to_use = ifelse(is.na(study_duplicated),StemDensity,NA)) %>% 
  write_csv("data/Wells_2012/data.csv")

#Wells individual level and species level data should be on the same species, but some names don't match
#all match in APC and names adjusted to those in Wells' species level data which are more current names 

read_csv("data/Wells_2012_2/data.csv") %>%
  distinct(Species) %>%
  mutate(Wells_2012_species_level = "Wells_2012_species_level") -> Wells_2012_species_level

read_csv("data/Wells_2012/data.csv") %>%
  distinct(Species) %>%
  mutate(Wells_2012_individual_level = "Wells_2012_individual_level") %>%
  full_join(Wells_2012_species_level) %>%
  write_csv("data/Wells_2012/raw/species_names_need_matching.csv")

#to merge in species level data, some species names needed to be aligned
read_csv("data/Wells_2012/raw/data_raw_ind_level.csv", guess_max = 10000) %>%
  mutate(Species = gsub("Aglaia tomentosa","Aglaia ferruginea",Species),
         Species = gsub("Erythroxylum ecarinatum","Erythroxylum sp. Brewer LA (B.Hyland 13373)",Species),
         Species = gsub("Haplostichanthus sp. (Johnstone River L.W.Jessup+ 471)","Haplostichanthus rufescens",Species),
         Species = gsub("Lantana camara var. camara","Lantana camara",Species),
         Species = gsub("Melodorum sp. (Topaz G.Sankowsky+ 244)","Melodorum topazensis",Species),
         Species = gsub("Piper novae-hollandiae","Piper hederaceum var. hederaceum",Species),
         Species = gsub("Pouteria myrsinodendron","Planchonella myrsinodendron",Species),
         Species = gsub("Psidium cattleianum var. cattleianum","Psidium cattleyanum",Species),
         Species = gsub("Tetrasynandra laxiflora","Steganthera laxiflora ssp. laxiflora",Species),
         Species = gsub("Wilkiea sp. (Barong L.W.Jessup 719)","Wilkiea cordata",Species)
         ) %>%
  write_csv("data/Wells_2012/raw/data_raw_ind_level.csv")

read_csv("data/Wells_2012_2/data.csv") %>%
  distinct(Species) %>%
  mutate(Wells_2012_species_level = "Wells_2012_species_level") -> Wells_2012_species_level

#double checking no unmatched species remain
read_csv("data/Wells_2012/raw/data_raw_ind_level.csv") %>%
  distinct(Species) %>%
  mutate(Wells_2012_individual_level = "Wells_2012_individual_level") %>%
  anti_join(Wells_2012_species_level) %>% View()
	
#merging together species level and individual level data
read_csv("data/Wells_2012/raw/species_level_data_to_merge.csv") -> species_level_data_to_merge

read_csv("data/Wells_2012/raw/data_raw_ind_level.csv") %>%
  full_join(species_level_data_to_merge) %>% 
  write_csv("data/Wells_2012/data.csv")

#seed mass duplicates

austraits_deduped$excluded_data %>%
  subset(dataset_id==current_study & error !="Missing value") %>%
  subset(trait_name == "seed_mass") %>% 
  write_csv("data/Wells_2012/raw/initial_seed_mass_duplicates.csv")

austraits$traits %>%
  subset(dataset_id == "Wells_2012" & trait_name == "seed_mass") %>%
  distinct(taxon_name) -> Wells_seed_mass_species
 
read_csv("data/Wells_2012/raw/initial_seed_mass_duplicates.csv") %>% 
  select(error,taxon_name,value, original_name) %>%
  rename(Species = original_name, SeedDM = value, seed_mass_duplicate = error)-> duplicate_seed_mass

read_csv("data/Wells_2012/raw/seed_mass_test.csv") %>%
  left_join(duplicate_seed_mass) %>% 
  mutate(seed_mass_to_use = ifelse(is.na(seed_mass_duplicate),seed_mass_to_use,NA)) %>%
  write_csv("data/Wells_2012/raw/seed_mass_test.csv")

austraits$traits %>%
  subset(dataset_id == "Wells_2012" & trait_name == "seed_mass") %>%
  mutate(seed_mass_Wells = as.numeric(value)) %>%
  select(taxon_name, seed_mass_Wells) -> Wells_values

austraits$traits %>%
  subset(trait_name == "seed_mass") %>%
  mutate(value = as.numeric(value)) %>%
  right_join(Wells_values) %>% 
  write_csv("data/Wells_2012/raw/more_possible_seed_mass_duplicates.csv")
