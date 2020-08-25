#This data set is a mix of data for four traits at the species and genus level. 
#For some genera, there are multiple rows of data with different trait values, but no way to know which members of the genus have which trait values
#Overall, Lizzy Wenk took the following steps to decipher the data and map the parts that were useful for AusTraits onto a list of all APC names:

#I went through the data file and separated data into four categories:
#1. Excluded non-native taxa
#2. Species with complete names, extracted from the submitted data set; need to have their taxonomy updated
#   Then these species names are "subtracted" off the complete list of APC names, leaving the remainder of the list as species that need to be
#   matched at the genus level.
#3. For genera where all four traits are conserved, match all known species within genus (from APC list) for all four trait values
#4. For genera where one or more traits aren't conserved, Lizzy looked through the genera and added columns flagging which traits are/aren't conserved
#   Traits that are "conserved" by all members of the genus can be mapped onto all species in the genus; 
#   Traits that have varible values within the genus are excluded, since we can't work out which species are attached to which trait values

#Explicit steps taken are detailed below.
# Only the raw data files are archived in the Github raw data folder associated with the study. Other intermediary files are archived in the GoogleDrive
#folder associated with the study



#steps to take
#first need to input complete species names list and align with new names - see above for creating data.csv file
#next remake AusTraits and check taxa for Sjostrom_2006
#save AusTraits component for study

#Step 1: open list of names without any naming authorities; created manually in Excel
read.csv("data/Sjostrom_2006/raw/raw_names3.csv") -> clean_names

#Step 2: for raw data file, indicate for each row of data, which traits data should be used, based on which traits are conserved 
#across all members of a genus; all data for entries with complete species names is preserved
read_csv("data/Sjostrom_2006/raw/data_raw.csv") %>%
  full_join(clean_names, by = "sort") %>%
  mutate(habit_to_use = ifelse(category == "all_traits_conserved" | category == "traits_conserved" | category == "complete_species_name" | habit_conserved == "yes",habit,NA),
         lifespan_to_use = ifelse(category == "all_traits_conserved" | category == "traits_conserved" | category == "complete_species_name" | lifespan_conserved == "yes",lifespan,NA),
         sex_type_to_use = ifelse(category == "all_traits_conserved" | category == "traits_conserved" | category == "complete_species_name" | sex_type_conserved == "yes",`sex type`,NA),
         fruit_to_use = ifelse(category == "all_traits_conserved" | category == "traits_conserved" | category == "complete_species_name" | fruit_conserved == "yes",fruit,NA)) %>%
  mutate(Taxon_clean = gsub(" spp","",Taxon_clean)) %>%
  mutate(Taxon_clean = gsub(" sp","",Taxon_clean)) %>%
  mutate(Taxon_clean = gsub(" sp.","",Taxon_clean)) %>%
  write_csv("data/Sjostrom_2006/raw/data_able_to_match.csv") 

#Step 3: save subset of data with complete species names to run through AusTraits to find and correct unknown names
read_csv("data/Sjostrom_2006/raw/data_able_to_match.csv") %>%
  subset(category == "complete_species_name") %>%
  rename(taxon_name = Taxon_clean) %>%
  write_csv("data/Sjostrom_2006/data_x.csv")

#Step 4: run AusTraits and match all taxa
#(an archived version of the metadata file, names metadata_match_complete_names.yml, will be archived in the raw data folder)
#file name changed, so it isn't overwritten by mistake
austraits$traits %>%
  subset(dataset_id == "Sjostrom_2006") %>%
  write_csv("data/Sjostrom_2006/raw/austraits_complete_names_temp_x.csv") -> austraits_complete_names_temp
  
#Step 5: create list of APC accepted species from which the complete names are subtracted (in Excel)
#file also includes Orchidaceae that are in our taxon list and in APNI, but not APC
#extract a list of original raw data names and APC aligned names (after complete names run through AusTraits and taxon names checked)
read_csv("data/Sjostrom_2006/raw/austraits_complete_names_temp.csv")  %>%
  select(original_name, taxon_name) %>%
  distinct(taxon_name,.keep_all = TRUE) -> complete_species_names

#Step 6: take the subset of raw Sjostrom data file where species have complete names, and merge in the APC-aligned names
read_csv("data/Sjostrom_2006/raw/data_able_to_match.csv") %>%
  subset(category == "complete_species_name") %>%
  select(taxon_name, habit_to_use,lifespan_to_use,sex_type_to_use,fruit_to_use) %>% 
  rename(original_name = taxon_name) %>%
  left_join(complete_species_names, by= "original_name") -> complete_names_to_bind

#Step 7: take the spreadsheet containing all APC-accepted names (names designated as 'species') and for Orchidaceae, 
#all APNI names that are already in the AusTraits taxon_names files (arbitrary, but at least captures many)

read_csv("data/Sjostrom_2006/raw/APC_all_accepted_species.csv") -> all_APC_species

#Step 8: anti_join (i.e. subtract) off the APC list all names where Sjostrom_2006 reports data for a complete species name
#create an extra column in data file that just indicates genus name as wel

all_APC_species %>%
  rename(taxon_name = canonicalName) %>%
  anti_join(complete_species_names) %>% 
  mutate(genus = sub("[[:space:]].*", "",taxon_name)) -> APC_names_to_match_by_genus

#Step 9: from raw Sjostrom data, extract a list of genera that aren't entirely introduced or entirely represented by complete species names
#change file name written, once initially created, to not overwrite
read_csv("data/Sjostrom_2006/raw/data_able_to_match.csv") %>%
  subset(category != "introduced" & category !="complete_species_name") %>%
  mutate(genus = sub("[[:space:]].*", "",Taxon_clean)) %>%
  distinct(genus,.keep_all = TRUE) %>%
  mutate(genus_with_sp = paste(genus,"sp")) %>%
  write_csv("data/Sjostrom_2006/data.csv") -> Sjostrom_genera_to_match

#Step 10:, run the data.csv with just genus names through AusTraits to figure out which genus names need updating
#then extract names from austraits$traits
#name changed after run to not overwrite original
austraits$traits %>%
  subset(dataset_id == "Sjostrom_2006") %>%
  write_csv("data/Sjostrom_2006/raw/austraits_genus_names_temp_y.csv")

#Step 11: create a list of all genus names
read_csv("data/Sjostrom_2006/raw/austraits_genus_names_temp.csv") %>%
  select(original_name, taxon_name) %>%
  mutate(genus_APC = gsub(" sp.","",taxon_name)) %>%
  mutate(genus_APC = gsub(" sp","",genus_APC)) %>%
  mutate(genus = gsub(" sp.","",original_name)) %>%
  mutate(genus = gsub(" sp","",genus)) %>%
  distinct(genus_APC,.keep_all = TRUE) -> complete_genus_names

#Step 12: merge updated genus names into raw Sjostrom file, allowing new APC species names to be matched to old genus names
#for genera that have been split, this process only captures one genus name and is going to lead to the exclusion of a small number of species
#there were 2-3 genera where this appeared to be an issue, and in each case the genus name that included the most species was used
Sjostrom_genera_to_match %>%
  full_join(complete_genus_names, by = "genus") %>%
  rename(genus_Sjostrom = genus) %>%
  select(genus_APC,genus_Sjostrom, habit_to_use,lifespan_to_use,sex_type_to_use,fruit_to_use) %>%
  write_csv("data/Sjostrom_2006/raw/Sjostrom_genera_to_match_to_edit_x.csv")

#Step 13: opened in excel to figure out why some genera not given an APC name; no clear pattern and 
#manually added based on substitutions list in metadata file; all cases were genera with names in substitutions, 
#but not all genus names in substitutions were missing
read_csv("data/Sjostrom_2006/raw/Sjostrom_genera_to_match_to_edit.csv") %>%
  distinct(genus_APC, .keep_all = TRUE) -> Sjostrom_genus_level_data

#Step 14: merge genus-level data to APC names (that aren't under complete names)
# and bind on the data with complete names
APC_names_to_match_by_genus %>%
  rename(genus_APC = genus) %>%
  inner_join(Sjostrom_genus_level_data,by="genus_APC") %>% 
  bind_rows(complete_names_to_bind) %>%
  subset(!is.na(taxon_name)) %>%
  distinct(taxon_name,.keep_all = TRUE) %>% 
  write_csv("data/Sjostrom_2006/data.csv")

#future additions
#for many genera, some of the four traits are conserved across the genus (and then included) while other traits have different values within the genus
#for the latter, the Sjostrom_2006 raw data file just records the number of species in the genus with each trait value
#but not which species have which trait value
#therefore these traits have been excluded for all members of the genus (excluding those with complete species names given)

