#testing to make sure all species from 4 sublists are in main data file

read_csv("data/McGlone_2015/raw/Tasmania_no_climbers.csv") -> no_climbers_Tassie

read_csv("data/McGlone_2015/raw/Tasmania_test_1.csv") -> Tassie_forest

read_csv("data/McGlone_2015/raw/Victoria_forest.csv") -> Vic_forest

read_csv("data/McGlone_2015/raw/Victoria_no_climbers.csv") -> no_climbers_Vic

read_csv("data/McGlone_2015/raw/all_species.csv") %>%
  full_join(Tassie_forest) %>%
  full_join(no_climbers_Tassie) %>% 
  full_join(Vic_forest) %>%
  full_join(no_climbers_Vic) %>% 
  write_csv("data/McGlone_2015/raw/species_by_sublist.csv") -> species_by_sublist

# they aren't; Zieria cytisoides, Cratistylis conocephala, 	Ozothamnus phoditotus, Eucalyptus verrucata, Phebalium bullatum 
# in Victoria_no_climbers  and/or Victoria_forest, but not in overall sheet
# extracted these species data from Victoria_no_climbers worksheet and saved as "missing_from_main_data_sheet.csv"

read_csv("data/McGlone_2015/raw/missing_from_main_data_sheet.csv") -> missing

read_csv("data/McGlone_2015/raw/main_data_sheet.csv") %>%
  mutate(`ht (m)` = as.numeric(`ht (m)`)) %>%
  bind_rows(missing) %>% 
  full_join(species_by_sublist) %>% 
  select(-all_species) %>% 
  mutate(in_Tasmania = ifelse(!is.na(Tasmania)|!is.na(Tassie_no_climbers),"Tasmania",""),
         in_Victoria = ifelse(!is.na(Victoria_forest)|!is.na(Victoria_no_climbers),"Victoria",""),
         sites = paste0(in_Tasmania,in_Victoria),
         sites = gsub("TasmaniaVictoria","Tasmania & Victoria",sites),
         sites = ifelse(sites=="","state not specified",sites)) %>% 
  write_csv("data/McGlone_2015/data.csv")