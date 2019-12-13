#code to first test if RBGK_2014 and Gallagher_2011_2 can be replaced by Kew_2019_2. 
#Turns out there are species in these studies that are not in Kew_2019_2
#then filter RBGK_2014 to keep those species that are unique to it.
#the one species that is in Gallagher_2011_2 but not in Kew_2019_2 is also in RBGK_2014, 
#so Gallahger_2011_2 can be deleted

#before running this, duplicated RBGK_2014 data. Original version in raw folder for future reference

#read in Kew data and create dataframe with a single entry per species
read_csv("data/Kew_2019_1/data.csv") %>%
  rename(species = taxa) %>%
  select(species,thousandseedweight) %>% 
  group_by(species) %>% 
  mutate(thousandseedweight = mean(thousandseedweight, na.rm=TRUE)) %>% 
  summarise_all(.funs = first) %>%
  ungroup()-> Kew

#remove subspecies from Kew
Kew %>%
  separate(col = species, into = c("genus","species_ep","subspecies"), sep=" ", remove = FALSE, extra = "merge") %>%
  unite(col="species", genus, species_ep, sep=" ") ->  Kew_no_subspecies

#write new data.csv file for RBGK_2014 that eliminates species also present in Kew_2019_1
read_csv("data/RBGK_2014/raw/data_original.csv") %>% 
  anti_join(Kew_no_subspecies,by="species") %>% 
  write_csv("data/RBGK_2014/data.csv")


#code for other tests previously done

read_csv("data/Kew_2019_1/data.csv") -> raw_2019
names(Kew)
plot(data=test,thousandseedweight~seed_mass,log="xy", 
     xlab="Seed mass, RBGK_2014",
     ylab = "Seed mass, Kew_2019_1")
abline(0,1, col="red")

read_csv("data/RBGK_2014/data.csv") %>%
  rename(species = name_original) %>%
  select(species,seed_mass) %>%
  group_by(species) %>% 
  mutate(seed_mass = mean(seed_mass, na.rm=TRUE)) %>% 
  ungroup() %>%
  left_join(Kew,by="species") -> RBGK_2014

read_csv("data/Gallagher_2011_2/data.csv") %>%
  rename(species = name_original) %>%
  select(species,raw_trait_value) %>%
  group_by(species) %>% 
  mutate(raw_trait_value = mean(raw_trait_value, na.rm=TRUE)) %>% 
  ungroup() %>%
  left_join(Kew,by="species") -> Gallagher_2011_2

subset(austraits$traits,dataset_id == "Kew_2019_1") -> Kew_new_Austraits
subset(austraits$traits,dataset_id == "RBGK_2014") -> RBGK_2014_Austraits
subset(austraits$traits,dataset_id == "Gallagher_2011_2") -> Gallagher_2011_2_Austraits

table(unique(Kew_new_Austraits$species_name) %in% unique(Gallagher_2011_2_Austraits$species_name))
table(unique(Gallagher_2011_2_Austraits$species_name) %in% unique(Kew_new_Austraits$species_name))

table(unique(Kew_new_Austraits$species_name) %in% unique(RBGK_2014_Austraits$species_name))
table(unique(RBGK_2014_Austraits$species_name) %in% unique(Kew_new_Austraits$species_name))

anti_join(Gallagher_2011_2_Austraits,Kew_new_Austraits, by="species_name") -> Gallagher_unique
anti_join(RBGK_2014_Austraits,Kew_new_Austraits, by="species_name") -> RBGK_2014_unique

View(RBGK_2014_unique)
