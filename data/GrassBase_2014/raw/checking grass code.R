read_csv("data/Osbourne_2014/data.csv") %>%
  subset(trait == "maximum leaf length") %>%
  select(species_name = Taxon,value = `trait value`) -> file_1
read_csv("data/GrassBase_2014/data.csv") %>%
  subset(trait == "maximum leaf length") %>%
  select(c("Taxon","trait value")) %>%
  full_join(file_1,by="Taxon") -> combined_file

combine_file %>%
  filter("trait value.x" != "trait value.y") -> unique_to_file_1

###code to create wide format
read_csv("data/Osbourne_2014/data.csv") %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) -> Osbourne_wide

read_csv("data/RBGSYD_2014/data.csv") %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) -> RBGSYD_wide

read_csv("data/AusGrass_2014/data.csv") %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) -> AusGrass_wide

read_csv("data/GrassBase_2014/data.csv") %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  mutate(i = seq_len(nrow(.))) %>%
  mutate(species_name = ifelse(i %in% c(137, 955), "Aristida sp. 2", species_name)) %>%
  select(-i) %>% 
  spread(trait,value) -> GrassBase_wide

#determined there was no overlap in species between Osbourne_2014 and Clayton_2014 (now GrassBase_2014)
#they present data on the same traits for different species these datasets share and they are completely unique species  
#Code for checking `maximum leaf width` but also checked all other traits 
#both data sets have and they are always different species

Osbourne_wide %>%
  select("species_name","maximum leaf width") %>%
  rename(species_name=species_name,Osbourne=`maximum leaf width`) -> check

GrassBase_wide %>%
  select("species_name","maximum leaf width") %>%
  rename(species_name=species_name,GrassBase=`maximum leaf width`) %>%
  full_join(check,by="species_name") %>%
  filter(GrassBase==Osbourne) -> overlap

#combining GrassBase_2014 and Osbourne_2014
Osbourne_wide %>%
  bind_rows(GrassBase_wide) -> combined_wide

#using combined GrassBase and Osbourne file
#determined there was also no overlap between RBGSYD_2014 and the above two studies
#there are some of the same species, but never data for the same trait
#RBGSYD_2014 only has growth habit data
combined_wide %>%
  select("species_name","growth habit") %>%
  rename(species_name=species_name,combined=`growth habit`)-> check

RBGSYD_wide %>%
  select("species_name","growth habit") %>%
  rename(species_name=species_name,RBGSYD=`growth habit`) %>%
  full_join(check,by="species_name") %>%
  filter(combined==RBGSYD)

#to visually check as well and confirmed all cells with value in RBGSYD have 'NA' in combined
RBGSYD_wide %>%
  select("species_name","growth habit") %>%
  rename(species_name=species_name,RBGSYD=`growth habit`) %>%
  left_join(combined_wide,by="species_name") 

#combining RBGSYD into combined; this duplicates species names as well
combined_wide %>%
  full_join(RBGSYD_wide,by="species_name") -> combined_wide

#so combining long versions
read_csv("data/GrassBase_2014/data.csv") %>%
  mutate(i = seq_len(nrow(.))) %>%
  mutate(Taxon = ifelse(i %in% c(137, 955), "Aristida sp. 2", Taxon)) %>%
  select(-i) %>% 
  bind_rows(read_csv("data/Osbourne_2014/data.csv")) %>%
  bind_rows(read_csv("data/RBGSYD_2014/data.csv")) %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) -> combined2_wide

#now checking all traits between combined2_wide and AusGrass
#something wrong here,because code suggests complete overlap, but not true when merging long file
combined2_wide %>%
  select("species_name","minimum seed length") %>%
  rename(species_name=species_name,combined=`minimum seed length`) -> check

AusGrass_wide %>%
  select("species_name","minimum seed length") %>%
  rename(species_name=species_name,AusGrass=`minimum seed length`) %>%
  full_join(check,by="species_name") %>%
  write_csv("leaf_test.csv")

#now trying by merging long files
#suggests completely unique data - if `NA` in combined not in Ausgrass and vice-versa
#and running to see what is "equal" gives an empty set
read_csv("data/GrassBase_2014/data.csv") %>%
  mutate(i = seq_len(nrow(.))) %>%
  mutate(Taxon = ifelse(i %in% c(137, 955), "Aristida sp. 2", Taxon)) %>%
  select(-i) %>% 
  bind_rows(read_csv("data/Osbourne_2014/data.csv")) %>%
  bind_rows(read_csv("data/RBGSYD_2014/data.csv")) %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) -> combined_long

read_csv("data/AusGrass_2014/data.csv") %>%
  select(species_name = Taxon,value_AusGrass = `trait value`, trait=trait) %>%
  full_join(combined_long,by=c("species_name","trait")) %>%
  filter("value_AusGrass" != "value") %>%
  write_csv("test_AusGrass.csv")

#now combining all long versions
#first a version with just leaf width, leaf length, height, seed mass data
read_csv("data/GrassBase_2014/data.csv") %>%
  mutate(i = seq_len(nrow(.))) %>%
  mutate(Taxon = ifelse(i %in% c(137, 955), "Aristida sp. 2", Taxon)) %>%
  select(-i) %>% 
  bind_rows(read_csv("data/Osbourne_2014/data.csv")) %>%
  bind_rows(read_csv("data/RBGSYD_2014/data.csv")) %>%
  bind_rows(read_csv("data/AusGrass_2014/data.csv")) %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) %>%
  select(-`photosynthetic pathway`,-`flowering time`,-`growth habit`,-`longevity`) %>%
  write_csv("data/GrassBase_2014/all_numeric_grass_data.csv")

#then a copy with just the categorical data
read_csv("data/GrassBase_2014/data.csv") %>%
  mutate(i = seq_len(nrow(.))) %>%
  mutate(Taxon = ifelse(i %in% c(137, 955), "Aristida sp. 2", Taxon)) %>%
  select(-i) %>% 
  bind_rows(read_csv("data/Osbourne_2014/data.csv")) %>%
  bind_rows(read_csv("data/RBGSYD_2014/data.csv")) %>%
  bind_rows(read_csv("data/AusGrass_2014/data.csv")) %>%
  select(species_name = Taxon,value = `trait value`, trait=trait) %>%
  spread(trait,value) %>%
  select(species_name,`photosynthetic pathway`,`flowering time`,`growth habit`,`longevity`) %>%
  write_csv("data/GrassBase_2014/all_categorical_grass_data.csv")