read_csv("data/Osbourne_2014/raw/Panicum_to_merge.csv") -> panicum

read_csv("data/Osbourne_2014/raw/Grass_genera_to_merge.csv") %>%
  mutate(genus_name = str_replace_all(`Genus Authority`, "[[:punct:]]", "")) %>%
  mutate(genus_name = gsub(' [A-z ]*', "", genus_name)) %>%
  write_csv("data/Osbourne_2014/raw/grass_genera_tweaked.csv")

#manually opened in excel to remove one special character; DO NOT RERUN PRECEEDING LINES
read_csv("data/Osbourne_2014/raw/grass_genera_tweaked.csv") -> grass_genera

read_csv("data/Osbourne_2014/data_old.csv") %>%
  rename(species_name = species_name_old_synonyms) %>%
  left_join(panicum,by="species_name") %>%
  mutate(`photosynthetic pathway`= ifelse(is.na(`photosynthetic pathway`),Pathway,`photosynthetic pathway`)) %>%
  mutate(genus_name = str_replace_all(`species_name`, "[[:punct:]]", "")) %>%
  mutate(genus_name = gsub(' [A-z ]*', "",genus_name)) %>%
  left_join(grass_genera,by="genus_name") %>%
  mutate(`photosynthetic pathway`= ifelse(is.na(`photosynthetic pathway`),`Photosynthetic types`,`photosynthetic pathway`)) %>%
  mutate(`Authority`= ifelse(is.na(`Authority`),`Genus Authority`,`Authority`)) %>%  
  mutate(`Reference`= ifelse(is.na(`Reference`),`References`,`Reference`)) %>% 
  select(-Pathway,-`Photosynthetic types`,-`Genus Authority`,-`References`) %>%
  write_csv("data/Osbourne_2014/data.csv")