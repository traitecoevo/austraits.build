read_csv("data/Moore_2006/data.csv") %>%
  select('site_tree_code', 'site_Longitude', 'site_Latitude', 'soil_TOC', 'soil_pH', 'soil_conductivity', 'soil_N', 'soil_P', 
         'soil_K', 'soil_Sand', 'soil_Silt', 'soil_Clay', 'site_elevation') %>%
  mutate(site_name = trunc(site_tree_code,0)) %>%
  select(-'site_tree_code') %>%
  mutate(site_name = paste0("site_",site_name)) %>%
  group_by(site_name) %>%
  distinct() %>%
  rename(`longitude (deg)` = `site_Longitude`,`latitude (deg)` = `site_Latitude`) %>%
  write_csv("data/Moore_2006/site_data.csv")

read_csv("data/Moore_2006/site_data.csv") -> Moore_sites

read_csv("data/Moore_2006/data.csv") %>%
  mutate(site_name = trunc(site_tree_code,0)) %>%
  mutate(site_name = paste0("site_",site_name)) %>%
  mutate(taxon = paste(Genus,species,sep=" ")) %>%
  write_csv("data/Moore_2006/data.csv")