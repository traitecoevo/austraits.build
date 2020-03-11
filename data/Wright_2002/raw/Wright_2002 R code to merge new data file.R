read_csv("data/Wright_2002/raw/data_old.csv") %>%
  rename(site_data_old = "site text", species_name = "name_original", max_height_m = "TRAIT: maximum height UNITS: m",
         SLA_cm2_g = "TRAIT: SLA UNITS: cm2/g", leaf_area_mm2 = "TRAIT: area leaf UNITS: mm2", 
         leaf_N_mass = "TRAIT: leaf N content UNITS: %", leaf_P_mass = "TRAIT: leaf P content UNITS: %",
         leaf_N_area_g_m2 = "TRAIT: leaf N content area UNITS: g/m2", 
         leaf_P_area_g_m2 = "TRAIT: leaf P content area UNITS: g/m2") ->  data_original

read_csv("data/Wright_2002/raw/Wright expanded traits.csv") %>%
  rename(site_name = "SITE", species_abb = "SPP", SLA_cm2_g_new = "SLAian", leaf_area_mm2_new = "LFSIZian", 
         leaf_N_mass_new = "Nmass", leaf_P_mass_new = "Pmass",  leaf_N_area_g_m2_new = "Narea", leaf_P_area_g_m2_new = "Parea") -> data_new

#confirming all values match, cycling through variables and sorting by diff to ensure no offset
data_new %>%
  select(species_name,site_data_old, leaf_P_area_g_m2_new) %>%
  full_join(data_original,by=c("species_name","site_data_old")) %>% 
  mutate(diff = leaf_P_area_g_m2_new - leaf_P_area_g_m2) %>%
  select(species_name, site_data_old, diff, leaf_P_area_g_m2_new, leaf_P_area_g_m2) %>% View()
         
#confirming all values are correct across the two datasets and then merging in height data (only in old file)
#and replacing data.csv with expanded dataset
data_original %>%
  select(species_name, site_data_old, max_height_m) -> old_data_to_keep

data_new %>%
  full_join(old_data_to_keep,by=c("species_name","site_data_old")) %>%
  write_csv("data/Wright_2002/data.csv")