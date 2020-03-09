read_csv("data/Niinemets_2009/raw/species_name_match.csv") -> name_match
read_csv("data/Niinemets_2009/raw/site_match.csv") -> site_match

read_csv("data/Niinemets_2009/raw/chlorophyll.csv") %>%
  select(-sppsite, -Spp, -site2, -corr, -site) %>%
  subset(!is.na(Species)) -> chlorophyll

read_csv("data/Niinemets_2009/raw/datafile-ian (sept09) (for Vincent M Jan 2013).csv") %>%
  select(site,Species,Thickness, Density) %>%
  rename(site_name = site) -> extra

read_csv("data/Niinemets_2009/raw/Reflectance_data.csv") %>%
  full_join(name_match, by=c("spp")) %>%
  full_join(site_match,by=c("climate")) %>%
  mutate(transmission = 1 - `abs400_700` - `R400-700`) %>%
  subset(!is.na(Species)) -> reflectance

read_csv("data/Niinemets_2009/raw/original_data.csv") %>%
  select(-spp,-`N%`,-`P%`) %>%
  subset(!is.na(Species)) -> original

read_csv("data/Niinemets_2009/raw/Averages-final (ulo Nov13) (for Vincent M Jan2013).csv") %>%
  rename(ID = `Leaf no`) %>%
  select(-FR, -"Measured Ci-Cc -less reliable!",-"Cc-modelled_1" ) %>%
  subset(!is.na(Species)) %>%
  full_join(original, by = c("ID","Species")) %>%
  full_join(chlorophyll, by = c("ID","Species")) %>%
  bind_rows(extra) %>%
  bind_rows(reflectance) %>%
  mutate(site_name = ifelse(is.na(site_name), site, site_name)) %>%
  mutate(site_name = gsub("MUforest","MU_forest", site_name),
         site_name = gsub("MUcampus","MU_campus", site_name),
         site_name = gsub("MUgarden","MU_campus", site_name),
         site_name = gsub("hiRhiP","WestHead", site_name),
         site_name = gsub("hiRloP","Murrua", site_name),
         site_name = gsub("loRhiP","Castlereagh", site_name),
         site_name = gsub("Castlereagh1","Castlereagh", site_name),
         site_name = gsub("Castlereagh2","Castlereagh", site_name),
         site_name = gsub("loRloP","AgnesBanks", site_name),
         site_name = gsub("SydhiP","WestHead", site_name),
         site_name = gsub("SydhiP","WestHead", site_name),
         site_name = gsub("SydloP","Murrua",site_name),
         site_name = gsub("KCNP_west","Murrua",site_name),
         site_name = gsub("KCNP_east","KCNP_Basin_Track",site_name),
         site_name = gsub("Murrua","KCNP_Murrua_Track",site_name)) %>%
  mutate(site_name = if_else(site_name=="KCNP","KCNP_unknown_track",site_name)) %>%
  write_csv("data/Niinemets_2009/data.csv") %>%
  write_csv("data/Wright_2009/data.csv") -> check
