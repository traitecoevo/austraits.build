read_csv("data/Roderick_2002/raw/Roderick_2002_Australia_component.csv") %>%
  mutate(site_name_complete = paste(AusTraits_key,site_name,sep=" ")) %>%
  write_csv("data/Roderick_2002/data.csv") %>%
  distinct(site_name_complete, .keep_all=TRUE) %>%
  select(site_name_complete, AusTraits_key, Long, Lat, site_name,`soil type`, `lat/lon source`, veg_stratum,`MAP (mm)`, `elevation (m)`,description) %>%
  rename(`latitude (deg)` = Lat, `longitude (deg)` = Long) %>%
  write_csv("data/Roderick_2002/raw/Roderick_2002_sites.csv") -> sites