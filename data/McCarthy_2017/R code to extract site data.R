read_csv("data/McCarthy_2017/raw/data_JKMupdate_update_with_sites.csv") %>%
  select(Site,Latitude,Longitude,`Coordinate uncertainty (m)`) %>%
  distinct(Latitude, Longitude, .keep_all = TRUE) %>%
  rename(`latitude (deg)` = Latitude,`longitude (deg)`= Longitude) %>%
  mutate(site_name = paste(Site,`latitude (deg)`,"deg_S",`longitude (deg)`,"deg_E",sep="_")) -> sites

