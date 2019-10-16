read_csv("data/McCarthy_2017/raw/data_JKMupdate_20191016.csv") %>%
  select(Site,Latitude,Longitude,`Coordinate uncertainty (m)`) %>%
  rename(`trip identifier`=Site,`latitude (deg)` = Latitude,`longitude (deg)`= Longitude) %>%
  mutate(site_name = ifelse(is.na(`latitude (deg)),`trip identifier`,
                            paste("site_at",`latitude (deg)`,"degS",`longitude (deg)`,"degE",sep="_"))) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  write_csv("data/McCarthy_2017/data.csv") -> sites

View(sites)
