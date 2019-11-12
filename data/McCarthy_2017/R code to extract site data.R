read_csv("data/McCarthy_2017/raw/data_JKMupdate_20191017.csv") %>%
  rename(`trip identifier`= Site,`latitude (deg)` = Latitude,`longitude (deg)`= Longitude) %>%
  mutate(site_name = ifelse(is.na(`latitude (deg)`),`trip identifier`,
                            paste("site_at",`latitude (deg)`,"degS",`longitude (deg)`,"degE",sep="_"))) %>%
  write_csv("data/McCarthy_2017/data.csv") %>%
  distinct(site_name, .keep_all = TRUE) -> sites
