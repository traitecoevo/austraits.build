read_csv("data/Jordan_2014/data.csv") %>%
  mutate(site_name = paste0(`Location notes`," (",habitat,")")) %>% 
  write_csv("data/Jordan_2014/data.csv") %>%
  select(site_name, `Location notes`,`latitude (deg)`,`longitude (deg)`,`Altitude (m)`,`habitat`) %>%
  rename(description = habitat) %>%
  distinct(site_name, .keep_all = TRUE) -> sites

sites %>%
  write_csv("data/Jordan_2014/raw/sites.csv")