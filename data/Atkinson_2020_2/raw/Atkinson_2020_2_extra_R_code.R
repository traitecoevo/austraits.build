read_csv("data/Atkinson_2020_2/raw/sla-wopr.csv") %>%  
  select(-location_lat,-location_long) %>% 
  mutate(site = paste("site_at_",-`latitude (deg)`,"_deg_S_and_",`longitude (deg)`,"_deg_E")) %>%
  write_csv("data/Atkinson_2020_2/data.csv") %>%
  select(site,`latitude (deg)`,`longitude (deg)`) %>%
  distinct(site,.keep_all=TRUE) -> sites