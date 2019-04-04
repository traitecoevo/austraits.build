read_csv("data/Dwyer_2017/raw/quadrat.scale.data.csv") %>%
  select(site, site.lat, site.long, tot.n, p, k, pH.H2O) %>%
  mutate(site_name = paste0("site_",site)) %>% 
  group_by(site_name) %>% 
  summarise(`latitude (deg)` = mean(site.lat),
            `longitude (deg)`= mean(site.long),
            `total soil N (%)`= mean(tot.n),
            `soil P`= mean(p),
            `soil K`= mean(k),
            `soil pH`= mean(pH.H2O)
  ) -> sites

metadata_add_sites("Dwyer_2017", sites)



