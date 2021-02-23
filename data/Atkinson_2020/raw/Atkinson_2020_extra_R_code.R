read_csv("data/Atkinson_2020/raw/pult_juni_ind_avgs_csv.csv") %>% 
  select(-latitude,-longitude) %>% 
  mutate(site = paste("site_at_",-`latitude (deg)`,"_deg_S_and_",`longitude (deg)`,"_deg_E")) %>%
  write_csv("data/Atkinson_2020/data.csv") %>%
  select(site,`latitude (deg)`,`longitude (deg)`,region,population) %>%
  distinct(site,.keep_all=TRUE) -> sites

#Questions for author
#I've included both region and population are values within site, because I wasn't sure if the population number was linked 
#elsewhere. Let me know if I should just include region.
#Study years