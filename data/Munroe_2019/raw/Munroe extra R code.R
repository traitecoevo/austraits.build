read_csv("data/Munroe_2019/raw/AusTraits_lat_longs.csv") %>%
  mutate(Species = gsub("_"," ",Species)) -> Munroe_lat

for (i in 1:nrow(Munroe_lat)) {
  Munroe_lat$Species[[i]] <- str_to_sentence(Munroe_lat$Species[[i]])
}

read_csv("data/Munroe_2019/raw/Munroe_2019_update_9122019.csv") %>%
  rename(Species = Genus_Species) %>%
  mutate(Species = gsub("_"," ",Species)) %>%
  full_join(Munroe_lat,by="Species",ignore_case = TRUE) %>%
  mutate(site_name = paste("site_at_",round(latitude,3),"_deg_lat_",round(longitude,3),"_deg_long",sep="")) %>%
  rename(`latitude (deg)` = latitude, `longitude (deg)` = longitude) %>%
  mutate(site_name = gsub("site_at_NA_deg_lat_NA_deg_long","no site data",site_name)) %>%
  write_csv("data/Munroe_2019/data.csv") %>%
  group_by(site_name) %>%
  select(-Species) %>%
  summarise_all(.funs=first) %>%
  ungroup() -> site_data