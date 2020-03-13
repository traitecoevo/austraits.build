read_csv("data/Ahrens_2019/raw/Ccalophylla_12pops.csv") %>%
  mutate(species = "Corymbia calophylla") %>%
  write_csv("data/Ahrens_2019/data.csv") %>%
  select(Pop, latitudes, longitude, TMAX, PMA, AI) %>%
  rename(`latitude (deg)` = latitudes, `longitude (deg)` = longitude, `max temp (C)`= TMAX, `aridity index`=AI, `MAP (mm)`=PMA) %>%
  group_by(Pop) %>%
  summarise_all(.funs = first) -> site_data