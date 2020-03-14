read_csv("data/Baker_2019/raw/export_AUSTRAITS_Baker_120919.csv") %>%
  mutate(site_name = paste("site_at",`latitude (deg)`,"and",`longitude (deg)`,sep="_")) %>%
  write_csv("data/Baker_2019/data.csv") %>%
  distinct(site_name,.keep_all=TRUE) %>%
  select(site_name,`latitude (deg)`,`longitude (deg)`) %>%
  write_csv("data/Baker_2019/raw/site_names_Baker.csv") -> site_names