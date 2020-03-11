read_csv("data/Gray_2019/data.csv") %>%
  select(AW_NEWSiteName,Latitude,Longitude) %>%
  group_by(AW_NEWSiteName) %>%
  distinct() %>%
  rename(`latitude (deg)` = Latitude, `longitude (deg)`=Longitude) -> site_data
