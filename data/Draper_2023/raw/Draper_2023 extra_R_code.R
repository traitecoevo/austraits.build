read_csv("data/Draper_2023/raw/allometric_traits_1.csv") %>%
  rename(`latitude (deg)` = Latitude, `longitude (deg)` = Longitude) %>%
  filter(Sex != "J") %>%
  group_by(Site, Sex) %>%
  mutate(
    `Height (cm)` = mean(`Height (cm)`),
    `Diameter (cm)` = mean(`Diameter (cm)`),
    `latitude (deg)` = mean(`latitude (deg)`),
    `longitude (deg)` = mean(`latitude (deg)`),
    Date = first(Date)
    ) %>% 
  ungroup() -> max_height_diameter


read_csv("data/Draper_2023/raw/Draper_2023_trait_data.csv") %>%
  mutate(
    Pollination = stringr::str_replace(Pollination, "\\)",""),
    pollination_split = stringr::str_split(Pollination, "\\("),
    `Seed dispersal` = stringr::str_replace(`Seed dispersal`, "\\)",""),
    seed_dispersal_split = stringr::str_split(`Seed dispersal`, "\\(")
  ) -> test
  
  
  
  test %>% select(pollination_split) %>% mutate(unnest_wider(pollination_split))