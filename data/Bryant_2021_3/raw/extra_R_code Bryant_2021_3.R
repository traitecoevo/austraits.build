locations <- read_csv("data/Bryant_2021_3/raw/Bryant_2021_3_CB.csv") %>% 
  select(original_name, location_name , `latitude (deg)`, `longitude (deg)`, plot_context_id) %>% 
  distinct() %>%
  filter(!is.na(location_name)) %>%
  mutate(location_name = paste0(location_name, " ", row_number()))

contexts <- (austraits %>% extract_dataset("Bryant_2021_3") %>% join_context_properties(format = "many_columns", include_description = FALSE))$traits %>%
  select(original_name, plot_context_id, `plot_context: salinity`, `plot_context: salinity (ppt)`) %>%
  distinct()

locations2 <- locations %>%
  left_join(contexts) %>%
  select(-plot_context_id) %>%
  rename(Salinity = `plot_context: salinity`, ppt = `plot_context: salinity (ppt)`) %>%
  mutate(Salinity = stringr::str_to_title(Salinity)) %>%
  arrange(Salinity)

read_csv("data/Bryant_2021_3/data.csv") %>%
  left_join(locations2 %>% rename(Species = original_name) %>% mutate(ppt = as.numeric(ppt))) %>% View()
