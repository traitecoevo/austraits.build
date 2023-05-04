
# Extract max height and max DBH from allometric traits
read_csv("data/Draper_2023/raw/allometric_traits_1.csv") %>%
  rename(`latitude (deg)` = Latitude, `longitude (deg)` = Longitude) %>%
  filter(Sex != "J") %>%
  group_by(Site, Sex) %>%
  mutate(
    `Height (cm)` = max(`Height (cm)`),
    `Diameter (cm)` = max(`Diameter (cm)`),
    `latitude (deg)` = mean(`latitude (deg)`),
    `longitude (deg)` = mean(`latitude (deg)`),
    Date = first(Date)
    ) %>% 
  ungroup() -> max_height_diameter


# Read trait data
data <- read_csv("data/Draper_2023/raw/Draper_2023_trait_data.csv")

# Fix typos and remove duplicate row (Pterostylis roensis)
data <- 
  data %>% 
  mutate(Pollination = str_replace_all(Pollination, "inesct", "insect")) %>%
  distinct(Species, .keep_all = TRUE)

# Extract pollination trait values
patterns <- 
  c("(?i)plant physical contact", "(?i)insect", "(?i)wind", "(?i)buzz", "(?i)bee", 
    "(?i)self", "(?i)bird", "(?i)mammal", "(?i)water", "(?i)ballistic")

data %>% 
  rowwise() %>% 
  mutate(pollination_extracted = list(str_extract(Pollination, patterns))) %>% 
  unnest(pollination_extracted) %>% 
  drop_na() %>% 
  group_by(across(c(-pollination_extracted))) %>% 
  summarise(pollination_new = paste(pollination_extracted, collapse = " ")) %>% 
  relocate(pollination_new, .after = Pollination) %>% 
  mutate(
    pollination_new = tolower(pollination_new),
    pollination_new = str_replace_all(pollination_new, "buzz", "bee"),
    pollination_new = str_replace_all(pollination_new, "plant physical contact", "abiotic")
    ) -> data_pollination
