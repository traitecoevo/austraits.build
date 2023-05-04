
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

# Fix typos and remove duplicate row (Pterostylis roensis) and empty column
data <- 
  data %>% 
  mutate(Pollination = str_replace_all(Pollination, "inesct", "insect"),
         `Seed dispersal` = str_replace_all(`Seed dispersal`, "Unasissted", "Unassisted")) %>%
  distinct(Species, .keep_all = TRUE) %>% 
  select(-14)

# Extract pollination trait values
patterns_pollination <- 
  c("(?i)plant physical contact", "(?i)insect", "(?i)wind", "(?i)buzz", "(?i)bee", 
    "(?i)self", "(?i)bird", "(?i)mammal", "(?i)water", "(?i)ballistic")

data %>% 
  rowwise() %>% 
  mutate(pollination_extracted = list(str_extract(Pollination, patterns_pollination))) %>% 
  unnest(pollination_extracted) %>% 
  drop_na(pollination_extracted) %>% 
  group_by(Species) %>% 
  mutate(pollination_new = paste(pollination_extracted, collapse = " ")) %>% 
  relocate(pollination_new, .after = Pollination) %>% 
  mutate(
    pollination_new = tolower(pollination_new),
    pollination_new = str_replace_all(pollination_new, "buzz", "bee"),
    pollination_new = str_replace_all(pollination_new, "plant physical contact", "abiotic")
  ) %>% 
  distinct(Species, .keep_all = TRUE) -> data_pollination

patterns_dispersal <-
  c("(?i)passive", "(?i)ingest", "(?i)moisture", "(?i)water", "(?i)adhesion", "(?i)biotic",
    "(?i)abiotic", "(?i)attach", "(?i)wind", "(?i)gravity", "(?i)unassisted", "(?i)ants", 
    "(?i)livestock", "(?i)bird", "(?i)ballistic", "(?i)animal", "(?i)fish", "(?i)vivipary", 
    "(?i)gut", "(?i)brush")

data_pollination %>% 
  rowwise() %>% 
  mutate(seed_dispersal_extracted = list(str_extract(`Seed dispersal`, patterns_dispersal))) %>% 
  unnest(seed_dispersal_extracted) %>% 
  drop_na(seed_dispersal_extracted) %>% 
  group_by(Species) %>% 
  mutate(seed_dispersal_new = paste(seed_dispersal_extracted, collapse = " ")) %>% 
  relocate(seed_dispersal_new, .after = `Seed dispersal`) %>% 
  mutate(
    seed_dispersal_new = tolower(seed_dispersal_new)
  ) %>% 
  distinct(Species, .keep_all = TRUE) %>% 
  select(-pollination_extracted, -seed_dispersal_extracted) -> data_full


# Join max DBH and height to trait data


