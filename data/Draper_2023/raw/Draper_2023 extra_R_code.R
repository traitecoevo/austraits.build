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

data <- read_csv("data/Draper_2023/raw/Draper_2023_trait_data.csv")

# Fix typos and remove duplicate row (Pterostylis roensis)
data <- 
  data %>% 
  mutate(Pollination = str_replace_all(Pollination, "inesct", "insect")) %>%
  distinct(Species, .keep_all = TRUE)

data %>% 
  mutate(pollination_extracted = "") %>% 
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)insect") ~ list(append(pollination_extracted, "insect")))) %>% 
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)wind") ~ list(append(pollination_extracted, "wind")))) %>% 
  rowwise() %>%  
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)buzz") ~ list(append(pollination_extracted, "bee")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)bee") ~ list(append(pollination_extracted, "bee")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)water") ~ list(append(pollination_extracted, "water")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)plant physical contact") ~ list(append(pollination_extracted, "abiotic")))) %>%
  rowwise() %>%   
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)ballistic") ~ list(append(pollination_extracted, "ballistic")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)self") ~ list(append(pollination_extracted, "self")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)bird") ~ list(append(pollination_extracted, "self")))) %>%  
  rowwise() %>% 
  mutate(
    pollination_extracted =
      case_when(str_detect(Pollination, "(?i)mammal") ~ list(append(pollination_extracted, "mammal")))) %>% 
  rowwise() %>% 
  mutate(
    pollination_new = paste(pollination_extracted, collapse = " ")) -> test
  
  
  
        str_detect(Pollination, "(?i)wind") ~ list(append(pollination_new, "wind")),
        str_detect(Pollination, "(?i)buzz") ~ list(append(pollination_new, "bee")),
        str_detect(Pollination, "(?i)bee") ~ list(append(pollination_new, "bee")),
        str_detect(Pollination, "(?i)self") ~ list(append(pollination_new, "self")),
        str_detect(Pollination, "(?i)bird") ~ list(append(pollination_new, "bird")),
        str_detect(Pollination, "(?i)mammal") ~ list(append(pollination_new, "mammal")),
        str_detect(Pollination, "(?i)water") ~ list(append(pollination_new, "water")),
        str_detect(Pollination, "(?i)plant physical contact") ~ list(append(pollination_new, "abiotic")),
        str_detect(Pollination, "(?i)ballistic") ~ list(append(pollination_new, "ballistic")),
        )) %>% 
  mutate(
    pollination_new = paste(pollination_extracted, collapse = " ")) -> test

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
  mutate(pollination_new = tolower(pollination_new),
         pollination_new = str_replace_all(pollination_new, "buzz", "bee"),
         pollination_new = str_replace_all(pollination_new, "plant physical contact", "abiotic")) -> test_3


data %>% 
  mutate(pollination_new = "") %>% 
  rowwise() %>% 
  mutate(
    pollination_new =
      if_else(
        str_detect(Pollination, "(?i)insect"), append(pollination_new, "insect"), "")) -> test

read_csv("data/Draper_2023/raw/Draper_2023_trait_data.csv") %>%
  mutate(
    Pollination = stringr::str_replace(Pollination, "\\)",""),
    pollination_split = stringr::str_split(Pollination, "\\("),
    `Seed dispersal` = stringr::str_replace(`Seed dispersal`, "\\)",""),
    seed_dispersal_split = stringr::str_split(`Seed dispersal`, "\\(")
  ) -> test
  
test %>% 
  unnest(cols = pollination_split) %>% 
  group_by(Species) %>% 
  mutate(key = row_number()) -> test_2

test %>% 
  unnest(cols = pollination_split) %>% 
  group_by(Species) %>% 
  mutate(key = row_number()) %>% 
  spread(key, pollination_split) -> test_3
  