
# Extract max height and max DBH from allometric traits
read_csv("data/Draper_2023/raw/allometric_traits_1.csv") %>%
  rename(`latitude (deg)` = Latitude, `longitude (deg)` = Longitude) %>%
  filter(!Sex %in% c("J", "I")) %>%
  group_by(Site) %>%
  mutate(
    `latitude (deg)` = first(`latitude (deg)`),
    `longitude (deg)` = first(`longitude (deg)`),
    Date = first(Date)
    ) %>% 
  ungroup() %>% 
  group_by(Site, Sex) %>% 
  mutate(
    `Height (cm)` = max(`Height (cm)`),
    `Diameter (cm)` = max(`Diameter (cm)`),
    replicates = n()
  ) %>% 
  ungroup() %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(Site = if_else(Site == "Baron'", "Baron", Site)) -> max_height_diameter


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
    "(?i)livestock", "(?i)bird", "(?i)ballistic", "animals \\(ref for genus\\)", "(?i)fish", "(?i)vivipary", 
    "(?i)gut", "(?i)brush", "(?i)passing")

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

data_full %>% 
  mutate(dispersal_syndrome = seed_dispersal_new,
         dispersers = seed_dispersal_new) %>% 
  mutate(dispersal_syndrome = str_replace_all(dispersal_syndrome, "passive", "barochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "ingest", "endozoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "moisture", "barochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "water", "hydrochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "adhesion", "epizoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "(?<!a)biotic", "zoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "abiotic", ""),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "attach", "epizoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "wind", "anemochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "gravity", "barochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "unassisted", "barochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "ants", "myrmecochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "livestock", "zoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "bird", "zoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "animals \\(ref for genus\\)", "zoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "fish", "zoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "vivipary", ""),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "gut", "endozoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "brush", "epizoochory"),
         dispersal_syndrome = str_replace_all(dispersal_syndrome, "passing", "epizoochory")) %>% 
  mutate(dispersers = str_replace_all(dispersers, "ingest", "vertebrates"),
         dispersers = str_replace_all(dispersers, "moisture", "passive"),
         dispersers = str_replace_all(dispersers, "adhesion", ""),
         dispersers = str_replace_all(dispersers, "(?<!a)biotic", "animals"),
         dispersers = str_replace_all(dispersers, "attach", "vertebrates"),
         dispersers = str_replace_all(dispersers, "gravity", "passive"),
         dispersers = str_replace_all(dispersers, "unassisted", "passive"),
         dispersers = str_replace_all(dispersers, "livestock", "mammals_domestic"),
         dispersers = str_replace_all(dispersers, "bird", "birds"),
         dispersers = str_replace_all(dispersers, "animals \\(ref for genus\\)", "invertebrates vertebrates"),
         dispersers = str_replace_all(dispersers, "vivipary", ""),
         dispersers = str_replace_all(dispersers, "gut", "vertebrates"),
         dispersers = str_replace_all(dispersers, "brush", "vertebrates"),
         dispersers = str_replace_all(dispersers, "passing", "vertebrates")) %>% 
  mutate(dispersal_unit = if_else(str_detect(seed_dispersal_new, "vivipary"), "plant", NA_character_)) -> data_final

data_final <-
  data_final %>% 
  mutate(dispersal_syndrome = str_squish(dispersal_syndrome),
         dispersers = str_squish(dispersers),
         dispersal_syndrome = paste(unique(unlist(strsplit(dispersal_syndrome, split = " "))), collapse = " "),
         dispersers = paste(unique(unlist(strsplit(dispersers, split = " "))), collapse = " "))


# Join max DBH and height to trait data
max_height_diameter %>% 
  mutate(Species = "Pimelea microcephala subsp. microcephala") -> max_height_diameter

full_join(data_final, max_height_diameter, by = "Species") -> data_final

data_final %>% write_csv("data/Draper_2023/data.csv")
