fruits <- read_csv("data/NHNSW_2024/raw/extracted_fruit_missed_pn checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  pivot_longer(cols = 2:4) %>%
  filter(!is.na(value))

seeds <- read_csv("data/NHNSW_2024/raw/extracted_seeds_missed_pn_checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  pivot_longer(cols = 2:5) %>%
  filter(!is.na(value))

leaves1 <- read_csv("data/NHNSW_2024/raw/extracted_leaves_1_missed_pn-checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  pivot_longer(cols = 2:7) %>%
  filter(!is.na(value))

leaves2 <- read_csv("data/NHNSW_2024/raw/extracted_leaves_2_missed_pn-checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  select(-leaf_arrangement_f, -`checked leaf_arrangement`) %>%
  pivot_longer(cols = 2:9) %>%
  filter(!is.na(value))

leaves3 <- read_csv("data/NHNSW_2024/raw/extracted_leaves_3_missed_pn-checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  pivot_longer(cols = 2:4) %>%
  filter(!is.na(value))

root <- read_csv("data/NHNSW_2024/raw/extracted_root_missed_pn-checked.csv") %>% 
  select(taxon_name, contains("_a")) %>%
  pivot_longer(cols = 2:3) %>%
  filter(!is.na(value))

height <- read_csv("data/NHNSW_2024/raw/extracted_heights_missed_pn.csv") %>% 
  select(taxon_name, absolute_min, min, max, absolute_max, units) %>%
  pivot_longer(cols = 2:5) %>%
  filter(!is.na(value)) %>%
  mutate(
    value_type = name,
    name = "plant_height")

fruit_length <- read_csv("data/NHNSW_2024/raw/fruit_dims checked.csv") %>%
  select(taxon_name, category, l_absolute_min, l_min, l_max, l_absolute_max, l_units) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = l_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "fruit_length",
    value_type = stringr::str_replace(value_type, "l_", "")
    )

fruit_width <- read_csv("data/NHNSW_2024/raw/fruit_dims checked.csv") %>%
  select(taxon_name, category, w_absolute_min, w_min, w_max, w_absolute_max, w_units) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = w_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "fruit_width",
    value_type = stringr::str_replace(value_type, "w_", "")
  )

seed_length <- read_csv("data/NHNSW_2024/raw/seed_dims checked.csv") %>% 
  select(taxon_name, category, l_absolute_min, l_min, l_max, l_absolute_max, l_units) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = l_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "seed_length",
    value_type = stringr::str_replace(value_type, "l_", "")
  )

seed_width <- read_csv("data/NHNSW_2024/raw/seed_dims checked.csv") %>%
  select(taxon_name, category, w_absolute_min, w_min, w_max, w_absolute_max, w_units) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = w_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "seed_width",
    value_type = stringr::str_replace(value_type, "w_", "")
  )

seed_thickness <- read_csv("data/NHNSW_2024/raw/seed_dims checked.csv") %>%
  select(taxon_name, category, t_min, t_max, t_units) %>%
  pivot_longer(cols = 3:4) %>%
  filter(!is.na(value)) %>%
  rename(units = t_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "seed_thickness",
    value_type = stringr::str_replace(value_type, "t_", "")
  )

leaf_length <- read_csv("data/NHNSW_2024/raw/leaf_dims-checked.csv") %>% 
  select(taxon_name, category, l_absolute_min, l_min, l_max, l_absolute_max, l_units) %>%
  mutate(
    category = case_when(
      category == "adultleaves" ~ "adult_leaves",
      category == "leaf" ~ "leaves",
      category == "leaflet" ~ "leaflets",
      category == "petiole" ~ "petioles",
      category == "petioles" ~ "petioles",
      category == "leaf_stalk" ~ "petioles",
      category == "petiolule" ~ "petiolules",
      category == "petiolules" ~ "petiolules",
      category == "pinnae" ~ "pinnules",
      category == "blade" ~ "lamina",
      category == "blades" ~ "lamina",
      category == "leafblade" ~ "lamina",
      category == "leafblades" ~ "lamina",
      category == "leaves on adut shoots" ~ "adult_leaves",
      category == "free_lamina" ~ "lamina",
      category == "leaves on juvenile plants" ~ "juvenile_plant_leaves",
      category == "leaves on juvenile shoots" ~ "juvenile_leaves",
      category == "leaves on leading shoots" ~ "leading_shoots",
      TRUE ~ category
      )
  ) %>% 
  filter(!category %in% c("crownshaft", "sheathing_base", "stipules", "segments")) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = l_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "leaf_length",
    name = ifelse(entity_measured %in% c("petioles", "petiolules"), "petiole_length", name),
    value_type = stringr::str_replace(value_type, "l_", "")
  )

leaf_width <- read_csv("data/NHNSW_2024/raw/leaf_dims-checked.csv") %>% 
  select(taxon_name, category, w_absolute_min, w_min, w_max, w_absolute_max, w_units) %>%
  mutate(
    category = case_when(
      category == "adultleaves" ~ "adult_leaves",
      category == "leaf" ~ "leaves",
      category == "leaflet" ~ "leaflets",
      category == "petiole" ~ "petioles",
      category == "petioles" ~ "petioles",
      category == "leaf_stalk" ~ "petioles",
      category == "petiolule" ~ "petiolules",
      category == "petiolules" ~ "petiolules",
      category == "pinnae" ~ "pinnules",
      category == "blade" ~ "lamina",
      category == "blades" ~ "lamina",
      category == "leafblade" ~ "lamina",
      category == "leafblades" ~ "lamina",
      category == "leaves on adut shoots" ~ "adult_leaves",
      category == "free_lamina" ~ "lamina",
      category == "leaves on juvenile plants" ~ "juvenile_plant_leaves",
      category == "leaves on juvenile shoots" ~ "juvenile_leaves",
      category == "leaves on leading shoots" ~ "leading_shoots",
      TRUE ~ category
    )
  ) %>% 
  filter(!category %in% c("crownshaft", "sheathing_base", "stipules", "segments")) %>%
  pivot_longer(cols = 3:6) %>%
  filter(!is.na(value)) %>%
  rename(units = w_units, value_type = name, entity_measured = category) %>%
  mutate(
    name = "leaf_width",
    value_type = stringr::str_replace(value_type, "w_", ""),
    name = ifelse(entity_measured %in% c("petioles", "petiolules"), "petiole_width", name)
  )

# extract authors from current NHNSW download; but don't keep copy of file here, since in NHNSW_2023

read_csv("data/NHNSW_2023/raw/PlantNET_raw.csv") %>%
  select(taxon_name, Source) %>%
  mutate(
    Source = stringr::str_replace(Source, "Taxon concept:$",""),
    Source = stringr::str_replace(Source, "Taxon concept","; Taxon concept")) %>% 
  filter(!(is.na(Source))) %>%
  group_by(taxon_name) %>%
  mutate(Source = paste0(Source, collapse = "; ")) %>%
  ungroup() %>%
  distinct() -> authors

fruit_length %>%
  bind_rows(fruit_width, height, leaf_length, leaf_width, seed_length, seed_thickness, seed_width) %>%
  mutate(
    value_type = stringr::str_replace(value_type, "min", "minimum"),
    value_type = stringr::str_replace(value_type, "max", "maximum"),
    value = as.character(value)
    ) %>%
  bind_rows(fruits, leaves1, leaves2, leaves3, root, seeds) %>%
  mutate(
    name = stringr::str_replace(name, "_a$", ""),
    value_type = ifelse(is.na(value_type), "mode", value_type)
  ) %>%
  rename(trait_name = name, trait_value = value) %>%
  select(taxon_name, trait_name, category = entity_measured, trait_value, units, value_type) %>%
  left_join(authors) %>%
  filter(!taxon_name %in% c("Pultenaea juniperina", "Pultenaea spinosa")) %>%
  write_csv("data/NHNSW_2024/data.csv") -> data

# tests to verify these really are separate taxa
# they are - only overlap is c("Pultenaea juniperina", "Pultenaea spinosa"), so filtering these out

NHNSW_David <-
  austraits$traits %>%
  filter(dataset_id %in% c("NHNSW_2023", "NHNSW_2022")) %>%
  select(taxon_name, trait_name, value, value_type)

NHNSW_extras <-
  austraits$traits %>%
  filter(dataset_id %in% c("NHNSW_2024")) %>%
  select(taxon_name, trait_name, value, value_type)
NHNSW_extras %>%
  rename(value2 = value) %>%
  left_join(NHNSW_David, by = c("taxon_name", "trait_name", "value_type")) %>% View()
