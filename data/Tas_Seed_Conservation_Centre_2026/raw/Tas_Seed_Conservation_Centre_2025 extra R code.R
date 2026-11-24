data <- read_csv("data/Tas_Seed_Conservation_Centre_2025/data.csv") %>% 
  mutate(
    location_name = paste0("seeds_collected_at_",`Decimal Latitude`,"_lat_and_",`Decimal Longitude`,"_long")
  ) 

data_split <- data %>% split(data$location_name)

for (i in 1:length(data_split)) {
  data_split[[i]] <- data_split[[i]] %>%
    group_by(Locality) %>%
    mutate(location_name = paste0(location_name, "_", cur_group_id())) %>%
    ungroup()
}

data <- bind_rows(data_split) %>%
  mutate(
    location_name = stringr::str_replace(location_name, "_1$", ""),
    location_name = ifelse(str_detect(location_name, "at_NA_lat_and_NA_long"), "seeds_collection_location_unknown", location_name),
    location_name = ifelse(!is.na(`Adjusted Germination`)|!is.na(`Thousand Seed Weight`)|!is.na(`Seed/gm`), location_name, NA)
  ) %>% write_csv("data/Tas_Seed_Conservation_Centre_2025/data.csv", na = "")

read_csv("data/Tas_Seed_Conservation_Centre_2025/data.csv") %>% 
  filter(!is.na(`Adjusted Germination`)|!is.na(`Thousand Seed Weight`)|!is.na(`Seed/gm`)) %>%
  distinct(location_name, `Decimal Latitude`,`Decimal Longitude`, Locality) %>% 
  mutate(
    notes = "latitude/longitude refer to seed collection location",
  ) %>%
  rename(`latitude (deg)`= `Decimal Latitude`,`longitude (deg)` = `Decimal Longitude`, locality = Locality)  -> locations

metadata_add_locations("Tas_Seed_Conservation_Centre_2025", locations)
