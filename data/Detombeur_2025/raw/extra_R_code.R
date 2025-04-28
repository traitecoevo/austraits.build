locations <- read_csv("data/Detombeur_2025/raw/soil_properties.csv") %>%
  group_by(location_name, zone) %>%
    summarise(
      across(c(2:8), ~mean(.x)),
      description = first(description)
    ) %>%
  select(`latitude (deg)`, `longitude (deg)`, description, everything()) %>%
  ungroup()

metadata_add_locations("Detombeur_2025", locations)
