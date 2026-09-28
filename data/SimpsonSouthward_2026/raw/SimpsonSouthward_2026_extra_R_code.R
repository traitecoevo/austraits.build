library(sf)
library(dplyr)
library(readr)

f <- "data/SimpsonSouthward_2026/raw/harriet_bark_dist.gpkg"

sites <- function(layer) {
  s <- st_read(f, layer, quiet = TRUE)
  xy <- st_coordinates(s)
  st_drop_geometry(s) %>%
    mutate(longitude = xy[, 1], latitude = xy[, 2]) %>%
    select(site_label, observer, longitude, latitude)
}

# Density and thickness were measured on the same trees but the file has no tree identifier,
# so trees are matched on site + species + dbh (rounded, as some dbh are means stored with
# float noise). Only one-to-one matches are linked; the few trees sharing site, species and
# dbh with another tree are kept as separate rows.
tree_key <- c("site_label", "original_name", "dbh")

thick <- st_read(f, "trees_thickness", quiet = TRUE) %>%
  rename(bark_type_thickness = bark_type_h) %>%
  mutate(thick_id = row_number(), dbh = round(dbh, 2))

dens <- st_read(f, "trees_density", quiet = TRUE) %>%
  rename(bark_type_density = bark_type_h) %>%
  mutate(dens_id = row_number(), dbh = round(dbh, 2))

matches <- inner_join(
    thick %>% select(all_of(tree_key), thick_id),
    dens %>% select(all_of(tree_key), dens_id),
    by = tree_key, relationship = "many-to-many"
  ) %>%
  add_count(thick_id, name = "n_thick") %>%
  add_count(dens_id, name = "n_dens") %>%
  filter(n_thick == 1, n_dens == 1) %>%
  select(thick_id, dens_id)

thick %>%
  left_join(matches, by = "thick_id") %>%
  left_join(dens %>% select(dens_id, bark_type_density, bark_density), by = "dens_id") %>%
  bind_rows(dens %>% filter(!dens_id %in% matches$dens_id)) %>%
  left_join(sites("sites_thickness"), by = "site_label") %>%
  select(site_label, observer, longitude, latitude, original_name, display_name, dbh,
         bark_type_thickness, bark_type_density, bark_thickness, bark_density, for_analysis) %>%
  mutate(
    bark_thickness = round(bark_thickness, 4),
    collection_date = case_when(
      observer == "Harriet" ~ "2019-08/2021-06",
      observer == "Eli" ~ "2018-02/2018-07"
    )
  ) %>%
  write_csv("data/SimpsonSouthward_2026/data.csv", na = "")

# Most recent fire at each site, from the NPWS fire history polygons (fire seasons 1970-2023)
sf_use_s2(FALSE)
site_points <- st_read(f, "sites_thickness", quiet = TRUE)
fire_history <- st_read(f, "npws_firehistory", quiet = TRUE) %>% st_make_valid()

last_fire <- st_intersects(site_points, fire_history) %>%
  lapply(function(i) {
    if (length(i) == 0) return(tibble(last_fire_season = NA_integer_, last_fire_type = NA_character_))
    last <- st_drop_geometry(fire_history)[i, ] %>% filter(fire_season == max(fire_season))
    tibble(
      last_fire_season = max(last$fire_season),
      last_fire_type = paste(sort(unique(last$fire_type)), collapse = " and ")
    )
  }) %>%
  bind_rows() %>%
  mutate(site_label = site_points$site_label)

# Vegetation type and fire frequency category are encoded in the secondary dataset's (Eli's) site codes;
# the primary dataset's (Harriet's) sites are all dry sclerophyll forest, per the sampling strategy.
sites("sites_thickness") %>%
  left_join(st_read(f, "predictors_all", quiet = TRUE) %>% select(-observer, -for_analysis), by = "site_label") %>%
  left_join(last_fire, by = "site_label") %>%
  mutate(
    vegetation_code = stringr::str_match(site_label, "^[A-Z]{2}(DSF|WSF)")[, 2],
    vegetation_code = if_else(observer == "Harriet", "DSF", vegetation_code),
    fire_frequency_code = stringr::str_match(site_label, "^[A-Z]{2}(?:DSF|WSF)(H|L)")[, 2]
  ) %>%
  transmute(
    location_name = site_label,
    `latitude (deg)` = latitude,
    `longitude (deg)` = longitude,
    `elevation (m)` = round(elevation, 1),
    `slope angle (degrees)` = round(slope, 1),
    `aridity index` = round(aridity, 3),
    lithology = f_lithology,
    `vegetation type` = recode(vegetation_code, DSF = "dry sclerophyll forest", WSF = "wet sclerophyll forest"),
    `fire frequency category` = recode(fire_frequency_code, H = "high", L = "low"),
    `fire frequency (number of fires)` = fire_frequency,
    `fire history (year of last fire)` = last_fire_season,
    `fire history (type of last fire)` = last_fire_type
  ) %>%
  write_csv("data/SimpsonSouthward_2026/raw/locations.csv", na = "")
