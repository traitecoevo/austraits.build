install.packages("euctraits", repos = "https://r-pkgs.williamkmorris.com")
library("euctraits")

euctraits::traits_grampians -> traits_grampians

euctraits::medians_grampians  %>%
  select(taxon, max_height_m) ->  Grampian_max_height

#newer version
read_csv("data/Pollock_2012/raw/grampians_EWenk_edited_latlon.csv", guess_max=10000) %>%
  subset(is.na(omit)) %>%
  bind_rows(Grampian_max_height) %>%
  rename(`longitude (deg)`= longitude_gda94, `latitude (deg)` = latitude_gda94, `elevation (m)` = altitude_m) %>%
  mutate(notes = ifelse(is.na(`latitude (deg)`),"generic Grampians lat/lon","")) %>%
  mutate(`latitude (deg)` = ifelse(is.na(`latitude (deg)`),-37.26125,`latitude (deg)`)) %>%
  mutate(`longitude (deg)` = ifelse(is.na(`longitude (deg)`),142.4448611,`longitude (deg)`)) %>%
  mutate(`elevation (m)` = ifelse(`elevation (m)` == 0,NA,`elevation (m)`)) %>%
  write_csv("data/Pollock_2012/data.csv") %>%
  select(locality_consistent,`longitude (deg)`, `latitude (deg)`, `elevation (m)`, notes, date_time, collector) %>%
  distinct(locality_consistent, .keep_all = TRUE) %>%
  write_csv("data/Pollock_2012/raw/site_data2.csv") -> site_data

#old version
read_csv("data/Pollock_2012/raw/grampians.csv") %>%
  mutate(seed_mass_mg = ifelse(TRUE,NA,seed_mass_mg)) %>%
  mutate(fruit_wall_width_mm = ifelse(TRUE,NA,fruit_wall_width_mm)) %>%
  bind_rows(Grampian_species_medians) %>%
  mutate(site_name = ifelse(is.na(latitude_gda94),"Grampians",
                                  paste("site_at",latitude_gda94,"deg_S",longitude_gda94,"deg_E", sep="_"))) %>%
  rename(`longitude (deg)`= longitude_gda94, `latitude (deg)` = latitude_gda94, `elevation (m)` = altitude_m) %>%
  mutate(`latitude (deg)` = ifelse(site_name == "Grampians",-37.26125,`latitude (deg)`)) %>%
  mutate(`longitude (deg)` = ifelse(site_name == "Grampians",142.4448611,`longitude (deg)`)) %>%
  mutate(check_all_NA = paste(height_m,stem_density_g_per_ml,stem_mass_g,stem_volume_ml,bark_mass_g,sla_mm2_per_mg,
                   leaf_area_cm2,leaf_mass_g,fruit_mass_mg,fruit_wall_width_mm,fruit_diameter_mm,
                   fruit_height_mm,seed_mass_mg,seed_mass_total_mg,number_of_seeds,sep="_")) %>%
  mutate(`elevation (m)` = ifelse(`elevation (m)` == 0,NA,`elevation (m)`)) %>%
  subset(check_all_NA != "NA_NA_NA_NA_NA_NA_NA_NA_NA_NA_NA_NA_NA_NA_NA") %>%
  select(-check_all_NA) %>%
  write_csv("data/Pollock_2012/data.csv") %>%
  select(site_name,`longitude (deg)`, `latitude (deg)`, `elevation (m)`) %>%
  distinct(site_name, .keep_all = TRUE) -> site_data
