#MANIPULATING R For PFAUSTH 2016

#grouping by species and tree and taking mean of various other data rows (where there are  many values per tree)

library(dplyr)

raw_Pfautsch_2016_data <- read.csv("data/Pfautsch_2016/raw/Pfautsch_2016_Eucalyptus_vessel_anatomy_combined.csv")



Pfautsch_2016_data_play <- read.csv("data/Pfautsch_2016/raw/Pfautsch_2016_Eucalyptus_vessel_anatomy_combined.csv") %>% 
  group_by(species,tree) %>% summarise_if(is.numeric, .funs=mean) %>% ungroup()
  
Pfautsch_2016_data_final <- read.csv("data/Pfautsch_2016/raw/Pfautsch_2016_Eucalyptus_vessel_anatomy_combined.csv") %>% 
  group_by(species,tree) %>% 
    summarise_if(is.character, .funs=first) %>% 
  ungroup() %>% 
  left_join(Pfautsch_2016_data_play, by = c('species', 'tree')) %>% 
  write.csv("data/Pfautsch_2016/data.csv")
  
 

#TO create site metadata (excluded these columns E, E_min, E_max, AI- because they produced different values for the same site)

site_info <- read.csv("data/Pfautsch_2016/data.csv") %>% distinct(location, lat, lon, state, country, elevation, T_min, T_max, P, P_min, P_max, E, E_min, E_max, AI) %>% group_by(location, lat, lon) %>% summarise_if(is.numeric, .funs=first) %>% ungroup() %>% rename('latitude (deg)'=lat, 'longitude (deg)'=lon, 'elevation (mm)'=elevation, 'min temperature (deg C)'=T_min, 'max temperature (deg C)'=T_max, 'MAP (mm)'=P, 'min precipitation (mm)'=P_min, 'max precipitation (mm)'=P_max, 'evaporation (mm)'=E, 'min evaporation (mm)'=E_min, 'max evaporation (mm)'=E_max, 'aridity index'=AI)  # %>% write.csv("data/Pfautsch_2016/raw_data/Pfautsch_2016_site.data.csv")

site_info$location <- sub('[.]', '_', make.names(site_info$location, unique=TRUE))

site_info %>% write.csv("data/Pfautsch_2016/raw/Pfautsch_2016_site.data.csv")




# to create context datamutate_if(is.numeric, round, digits = 2)

read.csv("data/Pfautsch_2016/data.csv") %>%
  distinct(context, .keep_all = TRUE) %>% select(context, distance_from_apex) %>%
  write.csv("data/Pfautsch_2016/raw/context_data.csv")


#redoing summarising to maintain both contexts & add replicate count

Pfautsch_data %>%
  select(-image, -vessel_area_per_image, -vessels_per_image, -image_area) %>%
  mutate(
    across(D_800:sapwood_density_stdev,~na_if(.,"N/A")),
    across(vessel_density_per_tree:sapwood_density_stdev, as.numeric),
    replicates = 1
  ) %>%
  group_by(species, context, location, tree) %>%
    summarise(
      across(c(state, country, lat, lon, elevation, T_min, T_max, P, P_min, P_max, E, E_min, E_max, AI), .fns = first),
      across(vessel_density_per_tree:sapwood_density_stdev, .fns = mean, na.rm = TRUE),
      replicates = sum(replicates)
    ) %>%
  ungroup() %>% 
  write_csv("data/Pfautsch_2016/data.csv")
