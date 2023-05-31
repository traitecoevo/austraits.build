library(dplyr)
library(austraits)
library(austraits.build)

# Load in raw data

Mitchell_2008_data_raw <-
  read_csv("data/Mitchell_2008/raw/Mitchell_2008.csv")


# Rename sites with more meaningful names

Mitchell_2008_data <-
  Mitchell_2008_data_raw %>% 
  mutate(Site = gsub("h", "heath", Site)) %>% 
  mutate(Site = gsub("w", "woodland", Site)) %>% 
  mutate(Site = gsub("m", "mallee", Site))


# Add genus names to species (in paper)

Mitchell_2008_data <-
  Mitchell_2008_data %>% 
  mutate(Species = gsub("A. campestris", "Allocasuarina campestris", Species)) %>% 
  mutate(Species = gsub("C. humilis", "Casuarina humilis", Species)) %>% 
  mutate(Species = gsub("D. vestita", "Dryandra vestita", Species)) %>% 
  mutate(Species = gsub("D. cirsiodes", "Dryandra cirsioides", Species)) %>% 
  mutate(Species = gsub("A. arenarius", "Actinostrobus arenarius", Species)) %>% 
  mutate(Species = gsub("E. albida", "Eucalyptus albida", Species)) %>% 
  mutate(Species = gsub("D. sessilis", "Dryandra sessilis", Species)) %>% 
  mutate(Species = gsub("N. floribunda", "Nuytsia floribunda", Species))


# Remove species characters from column names (either write out Greek letters or use equivalents - i.e. Y for Psi, D for delta) (I did P for Pi, E for Epsoilon) (do caps matter?)

Mitchell_2008_data <- 
  Mitchell_2008_data %>% 
  rename('Yleafmin' = '?leaf min', 'DYleaf' = '??leaf', 'Yleafmax' = '?leaf max', 'DYP100' = '??? 100', 'Demax' = '?e max', 'D13C' = '?13C')


# For sites, I'd include the national park where they were collected as the site property "locality" (same for all 3) and also the verbal soils info from the paper (e.g. "sandy loam of 0.1 m over deep clay" under "soil")

Mitchell_2008_data$locality <- c('Corrigin Nature Reserve')
Mitchell_2008_data$latitude <- c('-32.31667')
Mitchell_2008_data$longitude <- c('117.8667')

Mitchell_2008_data_soil <- 
  Mitchell_2008_data %>% 
  mutate(soil = case_when(
    Mitchell_2008_data$Site == 'mallee' ~ 'coarse sand 0.8- to 1-m depth over gravel/clay', 
    Mitchell_2008_data$Site == 'heath' ~ 'coarse sand of 0.1- to 0.2-m depth over laterite', 
    Mitchell_2008_data$Site == 'woodland' ~ 'sandy loam of 0.1 m over deep clay')) %>% 
  write.csv("data/Mitchell_2008/data.csv")


# Creating a Site data CSV

read_csv("data/Mitchell_2008/data.csv") %>%
  distinct(Site, .keep_all = TRUE) %>% 
  select('Site', 'latitude', 'longitude', 'locality', 'soil') %>% 
  rename('latitude (deg)' = 'latitude', 'longitude (deg)' = 'longitude') %>% 
  write_csv("data/Mitchell_2008/raw/site_data.csv")
