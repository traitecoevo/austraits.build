library(tidyverse)
library(readxl)

original_data <- read_csv("data/Nano_2011/raw/original_data.csv")

more_resprouting_data <- 
  read_xlsx("data/Nano_2011/raw/ecological_attributes.xlsx", sheet = "Resprouting")

#more_resprouting_data %>% write_csv("data/Nano_2011/raw/Crowley_data.csv")

detailed_data <- 
  read_xlsx("data/Nano_2011/raw/ecological_attributes.xlsx", sheet = "ecological_attributes") %>% 
  select(`Taxon Name`, `Eco Source`, `Fire Response Adult`, `Resprout Type`, `Seed Bank`) %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "-", NA_character_))) %>% 
  mutate(`Taxon Name` = sub("^\\w+\\s", "", `Taxon Name`))

stem_buds_data <- 
  read_xls("data/Nano_2011/raw/talltree.xls", sheet = "stembuds")

original_data %>% 
  full_join(detailed_data, by = c("Taxon" = "Taxon Name")) %>% 
  full_join(stem_buds_data, by = c("Taxon" = "species")) -> joined

#joined %>% filter(!is.na(`Vegetative persistence`), !is.na(Fire_Response_Adult)) %>% View()

#joined %>% write_csv("data/Nano_2011/raw/joined_data.csv")
joined %>% write_csv("data/Nano_2011/data.csv")