library(tidyverse)

data <- read_csv("data/Simpson_2021/raw/joined_data.csv")
species_not_to_filter <- read_csv("data/Simpson_2021/raw/species_not_to_filter.csv")

data %>% 
  mutate(
    species_not_to_filter = if_else(
      Species_name %in% species_not_to_filter$., "not to filter", NA_character_)
    ) -> data

data %>% write_csv("data/Simpson_2021/data.csv")
