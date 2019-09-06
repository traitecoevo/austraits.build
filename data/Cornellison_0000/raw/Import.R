
source("R/austraits.R")
library(dplyr)
library(readr)

data_raw.Herb <- read_csv("data/Cornellison_0000/raw/Herbivory.csv", col_types = cols()) 
  
data_sites <- 
  data_raw.Herb %>%
  select(Locality, Latitude, Longitude, `Altitude (m)`) %>%
  distinct()

data_sites

data.Herb <-
  data_raw.Herb %>% 
  select(species_name =`Plant species`,  individual = `Tree number`, leaf_area = `Total leaf area (cm2)- Image J`) %>%
  na_if("sample_missing") %>% 
  mutate(leaf_area = as.numeric(leaf_area)) %>%
  na.omit() %>%
  group_by(species_name, individual) %>%
  summarise(leaf_area = mean(leaf_area), replicates = n()) %>% 
  ungroup()



data_raw.height <- read_csv("data/Cornellison_0000/raw/Other_data.csv", col_types = cols()) %>% select(species_name = `species`, individual, height)  
data.Herb %>% 
  full_join(by = c("species_name", "individual"),
            data_raw.height
  ) %>% 
  write_csv("data/Cornellison_0000/data.csv", na = "")






#data_raw.height %>% list("Ceratopetalum gummiferum" = "Ceratopetalum gummifera", "Pultenaea daphnoidesÊ" = "Pultenaea daphnoides") %>% mutate_at( "species_name", recode, !!!spname)
