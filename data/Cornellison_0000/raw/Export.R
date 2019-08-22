library(dplyr)
library(readr)

data_raw.Herb <- read_csv("data/Cornellison_0000/raw/Herbivory.csv", col_types = cols()) 
  
data_sites <- 
  data_raw.Herb %>%
  select(Locality, Latitude, Longitude, `Altitude (m)`) %>%
  distinct()

data.Herb <-
  data_Herb.sla %>% 
  select(species_name =`Plant species`,  individual =  `Tree number`, leaf_area = `Total leaf area (cm2)- Image J`) %>%
  na_if("sample_missing") %>% 
  mutate(leaf_area = as.numeric(leaf_area)) %>%
  na.omit() %>%
  group_by(species_name, individual) %>%
  summarise(leaf_area = mean(leaf_area), replicates = n()) %>% 
  ungroup()

  select(-Plant Family, -Herbivores -Tree number, Leaf number,Leaf Damage Class - Dirzo method,Total leaf area (cm2)- Image J,Leaf area removed (cm2) - Image J,Herbivory (%) ,Miner damage class - Dirzo method,Gall damage class - Dirzo method,Collector) 
data.wd <- read_csv("data/Cornellison_0000/raw/Other_data.csv", col_types = cols()) %>% select(-units)

data.sla %>% 
  full_join(by = "species", 
            data.wd %>% select(species, wood.density)
  ) %>% 
  write_csv("data/Staples_2019/data.csv", na = "")