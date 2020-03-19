source("R/austraits.R")
library(dplyr)
library(readr)

data_raw.Schulze_incomplete <- read_csv("data/Schulze_1998/raw/NATTdata_incomplete.csv", col_types = cols()) 

data.Schulze_growthform <-
  data_raw.Schulze_incomplete %>% 
  select(Species =`species`, plant_growth_form = `growth form`) %>%
  distinct()

data.Schulze_1998 <- read_csv("data/Schulze_1998/data.csv", col_types = cols())   
data.Schulze_growthform %>% 
  right_join(by = c("Species"),
            data.Schulze_1998
  ) %>% 
  write_csv("data/Schulze_1998/data.csv", na = "") 
