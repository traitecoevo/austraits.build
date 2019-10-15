library(dplyr)
library(readr)

data.sla <- read_csv("data/Staples_2019/raw/Staples_et_al-2019-SLA_data.csv", col_types = cols()) %>% select(-units) 
data.wd <- read_csv("data/Staples_2019/raw/Staples_et_al-2019-wood_density_data.csv", col_types = cols()) %>% select(-units)
  
data.sla %>% 
  full_join(by = "species", 
            data.wd %>% select(species, wood.density)
  ) %>% 
  write_csv("data/Staples_2019/data.csv", na = "") 
