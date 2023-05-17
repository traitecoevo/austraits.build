
library(tidyverse)

data1 <- read_csv("data/Enright_2014/raw/PFT_datasetJEcol_1.csv")
data2 <- read_csv("data/Enright_2014/raw/PFT_datasetJEcol_2_ResproutersSeedAdult.csv")
plot_data <- read_csv("data/Enright_2014/raw/PFT_PlotScaleData_JEcol.csv")

data1 %>% full_join(data2) -> data
data %>% left_join(plot_data) -> data

data %>% 
  mutate(
    taxon_name = paste(genus, species)
  ) %>% 
  relocate(taxon_name, .before = genus) -> data

data %>% write_csv("data/Enright_2014/data.csv")
