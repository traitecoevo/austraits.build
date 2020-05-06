install.packages("euctraits", repos = "https://r-pkgs.williamkmorris.com")
library("euctraits")

read_csv("data/Pollock_2018/raw/max_height_appendix.csv") -> height

euctraits::traits_mallee %>%
  bind_rows(height) %>%
  write_csv("data/Pollock_2018/data.csv") %>%
  distinct(locality, .keep_all = TRUE) %>%
  write_csv("data/Pollock_2018/site_data.csv") -> site_data
  

read_csv("data/Pollock_2018/raw/site_data_Pollock_2018_LPcorrected.csv") -> site_data
