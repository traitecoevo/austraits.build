read_csv("data/Tomlinson_2013/raw/Tomlinson_2012_SI_table.csv") %>%
  subset(Continent == "Australia") %>%
  select(Species, `SMF (20 weeks; g g-1) - mean`) -> Tomlinson_2012_data


read_csv("data/Tomlinson_2013/raw/data_before_merging_2012_data.csv") %>%
  full_join(Tomlinson_2012_data,by="Species") %>% 
  write_csv("data/Tomlinson_2013/data.csv")