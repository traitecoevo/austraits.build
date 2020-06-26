read_csv("data/MacinnisNg_2016/raw/nutrient.csv") %>%
  mutate(season = gsub("w","winter", season),
         season = gsub("s","summer", season)) -> nutrient

read_csv("data/MacinnisNg_2016/raw/gas_exchange.csv") %>%
  group_by(species, season, tree) %>%
  summarise_all(.funs = mean) %>%
  ungroup -> gas_exchange
  
read_csv("data/MacinnisNg_2016/raw/hydraulic.csv") %>%
  rename(tree = `sample number`,season = `sample month`, species = Species) %>%
  full_join(gas_exchange,by=c("species","season","tree")) %>%
  bind_rows(nutrient) %>%
  write_csv("data/MacinnisNg_2016/data.csv")