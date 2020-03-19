read_csv("data/Stewart_1995/raw/Daintree_epiphyte.csv") -> Daintree

read_csv("data/Stewart_1995/raw/Gambubal.csv") -> Gambubal

read_csv("data/Stewart_1995/raw/Idalia.csv") %>%
  mutate(date = as.character(date)) -> Idalia

read_csv("data/Stewart_1995/raw/Loganholme.csv") -> Longholme

read_csv("data/Stewart_1995/raw/data_Stewart_1995.csv") %>%
  select(-"Study ID",-Genus,-species,-"traditional family" ,-"apg family", 
        -"Phylo1", -"Phylo2") %>%
  bind_rows(Gambubal) %>%
  bind_rows(Idalia) %>%
  bind_rows(Longholme) %>%
  write_csv("data/Stewart_1995/data.csv") 