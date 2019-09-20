read_csv("data/Kew_2019_1/raw/Kew_2010_data.csv") %>%
  filter(merge_code == "keep") %>%
  select(c("name_original","raw_trait_value")) -> Kew_2010_unique

names(Kew_2010_unique) <- c("taxa","thousandseedweight") 

read_csv("data/Kew_2019_1/raw/Liu_Kew_seed_mass.csv") %>%
  bind_rows(Kew_2010_unique) %>% 
  write.csv("data/Kew_2019_1/data.csv")
