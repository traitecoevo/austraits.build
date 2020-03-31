read_csv("data/Laxton_2005/raw/Laxton data.csv") %>%
  rename(species_name = spp,`SLA cm2/g` = "SLA(cm2/g)") %>%
  select(-sppcode, -"H2O(%)", -"thick(mm)", -"size(mm2)", -"FF(N)", -"Tough(N/m)") -> Laxton

read_csv("data/Laxton_2005/raw/data_long.csv") %>%
  mutate(column_header = paste(trait,units,sep=" ")) %>% 
  select(-trait,-units) %>%
  spread(key = column_header, value = raw_trait_value) %>% 
  rename(species_name = name_original) %>%
  full_join(Laxton,by=c("species_name","SLA cm2/g")) %>%
  write_csv("data/Laxton_2005/raw/test.csv")

##additional manual manipulations done to fix mistakes

names(read_csv("data/Laxton_2005/data.csv"))
