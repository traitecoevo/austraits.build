read_csv("data/Morgan_2005/raw/data_old.csv") %>%
  select(species,site,`height (m)`) %>%
  group_by(species,site) %>%
  summarise(max_height_m = max(`height (m)`,na.rm=TRUE)) -> height_data

read_csv("data/Morgan_2005/raw/data_old.csv") %>%
  select(-`height (m)`) %>%
  arrange(LMA) %>%
  group_by(species,site,rainfall,`soil tex`) %>%
  summarise_all(.funs = mean,na.rm=TRUE) %>%
  ungroup() %>% 
  full_join(height_data,by=c("species","site")) %>% 
  write_csv("data/Morgan_2005/data.csv")
