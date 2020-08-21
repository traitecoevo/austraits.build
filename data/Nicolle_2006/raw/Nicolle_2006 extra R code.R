read_csv("data/Nicolle_2006/raw/Yates_2017/data.csv") %>% 
  select('Species name',Serotinous) %>%
  rename(taxon = 'Species name') %>%
  subset(Serotinous == "Yes")-> Yates_serotiny

read_csv("data/Nicolle_2006/raw/Nicolle_data.csv",guess_max = 1500) %>%
  full_join(Yates_serotiny,by="taxon") %>% 
  mutate(regen_strategy = ifelse(Epicormic == "yes" & Lignotuber == "yes","epicormic lignotuber",NA), 
         regen_strategy = ifelse(Epicormic == "yes" & Lignotuber == "no","epicormic",regen_strategy),
         regen_strategy = ifelse(Epicormic == "no" & Lignotuber == "yes","lignotuber",regen_strategy),
         regen_strategy = ifelse(`strategy_Nicolle`=="Obligate seeder","fire_killed",regen_strategy)) %>%
  write_csv("data/Nicolle_2006/data.csv") 