read_csv("data/Kew_2019_6/raw/match_terms_Kew_2019_06.csv") -> match

read_csv("data/Kew_2019_6/raw/Liu_Kew_germination.csv", guess_max = 10000) %>%
  
  #extract numbers are beginning of cell
  mutate(germination_percent = as.numeric(str_sub(percentgerm,1,3))) %>%
  
  #extract numbers at end of cell
  mutate(seeds_sown = as.numeric(str_match(percentgerm,"(?<=sown\\: )[0-9]+"))) %>%
  select(-percentgerm) %>%
  left_join(match,by = "presowtreat") %>% 
  write_csv("data/Kew_2019_6/data.csv") 


#extract numbers are beginning of cell
data$percentgerm %>% str_sub(1,3) %>% as.numeric()

#extract numbers at end of cell
data$percentgerm %>% str_match("(?<=sown\\: )[0-9]+") %>% as.numeric()
