read_csv("data/Schmidt_1993/data.csv") -> check
unique(check$`mycorrhizal status`)


read_csv("data/Schmidt_1993/data.csv") %>%
  select(-X6) %>%
  mutate(nitrogen_fixing = `mycorrhizal status`) %>%
  mutate(nitrogen_fixing = str_extract(nitrogen_fixing,"nod")) %>%
  mutate(`mycorrhizal status` = gsub("nod","",`mycorrhizal status`)) %>%
  
  
  names(read_csv("data/Schmidt_2010/raw/data_Monksland_raw.csv"))
