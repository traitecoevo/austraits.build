read_csv("data/Standish_2019/Standish_2019.csv") %>%
  mutate(`Seed mass (mg)` = ifelse(`SM data source`=="Kew SID",NA,`Seed mass (mg)`)) %>%
  mutate(nitrogen_fixing = ifelse(startsWith(`Nutrient-acquisition strategy`,"Nfixer"),1,0)) %>%
  mutate(`Nutrient-acquisition strategy`= gsub("Nfixer/","",`Nutrient-acquisition strategy`)) %>%
  mutate(life_history = ifelse(`Life Form` %in% c("shrub","tree","perennial forb","perennial grass",
                                                  "perennial rush","perennial sedge"),"perennial","annual")) %>%  
  mutate(`Life Form` = gsub("annual ","",`Life Form`)) %>%
  mutate(`Life Form` = gsub("perennial ","",`Life Form`)) %>%
  mutate(`Species Name` = gsub("*","",`Species Name`)) %>%
  write_csv("data/Standish_2019/data.csv")