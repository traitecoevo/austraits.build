read_csv("data/Standish_2019/Standish_2019.csv") %>%
  mutate(`Seed mass (mg)` = ifelse(`SM data source`=="Kew SID",NA,`Seed mass (mg)`)) %>%
  mutate(nitrogen_fixer = ifelse(startsWith(`Nutrient-acquisition strategy`,"Nfixer"),1,0)) %>%
  mutate(`Nutrient-acquisition strategy`= gsub("Nfixer/","",`Nutrient-acquisition strategy`)) %>%
  write_csv("data/Standish_2019/data.csv")

       