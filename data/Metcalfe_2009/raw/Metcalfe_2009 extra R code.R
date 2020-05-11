read_csv("data/Metcalfe_2009/raw/site_data.csv") %>%
  rename(`latitude (deg)` = LAT_DECIMA, `longitude (deg)` = LONG_DECIM) -> sites


read_csv("data/Metcalfe_2009/raw/Metcalfe_2009_data.csv") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>%
  mutate(site_name = ifelse(`5.2`==1,"Weiss",""),
         site_name = ifelse(`4.2`==1,"Siam",site_name),
         site_name = ifelse(`3.2`==1,"Shane's",site_name),
         site_name = ifelse(`3.1`==1,"Mackay's",site_name),
         site_name = ifelse(`2.2`==1,"Henry's",site_name),
         site_name = ifelse(`2.1`==1,"Zonta",site_name),
         site_name = ifelse(`1.2`==1,"McDonald's",site_name),
         site_name = ifelse(`1.1`==1,"Harney's",site_name)) %>%
  mutate(nitrogen_fixing = ifelse(Nutrient_uptake_strategy == "nitrogen fixer"|Nutrient_uptake_strategy == "Nitrogen fixer","yes","no")) %>%
  mutate(Nutrient_uptake_strategy = gsub("nitrogen fixer","",Nutrient_uptake_strategy),
         Nutrient_uptake_strategy = gsub("Nitrogen fixer","",Nutrient_uptake_strategy)) %>%
  write_csv("data/Metcalfe_2009/data.csv")
