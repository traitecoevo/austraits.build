read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_biomass_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  mutate(root_shoot_ratio = `rootDW (g)`/(`leafDW (g)`+ `stemDW (g)`),
         leaf_area_ratio_cm2_g = `LA (cm-2)`/(`rootDW (g)`+`leafDW (g)`+ `stemDW (g)`),
         specific_leaf_area_cm2_g = `LA (cm-2)`/`leafDW (g)`) %>%
  rename(date_biomass = `Date`) %>%
  mutate(Potnum = as.numeric(Potnum)) -> biomass

read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea" & Potnum != "356") %>%
  subset(Organ == "leaf") %>%
  rename(leaf_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, leaf_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`, 
         date_carbo = `Date`) %>%
  select(-Organ) -> leaf_carbo
  
read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea" & Potnum != "356") %>%
  subset(Organ == "stem") %>%
  rename(stem_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, stem_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`) %>%
  select(-Organ, -Date) -> stem_carbo

read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea" & Potnum != "356") %>%
  subset(Organ == "root") %>%
  rename(root_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, root_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`) %>%
  select(-Organ, -Date) -> root_carbo

leaf_carbo %>%
  full_join(stem_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  full_join(root_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  mutate(Potnum = as.numeric(Potnum)) -> all_carbo
  
read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_GasExchange_20120326-20120808_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  mutate(Potnum = as.numeric(Potnum)) -> gas_exchange

gas_exchange %>%
  full_join(all_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  full_join(biomass, by=c("Species","Temp","CO2","Water","Potnum")) %>%
  group_by(Potnum) %>%
  arrange(rev(Day)) %>%
  mutate_at(vars(15:31),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  mutate(Species = "Callitris rhomboidea") %>%
  mutate_at(c("Photo (umol m-2 s-1)","Rdark (umol m-2 s-1)" ), ~na_if(.,0)) %>%
  mutate(context = paste("grown_at_",Temp,"_temp_and_",CO2,"_ppm_CO2_with_",Water,"_treatment",sep="")) %>%
  mutate(days_since_drought = if_else(Day < 24, "fewer_than_24", "xx"),
         days_since_drought = if_else(Day > 24 & Day < 45, "30_to_44",days_since_drought),
         days_since_drought = if_else(Day > 45 & Day < 87, "44_to_86",days_since_drought),
         days_since_drought = if_else(Day > 87,"more_than_88",days_since_drought)) %>%
  mutate(context = if_else(days_since_drought=="",context,paste(context,"_and_",days_since_drought,"_days_since_drought_treatment",sep=""))) %>% View()
  write_csv("data/Duan_2015/data.csv") %>%
  distinct(context,.keep_all = TRUE) %>%
  select(context, Temp, CO2, Water, days_since_drought) %>%
  mutate(type = "treatment",
         description = "Factorial experiment with plants grown at two temperatures, two CO2 concentrations, 
         and under two drought conditions; gas exchange measurements were made at various dates after the onset of drought.") %>% 
  write_csv("data/Duan_2015/raw/context_table.csv")

#notes
#In the carbohydrates data file, there are two entries for leaf values for Potnum 356, so this potnum has been excluded, since one of the two entries is incorrect

  