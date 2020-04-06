read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_BiochemistryGEleaves_20080300_L1.csv") %>%
  rename(leaf_area_cm2_one_leaf = `LA (cm2)`, leaf_dry_mass_g_one_leaf = `LeafDW (g)`, LMA_g_m2_one_leaf = `LMA (g m-2)`) -> biochem

read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_GasExchangeACi_20080331-20080403_L1.csv") -> gas_exchange 

gas_exchange %>%
  subset(CO2R > 395 & CO2R < 405) %>%
  group_by(Species, Date, Temp, CO2, Potnum, MeasTemp) %>%
  summarise_all(.funs = mean) %>%
  select(-"VpdL (kPa)", -"Area (cm2)", -"StmRat", -"BLCond", -"Tair", -"Tleaf", -"TBlk", -"CO2R" , -"CO2S", -"H2OR", -"H2OS", 
         -"RHR", -"RHS", -"Flow", -"PARi") %>%
  ungroup() %>%
  rename(A_sat_umol_per_m2_s1 = `Photo (umol m-2 s-1)`, Cond_at_A_sat_mol_per_m2_s1 = `Cond (mol m-2 s-1)`, 
         Ci_at_A_sat_ppm = `Ci (ppm)`, Trans_at_A_sat_mmol_per_m2_s1 = `Trmmol (mmol m-2 s-1)`) -> A_sat_means

gas_exchange %>%
  select(-"VpdL (kPa)", -"Area (cm2)", -"StmRat", -"BLCond", -"Tair", -"Tleaf", -"TBlk", -"CO2S", -"H2OR", -"H2OS", 
         -"RHR", -"RHS", -"Flow", -"PARi") %>%
  group_by(Species, Date, Temp, CO2, Potnum, MeasTemp) %>%
  top_n(1, CO2R) %>%
  rename(A_max_umol_per_m2_s1 = `Photo (umol m-2 s-1)`, Cond_at_A_max_mol_per_m2_s1 = `Cond (mol m-2 s-1)`, 
         Ci_at_A_max_ppm = `Ci (ppm)`, Trans_at_A_max_mmol_per_m2_s1 = `Trmmol (mmol m-2 s-1)`) -> A_max_values

read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_Harvest150DAP_20080400_L1.csv") %>%
  mutate(age_at_harvest = "150_days") %>%
  mutate(Potnum = as.numeric(Potnum)) -> harvest_150

read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_Harvest80DAP_20080200_L1.csv") %>%
  mutate(age_at_harvest = "80_days") -> harvest_80 

harvest_80 %>%
  bind_rows(harvest_150) %>% 
  full_join(biochem, by = c("Species", "Temp", "CO2", "Potnum")) -> part_1

A_sat_means %>%
  full_join(A_max_values, by = c("Species", "Date", "Temp", "CO2", "Potnum", "MeasTemp")) %>%
  arrange(MeasTemp) %>%
  full_join(part_1, by = c("Species", "Temp", "CO2", "Potnum")) %>%
  group_by(Potnum) %>%
  mutate_at(vars(18:49),funs(replace(.,duplicated(.),NA))) %>% 
  ungroup() %>%
  mutate(Species = gsub("E.","Eucalyptus",Species)) %>%
  mutate(harvest_age = if_else(is.na(age_at_harvest),"",paste("harvested_at_",age_at_harvest,sep=""))) %>%
  mutate(sample_temp = if_else(is.na(MeasTemp),"",paste("measurements_made_at_",MeasTemp,"_deg_C",sep=""))) %>%
  mutate(context = paste("grown_at_",Temp,"_temp_and_",CO2,"_ppm_CO2",sep="")) %>%
  mutate(context = if_else(sample_temp=="",context,paste(context,"_and_",sample_temp,sep=""))) %>%
  mutate(context = if_else(harvest_age=="",context,paste(context,"_and_",harvest_age,sep=""))) %>% 
  write_csv("data/Ghannoum_2010/data.csv") -> test

test %>%
  distinct(context, .keep_all = TRUE) %>%
  select(context, age_at_harvest, Temp, CO2, MeasTemp) %>%
  mutate(type = "treatment",
         description = context,
         description = gsub("_"," ",description),
         description = gsub("Amb","ambient",description),
         description = gsub("temp","temperature",description),
         description = gsub("Elv","elevated",description),
         `measurement temperature description` = if_else(MeasTemp == 28, "Plants measured at 28 deg C, current growing season temperature", 
                                                         "Plants measured at 34 deg C, 4 deg C above current growing season temperature"),
         `growing temperature description` = if_else(Temp == "Amb", "Grown at ambient temperature, 26/18 (day/night)", 
                                                     "Grown at elevated temperature, 30/22 (day/night)"),
         `growing CO2 description` = if_else(CO2 == 280, "Grown at 280 ppm CO2, sub-ambient CO2 levels", 
                                             if_else(CO2 == 400,"Grown at 400 ppm CO2, ambient CO2 levels",
                                                     "Grown at 600 ppm CO2, elevated CO2 levels")),
         `age at harvest description` = if_else(is.na(age_at_harvest),"",if_else(age_at_harvest == "150_days", 
                                                                         "Harvested at 150 days of age","Harvested at 80 days of age")),
         `context columns` = "age at harvest, growing CO2, growing temperature, measurement temperature") %>% 
  rename(`measurement temperature (C)` = MeasTemp, `growing temperature (C)` = Temp,
         `growing CO2 (ppm)` = CO2, `age at harvest (days)` = age_at_harvest) -> contexts
