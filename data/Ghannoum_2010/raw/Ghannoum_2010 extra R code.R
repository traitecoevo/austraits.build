read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_BiochemistryGEleaves_20080300_L1.csv") -> biochem

read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_GasExchangeACi_20080331-20080403_L1.csv") -> gas_exchange 

gas_exchange %>%
  subset(CO2R > 395 & CO2R < 405) %>%
  group_by(Species, Date, Temp, CO2, Potnum, MeasTemp) %>%
  summarise_all(.funs = mean) %>%
  select(-"VpdL (kPa)", -"Area (cm2)", -"StmRat", -"BLCond", -"Tair", -"Tleaf", -"TBlk", -"CO2R" , -"CO2S", -"H2OR", -"H2OS", 
         -"RHR", -"RHS", -"Flow", -"PARi") %>%
  ungroup() %>%
  rename(A_sat_umol_per_m2_s1 = `Photo (umol m-2 s-1)`, Cond_at_A_sat_mmol_per_m2_s1 = `Cond (mol m-2 s-1)`, Ci_at_A_sat_ppm = `Ci (ppm)`, 
         Trans_at_A_sat_mmol_per_m2_s1 = `Trmmol (mmol m-2 s-1)`) -> A_sat_means

gas_exchange %>%
  select(-"VpdL (kPa)", -"Area (cm2)", -"StmRat", -"BLCond", -"Tair", -"Tleaf", -"TBlk", -"CO2S", -"H2OR", -"H2OS", 
         -"RHR", -"RHS", -"Flow", -"PARi") %>%
  group_by(Species, Date, Temp, CO2, Potnum, MeasTemp) %>%
  top_n(1, CO2R) %>%
  rename(A_max_umol_per_m2_s1 = `Photo (umol m-2 s-1)`, Cond_at_A_max_mmol_per_m2_s1 = `Cond (mol m-2 s-1)`, Ci_at_A_max_ppm = `Ci (ppm)`, 
         Trans_at_A_max_mmol_per_m2_s1 = `Trmmol (mmol m-2 s-1)`) -> A_max_values

read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_Harvest150DAP_20080400_L1.csv") -> harvest_150


read_csv("data/Ghannoum_2010/raw/GHS30_Euc1-SxTxCO2xW_Harvest80DAP_20080200_L1.csv") -> harvest_80 


A_sat_means %>%
  full_join(A_max_values, by = c("Species", "Date", "Temp", "CO2", "Potnum", "MeasTemp")) %>%
  full_join(biochem, by = c("Species", "Temp", "CO2", "Potnum")) %>% 
  full_join(harvest_80, by = c("Species", "Temp", "CO2", "Potnum")) %>%
  View()