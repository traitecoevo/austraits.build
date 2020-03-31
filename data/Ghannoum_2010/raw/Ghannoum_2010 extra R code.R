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

unique(test$context)

[1] "grown_at_Amb_temp_and_640_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[2] "grown_at_Elv_temp_and_280_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[3] "grown_at_Amb_temp_and_280_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[4] "grown_at_Amb_temp_and_400_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[5] "grown_at_Elv_temp_and_400_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[6] "grown_at_Elv_temp_and_400_ppm_CO2_and_measurements_made_at_28_deg_C"                          
[7] "grown_at_Elv_temp_and_640_ppm_CO2_and_measurements_made_at_28_deg_C_and_harvested_at_150_days"
[8] "grown_at_Amb_temp_and_640_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[9] "grown_at_Elv_temp_and_280_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[10] "grown_at_Amb_temp_and_280_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[11] "grown_at_Amb_temp_and_400_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[12] "grown_at_Elv_temp_and_400_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[13] "grown_at_Elv_temp_and_640_ppm_CO2_and_measurements_made_at_34_deg_C"                          
[14] "grown_at_Amb_temp_and_400_ppm_CO2_and_harvested_at_80_days"                                   
[15] "grown_at_Amb_temp_and_280_ppm_CO2_and_harvested_at_80_days"                                   
[16] "grown_at_Amb_temp_and_640_ppm_CO2_and_harvested_at_80_days"                                   
[17] "grown_at_Elv_temp_and_400_ppm_CO2_and_harvested_at_80_days"                                   
[18] "grown_at_Elv_temp_and_280_ppm_CO2_and_harvested_at_80_days"                                   
[19] "grown_at_Elv_temp_and_640_ppm_CO2_and_harvested_at_80_days"                                   
[20] "grown_at_Amb_temp_and_400_ppm_CO2_and_harvested_at_150_days"                                  
[21] "grown_at_Amb_temp_and_280_ppm_CO2_and_harvested_at_150_days"                                  
[22] "grown_at_Amb_temp_and_640_ppm_CO2_and_harvested_at_150_days"                                  
[23] "grown_at_Elv_temp_and_400_ppm_CO2_and_harvested_at_150_days"                                  
[24] "grown_at_Elv_temp_and_280_ppm_CO2_and_harvested_at_150_days"                                  
[25] "grown_at_Elv_temp_and_640_ppm_CO2_and_harvested_at_150_days"                                  
[26] "grown_at_Elv_temp_and_280_ppm_CO2" 