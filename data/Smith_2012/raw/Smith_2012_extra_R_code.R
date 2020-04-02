read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_LeafBiochemistry_20090504-20090511_L1.csv") %>%
  mutate(Replicate = 1) -> biochem

read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_GasExchangeAsatAmax_20090504-20090511_L1.csv") %>%
  mutate(max_vs_sat = ifelse(CO2R>800,"max","sat")) %>%
  select(-`VpdL (kPa)`,-`Area (cm2)`, -`StmRat`, -Tair, -Tleaf, -TBlk, -CO2S, -H2OR, -H2OS, -RHR, -RHS, -Flow, -PARi) %>%
  group_by(Species, Temp, CO2, Potnum, max_vs_sat) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  select(-CO2R) %>%
  mutate(Replicate = 1) -> gas_exchange

gas_exchange %>%
  subset(max_vs_sat == "max") %>%
  select(-max_vs_sat) %>%
  rename(Photo_Amax_umol_per_m2_s = "Photo (umol m-2 s-1)", 
         gs_at_Amax_mol_per_m2_s = "Cond (mol m-2 s-1)", 
         ci_at_Amax_umol_per_mol = "Ci (umol mol-1)", 
         transpiration_at_A_max_mmol_per_m2_s = "Trmmol (mmol m-2 s-1)") -> gas_exchange_max

gas_exchange %>%
  subset(max_vs_sat == "sat") %>%
  select(-max_vs_sat) %>%
  rename(Photo_Asat_umol_per_m2_s = "Photo (umol m-2 s-1)", 
         gs_at_Asat_mol_per_m2_s = "Cond (mol m-2 s-1)", 
         ci_at_Asat_umol_per_mol = "Ci (umol mol-1)", 
         transpiration_at_A_sat_mmol_per_m2_s = "Trmmol (mmol m-2 s-1)") -> gas_exchange_sat
    
read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_LeafAnatomy_20090504-20090511_L1.csv") %>%
  group_by(Species, Temp, CO2, Potnum, Replicate) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  full_join(biochem, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  full_join(gas_exchange_max, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  full_join(gas_exchange_sat, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  mutate(context = paste(Temp,"_temperature_and_",CO2,"_ppm_CO2",sep="")) %>%
  mutate(Species = "Eucalyptus sideroxylon") %>%
  write_csv("data/Smith_2012/data.csv") -> for_context

for_context %>%
  distinct(context, .keep_all = TRUE) %>%
  select(context, Temp, CO2) %>%
  mutate(type = "treatment",
         description = paste("plants grown at ",CO2,"ppm CO2 and ambient temperature",sep="")) %>% 
  write_csv("data/Smith_2012/raw/context_table.csv") -> context_table

