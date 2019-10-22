read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_LeafBiochemistry_20090504-20090511_L1.csv") %>%
  mutate(Replicate = 1) -> biochem

read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_GasExchangeAsatAmax_20090504-20090511_L1.csv") %>%
  mutate(max_vs_sat = ifelse(800>CO2R,"max","sat")) %>%
  group_by(Species, Temp, CO2, Potnum, max_vs_sat) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(Replicate = 1) -> gas_exchange

gas_exchange %>%
  subset(max_vs_sat == "max") -> gas_exchange_max

gas_exchange %>%
  subset(max_vs_sat == "sat") %>%
  select(Species, Temp, CO2, Potnum, Replicate, `Photo (umol m-2 s-1)`, `Cond (mol m-2 s-1)`, `Ci (umol mol-1)`,`Trmmol (mmol m-2 s-1)`) %>%
  rename(Photosynthesis_saturated = `Photo (umol m-2 s-1)`, Conductance_saturated =  `Cond (mol m-2 s-1)`, 
         Ci_saturated = `Ci (umol mol-1)`,Transpiration_saturated = `Trmmol (mmol m-2 s-1)`) -> gas_exchange_sat
    
read_csv("data/Smith_2012/raw/GHS30_Euc2-SxTxCO2xW_LeafAnatomy_20090504-20090511_L1.csv") %>%
  group_by(Species, Temp, CO2, Potnum, Replicate) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  full_join(biochem, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  full_join(gas_exchange_max, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  full_join(gas_exchange_sat, by=c("Species", "Temp", "CO2", "Potnum", "Replicate")) %>%
  mutate(treatment_name = paste(Temp,CO2,Potnum,Replicate,sep="_")) %>%
  mutate(Species = "Eucalyptus sideroxylon") %>%
  write_csv("data/Smith_2012/data.csv") %>%
  distinct(treatment_name, .keep_all = TRUE) %>%
  select(treatment_name, Temp, CO2, Potnum, Replicate) %>%
  write_csv("data/Smith_2012/site_treatment.csv") -> treatments


