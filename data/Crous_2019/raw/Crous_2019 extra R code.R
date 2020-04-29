read_csv("data/Crous_2019/raw/RingMeans_Litter.csv") %>%
  rename(P_litter = Perc.P, N_litter = Perc.N, NP_litter = NP, CP_litter = CP, CN_litter = CN, CO2.treat = CO2Treat) -> litter

read_csv("data/Crous_2019/raw/FACE_P0020_RA_NPwood_2015-L2.csv") %>%
  rename(CO2.treat = CO2Treat, P_wood = "Perc.P.mean", N_wood = "Perc.N.mean", C_wood = "Perc.C.mean", NP_wood = "NP.mean", CP_wood = "CP.mean",
         CN_wood = "CN.mean") -> wood


read_csv("data/Crous_2019/raw/EucFACE_Austraits_NPleaf_2012-2018-L3.csv") %>%
  bind_rows(litter) %>%
  bind_rows(wood) %>%
  mutate(context = paste(Age,"_leaves_measured_on_plants_in_",CO2.treat,"_CO2_treatment"),
         site_name = "EucFACE",
         species_name = "Eucalyptus tereticornis") %>%
  write_csv("data/Crous_2019/data.csv") %>%
  distinct(context,.keep_all = TRUE) %>%
  select(context, Age, CO2.treat) %>%
  write_csv("data/Crous_2019/raw/context.csv")
  
read_csv("data/Crous_2019/raw/context.csv") -> context_table


