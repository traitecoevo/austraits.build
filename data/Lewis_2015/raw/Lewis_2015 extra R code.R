read_csv("data/Lewis_2015/raw/GHS30_WollemiTxCO2_GXAsat_20081125-20090623_L1.csv") %>%
  rename(date_gas_exchange = `Date`) %>%
  mutate(date_gas_exchange = as.Date(date_gas_exchange, format = "%d/%m/%Y")) %>%
  mutate(Species = "Wollemia nobilis") -> Lewis_gas_exchange

read_csv("data/Lewis_2015/raw/GHS30_WollemiTxCO2_Harvest_20100503_L1.csv") %>%
  mutate(leaf_area_ratio = `TotalLA (cm-2)`/ (`TotalLeafDW (g)`+`StemDW (g)`)) -> Lewis_harvest

read_csv("data/Lewis_2015/raw/GHS30_WollemiTxCO2_Isotopes_20100503_L1_edited.csv") %>%
  select(-Species) %>%
  rename(date_isotope_analysis = Date) %>%
  mutate(date_isotope_analysis = as.Date(date_isotope_analysis, format = "%d/%m/%Y")) -> Lewis_isotopes

Lewis_isotopes %>%
  subset(Organ == "Leaves") %>%
  rename(leaf_percentN = percentN, leaf_d15N = d15N, leaf_CN = CN, leaf_percentC = percentC,
         leaf_d13C = d13C) %>%
  select(-Organ) -> Lewis_leaf_isotopes

Lewis_isotopes %>%
  subset(Organ == "Stem") %>%
  rename(stem_percentN = percentN, stem_d15N = d15N, stem_CN = CN, stem_percentC = percentC,
         stem_d13C = d13C) %>%
  select(-Organ) -> Lewis_stem_isotopes

Lewis_isotopes %>%
  subset(Organ == "Roots") %>%
  rename(root_percentN = percentN, root_d15N = d15N, root_CN = CN, root_percentC = percentC,
         root_d13C = d13C) %>%
  select(-Organ) -> Lewis_root_isotopes

Lewis_leaf_isotopes %>%
  full_join(Lewis_stem_isotopes,by=c("date_isotope_analysis","Temp","CO2","Potnum")) %>%
  full_join(Lewis_root_isotopes,by=c("date_isotope_analysis","Temp","CO2","Potnum")) -> Lewis_isotopes_wide

read_csv("data/Lewis_2015/raw/GHS30_WollemiTxCO2_LeafAnatomy_2008-2010_L1.csv") %>%
  select(-Species) -> Lewis_anatomy

Lewis_gas_exchange %>%
  arrange(rev(date_gas_exchange)) %>%
  full_join(Lewis_isotopes_wide,by=c("Temp","CO2","Potnum")) %>%
  full_join(Lewis_harvest,by=c("Temp","CO2","Potnum")) %>%
  group_by(Potnum) %>%
  mutate_at(vars(date_isotope_analysis, leaf_percentN, leaf_d15N, leaf_CN, leaf_percentC, leaf_d13C, stem_percentN, stem_d15N, 
  root_d15N, root_CN, root_percentC, root_d13C, `StemFW (g)`, `InitialLeafLA (cm-2)`, `NewLeafLA (cm-2)`, `TotalLA (cm-2)`, 
  `InitialLeafFW (g)`, `NewLeafFW (g)`, `TotalLeafFW (g)`, `StemDW (g)`, `InitialLeafDW (g)`, `NewLeafDW (g)`, `TotalLeafDW (g)`, 
  `LMA (g m-2)`, `leaf_area_ratio`),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  bind_rows(Lewis_anatomy) %>%
  mutate(Species = "Wollemia nobilis") %>%
  mutate(PAR_round = round(PARi,digits = -2)) %>%
  mutate(PAR_desc = if_else(is.na(PARi),"",paste("measured_at_",PAR_round,"_PAR",sep=""))) %>%
  mutate(context = if_else(is.na(GrowthPeriod),paste(Temp,"_Temp_and",CO2,"_ppm_CO2",sep=""),
                           paste(Temp,"_Temp_and",CO2,"_ppm_CO2_at_growth_period_",GrowthPeriod,sep=""))) %>%
  mutate(context = if_else(is.na(PARi),context,paste(context,"_and_",PAR_desc,sep=""))) %>% View()
  write_csv("data/Lewis_2015/data.csv")


###In the spreadsheet with isotope data, both leaf and stem samples list pot 306. However, the leaf sample indicates this pot is at CO2 = 280 and the stem sample lists CO2 = 400. I've assumed a pot number refers to the same plant.
###Also pot 301 list Elevation temperature for the leaf sample and Ambient temperature for the stem sample.


