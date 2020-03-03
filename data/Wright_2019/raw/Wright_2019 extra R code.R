#read in list of full species names matched to 6-letter abbreviations
read_csv("data/Wright_2019/raw/species_names.csv") -> name_match

#old method
#read_csv("data/Wright_2019/raw/May_11_gas_exchange.csv") %>%
#  select(Species, spp) %>%
#  distinct(Species, spp) -> name_match

#read in Sep SLA data and add full names
read_csv("data/Wright_2019/raw/Sep10_raw_SLA.csv") %>%
  rename(leaf_area_mm2 = LfSize_mm2) %>%
  mutate(leaf_mass_mg = DryMass_mg / `n(lvs)`) %>%
  select(-area_mm2, - DryMass_mg, - spp_age, - `vial#`, -`n(lvs)`, -ID) %>%
  mutate(Rep = as.character(Rep)) -> Sep_SLA

#read in Sep stem mass
read_csv("data/Wright_2019/raw/Sep10_raw_stem_mass.csv") %>%
  select(spp, age, Rep, date, `LM/SM80`, LMF80)  %>%
  mutate(Rep = as.character(Rep)) -> Sep_stem_mass

#read in Sep WD data and merge into Sept SLA
read_csv("data/Wright_2019/raw/Sep10_raw_WD.csv") %>%
  select(sample_date, spp, age, Rep, date, WD) %>%
  mutate(Rep = as.character(Rep)) %>%
  mutate(WD = WD*1000) %>%
  rename(`WD kg/m3` = WD) %>%
  full_join(Sep_SLA,by=c("age","spp","Rep","date")) %>%
  full_join(Sep_stem_mass,by=c("age","spp","Rep", "date")) %>%
  mutate(duplicate = 1) -> Sept_data

#read in May SLA data
#making the assumption that leaf area and mass divided by 5 because 5 leaves collected per species
read_csv("data/Wright_2019/raw/May_11_raw_SLA.csv") %>%
  mutate(leaf_mass_mg = `Dry Mass (g)`*1000/5,
         SLAmm2mg = `SLA (m2/kg1)`,
         leaf_area_mm2 = `Total Area (cm2)`*100/5) %>%
  select(spp, age, Rep, date, notes, leaf_mass_mg, SLAmm2mg, leaf_area_mm2) %>%
  mutate(duplicate = 1) -> May_SLA

#read in bark and wood isotope data, micronutrient data
read_csv("data/Wright_2019/raw/isotope_bark.csv") %>%
select(spp, age, Rep, date, duplicate, d13C_bark, `C%_bark`, `d15N_bark`, `N%_bark`) -> bark_isotope

read_csv("data/Wright_2019/raw/isotope_wood.csv") %>%
  select(spp, age, Rep, date, duplicate, d13C_wood, `C%_wood`, `d15N_wood`, `N%_wood`) -> wood_isotope

read_csv("data/Wright_2019/raw/May_nutrients.csv") %>%
  mutate(`P%` = P/10000) %>%
  select(-Species_age, -`Vial # for N`, -`Sample MQN #`, -P) %>%
  mutate(duplicate = 1) %>%
  rename(calcium = Ca) -> micronutrients

#read in May WD data
read_csv("data/Wright_2019/raw/May_11_raw_WD.csv") %>%
  select(spp, age, Rep, date, date_WD, `WD kg/m3`, `Bark thickness (mm)`, `Bark area (cm2)`, `Pith diam (mm)`) %>%
  mutate(duplicate = 1) -> May_WD

#read in May gas exchange data
read_csv("data/Wright_2019/raw/May_11_gas_exchange.csv") %>%
  select(-Owner, -Reference, -Site, -Latitude, -Longitude, -Species, -ID, -logPhoto, -logAmass, -log_gs, -`DELETE?`) %>% 
  mutate(Rep = as.character(Rep)) %>%
  mutate(duplicate = 1) -> May_gas

#merge together May files
May_SLA %>%
  full_join(bark_isotope, by=c("age","spp","Rep","date","duplicate")) %>%
  full_join(wood_isotope, by=c("age","spp","Rep","date","duplicate")) %>%
  full_join(micronutrients, by=c("age","spp","Rep","date","duplicate")) %>%
  full_join(May_gas, by=c("age","spp","Rep","date","duplicate")) %>%
  full_join(May_WD, by=c("age","spp","Rep","date","duplicate")) -> May_ind_data

read_csv("data/Wright_2019/raw/isotope_leaf.csv") %>%
  select(-`Genus species`) %>%
  rename(LMA_leaf_isotope_gm2 = `LMA(g/m2)`) -> May_spp_data
  
Sept_data %>%
  bind_rows(May_ind_data) %>%
  bind_rows(May_spp_data) %>%
  full_join(name_match,by="spp") %>%
  subset(!is.na(spp)) %>%
  subset(!is.na(date)) %>%
  write_csv("data/Wright_2019/data.csv") -> test


#read in May species level data - now using individual files instead, 
#but this is still the only file with data on pith, sapwood, etc. fractions
read_csv("data/Wright_2019/raw/May_11_species_level.csv") %>%
  select(-Species, -`n(Amax)`, -`Amass(nmol/g/s)`, -`Aarea(umol/m2/s)`, 
         -`Gs(mmol/m2/s)`, -`Ci(ppm)`, -`Ci/Ca`, -`VpdL`, -`Ctleaf(oC)`) %>%
  mutate(Parea_g_m2 = `leafP(%)`*`gasSLA(mm2/mg)(photosynth leaves only)`,
         Narea_g_m2 = `leafN(%)`*`gasSLA(mm2/mg)(photosynth leaves only)`) %>%
  rename(`C%` = `leaf C(%)`, `N%` = `leafN(%)`, `P%` = `leafP(%)`, 
         `Bark thickness (mm)` = `Bark thickness (mm), of 1cm diam stem`)
-> May_species_level