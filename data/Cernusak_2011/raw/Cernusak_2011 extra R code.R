read_csv("data/Cernusak_2011/raw/full_curve_data.csv") %>%
  subset(`Curve routine` == "A/Ci" & `Step in curve` == 1) %>%
  select("Site", "Species", "Leaf number", "Canopy or understory", "Photosynthesis (umol m-2 s-1)",
         "Conductance to H2O (mol m-2 s-1)", "Ci (umol mol-1)", "Transpiration (mmol m-2 s-1)", "CO2 sample (umol mol-1)") %>%
  rename(`leaf no` = "Leaf number", `Asat_umol_m-2_s-1` = "Photosynthesis (umol m-2 s-1)", `g_at_Asat_mol_m-2_s-1` = "Conductance to H2O (mol m-2 s-1)", 
         `ci_at_Asat_umol_mol-1` = "Ci (umol mol-1)", `trans_at_Asat_mmol_m-2_s-1` = "Transpiration (mmol m-2 s-1)", 
         CO2_sample = "CO2 sample (umol mol-1)") %>%
  mutate(ci_over_ca_at_Asat = `ci_at_Asat_umol_mol-1` / CO2_sample) -> first_step_Aci

read_csv("data/Cernusak_2011/raw/full_curve_data.csv") %>%
  subset(`Curve routine` == "A/Ci") %>%
  group_by(Species,Site,`Leaf number`) %>%
  summarise_all(.funs = last) %>%
  ungroup() %>% 
  select("Site", "Species", "Leaf number", "Canopy or understory", "Photosynthesis (umol m-2 s-1)",
         "Conductance to H2O (mol m-2 s-1)", "Ci (umol mol-1)", "Transpiration (mmol m-2 s-1)") %>%
  rename(`leaf no` = "Leaf number", `Amax_umol_m-2_s-1` = "Photosynthesis (umol m-2 s-1)", `g_at_Amax_mol_m-2_s-1` = "Conductance to H2O (mol m-2 s-1)", 
         `ci_at_Amax_umol_mol-1` = "Ci (umol mol-1)", `trans_at_Amax_mmol_m-2_s-1` = "Transpiration (mmol m-2 s-1)") -> last_step_Aci

read_csv("data/Cernusak_2011/raw/leaf_trait_data_original.csv") %>%
  rename(Species = `Species 1`) %>%
  full_join(first_step_Aci,by=c("Species","Site","leaf no")) %>%
  full_join(last_step_Aci,by=c("Species","Site","leaf no")) %>% 
  mutate(`Asat_umol_m-2_s-1` = ifelse(is.na(`Asat_umol_m-2_s-1`),`initial photosynthesis (umol m-2 s-1)`,`Asat_umol_m-2_s-1`),
         `g_at_Asat_mol_m-2_s-1` = ifelse(is.na(`g_at_Asat_mol_m-2_s-1`),`initial conductance (mol m-2 s-1)`,`g_at_Asat_mol_m-2_s-1`),
         ci_over_ca_at_Asat = ifelse(is.na(ci_over_ca_at_Asat),`initial ci/ca`,ci_over_ca_at_Asat)) %>%
  write_csv("data/Cernusak_2011/data.csv")