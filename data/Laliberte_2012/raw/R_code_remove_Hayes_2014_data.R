read_csv("data/Hayes_2014/data.csv") -> Hayes_2014_data

read_csv("data/Laliberte_2012/data_with_Hayes_2014_duplicates.csv") %>%
  rename(plot.id = plot) %>%
  
  mutate(species = gsub("\\(.*","",species)) %>%
  mutate(species = gsub(" $","",species)) -> Laliberte_2012_data

Hayes_2014_data %>%
  full_join(Laliberte_2012_data, by = c("plot.id","species")) -> test

test %>%
  mutate(P_mature = ifelse(round(P_mature,4) == round(P_ug.g.DW,4),"",P_mature),
        Ca_mature = ifelse(round(Ca_mature,4) == round(Ca_ug.g.DW,4),"",Ca_mature),
        N_mature = ifelse(round(N_mature,4) == round(N_ug.g.DW,4),"",Na_mature),
        K_mature = ifelse(round(K_mature,4) == round(K_ug.g.DW,4),"",K_mature),
        Cu_mature = ifelse(round(Cu_mature,4) == round(Cu_ug.g.DW,4),"",Cu_mature),
        Mg_mature = ifelse(round(Mg_mature,4) == round(Mg_ug.g.DW,4),"",Mg_mature),
        Mn_mature = ifelse(round(Mn_mature,4) == round(Mn_ug.g.DW,4),"",Mn_mature),
        C_mature = ifelse(round(C_mature,4) == round(`C_%`,4),"",C_mature),
        Zn_mature = ifelse(round(Zn_mature,4) == round(Zn_ug.g.DW,4),"",Zn_mature),
        N_mature = ifelse(round(as.numeric(N_mature,4)) == round(N_ug.g.DW,4),"",N_mature),
        S_mature = ifelse(round(S_mature,4) == round(S_ug.g.DW,4),"",S_mature),
        Mo_mature = ifelse(round(Mo_mature,4) == round(Mo_ug.g.DW,4),"",Mo_mature),
        Fe_mature = ifelse(round(Fe_mature,4) == round(Fe_ug.g.DW,4),"",Fe_mature),
        senesced_leaf_K = ifelse(round(senesced_leaf_K,4) == round(K_senesced,4),"",senesced_leaf_K),
        senesced_leaf_P = ifelse(round(senesced_leaf_P,4) == round(P_senesced,4),"",senesced_leaf_P),
        senesced_leaf_N = ifelse(round(senesced_leaf_N,4) == round(N_senesced,4),"",senesced_leaf_N)) %>%
  select(-`Ca_ug.g.DW`, -`Cu_ug.g.DW`, -`Fe_ug.g.DW`, -`K_ug.g.DW`, -`Mg_ug.g.DW`,
         -`Mn_ug.g.DW`, -`Mo_ug.g.DW`, -`P_ug.g.DW`, -`S_ug.g.DW`, -`Zn_ug.g.DW`,
         -`N_ug.g.DW`, -`C_%`, -`CN`, -`senesced_leaf_Ca`, -`senesced_leaf_Cu`, -`senesced_leaf_Fe`,
         -`senesced_leaf_K`, -`senesced_leaf_Mg`, -`senesced_leaf_Mn`, -`senesced_leaf_Mo`, -`senesced_leaf_P`,
         -`senesced_leaf_S`, -`senesced_leaf_Zn`, -`senesced_leaf_N`) %>%
  write_csv("data/Laliberte_2012/data_new.csv")