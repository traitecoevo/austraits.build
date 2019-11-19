read_csv("data/Hayes_2014/Hayes_et_al_2014_site.data.csv") %>%
  select(-`bag.id`,-`sample.id`,-`dune`,-`subplot`,-`date`,-`type`) %>%
  group_by(plot.id) %>%
  summarise_all(.funs=mean) -> site_data_numbers

read_csv("data/Hayes_2014/Hayes_et_al_2014_site.data.csv") %>%
  select(`plot.id`,`bag.i
d`,`sample.id`,`dune`,`subplot`,`date`,`type`) %>%
  group_by(plot.id) %>%
  summarise_all(.funs=first) %>%
  left_join(site_data_numbers,by="plot.id") -> site_data

read_csv("data/Hayes_2014/Hayes_et_al_2014_data.csv") %>%
  subset(state=="senesced") %>%
  select(-dune.system,-uptake.strategy,-specific.uptake.strategy,-collection.date,-`C_%`,-CN,-state) %>%
  rename(senesced_leaf_Ca = Ca_ug.g.DW, senesced_leaf_Cu = Cu_ug.g.DW, senesced_leaf_Fe = Fe_ug.g.DW, 
         senesced_leaf_K = K_ug.g.DW, senesced_leaf_Mg = Mg_ug.g.DW, senesced_leaf_Mn = Mn_ug.g.DW, 
         senesced_leaf_Mo = Mo_ug.g.DW, senesced_leaf_P = P_ug.g.DW, senesced_leaf_S = S_ug.g.DW, 
         senesced_leaf_Zn = Zn_ug.g.DW, senesced_leaf_N = N_ug.g.DW) -> senesced
         names(senesced)
View(senesced)
