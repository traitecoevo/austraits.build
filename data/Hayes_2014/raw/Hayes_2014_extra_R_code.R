read_csv("data/Hayes_2014/raw/Hayes_et_al_2014_site.data.csv") %>%
  select(-`bag.id`,-`sample.id`,-`dune`,-`subplot`,-`date`,-`type`) %>%
  group_by(plot.id) %>%
  summarise_all(.funs=mean) -> site_data_numbers

read_csv("data/Laliberte_2012/site_data.csv") %>%
  rename(plot.id = plot) %>%
  select(-dune,-date) -> Laliberte_sites

read_csv("data/Hayes_2014/raw/Hayes_et_al_2014_site.data.csv") %>%
  select(`plot.id`,`bag.id`,`sample.id`,`dune`,`subplot`,`date`,`type`) %>%
  group_by(plot.id) %>%
  summarise_all(.funs=first) %>%
  left_join(site_data_numbers,by="plot.id") %>%
  left_join(Laliberte_sites,by="plot.id") %>%
  rename(`latitude (deg)`= lat,`longitude (deg)` = long)-> site_data



read_csv("data/Hayes_2014/raw/Hayes_et_al_2014_data.csv") %>%
  subset(state=="senesced") %>%
  select(-dune.system,-uptake.strategy,-specific.uptake.strategy,-collection.date,-`C_%`,-CN,-state) %>%
  rename(senesced_leaf_Ca = Ca_ug.g.DW, senesced_leaf_Cu = Cu_ug.g.DW, senesced_leaf_Fe = Fe_ug.g.DW, 
         senesced_leaf_K = K_ug.g.DW, senesced_leaf_Mg = Mg_ug.g.DW, senesced_leaf_Mn = Mn_ug.g.DW, 
         senesced_leaf_Mo = Mo_ug.g.DW, senesced_leaf_P = P_ug.g.DW, senesced_leaf_S = S_ug.g.DW, 
         senesced_leaf_Zn = Zn_ug.g.DW, senesced_leaf_N = N_ug.g.DW) %>%
  mutate(species = gsub("\\(.*","",species)) -> senesced

read_csv("data/Hayes_2014/raw/Hayes_et_al_2014_data.csv") %>%
  subset(state=="mature") %>%
  select(-state) %>%
  mutate(species = gsub("\\(.*","",species)) %>%
  left_join(senesced, by= c("plot.id","species")) %>%
  mutate(specific.uptake.strategy = ifelse(species=="Acacia spathulifolia","AM/N fixing", specific.uptake.strategy)) %>%
  mutate(specific.uptake.strategy = ifelse(species=="Acacia rostellifera ","AM/N fixing", specific.uptake.strategy)) %>%
  mutate(specific.uptake.strategy = ifelse(species=="Hardenbergia comptoniana","AM/N fixing", specific.uptake.strategy)) %>%
  mutate(specific.uptake.strategy = ifelse(species=="Jacksonia floribunda ","AM/EM/N fixing", specific.uptake.strategy)) %>%
  mutate(specific.uptake.strategy = ifelse(duplicated(species), NA, specific.uptake.strategy)) %>%
  write_csv("data/Hayes_2014/data.csv")
  
  
  


notes on species mycorrhizal reassignments:
Acacia spathulifolia = "arbuscular_mycorrhizal" (confirmed for species in the study area, Zemunik et al., 2015)
Acacia rostellifera = "arbuscular_mycorrhizal" (confirmed for genus in south-western Australia, Jasper et al. 1987, Brundrett & Abbott 1991)
Hardenbergia comptoniana = "arbuscular_mycorrhizal" (confirmed for species in south-western Australia, Brundrett & Abbott 1991)
Jacksonia floribunda = "arbuscular_mycorrhizal ectomycorrhizal" (confirmed for species in the study area, Zemunik et al., 2015)
