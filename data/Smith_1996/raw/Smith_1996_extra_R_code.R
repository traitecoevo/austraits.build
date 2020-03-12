read_csv("data/Smith_1996/raw/site_data.csv") -> site_data

#leaf_chlorophyll_per_dry_mass = ((17.9*A647) - (8.08*A664))/(FW.CHL*VOL.CHL*0.0001),

read_csv("data/Smith_1996/raw/data_raw_separate_by_individual.csv",guess_max = 50000) %>%
  mutate(leaf_chlorophyll_A_per_dry_mass = (10000/893.51)*(((12.7*A664) - (2.79*A647))*VOL.CHL*0.0001/FW.CHL),
         leaf_chlorophyll_B_per_dry_mass = (10000/907.47)*(((20.7*A647) - (4.62*A664))*VOL.CHL*0.0001/FW.CHL),
         leaf_chlorophyll_per_dry_mass2 = (10000/900)*((17.9*A647) - (8.08*A664))*VOL.CHL*0.0001/FW.CHL,
         leaf_chlorophyll_per_dry_mass = leaf_chlorophyll_A_per_dry_mass + leaf_chlorophyll_B_per_dry_mass,
         chlorophyll_A_B_ratio = leaf_chlorophyll_A_per_dry_mass/leaf_chlorophyll_B_per_dry_mass) %>%
  mutate(count_leaves = as.numeric(NPSU),
         leaf_area = as.numeric(AREA),
         stalk_dry_mass = as.numeric(WT.STALK)) %>% 
  mutate(leaf_fresh_mass = ifelse(is.na(count_leaves),NA,FRESH.WT/count_leaves),
         leaf_dry_mass = ifelse(is.na(count_leaves),NA,DRY.WT/count_leaves),
         leaf_dry_matter_content = ifelse(is.na(leaf_dry_mass)|is.na(leaf_fresh_mass),NA,
                                          leaf_dry_mass/leaf_fresh_mass),
         leaf_water_content_per_mass = (leaf_fresh_mass-leaf_dry_mass)/leaf_dry_mass,
         support_fraction = stalk_dry_mass  / (stalk_dry_mass + DRY.WT)) -> individual_measurements

individual_measurements %>%
  select(species,site,site2,species_code,contextual,leaf_area,leaf_dry_mass) %>%
  group_by(species,site,site2,species_code,contextual) %>%
  summarise_all(.funs = mean, na.rm = TRUE) %>%
  ungroup() %>% 
  mutate(specific_leaf_area = leaf_area/leaf_dry_mass) %>%
  select(-leaf_area,-leaf_dry_mass) -> site_SLA

individual_measurements %>%
  bind_rows(site_SLA) %>%
  write_csv("data/Smith_1996/data.csv")


