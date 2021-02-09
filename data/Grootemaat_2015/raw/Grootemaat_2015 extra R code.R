read_csv("data/Grootemaat_2015/raw/species.csv") -> species_list

read_csv("data/Grootemaat_2015/raw/data_raw.csv") %>%
  rename(Abbreviation = species) %>%
  left_join(species_list,by="Abbreviation") %>%
  mutate(taxon = paste0(Genus," ",Species),
         site_conditions = paste0(`Rainfall a`," rainfall ",`Soil P b`," soil P"),
         site_name = site_conditions) %>%
  mutate(site_name = gsub("low rainfall high soil P", "Round Hill woodland", site_name),
         site_name = gsub("low rainfall low soil P", "Round Hill mallee", site_name),
         site_name = gsub("high rainfall high soil P", "Kuring-gai NP hiP", site_name),
         site_name = gsub("high rainfall low soil P", "Kuring-gai NP lowP", site_name),
         site_name = gsub("NA rainfall NA soil P","unknown site",site_name)) %>% 
  mutate(`length (mm)` = ifelse(`material`=="FRESH",`length (mm)`,NA),
         `width (mm)` = ifelse(`material`=="FRESH",`width (mm)`,NA),
         `thickness (mm)` = ifelse(`material`=="FRESH",`thickness (mm)`,NA),
         `actual mass (g)` = ifelse(`material`=="FRESH",`actual mass (g)`,NA),
         `dry mass (g)` = ifelse(`material`=="FRESH",`dry mass (g)`,NA),
         `SA (cm2)` = ifelse(`material`=="FRESH",`SA (cm2)`,NA),
         `density (dry mass) (g/cm3)` = ifelse(`material`=="FRESH",`density (dry mass) (g/cm3)`,NA),
         `FMC  (% odw)` = ifelse(`material`=="FRESH",`FMC  (% odw)`,NA),
         `lignin (%mass)` = ifelse(`material`=="FRESH",NA,`lignin (%mass)`),
         senesced_leaf_N_per_dry_mass = ifelse(`material`=="SENESCED",`N (%mass)`,NA),
         senesced_leaf_P_per_dry_mass = ifelse(`material`=="SENESCED",`P (%mass)`,NA),
         `N (%mass)` =  ifelse(`material`=="DRIED",`N (%mass)`,NA),
         `P (%mass)` =  ifelse(`material`=="DRIED",`P (%mass)`,NA)
         ) %>%
  mutate(SLA = `SA (cm2)`/`dry mass (g)`) %>% View()
  write_csv("data/Grootemaat_2015/data.csv")