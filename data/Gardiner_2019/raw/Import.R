
source("R/austraits.R")
library(dplyr)
library(readr)

data_raw.leaf<- read_csv("data/Gardiner_2019/raw/Oxley_seedling_leaf_traits_28_08_17.csv", col_types = cols()) 




data.leaf <-
  data_raw.leaf %>% 
  select(species_name =`species_code`,  individual = `replicate`,fresh_mass_g, leaf_area_mm2, dry_mass_mg, leaflet_area_mm2, sla_mm2_mg, ldmc ) %>%
  na_if("sample_missing") %>% 
  #mutate(leaf_area = as.numeric(leaf_area)) %>%
  na.omit() %>%
  group_by(species_name, individual) %>%
  summarise(fresh_mass_g = mean(fresh_mass_g), leaf_area_mm2 = mean(leaf_area_mm2), dry_mass_mg = mean(dry_mass_mg), leaflet_area_mm2 = mean(leaflet_area_mm2), sla_mm2_mg = mean(sla_mm2_mg), ldmc = mean(ldmc), replicates = n()) %>% 
  ungroup()



data_raw.wood <- read_csv("data/Gardiner_2019/raw/Oxley_seedling_wood_&_root_traits_28_08_17.csv", col_types = cols()) %>% select(species_name = `species_code`, individual = `replicate`, stem_dry_mass_mg, stem_volume_mm3, wood_density_mg_mm3, root_dry_mass_g, root_dry_mass_mg,	root_length_m,	root_volume_mm3,	srl_m_g,	rtd_mg_mm3)  
data.leaf %>% 
  full_join(by = c("species_name", "individual"),
            data_raw.wood
  ) %>% 
  write_csv("data/Gardiner_2019/data.csv", na = "")






#data_raw.height %>% list("Ceratopetalum gummiferum" = "Ceratopetalum gummifera", "Pultenaea daphnoidesÊ" = "Pultenaea daphnoides") %>% mutate_at( "species_name", recode, !!!spname)
