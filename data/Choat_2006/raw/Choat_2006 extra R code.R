read_csv("data/Choat_2006/raw/ManyPeaksRangeXylem_Choat.csv") %>% 
  group_by(Species,Tree,Phenology) %>%
  summarise(Diameter = mean(Diameter, na.rm=TRUE),
            count_xylem_diameter = n()) %>%
  ungroup() %>%
  mutate(Species = gsub("A. bidwillii","Gossia bidwillii",Species),
         Species = gsub("A. excelsa","Alphitonia excelsa", Species),
         Species = gsub("B. australis","Brachychiton australis",Species),
         Species = gsub("C. gillivraei","Cochlospermom gillivraei",Species)) -> xylem

read_csv("data/Choat_2006/raw/ManyPeaksRangeVesselDensity_Choat.csv") %>%
  group_by(Species,Tree) %>%
  summarise(`Ves Count` = mean(`Ves Count`, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(Species = gsub("A. bidwillii","Gossia bidwillii",Species),
         Species = gsub("A. excelsa","Alphitonia excelsa", Species),
         Species = gsub("B. australis","Brachychiton australis",Species),
         Species = gsub("C. gillivraei","Cochlospermom gillivraei",Species)) %>%
  full_join(xylem) %>%
  rename(Plant = Tree) -> vessel_density

read_csv("data/Choat_2006/raw/ManyPeaksRangePsi_Choat.csv") %>%
  mutate(WP = as.numeric(WP)) %>% 
  group_by(Site,Season,Species,Plant,Time) %>%
  summarise(WP = mean(WP, na.rm = TRUE),
            count_WP = n()) %>%
  ungroup() %>%
  mutate(WP_pre_dawn = ifelse(Time ==18000,WP,NA),
         WP_midday = ifelse(Time > 18000,WP,NA),) -> WP

read_csv("data/Choat_2006/raw/ManyPeaksRangeSLA_Choat.csv") %>%
  mutate(Species = gsub("A. bidwillii","Gossia bidwillii",Species),
         Species = gsub("A. excelsa","Alphitonia excelsa", Species),
         Species = gsub("B. australis","Brachychiton australis",Species),
         Species = gsub("C. gillivraei","Cochlospermom gillivraei",Species)) %>%
  select(Species, SLA) -> SLA

read_csv("data/Choat_2006/raw/ManyPeaksRangeGasExchange_Choat.csv") %>% 
  subset(Time > 18000) %>% 
  group_by(Site,Season,Species,Plant,Time) %>%
  summarise(A = mean(A, na.rm = TRUE),
            E = mean(E, na.rm = TRUE),
            gs = mean(gs, na.rm = TRUE)) %>%
  full_join(WP) %>%
  full_join(vessel_density) %>%
  group_by(Species,Plant) %>%
  mutate_at(vars(`Ves Count`,Diameter),replace_duplicates_with_NA) %>%
  ungroup() %>% 
  group_by(Species) %>%
  mutate_at(vars(Phenology),replace_duplicates_with_NA) %>%
  ungroup() %>% 
  bind_rows(SLA) %>%
  mutate(context = paste("measurements made in the",Season,"season at",Time)) %>% 
  write_csv("data/Choat_2006/data.csv") %>%
  distinct(context,.keep_all = TRUE) %>%
  select(context, Time, Season) %>%
  mutate(type = "field",
         description = "measurements made in the wet vs dry season at different times of day") -> context




