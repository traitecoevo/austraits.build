read_csv("data/Tomlinson_2018/raw/Tomlinson Austraits locations.csv") %>%
  select(SPECIES, `Seed location`) %>%
  rename(Species = SPECIES) -> site_names

read_csv("data/Tomlinson_2018/raw/week_5_data.csv") %>%
  subset(Continent == "Australia") %>%
  select(Individual,Species, Batch, 'Nutrient Treatment', 'Grass Treatment', STRL_m_per_g) -> week_05

read_csv("data/Tomlinson_2018/raw/week_20_data.csv") %>%
  subset(Continent == "Australia") %>%
  mutate(photosynthetic_rate_per_area_saturated = A_nmol_CO2_per_g_s / SLA_mm2_per_mg,
         WUE_intrinsic = photosynthetic_rate_per_area_saturated/gs_mmol_H2O_per_m2_s) %>%
  full_join(week_05, by=c("Individual","Species", "Batch", "Nutrient Treatment", "Grass Treatment")) %>% 
  left_join(site_names,by="Species") %>%
  mutate(context = paste("Nutrient_treatment_",`Nutrient Treatment`,"_and_grass_treatment_",`Grass Treatment`,sep="")) %>%
  write_csv("data/Tomlinson_2018/data.csv") %>%
  select(context,`Nutrient Treatment`,`Grass Treatment`) %>%
  group_by(context) %>%
  mutate_at(vars(`Nutrient Treatment`,`Grass Treatment`),funs(replace(.,duplicated(.),NA))) %>%
  subset(!is.na(`Nutrient Treatment`)) -> context_table

context_table %>%
  mutate(type = "treatment_favourable",
         description = "high nutrient and grass competition") -> context_table


read_csv("data/Tomlinson_2018/raw/sites.csv") -> sites
