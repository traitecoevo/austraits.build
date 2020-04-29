read_csv("data/Moore_2019_2/raw/data_Moore_2019_2.csv") %>%
  mutate(resprout_prop = Tillers_postfire / Tillers_prefire) %>%
  group_by(Species) %>%
  mutate_at(vars(Growth_habit, Life_cycle,bud_location,Photosynthetic_pathway),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  group_by(Species,Water_treatment) %>%
  mutate_at(vars(LDMC,Wet_weight_mg,LA_mm2, Dry_weight_mg,SLA),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  write_csv("data/Moore_2019_2/data.csv")

#additional code after report responses
read_csv("data/Moore_2019_2/data.csv") %>%
  group_by(Species,Water_treatment) %>%
  mutate(died = ifelse(resprout_prop == 0, 0, 1)) %>%
  summarise(resprouting_proportion_individuals = mean(died)) -> treatment_resprout

read_csv("data/Moore_2019_2/data.csv") %>%
  bind_rows(treatment_resprout) %>%
  write_csv("data/Moore_2019_2/data.csv")