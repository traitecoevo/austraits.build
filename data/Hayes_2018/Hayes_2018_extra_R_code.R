read_csv("data/Hayes_2018/Hayes_et_al._2018_site.data.csv") %>%
  select(`site.id`,`date`,`location detail`,`latitude (deg)`,`longitude (deg)`) %>%
  group_by(`site.id`) %>%
  summarise_all(.funs = first) -> sites_char_info

read_csv("data/Hayes_2018/Hayes_et_al._2018_site.data.csv") %>%
select(-`sample #`,-`sample detail`,-`species sampled`,-`replicate #`,-`country`,-`laboratory`,
       -`latitude (deg)`,-`longitude (deg)`,-`date`,-`location detail`,-`GPS coords`) %>%
group_by(`site.id`) %>%
  summarise_all(.funs = mean) %>%
ungroup() %>%
left_join(sites_char_info,by="site.id") -> site_data 

read_csv("data/Hayes_2018/Hayes_et_al._2018_leaf.data.csv") %>%
  mutate(Zn_mg.kg = gsub("<1.0","1.0",Zn_mg.kg)) %>%
  mutate(Al_mg.kg = gsub("<20","20",Al_mg.kg)) %>%
  mutate(Zn_mg.kg_numeric = as.numeric(Zn_mg.kg)) %>% 
  mutate(Al_mg.kg_numeric = as.numeric(Al_mg.kg)) %>%
  select(-`GPS co-ords`,- `Al_mg.kg`, -`Zn_mg.kg`) %>%
  rename(biological.rep = `replicate`)%>%
  rename(replicate_individuals = `sample code`) -> leaf_data

read_csv("data/Hayes_2018/Hayes_et_al._2018_cell.data.csv") %>%
  select(-`GPS coords`,-`spectrum.label`,-`sample`) %>%
  mutate(cell.type = gsub(".Ca","",cell.type),
         cell.type = gsub("EP","epidermis",cell.type),
         cell.type = gsub("HY","hypodermis",cell.type),
         cell.type = gsub("PM","palisade_mesophyll",cell.type),
         cell.type = gsub("SM","spongy_mesophyll",cell.type),
         cell.type = gsub("IP","internal_parenchyma",cell.type),
         cell.type = gsub("SC","sclerenchyma",cell.type)) %>%
  mutate(replicate_individuals = paste(species,biological.rep,sep="_")) -> cell_temp

cell_temp %>%
  group_by(site.id, cell.type,species, biological.rep, replicate_individuals) %>%
  summarise(sample_reps = n()) %>%
  ungroup() -> counts
  
cell_temp %>%
  group_by(site.id, cell.type,species, biological.rep, replicate_individuals) %>%
  summarise_all(.funs = mean) %>%
  ungroup() %>%
  left_join(counts, by = c("species","replicate_individuals","site.id","cell.type","biological.rep")) -> before_spread

before_spread %>%
  select(-Ca, -P) %>%
  spread(key = cell.type, value = sample_reps) %>%
  rename(epidermis_counts = epidermis, hypodermis_counts = hypodermis, 
         palisade_mesophyll_counts = palisade_mesophyll,
         spongy_mesophyll_counts = spongy_mesophyll, 
         internal_parenchyma_counts = internal_parenchyma, 
         sclerenchyma_counts = sclerenchyma) %>%
  select(-species, -biological.rep, - `latitude (deg)`, -`longitude (deg)`) -> counts_spread

before_spread %>%
  select(-Ca, -sample_reps) %>%
  spread(key = cell.type, value = P) %>%
  rename(epidermis_P = epidermis, hypodermis_P = hypodermis, palisade_mesophyll_P = palisade_mesophyll,
         spongy_mesophyll_P = spongy_mesophyll, internal_parenchyma_P = internal_parenchyma, 
         sclerenchyma_P = sclerenchyma) %>%
  select(-species, -biological.rep, - `latitude (deg)`, -`longitude (deg)`) -> cell_P_data

before_spread %>%
  select(-P, -sample_reps) %>%
  spread(key = cell.type, value = Ca) %>%
  rename(epidermis_Ca = epidermis, hypodermis_Ca = hypodermis, palisade_mesophyll_Ca = palisade_mesophyll,
         spongy_mesophyll_Ca = spongy_mesophyll, internal_parenchyma_Ca = internal_parenchyma, 
         sclerenchyma_Ca = sclerenchyma) %>%
  select(- `latitude (deg)`, -`longitude (deg)`) %>%
  left_join(cell_P_data, by = c("site.id","replicate_individuals")) %>%
  left_join(counts_spread, by = c("site.id","replicate_individuals")) -> cell_data

cell_data %>%
  bind_rows(leaf_data) %>%
  write_csv("data/Hayes_2018/data.csv")
