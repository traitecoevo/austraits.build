read_csv("data/Geange_2017/data.csv") %>%
  mutate(species_name = ifelse(`Species!`=="ag","Aciphylla glacialis",NA),
         species_name = ifelse(`Species!`=="oe","Oreomyrrhis eriopoda",species_name),
         species_name = ifelse(`Species!`=="wc","Wahlenbergia ceracea",species_name)) %>%
  mutate(population = ifelse(`Elevation!`=="LoElev","low elevation","high elevation"),
         water_treatment = ifelse(`Treatment!`=="LoWat","low water","high water"),
         context = paste0(population," maternal lineage ",`Mother!`," grown under ",water_treatment," conditions")) %>% 
  write_csv("data/Geange_2017/data.csv") %>%
  select(context,population,water_treatment,`Mother!`) %>%
  group_by(context) %>%
  summarise_all(.funs=first) %>%
  ungroup() %>%
  rename(maternal_lineage = `Mother!`) %>%
  mutate(maternal_lineage = round(maternal_lineage,0),
         type = "treatment",
         description = "maternal lineages from high and low elevation growth factorially under high and low water treatments") -> context


subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value") %>%
  write_csv("data/Gray_2019/raw/Gray_2019_duplicates.csv")


