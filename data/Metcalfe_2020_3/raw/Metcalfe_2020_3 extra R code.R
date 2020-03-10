read_csv("data/Metcalfe_2020_3/raw/Cooper & Cooper_Australian Rainforest Plants.csv") %>%
  select(genus, species, form, height, `stem diameter`, `start of flower`, `end of flower`, fruits, 
         `start of fruit season`, `end of fruit season`, dispersers, dispersers_simple, `min fruit length`, `max fruit length`,
         `min fruit width`, `max fruti width`, `min seed length`, `max seed length`) %>%
  mutate(species_name = paste(genus,species,sep=" ")) %>%
  mutate(`start of flower` = gsub("\\(","", `start of flower`),
         `end of flower` = gsub("\\(","", `end of flower`),
         `start of flower` = gsub("\\)","", `start of flower`),
         `end of flower` = gsub("\\)","", `end of flower`)) %>% 
  mutate(flower_start = substr(`start of flower`,1,3),
         flower_end = substr(`end of flower`,1,3)) %>% 
  mutate(flowering = paste(flower_start, flower_end, sep="-")) %>% 
  mutate(flowering_span = convert_month_range_vec_to_binary(flowering)) %>% 
  mutate(`start of fruit season` = gsub("\\(","", `start of fruit season`),
         `end of fruit season` = gsub("\\(","", `end of fruit season`),
         `start of fruit season` = gsub("\\)","", `start of fruit season`),
         `end of fruit season` = gsub("\\)","", `end of fruit season`)) %>% 
  mutate(fruit_start = substr(`start of fruit season`,1,3),
         fruit_end = substr(`end of fruit season`,1,3)) %>% 
  mutate(fruiting = paste(fruit_start, fruit_end, sep="-")) %>% 
  mutate(fruiting_span = convert_month_range_vec_to_binary(fruiting))  %>% 
  select(-`start of fruit season`, -`end of fruit season`, -`start of flower`, -`end of flower`, - flowering, -fruiting,
         -flower_start, -fruit_start, -flower_end, -fruit_end) %>% 
  write_csv("data/Metcalfe_2020_3/data.csv") 