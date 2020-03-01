read_csv("data/Metcalfe_2020_3/raw/Cooper & Cooper_Australian Rainforest Plants.csv") %>%
  select(genus, species, form, height, `stem diameter`, `start of flower`, `end of flower`, fruits, 
         `start of fruit season`, `end of fruit season`, dispersers, `min fruit length`, `max fruit length`,
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
         -flower_start, -fruit_start, -flower_end, -fruit_end) %>% View()
    
    
    
    fruiting = convert_month_range_vec_to_binary(fruiting),
  write_csv("data/Metcalfe_2020_3/data.csv") -> test


%>%
  select(-"PlantID", -"Order - APG", -"Family - APG", -"Family - BRI old", -"Genus - current", -"Genus - synonyms", 
         -"Genus - synonyms_1", -"Genus - synonyms_2", -"Genus - synonyms_3", -"Species - current", 
         -"Species- synonyms", -"Species- synonyms_1", -"Species- synonyms_2", - "Species- synonyms_3", 
         -"Species- synonyms_4", -"current full name with authority", -"NCA Conservation_status") %>%
  mutate(Raunkier_life_form = ifelse(is.na(Raunkier_life_form_1), Raunkier_life_form, 
                                     paste(`Raunkier_life_form`,`Raunkier_life_form_1`,sep=" "))) %>%
  mutate(`Sex habit` = ifelse(is.na(`Sex habit_1`), `Sex habit`, 
                                     paste(`Sex habit`,`Sex habit_1`,sep=" "))) %>%
  mutate(`Sex habit` = ifelse(is.na(`Sex habit_2`), `Sex habit`, 
                                     paste(`Sex habit`,`Sex habit_2`,sep=" "))) %>%
  mutate(`1_Dispersal_mode` = ifelse(is.na(`2_Dispersal_mode`), `1_Dispersal_mode`, 
                              paste(`1_Dispersal_mode`,`2_Dispersal_mode`,sep=" "))) %>%
  mutate(regen_strategy = Clonality) %>%
  mutate(Clonality = ifelse(is.na(Clonality),"not_vegetative",Clonality)) %>%
  select(-`Raunkier_life_form_1`,-`Sex habit_1`,-`Sex habit_2`,-`2_Dispersal_mode`) %>%
