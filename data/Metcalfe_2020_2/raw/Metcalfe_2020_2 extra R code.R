read_csv("data/Metcalfe_2020_2/raw/POACEAE_database.csv") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-"PlantID", -"Order - APG", -"Family - APG", -"Family - BRI old", -"Genus - current", -"Genus - synonyms", 
         -"Genus - synonyms_1", -"Genus - synonyms_2", -"Genus - synonyms_3", -"Species - current", 
         -"Species- synonyms", -"Species- synonyms_1", -"Species- synonyms_2", - "Species- synonyms_3", 
         -"Species- synonyms_4", -"current full name with authority", -"EPBC Conservation_status",  -"NCA Conservation_status") %>%
  mutate(Raunkier_life_form = ifelse(is.na(Raunkier_life_form_1), Raunkier_life_form, 
                                     paste(`Raunkier_life_form`,`Raunkier_life_form_1`,sep=" "))) %>%
  mutate(`Sex habit` = ifelse(is.na(`Sex habit_1`), `Sex habit`, 
                              paste(`Sex habit`,`Sex habit_1`,sep=" "))) %>%
  mutate(`Sex habit` = ifelse(is.na(`Sex habit_2`), `Sex habit`, 
                              paste(`Sex habit`,`Sex habit_2`,sep=" "))) %>%
  mutate(`Sex habit` = ifelse(is.na(`Sex habit_3`), `Sex habit`, 
                              paste(`Sex habit`,`Sex habit_3`,sep=" "))) %>%
  mutate(`1_Dispersal_mode` = ifelse(is.na(`2_Dispersal_mode`), `1_Dispersal_mode`, 
                              paste(`1_Dispersal_mode`,`2_Dispersal_mode`,sep=" "))) %>%
  mutate(`Clonality` = ifelse(is.na(`Clonality_1`), `Clonality`, 
                                     paste(`Clonality`,`Clonality_1`,sep=" "))) %>%
  select(-`Raunkier_life_form_1`,-`Sex habit_1`,-`Sex habit_2`,-`Sex habit_3`,-`2_Dispersal_mode`, -`Clonality_1`) %>%
  mutate(regen_strategy = Clonality) %>%
  mutate(Clonality = ifelse(is.na(Clonality),"not_vegetative","vegetative")) %>%
  write_csv("data/Metcalfe_2020_2/data.csv") -> test
 
names(test)



