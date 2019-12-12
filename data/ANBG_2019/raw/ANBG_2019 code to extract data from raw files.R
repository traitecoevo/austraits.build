read_csv("data/ANBG_2019/raw/ANBG_ABCD_seedbank_unit.csv") %>%
  select(ITEM_ID,GENUSORMONOMIAL,FIRSTEPITHET,RANK,INFRASPECIFICEPITHET, GATHERINGISODATEBEGIN,
         ACTUALLATITUDEDECIMAL,ACTUALLONGITUDEDECIMAL,LOCALITYTEXT,PRIMARYCOLLECTOR) %>%
  mutate(FIRSTEPITHET = replace_na(FIRSTEPITHET," ")) %>%
  mutate(RANK = replace_na(RANK," ")) %>%
  mutate(INFRASPECIFICEPITHET = replace_na(INFRASPECIFICEPITHET," ")) %>%
  mutate(species_name = ifelse(is.na(RANK),paste(GENUSORMONOMIAL,FIRSTEPITHET),
                               paste(GENUSORMONOMIAL,FIRSTEPITHET,RANK,INFRASPECIFICEPITHET, sep = " "))) %>%
  mutate(species_name = gsub("  "," ",species_name)) %>%
  select(-GENUSORMONOMIAL,-FIRSTEPITHET,-RANK,-INFRASPECIFICEPITHET) %>%
  rename(`item_ID`= ITEM_ID, `gathering_date`= GATHERINGISODATEBEGIN,`latitude (deg)`= ACTUALLATITUDEDECIMAL,
         `longitude (deg)`= ACTUALLONGITUDEDECIMAL,`locality`= LOCALITYTEXT,`collector`= PRIMARYCOLLECTOR) %>%
  mutate(locality = str_replace_all(locality,"^[[:punct:]]","")) %>%
  mutate(locality = str_replace_all(locality,"^ ","")) %>%
  mutate(locality = str_replace_all(locality,"\\:","")) %>%
  mutate(locality = str_replace_all(locality,"\\?","")) %>%
  mutate(locality = str_replace_all(locality,"\\[","")) %>%
  mutate(locality = str_replace_all(locality,"\\]","")) %>%
  mutate(locality = str_replace_all(locality," $","")) %>%
  mutate(locality = str_replace_all(locality,'\\"','')) %>%
  mutate(locality = str_replace_all(locality,"\\'","")) %>%
  mutate(locality = gsub("\r\n","",locality)) %>%
  mutate(site_name = ifelse(is.na(`latitude (deg)`), locality,
    paste("site_at",`latitude (deg)`,"degS_and",`longitude (deg)`,"degE",sep = "_"))) %>%
  mutate(site_name_shorter = ifelse(is.na(`latitude (deg)`), substr(site_name,1,86),
    paste("site_at",`latitude (deg)`,"degS_and",`longitude (deg)`,"degE",sep = "_"))) %>%
  mutate(locality = str_replace_all(locality," $","")) %>%
  #distinct(site_name_shorter, .keep_all = TRUE) %>%
  write_csv("data/ANBG_2019/site_data.csv") -> site_data

#ran these lines before tweaking above code with "distinct"
site_data %>%
  select(species_name,item_ID,gathering_date,site_name,site_name_shorter, locality, collector,`latitude (deg)`,`longitude (deg)`) -> name_id_match

read_csv("data/ANBG_2019/treatment_terms.csv") -> treatment_terms

#View(name_id_match)
#merging species names into trait data spreadsheet
read_csv("data/ANBG_2019/raw/ANBG_ABCD_seedbank_treatment.csv") %>%
  rename(`item_ID`= "ITEM_ID",`trait`= "TREATMENTORTESTTYPE",`trait_2` = "CONDITIONDESCRIPTION", 
         detailed_description = "TREATMENTANDTESTNOTES", `value_numeric` = "TREATMENTMEASUREMENT1",
         `units` = "TREATMENTUNITOFMEASUREMENT1",value_2 = "TREATMENTMEASUREMENT2",
         `units_2` = "TREATMENTUNITOFMEASUREMENT2") %>%
  subset(`trait` == "Seed Sample" & trait_2 == "TSW" & detailed_description != "Number of seed per sample" & str_detect(detailed_description,"^Sample",negate=TRUE)
         | `trait` == "Propagation" & `trait_2` %in% c("Imbibation","Heat","Scarify","Smoke","Stratification") 
         | `trait` == "Seed morphology") %>%
  mutate(`trait` = ifelse(`trait`== "Seed morphology",`trait_2`,`trait`)) %>%
  mutate(detailed_description = gsub("Seed shape ","",detailed_description),
         detailed_description = gsub("Seed shape","",detailed_description),
         detailed_description = ifelse(`trait`=="Shape",gsub("or ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Shape",gsub("to ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Texture",gsub("or ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Texture",gsub("and ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Texture",gsub("to ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Texture",gsub(",","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Appendage",gsub("and ","",detailed_description),detailed_description),
         detailed_description = ifelse(`trait`=="Appendage",gsub(",","",detailed_description),detailed_description),
         detailed_description = gsub("Seed texture ","",detailed_description),
         detailed_description = gsub("Seed appendages ","",detailed_description)) %>%
  mutate(treatment_3 = ifelse(trait_2=="Smoke","disc_treatment",""),
         value_3 = ifelse(trait_2=="Smoke",value_numeric,""),
         unts_3 = ifelse(trait_2=="Smoke",units,""),
         value_numeric = ifelse(trait_2=="Smoke","",value_numeric)) %>%
  mutate(value_2 = ifelse(trait_2=="Imbibation",value_numeric,""),
         unts_2 = ifelse(trait_2=="Imbibation",units,""),
         value_numeric = ifelse(trait_2=="Imbibation","",value_numeric)) %>%
  mutate(treatment_3 = ifelse(str_detect(detailed_description,"at a temperature"),"temperature_treatment",""),
         value_3 = ifelse(str_detect(detailed_description,"at a temperature"),value_numeric,""),
         unts_3 = ifelse(str_detect(detailed_description,"at a temperature"),units,""),
         value_numeric = ifelse(str_detect(detailed_description,"at a temperature"),"",value_numeric)) %>%
  left_join(treatment_terms,by="detailed_description") %>% 
  mutate_at(c("value_numeric"), ~na_if(.,0)) %>%
  mutate(value_categorical = ifelse(is.na(value_categorical) & (is.na(value_numeric)|value_numeric==""),detailed_description,value_categorical)) %>%
  mutate(value = ifelse((is.na(value_numeric)|value_numeric==""),value_categorical,value_numeric)) %>%
  left_join(name_id_match,by="item_ID") %>%
  #select(-value_categorical,-value_numeric) %>%
  filter(!(value %in% c(
                      "Calculated length of seed (average, standard deviation)",
                      "Calculated width of seed (average, standard deviation)"
                      ))) %>%
  write_csv("data/ANBG_2019/data.csv") %>%
  distinct(site_name_shorter, .keep_all = TRUE) -> site_data

###compared left_join and full_join to confirm that all item_IDs in the treatment/traits spreadsheet have a name match
      
         
check <- subset(austraits$excluded_data, dataset_id == "ANBG_2019" & trait_name =="seed_mass")
unique(check$value)

View(check)




build_study_report("ANBG_2019",overwrite=TRUE)

