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
  rename(`item_ID`= ITEM_ID, `date`= GATHERINGISODATEBEGIN,`latitude (deg)`= ACTUALLATITUDEDECIMAL,
         `longitude (deg)`= ACTUALLONGITUDEDECIMAL,`locality`= LOCALITYTEXT,`collector`= PRIMARYCOLLECTOR) %>%
  mutate(locality = gsub(": ","_",locality),
         locality = gsub(", ","_",locality),
         locality = gsub("' ","",locality),
         locality = gsub("; ","_",locality),
         locality = gsub(" ","_",locality)) %>%
  mutate(site_name = ifelse(is.na(`latitude (deg)`), `locality`,
        paste("site_at",`latitude (deg)`,"degS_and",`longitude (deg)`,"degE",sep = "_"))) %>%
  #distinct(site_name, .keep_all = TRUE) %>%
  write_csv("data/ANBG_2019/site_data.csv") -> site_data

#ran these lines before tweaking above code with "distinct"
site_data %>%
  select(species_name,item_ID,date,site_name) -> name_id_match

read_csv("data/ANBG_2019/raw/ANBG_ABCD_seedbank_treatment.csv") %>%
  select("ITEM_ID","TREATMENTORTESTTYPE","TREATMENTANDTESTNOTES","CONDITIONDESCRIPTION",
         "TREATMENTMEASUREMENT1","TREATMENTUNITOFMEASUREMENT1") %>%
  rename(`item_ID`= "ITEM_ID",`trait`= "TREATMENTORTESTTYPE",`trait_2` = "CONDITIONDESCRIPTION", `value_categorical` = "TREATMENTANDTESTNOTES", `value_numeric` = "TREATMENTMEASUREMENT1",`units` = "TREATMENTUNITOFMEASUREMENT1") %>%
  subset(`trait` == "Seed Sample" & trait_2 == "TSW" & value_categorical != "Number of seed per sample" | `trait` == "Propagation" & `trait_2` %in% c("Imbibation","Heat","Scarify","Smoke","Stratification") | `trait` == "Seed morphology") %>%
  mutate(`trait` = ifelse(`trait`== "Seed morphology",`trait_2`,`trait`)) %>%
  mutate(value_categorical = gsub("Seed shape ","",value_categorical),
         value_categorical = gsub("Seed shape","",value_categorical),
         value_categorical = ifelse(`trait`=="Shape",gsub("or ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Shape",gsub("to ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Texture",gsub("or ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Texture",gsub("and ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Texture",gsub("to ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Texture",gsub(",","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Appendage",gsub("and ","",value_categorical),value_categorical),
         value_categorical = ifelse(`trait`=="Appendage",gsub(",","",value_categorical),value_categorical),
         value_categorical = gsub("Seed texture ","",value_categorical),
         value_categorical = gsub("Seed appendages ","",value_categorical)) %>%
  mutate(`value` = ifelse(is.na(`value_numeric`),`value_categorical`,`value_numeric`)) %>%
  left_join(name_id_match,by="item_ID") %>%  write_csv("data/ANBG_2019/data.csv")


check <- subset(austraits$excluded_data, dataset_id == "ANBG_2019" & trait_name =="seed_shape")
unique(check$value)

check <- subset(austraits$excluded_data, dataset_id == "ANBG_2019" & trait_name =="germination_treatment")
unique(check$value)


build_study_report("ANBG_2019",overwrite=TRUE)

