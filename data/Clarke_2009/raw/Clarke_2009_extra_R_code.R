austraits$traits %>% filter(dataset_id %in% c("Clarke_2009","Clarke_2015")) %>% select(taxon_name,original_name,dataset_id) %>% group_by(dataset_id) %>% distinct(,.keep_all = TRUE) %>% ungroup -> species_lists

species_lists %>% mutate(value = 1) %>% spread(key = dataset_id, value = value) %>% filter(is.na(Clarke_2015)) %>% select(original_name) %>% mutate(not_in_Clarke_2015 = "x") %>% rename (Species = original_name) -> Clarke_2009_only

read_csv("data/Clarke_2009/data.csv") %>% 
  left_join(Clarke_2009_only) %>% 
  mutate(seed_storage_location = ifelse(`Gill category` %in% c("I"), "canopy", NA),
         seed_storage_location = ifelse(`Gill category` %in% c("II"), "soil", seed_storage_location),
         fire_response = ifelse(`Gill category` %in% c("I","II"), "fire_killed", NA),
         fire_response = ifelse(`Gill category` %in% c("IV","V","VI","VII","V, VI","V, VII"), "resprouts",fire_response),
         regen_strategy = ifelse(`Gill category` %in% c("IV"),"bud-bearing_root",NA),
         regen_strategy = ifelse(`Gill category` %in% c("V"),"basal_stem_buds",regen_strategy),
         regen_strategy = ifelse(`Gill category` %in% c("VI"),"stem_buds",regen_strategy),
         regen_strategy = ifelse(`Gill category` %in% c("VII"),"apical_buds",regen_strategy),
         regen_strategy = ifelse(`Gill category` %in% c("V, VI"),"basal_stem_buds stem_buds",regen_strategy),
         regen_strategy = ifelse(`Gill category` %in% c("V, VII"),"basal_stem_buds apical_buds",regen_strategy)) %>% write_csv("data/Clarke_2009/data2.csv")
         
         
         
for custom_R_code:
  
mutate(fire_response = ifelse(not_in_Clarke_2015 == "x",fire_response,NA),
               regen_strategy = ifelse(not_in_Clarke_2015 == "x",regen_strategy,NA))