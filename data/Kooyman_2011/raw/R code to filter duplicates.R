current_study <-  "Kooyman_2011"
austraits_deduped <- remove_suspected_duplicates(austraits)
duplicates_for_dataset_id  <- subset(austraits_deduped$excluded_data,dataset_id==current_study & error !="Missing value")

duplicates_for_dataset_id %>%
  distinct(trait_name)
```
trait_name       
<chr>            
1 seed_width       
2 plant_height     
3 plant_growth_form
4 seed_length      
5 leaf_compoundness
6 leaf_length      
7 leaf_phenology   
8 leaf_width       
9 leaf_margin      
10 wood_density     
11 seed_breadth    

read_csv("data/Kooyman_2011/data.csv") -> Kooyman

Kooyman %>% names()
[1] "Species_reconciled with updates and additions" "growth form"                                   "seed size categorical: <10mm"                  "seed size categorical: >10mm"                 
[5] "leaf type"                                     "leaves lobed"                                  "leaf longevity"                                "wood density species"                         
[9] "wood density genus"                            "wood density family"                           "Height (m)"                                    "leaf length minimum (mm)"                     
[13] "leaf length maximum (mm)"                      "leaf width minimum (mm)"                       "leaf width maximum (mm)"                       "seed length minimum (mm)"                     
[17] "seed length maximum (mm)"                      "seed width minimum (mm)"                       "seed width maximum (mm)"                       "seed breadth (mm)" 

duplicates_for_dataset_id %>% write_csv("data/Kooyman_2011/raw/original_Kooyman_duplicates.csv")
in Excel, removed duplicates from Bear_1997, Chinnock_2007, Choat_2006, Cunningham_1999, Curran_2009, Edwards_2000, 
Falster_2003, Fonseca_2000, Grubb_1996, Hughes_1992, Hyland_2003, Jordan_2007, Jurado_1993, Knox_2011,
Leishman_1992, Leishman_1995, Lord_1997, Meers_2007, Peeters_2002, Read_2003, Rice_1991, Richards_2008, Schulze_1998,
Sjostrom_2006, Smith_1996, Stewart_1995, Thompson_2001, Westoby_2004, Wheeler_2002, Zanne_2009
(These are mostly duplicate categorical variables and others are clearly not Kooymans source pf data)

read_csv("data/Kooyman_2011/raw/duplicates_filtered.csv") -> duplicates_for_dataset_id

plant_height_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "plant_height") %>% 
  select(error, original_name) %>% 
  rename(plant_height_duplicate = error)

wood_density_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "wood_density") %>% 
  select(error, original_name) %>% 
  rename(wood_density_duplicate = error)

seed_width_min_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "seed_width" & value_type == "expert_min") %>% 
  select(error, original_name) %>% 
  rename (seed_width_min_duplicate = error)

seed_width_max_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "seed_width" & value_type == "expert_max") %>% 
  select(error, original_name) %>% 
  rename (seed_width_max_duplicate = error)

seed_length_min_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "seed_length" & value_type == "expert_min") %>% 
  select(error, original_name) %>% 
  rename (seed_length_min_duplicate = error)

seed_length_max_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "seed_length" & value_type == "expert_max") %>% 
  select(error, original_name) %>% 
  rename (seed_length_max_duplicate = error)

leaf_width_min_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "leaf_width" & value_type == "expert_min") %>% 
  select(error, original_name) %>% 
  rename (leaf_width_min_duplicate = error)

leaf_width_max_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "leaf_width" & value_type == "expert_max") %>% 
  select(error, original_name) %>% 
  rename (leaf_width_max_duplicate = error)

leaf_length_min_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "leaf_length" & value_type == "expert_min") %>% 
  select(error, original_name) %>% 
  rename (leaf_length_min_duplicate = error)

leaf_length_max_duplicates <- duplicates_for_dataset_id %>% 
  filter(trait_name == "leaf_length" & value_type == "expert_max") %>% 
  select(error, original_name) %>% 
  rename (leaf_length_max_duplicate = error)

Kooyman %>%
  left_join(plant_height_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(wood_density_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(seed_width_min_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(seed_width_max_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(seed_length_min_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(seed_length_max_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(leaf_width_min_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(leaf_width_max_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(leaf_length_min_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  left_join(leaf_length_max_duplicates, by = c("Species_reconciled with updates and additions" = "original_name")) %>% 
  write_csv("data/Kooyman_2011/data.csv")

#code to add to custom R code 
data %>%
  mutate(`Height (m)` = ifelse(is.na(plant_height_duplicate),`Height (m)`,NA),
         `wood density species` = ifelse(is.na(wood_density_duplicate),`wood density species`,NA),
         `seed width minimum (mm)` = ifelse(is.na(seed_width_min_duplicate),`seed width minimum (mm)`,NA),
         `seed width maximum (mm)` = ifelse(is.na(seed_width_max_duplicate),`seed width maximum (mm)`,NA),
         `seed length minimum (mm)` = ifelse(is.na(seed_length_min_duplicate),`seed length minimum (mm)`,NA),
         `seed length maximum (mm)` = ifelse(is.na(seed_length_max_duplicate),`seed length maximum (mm)`,NA),
         `leaf width minimum (mm)` = ifelse(is.na(leaf_width_min_duplicate),`leaf width minimum (mm)`,NA),
         `leaf width maximum (mm)` = ifelse(is.na(leaf_width_max_duplicate),`leaf width maximum (mm)`,NA),
         `leaf length minimum (mm)` = ifelse(is.na(leaf_length_min_duplicate),`leaf length minimum (mm)`,NA),
         `leaf length maximum (mm)` = ifelse(is.na(leaf_length_max_duplicate),`leaf length maximum (mm)`,NA)
  )