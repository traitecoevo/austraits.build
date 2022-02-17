read_csv("data/Dwyer_2017/raw/quadrat.scale.data.csv") %>%
  select(remnant, site.lat, site.long, tot.n, p, k, pH.H2O) %>%
  group_by(remnant) %>% 
  summarise(`latitude (deg)` = first(site.lat),
            `longitude (deg)`= first(site.long),
            description = "York gum (Eucalyptus loxophleba)-jam (Acacia acuminata) woodlands",
            `total soil N (%)`= mean(tot.n),
            `soil P`= mean(p),
            `soil K`= mean(k),
            `soil pH`= mean(pH.H2O)) %>%
  write_csv("data/Dwyer_2017/raw/sites.csv") -> sites

metadata_add_sites("Dwyer_2017", sites) 

read_csv("data/Dwyer_2017/raw/species.in.quadrat.scale.data.csv") %>%
  mutate(species = gsub(".", " ", species, fixed=TRUE), 
         seed_mass = exp(mean.log.seed.mass), 
         max_height = rem.scale.max.sqrt.height^2,
         specific_leaf_area = exp(rem.scale.mean.log.sla)) %>%
  select(year,species,remnant,seed_mass,max_height,specific_leaf_area) %>%
  group_by(remnant,species) %>%
  summarise_all(.funs=mean) %>%
  ungroup() %>%
  mutate(seed_mass_distinct = seed_mass,
         max_height_distinct = max_height,
         specific_leaf_area_distinct = specific_leaf_area) %>%
  group_by(species) %>%
  mutate_at(vars(seed_mass_distinct,max_height_distinct,specific_leaf_area_distinct),
            funs(replace(.,duplicated(.),NA))) %>%
  write_csv("data/Dwyer_2017/data.csv")
      

#E. Wenk recreated data.csv file 2021.11.11 to include individual replicate counts; previous work by C. Baxter
read_csv("data/Dwyer_2017/raw/species.in.quadrat.scale.data.csv") %>%
  mutate(species = gsub(".", " ", species, fixed=TRUE), 
         seed_mass = exp(mean.log.seed.mass), 
         max_height = rem.scale.max.sqrt.height^2,
         specific_leaf_area = exp(rem.scale.mean.log.sla)) %>%
  write_csv("data/Dwyer_2017/data.csv")
