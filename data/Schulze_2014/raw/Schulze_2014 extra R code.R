read_csv("data/Schulze_2014/raw/WA_Tour_2010.csv") %>%
  replace_na(list(species = "", `working title` = "")) %>%
  mutate(species_name = paste(Genus,species,`working title`,sep=" ")) %>%
  mutate(`latitude (deg)` = round(-`S deg`-(`S min`/60)-(`S sec`/3600),3)) %>%
  mutate(`longitude (deg)` = round(`E deg`-(`E min`/60)-(`E sec`/3600),3)) %>%
  mutate(site_name = if_else(is.na(`Site Description`),paste("site_at_",`latitude (deg)`,"S_and_",`longitude (deg)`,"E",sep=""),
                             `Site Description`)) %>%
  rename(date_sampled = `Date`) %>%
  mutate(`# of leafs for LA` = as.numeric(`# of leafs for LA`),
         `LA (cm)` = as.numeric(`LA (cm)`)) %>% 
  mutate(leaf_area_cm2 = `LA (cm)`/`# of leafs for LA`) %>%
  write_csv("data/Schulze_2014/data.csv") %>%
  distinct(site_name,.keep_all = TRUE) %>%
  select(site_name,`latitude (deg)`,`longitude (deg)`,date_sampled,`Site Description`) %>%
  rename(description = `Site Description`) %>%
  write_csv("data/Schulze_2014/raw/site_names_Schulze_2014.csv") -> sites
