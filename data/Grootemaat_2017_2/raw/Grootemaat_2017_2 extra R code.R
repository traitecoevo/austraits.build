read_csv("data/Grootemaat_2017_2/raw/species.csv") %>%
  mutate(Taxon = paste0(Genus," ",Species)) %>%
  select(Abbreviation,Taxon,`Rainfall a`,`Soil P b`) -> species_match

read_csv("data/Grootemaat_2017_2/raw/Oikos_curliness_measurements_20210211.csv") %>%
  subset(material == "dried") %>%
  rename(material_code = X1,Abbreviation = Species) %>%
  left_join(species_match) -> curliness

read_csv("data/Grootemaat_2017_2/raw/consumability.csv") %>%
  select(species, `rep#`,`mass ash( g)`,`mass char (g)`,`mass unburnt (g)`,
         `ash% of mass before`,`char% of mass before`,`unburnt% of mass before`) %>% 
  rename(Abbreviation = species) %>%
  left_join(species_match) %>%
  select(-Abbreviation) -> consumability

read_csv("data/Grootemaat_2017_2/raw/burning.csv") %>%
  subset(material == "dried") %>%
  rename(Abbreviation = species) %>%
  left_join(species_match) %>%
  full_join(consumability, by=c("Taxon","rep#","Rainfall a","Soil P b")) -> burning

read_csv("data/Grootemaat_2017_2/raw/burning.csv") %>%
  subset(material == "senes") %>%
  mutate(material = gsub("senes","senesced",material)) %>%
  rename(senesced_leaf_N = `%N leaves`,
         senesced_leaf_C = `%C leaves`,
         senesced_leaf_P = `%P leaves`) %>%
  rename(Abbreviation = species) %>%
  left_join(species_match) -> burning_senesced

curliness %>%
  bind_rows(burning) %>%
  bind_rows(burning_senesced) %>% 
  mutate(site_conditions = paste0(`Rainfall a`," rainfall ",`Soil P b`," soil P"),
         site_name = site_conditions) %>%
  mutate(site_name = gsub("low rainfall high soil P", "Round Hill woodland", site_name),
         site_name = gsub("low rainfall low soil P", "Round Hill mallee", site_name),
         site_name = gsub("high rainfall high soil P", "Kuring-gai NP hiP", site_name),
         site_name = gsub("high rainfall low soil P", "Kuring-gai NP lowP", site_name),
         site_name = gsub("NA rainfall NA soil P","unknown site",site_name)) %>%
  write_csv("data/Grootemaat_2017_2/data.csv")


