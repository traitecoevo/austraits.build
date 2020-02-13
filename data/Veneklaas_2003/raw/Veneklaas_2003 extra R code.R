read_csv("data/Veneklaas_2003/data.csv") %>%
  mutate(site_name = "Melaleuca Park Conservation Area") %>%
  mutate(species_name = paste(Genus,Species,sep=" ")) %>% 
  write_csv("data/Veneklaas_2003/data.csv")