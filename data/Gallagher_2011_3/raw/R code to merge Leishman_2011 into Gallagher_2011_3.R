Gallagher_2011_3 <- read_csv("data/Gallagher_2011_3/raw/data_Gallagher_2011_3.csv")

Leishman_2011 <- read_csv("data/Gallagher_2011_3/raw/data_Leishman_2011.csv")

names(Leishman_2011) <- c("name_original","trait","raw_trait_value","units")

dplyr::bind_rows(Gallagher_2011_3,Leishman_2011) %>%
  group_by(name_original) %>%
  summarise(raw_trait_value = mean(raw_trait_value)) %>%
  mutate(units = "mm2 mg") %>%
  mutate(trait = "SLA") %>%
  mutate(primary_source_id = 4) %>%
  mutate(metadata_id = 7) -> new_data

write_csv(new_data,"data/Gallagher_2011_3/data.csv")