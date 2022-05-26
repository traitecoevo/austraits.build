austraits$traits %>%
  filter(trait_name %in% c("seed_mass")) %>%
  filter(dataset_id %in% c("Jurado_1991","Leishman_1992")) %>% 
  select(taxon_name, dataset_id, value) %>% 
  spread(key = dataset_id, value = value) %>% 
  filter(Jurado_1991 == Leishman_1992) -> filter_from_Jurado
 
austraits$traits %>%
  filter(trait_name %in% c("seed_mass")) %>%
  filter(dataset_id %in% c("Jurado_1991")) %>%
  filter(taxon_name %in% filter_from_Jurado$taxon_name) %>%
  select(original_name) %>%
  mutate(filtering_column = "value identical to Leishman_1992; filtering out of this dataset") %>%
  rename(name_original = original_name) -> to_filter

read_csv("data/Jurado_1991/data.csv") %>%
  left_join(to_filter) %>% 
  write_csv("data/Jurado_1991/data.csv")

