read_csv("data/Kotowska_2020/raw/main_trait_data.csv") %>%
  select(ID, Species) %>%
  group_by(Species) %>%
  summarise(ID = first(ID)) %>%
  ungroup() %>% 
  mutate(code = str_sub(ID, end = -3L)) %>% select(-ID) -> species_match

read_csv("data/Kotowska_2020/raw/lifespan.csv") %>%
  group_by(Species,tree) %>%
  summarise(lifespan = mean(lifespan)) %>%
  ungroup() %>%
  rename(code = Species) %>%
  mutate(ID = paste(code,tree,sep="")) %>% select(-tree) %>%
  full_join(species_match,by="code") %>% select(-code) -> leaf_lifespan

read_csv("data/Kotowska_2020/raw/main_trait_data.csv") %>%
  full_join(leaf_lifespan,by=c("Species","ID")) %>%
  write_csv("data/Kotowska_2020/data.csv")

read_csv("data/Kotowska_2020/raw/sites.csv") -> sites

read_csv("data/Kotowska_2020/raw/main_trait_data.csv") %>% 
  select(Plot,soil_N,soil_P,soil_Ptot) %>%
  group_by(Plot) %>%
  summarise_all(.funs = first) %>%
  ungroup() %>%
  full_join(sites,by="Plot") %>%
  write_csv("data/Kotowska_2020/raw/main_trait_data.csv") -> sites


