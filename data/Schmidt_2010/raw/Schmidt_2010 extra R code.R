read_csv("data/Schmidt_2010/raw/data_Monksland.csv") %>%
  rename(name_matched = `gen spp`, Property = `site_text`) %>%
  select(name_matched,Property,C3C4,Nfixer,`growth form`, leaf_type, `deciduous/evergreen`) -> spp_level

read_csv("data/Schmidt_2010/raw/data_Monksland_raw.csv") %>%
  bind_rows(spp_level) %>%
  mutate_at(c("Al mg/kg","Zn mg/kg"), ~na_if(.,0)) %>%
  write_csv("data/Schmidt_2010/data.csv")
