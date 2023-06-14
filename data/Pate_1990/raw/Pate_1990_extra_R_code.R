library(tidyverse)

data_long <- read_csv("data/Pate_1990/raw/long_data.csv")
data_long %>% 
  pivot_wider(
    names_from = trait_name, 
    values_from = value, 
    id_cols = "taxon_name"
    ) -> data_wide

taxonomic_authority <- 
  data_long %>% 
  select(taxon_name, authority) %>% 
  distinct(taxon_name, .keep_all = TRUE)

extra_context_columns <-
  data_long %>% 
  select(taxon_name, plant_age, total_dry_weight, total_dry_weight_SE) %>% 
  distinct(taxon_name, .keep_all = TRUE)

data_wide %>% 
  left_join(taxonomic_authority, by = "taxon_name") %>% 
  left_join(extra_context_columns, by = "taxon_name") %>% 
  relocate(authority, .after = taxon_name) -> data_wide_full

data_wide_full %>% write_csv("data/Pate_1990/raw/wide_data.csv")
