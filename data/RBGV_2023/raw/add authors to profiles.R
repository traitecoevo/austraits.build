# extract authors from current RBGV download

read_csv("data/RBGV_2023/raw/120224_downloaded.csv") %>%
  select(taxon_name, created_by) %>%
  group_by(taxon_name) %>%
  mutate(created_by = paste0(created_by, collapse = "; ")) %>%
  ungroup() %>% 
  distinct() -> authors
  
# add authors to 2 RBGV dataset

read_csv("data/RBGV_2023/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/RBGV_2023/data.csv") 

read_csv("data/RBGV_2022/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/RBGV_2022/data.csv") 

