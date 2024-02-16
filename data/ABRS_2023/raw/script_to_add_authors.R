read_csv("data/ABRS_2023/raw/FoA_authorship.csv") -> authors
read_csv("data/ABRS_2023/data.csv") -> data

authors <- authors %>%
  mutate(joined = paste0(category, ": ", text)) %>%
  group_by(taxon) %>%
  mutate(joined = paste(joined, collapse = "; ")) %>% 
  distinct(joined, taxon) %>%
  ungroup() %>%
  rename(taxon_name = taxon)

data %>% left_join(authors) %>% write_csv("data/ABRS_2023/data.csv")

read_csv("data/ABRS_2022/data.csv") -> data
data %>% left_join(authors) %>% write_csv("data/ABRS_2022/data.csv")
