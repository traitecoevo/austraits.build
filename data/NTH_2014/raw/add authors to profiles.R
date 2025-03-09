# extract authors from current NTH download

read_csv("data/NTH_2014/raw/NT_raw.csv") %>%
  select(taxon_name, `Taxonomic.Literature.`) %>%
  filter(!is.na(`Taxonomic.Literature.`)) %>%
  group_by(taxon_name) %>%
  mutate(authors = paste0(`Taxonomic.Literature.`, collapse = "; ")) %>%
  ungroup() %>%
  select(taxon_name, authors) %>%
  distinct() -> authors

# add authors to all 3 NTH dataset

read_csv("data/NTH_2014/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NTH_2014/data.csv") 

read_csv("data/NTH_2022/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NTH_2022/data.csv") 

read_csv("data/NTH_2023/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NTH_2023/data.csv") 