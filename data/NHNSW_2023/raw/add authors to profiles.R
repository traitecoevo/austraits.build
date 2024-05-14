# extract authors from current NHNSW download

read_csv("data/NHNSW_2023/raw/PlantNET_raw.csv") %>%
  select(taxon_name, Source) %>%
  mutate(
    Source = stringr::str_replace(Source, "Taxon concept:$",""),
    Source = stringr::str_replace(Source, "Taxon concept","; Taxon concept")) %>% 
  filter(!(is.na(Source))) %>%
  group_by(taxon_name) %>%
    mutate(Source = paste0(Source, collapse = "; ")) %>%
  ungroup() %>%
    distinct() -> authors

# add authors to all 3 SAH dataset

read_csv("data/NHNSW_2023/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NHNSW_2023/data.csv") 

read_csv("data/NHNSW_2022/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NHNSW_2022/data.csv") 

read_csv("data/NHNSW_2016/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NHNSW_2016/data.csv") 

read_csv("data/NHNSW_2014/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NHNSW_2014/data.csv") 

read_csv("data/NHNSW_2014_2/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/NHNSW_2014_2/data.csv") 
