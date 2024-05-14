# extract authors from current SAH download

read_csv("data/SAH_2014/raw/SA_raw.csv") %>%
  select(taxon_name, author, citation) %>%
  mutate(author = ifelse(author == "Author:Not yet available", NA, author)) %>%
  filter(!(is.na(author) & is.na(citation))) %>%
  mutate(
    authors = ifelse(is.na(author), citation, NA),
    authors = ifelse(is.na(citation), author, authors),
    authors = ifelse(!is.na(citation) & !is.na(author), paste0(author, "; ", citation), authors)
  ) %>%
  select(taxon_name, authors) %>%
  distinct() %>% 
  write_csv("data/SAH_2014/raw/authors.csv")

# manually remove special characters
read_csv("data/SAH_2014/raw/authors.csv") -> authors

# add authors to all 3 SAH dataset

read_csv("data/SAH_2014/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/SAH_2014/data.csv") 

read_csv("data/SAH_2022/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/SAH_2022/data.csv") 

read_csv("data/SAH_2023/data.csv") %>%
  left_join(authors) %>%
  write_csv("data/SAH_2023/data.csv") 

