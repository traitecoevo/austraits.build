read_csv("data/Jordan_2020/raw/Proteaceae data for austraits.csv") %>%
  mutate(site_name = ifelse(is.na(`native to`),habitat,paste0(habitat," in ", `native to`))) %>%
  select(site_name, `native to`, habitat) %>%
  distinct(site_name, .keep_all = TRUE) %>% 
  rename(description = habitat) %>%
  write_csv("data/Jordan_2020/raw/sites.csv") %>%
  mutate(`latitude (deg)` = NA,
         `longitude (deg)` = NA) -> sites


read_csv("data/Jordan_2020/raw/Proteaceae data for austraits.csv") %>%
  mutate(site_name = ifelse(is.na(`native to`),habitat,paste0(habitat," in ", `native to`))) %>%
  mutate(leaf_division = `adult leaf type`,
         leaf_compoundness = ifelse(stringr::str_detect(leaf_division,"simple"),"simple",NA),
         leaf_compoundness = ifelse(stringr::str_detect(leaf_division,"compound"),"compound",leaf_compoundness),
         leaf_margin = ifelse(stringr::str_detect(leaf_division,"entire"),"entire",NA),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"terete"),"terete",NA),
         leaf_division = gsub("terete","",leaf_division),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"cuneate"),"cuneate",leaf_shape),
         leaf_division = gsub("cuneate","",leaf_division),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"flat"),"flat",leaf_shape),
         leaf_division = gsub("flat","",leaf_division),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"auriculate"),"auriculate",leaf_shape),
         leaf_division = gsub("auriculate","",leaf_division),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"flabelliform"),"flabelliform",leaf_shape),
         leaf_division = gsub("flabelliform","",leaf_division),
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"linear"),"linear",leaf_shape),
         leaf_division = gsub("linear","",leaf_division),       
         leaf_shape = ifelse(stringr::str_detect(leaf_division,"usually sub"),"subulate",leaf_shape),
         leaf_division = gsub("usually sub","",leaf_division)) %>% 
  write_csv("data/Jordan_2020/data.csv")

###leaf_divisions values then manually added and others slightly changed