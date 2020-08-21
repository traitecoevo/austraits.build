read_csv("data/Everingham_2020/raw/SE_Germination Data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") -> germination

read_csv("data/Everingham_2020/raw/SE_LMA data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") -> LMA

read_csv("data/Everingham_2020/raw/SE_Photosynthesis data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") %>%
  group_by(`Species`,`Modern/Historic`,`Date Collected`,`latitude (deg)`,`longitude (deg)`,`site`,`Plant ID`) %>%
  summarise_all(.funs = mean) %>%
  ungroup() -> photosynthesis 

read_csv("data/Everingham_2020/raw/SE_Seed dimension data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)","Length transformed","Width transformed",
         "Depth transformed","Sum","Variance") -> seed_size

read_csv("data/Everingham_2020/raw/SE_Seed mass data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") -> seed_mass

read_csv("data/Everingham_2020/raw/SE_stem density data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") -> stem_density

read_csv("data/Everingham_2020/raw/SE_Root to shoot data.csv") %>% 
  select(-"Latitude (deg min sec S/N)",-"Longitude (deg min sec E/W)") -> root_shoot



seed_size %>%
  bind_rows(seed_mass) %>%
  bind_rows(LMA) %>%
  bind_rows(stem_density) %>%
  bind_rows(germination) %>%
  bind_rows(photosynthesis) %>%
  bind_rows(root_shoot) %>%
  write_csv("data/Everingham_2020/data.csv") %>%
  distinct(site, .keep_all = TRUE) %>%
  select(`latitude (deg)`, `longitude (deg)`, `site`) -> site

View(read_csv("data/Everingham_2020/data.csv", guess_max = 10000))
