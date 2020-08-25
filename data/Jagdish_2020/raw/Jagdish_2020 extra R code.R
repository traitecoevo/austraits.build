read_csv("data/Jagdish_2020/raw/AusTraits_raw.csv") %>%
  rename(`latitude (deg)` = `Latitude (S)`,`longitude (deg)` = `Longitude (E)`) %>%
  write_csv("data/Jagdish_2020/data.csv") -> data_Jagdish

data_Jagdish %>%
  select(`latitude (deg)`, `longitude (deg)`,`Site`, `LAI`, `Site abbreviation`, `description`) %>%
  distinct(Site, .keep_all = TRUE) -> sites

data_Jagdish %>%
  select(`Light Treatment`,`Light Treatment (Categorical)`) %>%
  distinct(`Light Treatment (Categorical)`, .keep_all = TRUE) %>%
  mutate(type = "treatment",
         description = ifelse(`Light Treatment (Categorical)` == "control","control light treatment with 12 hours light, 12 hours dark", NA),
         description = ifelse(`Light Treatment (Categorical)` == "short","short light treatment with 6 hours light, 18 hours dark", description),
         description = ifelse(`Light Treatment (Categorical)` == "long","long light treatment with 18 hours light, 6 hours dark", description)) -> context