read_csv("data/Doherty_2021/raw/data_raw.csv") %>%
  mutate(
    establishment_means = ifelse(str_detect(Genus, "\\*"), "naturalised", "native"),
    Genus = str_replace(Genus, "\\*", ""),
    species = paste0(Genus, " ", species),
    `3` = as.numeric(`3`)
    ) %>%
  select(-Genus) %>%
  tidyr::pivot_longer(cols = 2:8) %>%
  filter(!is.na(value)) %>%
  rename(resprouting_capacity = name, Gill_and_Bradstock_code = value) %>%
  mutate(
    seedbank_location = resprouting_capacity,
    bud_bank_location = resprouting_capacity,
    resprouting_capacity = case_when(
      resprouting_capacity == 1 ~ "fire_killed",
      resprouting_capacity == 2 ~ "fire_killed",
      resprouting_capacity == 3 ~ "fire_killed",
      resprouting_capacity == 4 ~ "resprouts",
      resprouting_capacity == 5 ~ "resprouts",
      resprouting_capacity == 6 ~ "resprouts",
      resprouting_capacity == 7 ~ "resprouts" 
    ),
    seedbank_location = case_when(
      seedbank_location == 1 ~ "canopy_seedbank",
      seedbank_location == 2 ~ "soil_seedbank",
      seedbank_location == 3 ~ "none",
      seedbank_location == 4 ~ NA,
      seedbank_location == 5 ~ NA,
      seedbank_location == 6 ~ NA,
      seedbank_location == 7 ~ NA
    ),
    bud_bank_location = case_when(
      bud_bank_location == 1 ~ "none",
      bud_bank_location == 2 ~ "none",
      bud_bank_location == 3 ~ "none",
      bud_bank_location == 4 ~ "bud-bearing_root",
      bud_bank_location == 5 ~ "basal_stem_buds",
      bud_bank_location == 6 ~ "epicormic_buds",
      bud_bank_location == 7 ~ "apical_buds" 
    )
  ) %>%
  group_by(species, establishment_means) %>%
    mutate(
      resprouting_capacity = paste0(resprouting_capacity, collapse = " "),
      seedbank_location = paste0(na.omit(seedbank_location), collapse = " "),
      bud_bank_location = paste0(bud_bank_location, collapse = " "),
      Gill_and_Bradstock_code = paste0(Gill_and_Bradstock_code, collapse = "; ")
    ) %>%
  ungroup() %>%
  write_csv("data/Doherty_2021/data.csv")
