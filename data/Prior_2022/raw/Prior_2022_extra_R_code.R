read_csv("data/Prior_2022/raw/data_tmp.csv") %>%
  select(taxon_name, Alive, Scorch, Epicormic, Basal, Subplot, Location, `Char.ht`, Intensity) %>%
  mutate(replicates = 1) %>%
  group_by(taxon_name, Subplot, Location, Intensity) %>%
  summarise(
    resprout_mean = mean(Alive),
    epicormic_consensus = mean(Epicormic),
    basal_consensus = mean(Basal),
    `Char.ht` = mean(`Char.ht`, na.rm = TRUE),
    Scorch = mean(Scorch, na.rm = TRUE) %>% round(digits = 0),
    replicates = sum(replicates)
    ) %>%
  ungroup() %>%
  mutate(
    bud_bank_location = ifelse(epicormic_consensus > 0 & basal_consensus > 0, "epicormic_buds basal_buds", NA),
    bud_bank_location = ifelse(epicormic_consensus > 0 & basal_consensus == 0, "epicormic_buds", bud_bank_location),
    bud_bank_location = ifelse(epicormic_consensus == 0 & basal_consensus > 0, "basal_buds", bud_bank_location),
    bud_bank_location = ifelse(epicormic_consensus == 0 & basal_consensus == 0, "none", bud_bank_location),
    fire_response = ifelse(resprout_mean > 0.7, "resprouts", NA),
    fire_response = ifelse(resprout_mean < 0.3, "fire_killed", fire_response),
    fire_response = ifelse(resprout_mean < 0.7 & resprout_mean > 0.3, "partial_resprouting", fire_response),
    across(c(`Char.ht`, Scorch), ~ na_if(.x, "NaN")),
    Scorch = ifelse(is.na(Scorch), "unknown", Scorch)
  ) %>%
  write_csv("data/Prior_2022/data.csv")