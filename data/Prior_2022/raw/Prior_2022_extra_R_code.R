read_csv("data/Prior_2022/raw/data_tmp.csv") %>%
  select(taxon_name, Alive, Scorch, Epicormic, Basal, Subplot, Location, `Char.ht`, Intensity, DBH) %>%
  mutate(replicates = 1) %>%
  group_by(taxon_name, Subplot, Location, Intensity) %>%
  summarise(
    resprout_mean = mean(Alive),
    epicormic_consensus = mean(Epicormic),
    basal_consensus = mean(Basal),
    `Char.ht` = mean(`Char.ht`, na.rm = TRUE),
    Scorch = mean(Scorch, na.rm = TRUE) %>% round(digits = 0),
    replicates = sum(replicates),
    DBH_median = median(DBH, na.rm = TRUE),
    DBH_min = min(DBH, na.rm = TRUE),
    DBH_max = max(DBH, na.rm = TRUE)
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
    across(c(`Char.ht`, Scorch, DBH_median), ~ na_if(.x, "NaN")),
    across(c(`Char.ht`, Scorch, DBH_min), ~ na_if(.x, "Inf")),
    across(c(`Char.ht`, Scorch, DBH_max), ~ na_if(.x, "-Inf")),
    Scorch = ifelse(is.na(Scorch), "unknown", Scorch),
    DBH = ifelse((!is.na(DBH_median) & round(DBH_median,0) != round(DBH_min,0)),paste0(DBH_median," (", DBH_min," - ",DBH_max,")"),DBH_median)
  ) %>%
  write_csv("data/Prior_2022/data.csv")
