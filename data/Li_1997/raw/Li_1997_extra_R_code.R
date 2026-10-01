locations <- data %>%
  mutate(
    `lat/long pair` = str_replace_all(`lat/long pair`, "'", ""),
    `lat/long pair` = str_replace_all(`lat/long pair`, "°", "")
         ) %>%
  tidyr::separate_wider_delim(cols = `lat/long pair`, delim = " ", names_sep = "_") %>%
  mutate(
    `latitude (deg)` = as.numeric(`lat/long pair_1`) + (as.numeric(`lat/long pair_2`)/60),
    `longitude (deg)` = -as.numeric(`lat/long pair_3`) - (as.numeric(`lat/long pair_4`)/60)
  ) %>%
  distinct(locality, `latitude (deg)`, `longitude (deg)`) %>%
  arrange(locality)
