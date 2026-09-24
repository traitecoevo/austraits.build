metadata_check_custom_R_code(current_study) |>
  distinct(pick(5:13, 33, 34)) |> arrange(`Pollinator match value`) |>
  write_csv(paste0("data/", current_study, "/raw/Prendergast_2026_pollinator_taxon_substitutions.csv"))