read_csv("data/ACT_Government_2003/raw/raw_data.csv") %>%
  tidyr::separate_longer_delim(`Regenerative Mechanisms`, delim = ", ") %>%
  dplyr::mutate(
    bud_bank_location = case_when(
      `Regenerative Mechanisms` == "C" ~ "root_crown",
      `Regenerative Mechanisms` == "E" ~ "epicormic_buds",
      `Regenerative Mechanisms` == "L" ~ "basal_buds",
      `Regenerative Mechanisms` == "L N" ~ "basal_buds none",
      `Regenerative Mechanisms` == "N" ~ "none",
      `Regenerative Mechanisms` == "Se" ~ "none",
      `Regenerative Mechanisms` == "Se?" ~ "none",
      `Regenerative Mechanisms` == "Su" ~ "bud-bearing_root",
      `Regenerative Mechanisms` == "Not burnt at survey site(s)" ~ NA
    ),
    resprouting_capacity = case_when(
      `Regenerative Mechanisms` == "C" ~ "resprouts",
      `Regenerative Mechanisms` == "E" ~ "resprouts",
      `Regenerative Mechanisms` == "L" ~ "resprouts",
      `Regenerative Mechanisms` == "L N" ~ "resprouts fire_killed",
      `Regenerative Mechanisms` == "N" ~ "fire_killed",
      `Regenerative Mechanisms` == "Se" ~ "fire_killed",
      `Regenerative Mechanisms` == "Se?" ~ "fire_killed",
      `Regenerative Mechanisms` == "Su" ~ "resprouts",
      `Regenerative Mechanisms` == "Not burnt at survey site(s)" ~ NA
    ),
    post_fire_recruitment = case_when(
      `Regenerative Mechanisms` == "C" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "E" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "L" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "L N" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "N" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "Se" ~ "post_fire_recruitment",
      `Regenerative Mechanisms` == "Se?" ~ "post_fire_recruitment",
      `Regenerative Mechanisms` == "Su" ~ "post_fire_recruitment_absent",
      `Regenerative Mechanisms` == "Not burnt at survey site(s)" ~ NA
    )
  ) %>%
  group_by(Species) %>%
  mutate(
    `Regenerative Mechanisms` = paste0(`Regenerative Mechanisms`, collapse = ", ")
    ) %>%
  ungroup() %>%
  distinct() %>%
  group_by(Species) %>%
  mutate(
    bud_bank_location = paste0(bud_bank_location, collapse = " "),
    resprouting_capacity = paste0(resprouting_capacity, collapse = " "),
    post_fire_recruitment = paste0(post_fire_recruitment, collapse = " ")
  ) %>%
  ungroup() %>%
  distinct() %>%
  mutate(
    bud_bank_location = ifelse(`Regenerative Mechanisms` == "N", stringr::str_replace(bud_bank_location, " none", ""), bud_bank_location)
  ) %>%
  distinct() %>%
  mutate(
    across(c(bud_bank_location, resprouting_capacity, post_fire_recruitment), ~ str_replace(.x, " NA", ""))
  ) %>%
  write_csv("data/ACT_Government_2003/data.csv")

(austraits %>% extract_dataset("ACT_Government_2003"))$traits %>% distinct(taxon_name) -> ACT_taxa
align_taxa(ACT_taxa$taxon_name, identifier = "ACT_Government_2003", resources = resources) -> ACT_aligned
ACT_aligned %>% select(find = original_name, replace = aligned_name, reason = aligned_reason, taxonomic_rank = taxon_rank) -> to_add

  