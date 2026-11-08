# ATRP_2026: seedling traits the parser does not read -> raw/ecology_vetted.csv rows, which the parser
# (data/ABRS_AtoM_2026/raw/build_data_csv.R, FOA_SOURCE=atrp) reads.
#   - seedling_germination_location: epigeal / hypogeal, from the page's photo caption ("Cotyledon stage, epigeal germination. (c) CSIRO";
#     atrp_page_extras.csv, from ATRP_scripts/page_extras.py); both when the captions give both
#   - seed_germination_time (days): the Seedlings field's closing sentence, "Seed germination time 15 to 19 days." -> 15-19,
#     "Seed germination time 21 days." -> 21, "ca. 30 days" -> 30, "to 43 days" -> max:43 ("unknown" and "433" without a unit are not read)
# Run from the repository root: Rscript data/ATRP_2026/raw/seedlings.R

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr); library(tidyr) })

dx <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra"
prof <- read_csv(file.path(dx, "atrp_fact_sheets.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  rename(file = 1) %>% filter(taxon_rank != "family") %>% mutate(taxon_name = str_replace_all(str_squish(index_name), "\\(\\s+", "("))   # (as the parser names them)
ex <- read_csv(file.path(dx, "atrp_page_extras.csv"), show_col_types = FALSE, col_types = cols(.default = "c"))

germ <- prof %>% select(file, taxon_name) %>% inner_join(ex %>% filter(!is.na(germination)), by = "file") %>%
  transmute(taxon_name, trait_name = "seedling_germination_location", value = germination, evidence = germination_caption)

gt_re <- "\\b[Ss]eed germination time (?:in |ca\\.? |c\\. )?(to )?([0-9]+)(?:\\s*(?:to|-|–)\\s*([0-9]+))?\\s*days\\b"
gtime <- prof %>% filter(str_detect(coalesce(Seedlings, ""), gt_re)) %>%
  mutate(m = str_match(Seedlings, gt_re), value = case_when(!is.na(m[, 2]) ~ paste0("max:", m[, 3]), is.na(m[, 4]) | m[, 3] == m[, 4] ~ m[, 3],
                                                             TRUE ~ paste0(m[, 3], "-", m[, 4]))) %>%
  transmute(taxon_name, trait_name = "seed_germination_time", value, evidence = m[, 1])

out <- bind_rows(germ, gtime) %>%
  transmute(taxon_name, trait_name, value, commonness_qualifier = NA_character_, trait_scoring_method = NA_character_, evidence,
            scoring_certainty = NA_character_) %>%
  arrange(taxon_name, trait_name)
write_csv(out, "data/ATRP_2026/raw/ecology_vetted.csv", na = "")
print(count(out, trait_name))
# germination-time sentences not read
left <- prof %>% filter(str_detect(coalesce(Seedlings, ""), "germination time"), !str_detect(Seedlings, gt_re))
print(left %>% transmute(taxon_name, s = str_extract(Seedlings, "[Ss]eed germination time[^.]*(?:\\.[0-9][^.]*)?")), width = 200)
