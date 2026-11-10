suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr)})
S <- Sys.getenv("S")
rd <- function(f) read_csv(f, col_types = cols(.default = "c"))
# the extraction is split across data/ABRS_*_2026/data.csv; the old snapshot is $S/data_old/ (one csv per dataset) or $S/data_old.csv
rd_all <- function(fs) bind_rows(lapply(fs, rd))
old_fs <- if (dir.exists(file.path(S, "data_old"))) list.files(file.path(S, "data_old"), "\\.csv$", full.names = TRUE) else file.path(S, "data_old.csv")
o <- rd_all(old_fs); n <- rd_all(Sys.glob("data/ABRS_*_2026/data.csv")); if (!"plant_organ_measured" %in% names(o)) o$plant_organ_measured <- NA_character_
n_prev <- n %>% filter(family %in% unique(o$family))
cat("old rows", nrow(o), " new rows (old families)", nrow(n_prev), " new families rows", nrow(n) - nrow(n_prev), "\n")
keys <- c("taxon_name","commonness_qualifier","leaf_division","entity_measured","population_region","plant_organ_measured","trait_scoring_method")
lg <- function(x) x %>% select(-any_of(c("family","taxon_rank","foa_url","common_name","biostatus","profile_author"))) %>% mutate(.r = row_number()) %>%
  pivot_longer(-c(all_of(keys), .r), values_drop_na = TRUE) %>% select(-.r) %>% filter(!grepl("_description$|_text$|^notes$", name))
lo <- lg(o); ln <- lg(n_prev)
rm_ <- anti_join(lo, ln, by = c(keys, "name", "value")); ad <- anti_join(ln, lo, by = c(keys, "name", "value"))
cat("\nREMOVED values:", nrow(rm_), "\n"); print(rm_ %>% count(name, trait_scoring_method, sort = TRUE), n = 40)
cat("\nADDED values:", nrow(ad), "\n"); print(ad %>% count(name, trait_scoring_method, sort = TRUE), n = 40)
saveRDS(list(rm = rm_, ad = ad), file.path(S, "diff.rds"))
