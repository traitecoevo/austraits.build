# Chinnock_2007_2: what the book's family description states of every taxon -> raw/ecology_vetted.csv rows, which the parser
# (data/ABRS_AtoM_2026/raw/build_data_csv.R, FOA_SOURCE=chinnock) reads.
#   - leaf_compoundness: simple. The taxon descriptions do not say so; the family description does, of all its members ("Leaves simple,
#     alternate or spiral, more rarely opposite or whorled"), a single value (user 2026-11-09).
# Run from the repository root: Rscript data/Chinnock_2007_2/raw/vet_family.R

suppressPackageStartupMessages({ library(dplyr); library(readr) })

prof <- read_csv("~/GitHub/austraits.build-data.scraping.scripts/data_extra/chinnock2007_profiles.csv", show_col_types = FALSE, col_types = cols(.default = "c"))
out <- prof %>% transmute(taxon_name = taxon_name, trait_name = "leaf_compoundness", value = "simple", commonness_qualifier = NA_character_,
                          trait_scoring_method = "inferred_from_family", evidence = "[family description] Leaves simple, alternate or spiral, more rarely opposite or whorled",
                          scoring_certainty = NA_character_) %>% distinct()
write_csv(out, "data/Chinnock_2007_2/raw/ecology_vetted.csv", na = "")
cat(nrow(out), "rows\n")
