library(tidyverse)

# Adds Ks, midday leaf water potential, xylem anatomy and LMA from the supplementary
# table (gcb15641-sup-0002-supinfo.csv) to data.csv, which holds Table 2 of the manuscript.
# Values are joined as reported in the supplementary table; any transformations
# (sign of PSI leaf, vessel/tracheid split) are applied in custom_R_code.
# Run from the repository root. Safe to re-run: only the Table 2 columns of data.csv are kept
# before the join.

table_2_columns <- c("Site", "Species", "psipd (MPa)", "psimin (MPa)", "psiTLP (MPa)",
                     "P50 (MPa)", "P88 (MPa)", "Method", "wood density (g cm-3)", "HSM",
                     "collection_date")

supp_info <-
  read_csv("data/Peters_2021/raw/gcb15641-sup-0002-supinfo.csv", col_types = cols(.default = "c"),
           locale = locale(encoding = "latin1"), name_repair = "unique_quiet") %>%
  slice(-1) %>%   # units row
  mutate(
    # species names misspelled in the supplementary table
    Species = case_when(
      Species == "Acacia Melanoxylon" ~ "Acacia melanoxylon",
      Species == "Dysoxylum papauanum" ~ "Dysoxylum papuanum",
      Species == "Eleocarpus grandis" ~ "Elaeocarpus grandis",
      Species == "Eucalyptus salmonphlioa" ~ "Eucalyptus salmonophloia",
      TRUE ~ Species)
  ) %>%
  select(Species, ks, `PSI leaf`, Dh.mean, Vdensity.mean, Kth.mean, lma.mean)

data <-
  read_csv("data/Peters_2021/data.csv", col_types = cols(.default = "c")) %>%
  select(all_of(table_2_columns))

stopifnot(all(data$Species %in% supp_info$Species))

data %>%
  left_join(supp_info, by = "Species") %>%
  write_csv("data/Peters_2021/data.csv", na = "")
