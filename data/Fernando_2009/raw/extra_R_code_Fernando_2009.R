# Writes data/Fernando_2009/data.csv from raw/trait_data.csv. Run from the repo root.
#   - specimen_group: the herbarium specimen group a value summarises, i.e. the 11
#     geographically isolated G. bidwillii populations (B-1 to B-11, Table 2) and the
#     Maytenus cunninghamii groups from Weipa and Burke (Results text); blank otherwise.
#   - individual_id: single specimens quoted in the Results text, numbered within taxon
#     (e.g. the three G. bamagensis values); blank for species/group means.
#   - replicates: plants per species/group (Table 2 n, or the count stated in the text);
#     1 for single specimens.

library(dplyr)
library(readr)
library(stringr)

data <- read_csv("data/Fernando_2009/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    specimen_group = if_else(str_detect(context, "^population: "),
                             str_remove(context, "^population: "), ""),
    replicates = if_else(entity_type == "individual", "1", str_extract(n, "^[0-9]+"))
  ) |>
  group_by(taxon_name, entity_type) |>
  mutate(individual_id = if_else(entity_type == "individual",
                                 paste("specimen", cumsum(entity_type == "individual")), "")) |>
  ungroup() |>
  relocate(specimen_group, individual_id, .after = entity_context) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Fernando_2009/data.csv", na = "", eol = "\n")
