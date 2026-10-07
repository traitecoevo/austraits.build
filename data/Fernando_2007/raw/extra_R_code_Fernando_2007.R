# Writes data/Fernando_2007/data.csv from raw/trait_data.csv. Run from the repo root.
#   - replicates: number of trees per site (Table 1); 1 for the single-tree sites 5 and 6;
#     47 (all trees, sites 1-8) for the overall minimum/maximum in the Abstract.

library(dplyr)
library(readr)

data <- read_csv("data/Fernando_2007/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(replicates = if_else(entity_type == "individual", "1", n)) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Fernando_2007/data.csv", na = "", eol = "\n")
