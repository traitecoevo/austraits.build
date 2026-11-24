# Writes data/Fernando_2006/data.csv from raw/trait_data.csv. Run from the repo root.
#   - replicates: the paper does not state how many plants were analysed for bulk leaf Mn
#     (PIXE/EDAX used at least two leaves from six G. bidwillii trees and from a single
#     plant of each other species), so replicates is left as "unknown".
#   - location_name: blanked for Virotia neurophylla, which was collected in New Caledonia
#     and is excluded in metadata.yml; the site details stay in raw/location_data.csv.

library(dplyr)
library(readr)

data <- read_csv("data/Fernando_2006/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    replicates = if_else(n != "", n, "unknown"),
    location_name = if_else(taxon_name == "Virotia neurophylla", "", location_name)
  ) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Fernando_2006/data.csv", na = "", eol = "\n")
