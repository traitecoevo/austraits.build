# Writes data/Hodgkinson_1998/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - value: the build value - source term (trait_value) for categorical rows, the parsed
#     number (trait_value_clean) for numeric rows.
#   - replicates: copy of n, renamed so metadata.yml never refers to a bare `n`;
#     "unknown" where the source gives no sample size.
#   - basis_of_record: records from the burning and cutting experiment (burnt, cut and
#     unburnt control plots) are field_experiment; bark thickness, water potential and
#     species descriptions come from unmanipulated plants and are field.

library(dplyr)
library(readr)

data <- read_csv("data/Hodgkinson_1998/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = if_else(treatment %in% c("burnt", "cut", "unburnt control"),
                              "field_experiment", "field"),
    replicates = if_else(n == "", "unknown", n),
    value = if_else(value_type == "mode", trait_value, trait_value_clean)
  ) |>
  relocate(basis_of_record, .after = entity_context) |>
  relocate(value, .after = trait_value_clean) |>
  relocate(replicates, .after = n)

write_csv(data, "data/Hodgkinson_1998/data.csv", na = "", eol = "\n")
