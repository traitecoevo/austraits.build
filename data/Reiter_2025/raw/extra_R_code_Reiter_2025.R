# Writes data/Reiter_2025/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - basis_of_record: species descriptions cited to Towle et al. (2022) and Copeland and
#     Backhouse (2022) are literature; root samples from the wild sites are field; symbiotic
#     germination trials are lab; observations on nursery plants, and the breeding system,
#     growth form and life history statements drawn from them, are captive_cultivated; the
#     authors' own statement of wild flowering time is field.
#   - replicates: the source's n column, renamed so metadata.yml never references a bare `n`.

library(dplyr)
library(readr)

data <- read_csv("data/Reiter_2025/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    basis_of_record = case_when(
      source_id %in% c("Towle_2022", "Copeland_2022") ~ "literature",
      location_name %in% c("Site LM", "Site BW", "Site WA") ~ "field",
      trait %in% c("seed_germination", "seed_germination_treatment") ~ "lab",
      location_name == "RBGV Cranbourne nursery" ~ "captive_cultivated",
      trait %in% c("breeding_system", "pollination_vector_known", "plant_growth_form",
                   "life_history") ~ "captive_cultivated",
      TRUE ~ "field"
    )
  ) |>
  rename(replicates = n) |>
  relocate(basis_of_record, .after = entity_context)

write_csv(data, "data/Reiter_2025/data.csv", na = "", eol = "\n")
