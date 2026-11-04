# Writes data/Freestone_2026/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - source_id: values the paper cites from other studies (row notes start "Cites ...")
#     take the original study as their source; everything else is Freestone_2026.
#   - collection_date: 2017/18 flowering season for this study; publication year for
#     cited values.
#   - pollination_treatment: the treatment named in the context string.
#   - replicates: the source's n column, renamed so metadata.yml never references a bare `n`.

library(dplyr)
library(readr)
library(stringr)

data <- read_csv("data/Freestone_2026/raw/trait_data.csv", col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE)

data <- data |>
  mutate(
    source_id = case_when(
      str_detect(notes, "^Cites Bernhardt & Burns-Balogh 1986") ~ "Bernhardt_1986",
      str_detect(notes, "^Cites Bates 1984a") ~ "Bates_1984",
      str_detect(notes, "^Cites Elliott & Ladd 2002") ~ "Elliott_2002",
      str_detect(notes, "^Cites Peakall 1987, 1989") ~ "Peakall_1987 Peakall_1989",
      str_detect(notes, "^Cites Peakall 1987") ~ "Peakall_1987",
      str_detect(notes, "^Cites Hayashi et al. 2024") ~ "Hayashi_2024",
      str_detect(notes, "^Cites Freestone et al. 2023") ~ "Freestone_2023",
      TRUE ~ "Freestone_2026"
    ),
    collection_date = case_when(
      source_id == "Freestone_2026" ~ "2017/2018",
      source_id == "Peakall_1987 Peakall_1989" ~ "1987/1989",
      TRUE ~ str_extract(source_id, "[0-9]{4}$")
    ),
    pollination_treatment = str_trim(str_extract(context, "(?<=pollination treatment: )[^(;]+"))
  ) |>
  relocate(pollination_treatment, .after = context) |>
  rename(replicates = n)

write_csv(data, "data/Freestone_2026/data.csv", na = "", eol = "\n")
