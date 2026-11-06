# Writes data/Chu_2022/data.csv from the authors' FigShare workbook
# (raw/Chu_2022_FigShare_data.xlsx, doi.org/10.26188/5ebce2c3c57bd). Run from the repo root.
# Each sheet holds the species means and standard errors plotted in one figure, as blocks
# headed "Acronym, LifeForm, Species, ...". Every variable appears in several blocks; the
# script reads all blocks, checks the repeated values agree and keeps one copy of each.
# Exception: the blocks for Fig. 1D (daily ET per shoot dry weight vs SRL) and Fig. 3C
# (SLA vs SRL) average only the 3 plants per species with root scans, so their shootET
# and SLA values differ from the 5-plant means used everywhere else. These are kept as
# separate *_SRL_plants columns.
# Derived columns: species (taxon name without the leading acronym) and location_name.

library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)

f <- "data/Chu_2022/raw/Chu_2022_FigShare_data.xlsx"

read_blocks <- function(sheet) {
  d <- as.matrix(read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "unique_quiet"))
  starts <- which(d == "Acronym", arr.ind = TRUE)
  purrr::map_dfr(seq_len(nrow(starts)), function(i) {
    r <- starts[i, 1]
    c <- starts[i, 2]
    header <- d[r, c:ncol(d)]
    header <- header[seq_len(min(c(which(is.na(header)), length(header) + 1)) - 1)]
    last <- r
    while (last < nrow(d) && !is.na(d[last + 1, c])) last <- last + 1
    block <- as.data.frame(d[(r + 1):last, c:(c + length(header) - 1), drop = FALSE])
    names(block) <- header
    block |>
      pivot_longer(-c(Acronym, LifeForm, Species), names_to = "variable") |>
      mutate(block = paste(sheet, LETTERS[i]), value = as.numeric(value))
  })
}

values <- purrr::map_dfr(c("Figure 1", "Figure 2", "Figure 3", "Figure 4"), read_blocks)

# In the SRL blocks, shootET and an SLA plotted with its SE are 3-plant means. An SLA_mean
# without SE is only the symbol colour scale, which is the 5-plant mean.
values <- values |>
  mutate(
    srl_block = any(variable == "SRL_mean"),
    sla_plotted = any(variable == "SLA_SE"),
    .by = block
  ) |>
  mutate(
    variable = if_else(
      srl_block & (str_starts(variable, "shootET_") | (sla_plotted & str_starts(variable, "SLA_"))),
      paste0(variable, "_SRL_plants"),
      variable
    )
  ) |>
  select(-srl_block, -sla_plotted)

stopifnot(
  n_distinct(values$Species) == 14,
  values |>
    summarise(n = n_distinct(signif(value, 12)), .by = c(Species, variable)) |>
    pull(n) |>
    max() == 1
)

data <- values |>
  distinct(Acronym, LifeForm, Species, variable, value) |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
    species = str_remove(Species, "^\\([A-Za-z]+\\)\\s*"),
    location_name = "Burnley Campus glasshouse"
  ) |>
  select(
    species, Acronym, LifeForm, location_name,
    SLA_mean, SLA_SE, RMF_mean, RMF_SE, SRL_mean, SRL_SE,
    shootDW_mean, shootDW_SE, dailyET_mean, dailyET_SE, shootET_mean, shootET_SE,
    stomata.days_mean, stomata.days_SE, cumET_mean, cumET_SE, deltaMD,
    SLA_mean_SRL_plants, SLA_SE_SRL_plants, shootET_mean_SRL_plants, shootET_SE_SRL_plants
  ) |>
  arrange(species)

write_csv(data, "data/Chu_2022/data.csv", na = "", eol = "\n")
