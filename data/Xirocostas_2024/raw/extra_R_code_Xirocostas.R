library(dplyr)
library(tidyr)
library(readr)

raw <- read_csv("data/Xirocostas_2024/raw/pollination_final_NOR_excl.csv",
                col_types = cols(.default = "c"), name_repair = "minimal")

group_cols <- names(raw)[12:21]

visitor_map <- c(
  "Bees/wasps (Apoidea)"            = "hymenoptera",
  "flies (diptera)"                 = "fly",
  "beetles (coleoptera)"            = "beetle",
  "butterflies/moths (lepidoptera)" = "lepidoptera",
  "ants (formicidae)"               = "ant",
  "true bugs (hemiptera)"           = "hemiptera",
  "spiders (araneae)"               = "spider",
  "thrips (thysanoptera)"           = "thrips",
  "Dragonflies (Odonata)"           = "dragonfly",
  "Grasshoppers (orthoptera)"       = "orthoptera"
)

surveys <- raw |>
  mutate(
    survey_id = sprintf("survey_%03d", row_number()),
    collection_date = format(as.Date(date, format = "%d/%m/%Y"), "%Y-%m-%d"),
    collection_date = if_else(date == "17/07/2010", "2019-07-17", collection_date)
  ) |>
  mutate(
    location_name = case_when(
      locality == "ANU Campus" & species == "Hypericum perforatum" ~ "ANU Acton campus",
      locality == "ANU Campus" & species == "Prunella vulgaris" ~ "ANU Daley Road",
      site == "Canberra" & species == "Centranthus ruber" ~ "ANU Sullivans Creek Road",
      site == "Canberra" & species == "Trifolium repens" ~ "Black Mountain Peninsula",
      site == "Melbourne" & species == "Centranthus ruber" ~ "Clayton",
      site == "Melbourne" & species == "Trifolium repens" ~ "Damper Creek Reserve",
      locality == "Revesby" & species == "Hypericum perforatum" ~ "Revesby (suburb)",
      locality == "Tharwa" ~ "Paddys River",
      locality == "Cooma" & species == "Convolvulus arvensis" ~ "Cooma_1",
      locality == "Cooma" & species == "Ranunculus repens" ~ "Cooma_2",
      locality == "Hobart Rivulet" ~ "South Hobart",
      locality == "Tasman Hwy" ~ "Buckland",
      locality == "Miraflores" ~ "Miraflores de la Sierra",
      locality %in% c("Guadalix", "Guadelix") ~ "Guadalix de la Sierra",
      locality %in% c("Bustarviejo", "Busarviejo") & species == "Leucanthemum vulgare" ~ "Bustarviejo-Miraflores road",
      locality %in% c("Bustarviejo", "Bustarviejo (urban)") & species == "Convolvulus arvensis" ~ "Bustarviejo_2",
      locality == "Bustarviejo" & species == "Ranunculus repens" ~ "Bustarviejo_3",
      locality == "Bustarviejo" ~ "Bustarviejo_1",
      locality == "Lysterfield" & species == "Hypericum perforatum" ~ "Lysterfield_1",
      locality == "Lysterfield" & species == "Silene gallica" ~ "Lysterfield_2",
      locality == "M1 (near Scotchmans)" ~ "Mount Waverley, Therese Avenue",
      locality == "Scotchmans creek trail" ~ "Scotchmans Creek Trail",
      locality == "Waterside" ~ "University of Northampton Waterside Campus",
      locality == "Revesby" & species == "Trifolium repens" ~ "Padstow",
      locality == "Konguta" & species %in% c("Convolvulus arvensis", "Prunella vulgaris") ~ "Konguta_1",
      locality == "Konguta" & species %in% c("Hypericum perforatum", "Lotus corniculatus") ~ "Konguta_2",
      locality == "Konguta" & species == "Leucanthemum vulgare" ~ "Vahessaare",
      locality == "Konguta" & species == "Trifolium repens" ~ "Annikoru",
      TRUE ~ locality
    )
  ) |>
  relocate(survey_id) |>
  relocate(collection_date, .after = date) |>
  relocate(location_name, .after = locality)

visited <- surveys |>
  pivot_longer(all_of(group_cols), names_to = "visitor_group", values_to = "visitor_count") |>
  mutate(visitor_count = as.integer(visitor_count)) |>
  filter(visitor_count > 0) |>
  mutate(flower_visitor = unname(visitor_map[visitor_group]))

not_visited <- surveys |>
  filter(total.visits == "0") |>
  select(-all_of(group_cols)) |>
  mutate(visitor_group = "no visitors", visitor_count = 0L, flower_visitor = NA_character_)

out <- bind_rows(visited, not_visited) |>
  arrange(survey_id, match(visitor_group, c(group_cols, "no visitors")))

stopifnot(!anyNA(out$flower_visitor[out$visitor_count > 0]), n_distinct(out$survey_id) == nrow(raw))

write_csv(out, "data/Xirocostas_2024/data.csv", na = "")
