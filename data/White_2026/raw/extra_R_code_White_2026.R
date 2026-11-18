# Builds data/White_2026/data.csv from the contributor's files in raw/
# (watches.csv, visits.csv, sites.csv; README.docx describes the study).
#
# One row per visitor taxon per watch (visits.csv joined to watches.csv), plus one
# row for each watch with no visitors (visitor columns blank, n_individuals = 0,
# flower_visitor blank). The raw columns are kept unchanged. The columns added are:
#   collection_date      yyyy-mm from the date column (only month and year survive
#                        in watches.csv: "Mar-14", "Feb-15")
#   location_name        metadata.yml location for each site_id
#   visitor_match_value  most precise visitor name that maps to a controlled value
#   flower_visitor       the flower_visitor value it maps to
#
# Run from the repo root: Rscript data/White_2026/raw/extra_R_code_White_2026.R

library(dplyr)
library(readr)

watches <- read_csv("data/White_2026/raw/watches.csv", col_types = cols(.default = "c"))
visits <- read_csv("data/White_2026/raw/visits.csv", col_types = cols(.default = "c"))

stopifnot(
  nrow(watches) == 197,
  n_distinct(watches$watch_id) == nrow(watches),
  all(visits$watch_id %in% watches$watch_id),
  all(watches$date %in% c("Mar-14", "Feb-15"))
)

watches <- watches |>
  mutate(
    collection_date = case_when(
      date == "Mar-14" ~ "2014-03",
      date == "Feb-15" ~ "2015-02"
    ),
    location_name = case_when(
      site_id == "CAI_BLB_RF" ~ "Barron Gorge NP and Lake Barrine",
      site_id == "CAI_DAV_GRS" ~ "Davies Creek NP",
      site_id == "CAI_KUR_WDL" ~ "Kuranda and Dinden NPs"
    )
  ) |>
  relocate(collection_date, .after = date) |>
  relocate(location_name, .after = site_id)

genus <- function(x) sub(" .*", "", x)

visits <- visits |>
  mutate(
    visitor_match_value = case_when(
      visitor_taxon == "Apis mellifera" ~ "Apis mellifera",
      genus(visitor_taxon) %in% c("Tetragonula", "Austroplebeia", "Xylocopa", "Amegilla") ~
        genus(visitor_taxon),
      visitor_family %in% c(
        "Apidae", "Megachilidae", "Halictidae", "Colletidae", "Ichneumonidae", "Formicidae",
        "Chalcidoidea", "Scoliidae", "Sphecidae", "Vespidae",
        "Syrphidae", "Bombyliidae", "Calliphoridae", "Sarcophagidae",
        "Nymphalidae", "Pieridae", "Lycaenidae", "Papilionidae", "Hesperiidae",
        "Crambidae", "Erebidae", "Sphingidae"
      ) ~ visitor_family,
      TRUE ~ visitor_order
    ),
    flower_visitor = case_when(
      visitor_match_value == "Apis mellifera" ~ "honeybee",
      visitor_match_value %in% c("Tetragonula", "Austroplebeia") ~ "stingless_bee",
      visitor_match_value == "Xylocopa" ~ "carpenter_bee",
      visitor_match_value == "Amegilla" ~ "anthophorini",
      visitor_match_value == "Apidae" ~ "bee",
      visitor_match_value == "Megachilidae" ~ "megachilidae",
      visitor_match_value == "Halictidae" ~ "sweat_bee",
      visitor_match_value == "Colletidae" ~ "colletidae",
      visitor_match_value == "Ichneumonidae" ~ "ichneumon_wasp",
      visitor_match_value == "Formicidae" ~ "ant",
      visitor_match_value %in% c("Chalcidoidea", "Scoliidae", "Sphecidae", "Vespidae") ~ "wasp",
      visitor_match_value == "Syrphidae" ~ "hoverfly",
      visitor_match_value == "Bombyliidae" ~ "bee_fly",
      visitor_match_value %in% c("Calliphoridae", "Sarcophagidae") ~ "carrion_fly",
      visitor_match_value == "Diptera" ~ "fly",
      visitor_match_value %in% c("Nymphalidae", "Pieridae", "Lycaenidae", "Papilionidae",
                                 "Hesperiidae") ~ "butterfly",
      visitor_match_value %in% c("Crambidae", "Erebidae", "Sphingidae") ~ "moth",
      visitor_match_value == "Coleoptera" ~ "beetle",
      visitor_match_value == "Hemiptera" ~ "hemiptera",
      visitor_match_value == "Thysanoptera" ~ "thrips"
    )
  )

visited <- inner_join(watches, visits, by = "watch_id")

not_visited <- watches |>
  filter(!watch_id %in% visits$watch_id) |>
  mutate(n_individuals = "0")

out <- bind_rows(visited, not_visited) |>
  arrange(watch_id)

stopifnot(
  !anyNA(out$location_name),
  !anyNA(out$collection_date),
  !anyNA(out$flower_visitor[!is.na(out$visitor_taxon)]),
  nrow(out) == nrow(visits) + sum(!watches$watch_id %in% visits$watch_id),
  n_distinct(out$watch_id) == nrow(watches)
)

write_csv(out, "data/White_2026/data.csv", na = "")
