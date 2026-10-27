# Builds data/Popic_2013/data.csv from the contributors' xlsx.
#
# The xlsx holds two plant x bee matrices with the same row and column order
# (plants down the rows, bee species across the columns):
#   flower_visitor               number of visits (individual bees caught) by each bee
#                                species on each plant species (the visitation network)
#   pollination_vector_possible  pollen of each plant species transported by each bee
#                                species (the pollen-transport network)
# The Information sheet holds the study details used in metadata.yml.
#
# Each row of data.csv is one plant x bee species pair with a visit or pollen transport
# (or both); blank cells are pairs with no interaction and are dropped. The columns are:
#   Plant species                plant name as given in the flower_visitor sheet (the
#                                pollen sheet repeats the same plants, with a few spelling
#                                slips, e.g. "Scaveola", "Abitilon", "Goodeniaceae.sp1")
#   Bee species                  bee column header as given in the flower_visitor sheet
#   location_name                metadata.yml location (data pooled across six sites)
#   visitor_taxon_name           Bee species without the authority, with the genus
#                                misspelling "Leiproctus" corrected to "Leioproctus"
#   visitor_family               bee family
#   visitor_match_value          most precise visitor name that maps to a controlled value
#                                (tribe-level genus for Amegilla/Exoneurella, else family)
#   visits                       value from the flower_visitor sheet
#   pollen_transport             value from the pollination_vector_possible sheet, rounded
#                                to 2 decimals (two cells are fractions, e.g. 436.333...)
#   flower_visitor               mapped value, where visits were recorded
#   pollination_vector_possible  mapped value, where pollen was transported
#
# Run from the repo root: Rscript data/Popic_2013/raw/extra_R_code_Popic_2013.R

library(dplyr)

f <- "data/Popic_2013/raw/Popic, Wardle, Davila 2013 interaction dataset.xlsx"

read_matrix <- function(sheet) {
  readxl::read_excel(f, sheet = sheet, col_types = "text", col_names = FALSE, .name_repair = "minimal") |>
    as.matrix()
}

visits <- read_matrix("flower_visitor")
pollen <- read_matrix("pollination_vector_possible")

# the two sheets must list the same plants and bees in the same order
stopifnot(
  identical(dim(visits), dim(pollen)),
  identical(sub(" \\(.*", "", visits[1, -1]), sub(" \\(.*", "", pollen[1, -1])),
  all(diag(adist(gsub("[^a-z0-9]", "", tolower(visits[-1, 1])), gsub("[^a-z0-9]", "", tolower(pollen[-1, 1])))) <= 2)
)

plants <- visits[-1, 1]
bees <- visits[1, -1]

to_long <- function(m, value_name) {
  tibble(
    `Plant species` = rep(plants, times = length(bees)),
    `Bee species` = rep(bees, each = length(plants)),
    !!value_name := as.vector(m[-1, -1])
  )
}

out <- to_long(visits, "visits") |>
  left_join(to_long(pollen, "pollen_transport"), by = c("Plant species", "Bee species")) |>
  filter(!is.na(visits) | !is.na(pollen_transport)) |>
  mutate(
    pollen_transport = as.character(round(as.numeric(pollen_transport), 2)),
    location_name = "north-eastern Simpson Desert",
    visitor_taxon_name = `Bee species` |>
      sub(" \\(.*$", "", x = _) |>
      sub("^Leiproctus ", "Leioproctus ", x = _),
    genus = sub(" .*", "", visitor_taxon_name),
    visitor_family = case_when(
      genus %in% c("Homalictus", "Lipotriches", "Lasioglossum") ~ "Halictidae",
      genus %in% c("Hylaeus", "Leioproctus", "Euhesma", "Callohesma") ~ "Colletidae",
      genus %in% c("Megachile") ~ "Megachilidae",
      genus %in% c("Amegilla", "Exoneurella", "Thyreus") ~ "Apidae"
    ),
    visitor_match_value = case_when(
      genus %in% c("Amegilla", "Exoneurella") ~ genus,
      genus == "Thyreus" ~ "Anthophila",
      TRUE ~ visitor_family
    ),
    visitor_group = case_when(
      visitor_match_value == "Halictidae" ~ "sweat_bee",
      visitor_match_value == "Colletidae" ~ "colletidae",
      visitor_match_value == "Megachilidae" ~ "megachilidae",
      visitor_match_value == "Amegilla" ~ "anthophorini",
      visitor_match_value == "Exoneurella" ~ "allodapini",
      visitor_match_value == "Anthophila" ~ "bee"
    ),
    flower_visitor = if_else(!is.na(visits), visitor_group, NA_character_),
    pollination_vector_possible = if_else(!is.na(pollen_transport), visitor_group, NA_character_)
  ) |>
  select(
    `Plant species`, `Bee species`, location_name, visitor_taxon_name, visitor_family,
    visitor_match_value, visits, pollen_transport, flower_visitor, pollination_vector_possible
  )

stopifnot(
  !anyNA(out$visitor_family),
  !anyNA(out$visitor_match_value),
  sum(as.numeric(out$visits), na.rm = TRUE) == 676,
  sum(!is.na(out$flower_visitor)) == 168,
  sum(!is.na(out$pollination_vector_possible)) == 64
)

readr::write_csv(out, "data/Popic_2013/data.csv", na = "")
