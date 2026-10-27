# Builds data/Lopresti_2026/data.csv from the authors' xlsx
# (sheet "Floral visitors_ALL"; the Metadata sheet explains each column).
#
# Each row is one floral-visitor record on Senna obtusifolia, from human observations,
# digital video recordings (DVR) or captured specimens. The raw columns are kept unchanged
# (read as text, so no Excel rounding). The columns added are:
#   taxon_name                   the study plant, Senna obtusifolia (not a column in the xlsx)
#   collection_date              yyyy-mm-dd, from `date`, which holds Excel date serials, DVR
#                                clip labels ("01Jun_1030 WNR", "Chochola_10102023_GoPro9")
#                                or a plain year. The Springvale serial 43997 (2020-06-15) is
#                                read as 2022-06-15, matching the Springvale DVR clip; the
#                                Xmatkuil serial 45026 (2023-04-10) is read as 2023-10-04
#                                (day and month swapped on entry)
#   location_name                metadata.yml location for each studySite (Table S1 names);
#                                blank where studySite is ND
#   foraging_behaviour           foragingbehaviour in lower case, or "non-buzzing (undescribed)"
#                                where buzzing is "Not buzzing" but no behaviour was recorded
#                                (Appendix S1, Table S3)
#   flower_visitor               flower_visitor value for the visitor's taxon (speciesScientific)
#   pollination_vector_possible  the same value, for records where the visitor contacted the
#                                stigma or carried Senna pollen (sicklepodPollenCount > 0)
#
# Two further rows, with no range, site or visitor, hold species-level traits stated in the
# paper's description of the study species (Methods, Study species and sites):
#   flower_nectar_production     "nectarless", as worded in the paper
#   flower_anther_dehiscence     "poricidal"
#   flower_pollinator_reward     "pollen": flowers are nectarless and visitors forage for pollen
#   breeding_system              "self-compatible", from the authors' hand-pollination experiments
#   source_text                  the sentences these values come from
#
# Run from the repo root: Rscript data/Lopresti_2026/raw/extra_R_code_Lopresti_2026.R

library(dplyr)

raw <- readxl::read_excel(
  "data/Lopresti_2026/raw/Floral visitors_ALL_publishing.xlsx",
  sheet = "Floral visitors_ALL", col_types = "text"
)

dvr_au <- "^([0-9]{1,2}) ?([A-Z][a-z]{2})_.*$"
dvr_mx <- "^.*_([0-9]{2})([0-9]{2})(20[0-9]{2})_.*$"

out <- raw |>
  filter(!is.na(uniqueID)) |>
  mutate(
    taxon_name = "Senna obtusifolia",
    collection_date = case_when(
      date == "43997" ~ "2022-06-15",
      date == "45026" ~ "2023-10-04",
      grepl("^[0-9]{5}$", date) ~ format(as.Date(suppressWarnings(as.numeric(date)), origin = "1899-12-30")),
      grepl(dvr_au, date) ~ format(as.Date(sub(dvr_au, "\\1 \\2 2022", date), format = "%d %b %Y")),
      grepl(dvr_mx, date) ~ sub(dvr_mx, "\\3-\\2-\\1", date),
      grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", date) ~ format(as.Date(date, format = "%d/%m/%Y")),
      date == "2022" ~ "2022"
    ),
    location_name = case_when(
      studySite == "Centenary Park" ~ "Trinity Beach",
      studySite == "White Rock" ~ "White Rock",
      studySite == "Barron River" ~ "Kamerunga, Barron River",
      studySite == "Springvale" ~ "Lakeland",
      studySite == "West Normanby River" ~ "West Normanby River",
      studySite %in% c("Chochola", "Xmatkuil", "Tekik de Regil") ~ studySite
    ),
    foraging_behaviour = case_when(
      !is.na(foragingbehaviour) ~ tolower(foragingbehaviour),
      buzzing == "Not buzzing" ~ "non-buzzing (undescribed)"
    ),
    flower_visitor = case_when(
      speciesScientific == "Apis mellifera" ~ "honeybee",
      range == "Invasive" & (genus %in% c("Tetragonula", "Austroplebeia") | speciesScientific == "Meliponini") ~ "stingless_bee",
      genus == "Xylocopa" ~ "carpenter_bee",
      genus == "Amegilla" ~ "anthophorini",
      family %in% c("Halictidae", "Halicitdae") ~ "sweat_bee",
      family == "Colletidae" ~ "colletidae",
      family == "Apidae" ~ "bee",
      family == "Formicidae" ~ "ant",
      family %in% c("Vespidae", "Chalcididae") ~ "wasp",
      family == "Syrphidae" ~ "hoverfly",
      family == "Calliphoridae" ~ "carrion_fly",
      family == "Nymphalidae" ~ "butterfly",
      order == "Coleoptera" ~ "beetle",
      order == "Blattodea" ~ "cockroach",
      order == "Lepidoptera" ~ "lepidoptera",
      order == "Diptera" ~ "fly",
      order == "Hymenoptera" ~ "hymenoptera"
    ),
    pollination_vector_possible = if_else(
      `stigma contact` %in% "1" | coalesce(suppressWarnings(as.numeric(sicklepodPollenCount)) > 0, FALSE),
      flower_visitor, NA_character_
    )
  )

aus <- filter(out, range == "Invasive")

stopifnot(
  n_distinct(out$uniqueID) == nrow(out),
  !anyNA(aus$collection_date),
  !anyNA(out$flower_visitor),
  all(!is.na(aus$location_name) | aus$studySite == "ND"),
  all(substr(aus$collection_date, 1, 4) == "2022")
)

species_rows <- tibble(
  taxon_name = "Senna obtusifolia",
  collection_date = "2026",
  flower_nectar_production = c("nectarless", NA),
  flower_anther_dehiscence = c("poricidal", NA),
  flower_pollinator_reward = c("pollen", NA),
  breeding_system = c(NA, "self-compatible"),
  source_text = c(
    paste(
      "It has common traits of buzz-pollinated plants, including poricidal anthers and nectarless flowers.",
      "For every floral visitor, we recorded the behavior the individual used to extract pollen (buzzing or not buzzing)."
    ),
    paste(
      "Hand pollination experiments showed that S. obtusifolia is fully self-compatible but that it can set little seed",
      "in the absence of pollinators in both the native and invaded regions (Lopresti et al., unpublished)."
    )
  )
)

out <- bind_rows(out, species_rows)

readr::write_csv(out, "data/Lopresti_2026/data.csv", na = "")
