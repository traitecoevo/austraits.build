# Builds data/DrewSmythe_2023/data.csv from the contributors' xlsx
# (sheet IXN_DATA; the Information sheet holds the study details used in metadata.yml).
#
# Each row is one flower-visitor record. The raw columns are kept unchanged, except
# that Date is written as yyyy-mm-dd and the empty rows below the data are dropped.
# The columns added are:
#   location_name                metadata.yml location for each Area code
#   visitor_taxon_name           the authors' Label, with formatting made consistent
#                                ("Apis.mellifera" -> "Apis mellifera", ".spA" -> " sp. A")
#                                and four genus/species misspellings corrected
#   visitor_match_value          most precise visitor name that maps to a controlled value
#   flower_visitor               the flower_visitor value it maps to
#
# Run from the repo root: Rscript data/DrewSmythe_2023/raw/extra_R_code_DrewSmythe_2023.R

library(dplyr)

raw <- readxl::read_excel(
  "data/DrewSmythe_2023/raw/Drew-Smythe, Emery, Davila 2022-23 interaction data.xlsx",
  sheet = "IXN_DATA", col_types = "text"
)

out <- raw |>
  filter(!is.na(`Plant species`)) |>
  mutate(
    Date = format(as.Date(as.numeric(Date), origin = "1899-12-30"), "%Y-%m-%d"),
    location_name = case_when(
      Area == "REF" ~ "Appin North reference area",
      Area == "STG1" ~ "Appin North rehabilitation Stage 1",
      Area == "STG2" ~ "Appin North rehabilitation Stage 2",
      Area == "STG3" ~ "Appin North rehabilitation Stage 3"
    ),
    visitor_taxon_name = case_when(
      grepl("\\.sp[A-Z]$", Label) ~ sub("\\.sp([A-Z])$", " sp. \\1", Label),
      grepl("^[A-Za-z]+\\.[a-z]+$", Label) ~ sub(".", " ", Label, fixed = TRUE),
      TRUE ~ Label
    ),
    visitor_taxon_name = visitor_taxon_name |>
      sub("^Exoneura bicolour$", "Exoneura bicolor", x = _) |>
      sub("^Frogatella ", "Froggattella ", x = _) |>
      sub("^Polyrachis ", "Polyrhachis ", x = _) |>
      sub("^Nylandaria ", "Nylanderia ", x = _),
    order_clean = case_when(
      `Visitor Order` == "Aranae" ~ "Araneae",
      `Visitor Order` == "Thrysanoptera" ~ "Thysanoptera",
      TRUE ~ `Visitor Order`
    ),
    family_clean = sub("^Superfamily:[^A-Za-z]*", "", `Visitor Family`),
    visitor_match_value = case_when(
      `Visitor Genus` == "Apis" ~ "Apis mellifera",
      grepl("^Exoneura", `Visitor Genus`) ~ "Exoneura",
      family_clean %in% c("Halictidae", "Colletidae", "Formicidae", "Syrphidae", "Calliphoridae",
                          "Bombyliidae", "Scoliidae", "Eucharitidae", "Chalcidoidea",
                          "Lycaenidae", "Nymphalidae", "Zygaenidae", "Psychidae", "Heliozelidae") ~ family_clean,
      TRUE ~ order_clean
    ),
    flower_visitor = case_when(
      visitor_match_value == "Apis mellifera" ~ "honeybee",
      visitor_match_value == "Exoneura" ~ "allodapini",
      visitor_match_value == "Halictidae" ~ "sweat_bee",
      visitor_match_value == "Colletidae" ~ "colletidae",
      visitor_match_value == "Formicidae" ~ "ant",
      visitor_match_value %in% c("Scoliidae", "Eucharitidae", "Chalcidoidea") ~ "wasp",
      visitor_match_value == "Syrphidae" ~ "hoverfly",
      visitor_match_value == "Calliphoridae" ~ "carrion_fly",
      visitor_match_value == "Bombyliidae" ~ "bee_fly",
      visitor_match_value %in% c("Lycaenidae", "Nymphalidae") ~ "butterfly",
      visitor_match_value %in% c("Zygaenidae", "Psychidae", "Heliozelidae") ~ "moth",
      visitor_match_value == "Coleoptera" ~ "beetle",
      visitor_match_value == "Diptera" ~ "fly",
      visitor_match_value == "Hemiptera" ~ "hemiptera",
      visitor_match_value == "Lepidoptera" ~ "lepidoptera",
      visitor_match_value == "Orthoptera" ~ "orthoptera",
      visitor_match_value == "Thysanoptera" ~ "thrips",
      visitor_match_value == "Araneae" ~ "spider"
    )
  )

stopifnot(
  !anyNA(out$location_name),
  !anyNA(out$Date),
  !anyNA(out$flower_visitor),
  n_distinct(out$`Obs#`) == nrow(out)
)

out <- out |> select(-order_clean, -family_clean)

readr::write_csv(out, "data/DrewSmythe_2023/data.csv", na = "")
