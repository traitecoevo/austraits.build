# Rebuild the `source_id` column of data/Wenk_2025/data.csv from the reference
# columns already in the file (author, year, short_title, title, type).
#
# Rules (one source_id per distinct `reference` string):
#   1. Online floras / databases, and works by an institution -> flora or agency
#      code + year (e.g. ABRS_2025, NHNSW_2025, RBGV_2025, WAH_2026, ATRP_2026;
#      printed Flora of Australia volumes -> ABRS_<year>). Codes in `codes` below.
#   2. Works with a personal author -> first-author surname + year
#      (ASCII-transliterated, spaces/apostrophes removed, e.g. deVogel_2026).
#   3. Works with no author and no code -> compact title + year
#      (e.g. FloraKimberleyRegion_1992).
#   4. Year missing -> no year suffix.
#   5. Rows with no matched reference (reference == "NA NA. NA. NA NA. NA.") -> blank.
#   6. Distinct references sharing an id -> suffixes _1, _2, ... (ordered by reference).
#
# Run from the repo root: Rscript data/Wenk_2025/raw/scripts/add_source_id.R
# The references are NOT yet mapped as sources in metadata.yml.

suppressMessages({library(dplyr); library(readr); library(stringr)})

f <- "data/Wenk_2025/data.csv"
d <- read_csv(f, col_types = cols(.default = col_character()), na = character(), trim_ws = FALSE,
              show_col_types = FALSE)

nz <- function(x) ifelse(x %in% c("NA", ""), NA_character_, x)

# Codes for institutional / unauthored sources, matched on author, else short_title, else title
codes <- tribble(
  ~field,        ~match,                                            ~code,
  "short_title", "^Flora of Australia [0-9]+[AB]?$",                "ABRS",
  "author",      "^Australian Biological Resources Study$",         "ABRS",
  "short_title", "^ATRP$",                                          "ATRP",
  "short_title", "^Australian Tropical Ferns and Lycophytes$",      "ATH",
  "author",      "^National Herbarium of New South Wales$",         "NHNSW",
  "short_title", "^VicFlora$",                                      "RBGV",
  "author",      "^Western Australian Herbarium$",                  "WAH",
  "author",      "^Australian Systematic Botany Society$",          "ASBS",
  "author",      "^Royal Botanic Gardens \\(Vic\\.\\)",             "RBGV",
  "author",      "^Royal Botanic Garden, Calcutta",                 "Roxburgh",
  "author",      "^Garden, New York Botanical",                     "NYBG",
  "author",      "^Geology, British Museum",                        "Andrews",
  "author",      "^Moskovskoe obshchestvo liubitelei prirody",      "MOIP",
  "author",      "^Queensland\\. National Parks and Wildlife",      "QPWS",
  "author",      "^Queensland Government$",                         "QldPoisonsInfo",
  "author",      "^Brisbane City Council$",                         "BrisbaneWeedID",
  "author",      "^Northern Territory Government$",                 "NTThreatenedPlants",
  "author",      "^South Australian Seed Conservation Centre$",     "SeedsOfSA",
  "author",      "^South African National Biodiversity Institute$", "SANBIeFlora",
  "author",      "^Native Plant Trust$",                            "GoBotany",
  "author",      "^Jepson Flora Project",                           "Jepson",
  "author",      "^Missouri Botanical Garden$",                     NA,   # use short_title
  "author",      "^Department of Primary Industries",               NA,
  "author",      "^Centre for Invasive Species Solutions$",         NA,
  "author",      "^North Carolina",                                 NA,
  "author",      "^New York Botanic Gardens$",                      NA,
  "author",      "^University of Florida$",                         NA,
  "author",      "^Tasmania Government$",                           NA,
  "short_title", "^eFloras\\.org$",                                 "eFloras",
  "short_title", "^Flora of South Australia\\. 4$",                 "FloraSouthAustralia4",
  "short_title", "^Decades Kewenses LXXXV$",                        "DecadesKewenses85",
  "title",       "^Flora of the Kimberley Region$",                 "FloraKimberleyRegion",
  "title",       "^Flora of New South Wales\\. 4\\.?$",             "FloraNSW4",
  "title",       "^Repertorium specierum novarum",                  "FeddesRepertorium23",
  "title",       "^Type of Digitaria blakei",                       "JSTORGlobalPlants",
  "title",       "^iNaturalist$",                                   "iNaturalist"
)

lookup_code <- function(author, short_title, title) {
  for (i in seq_len(nrow(codes))) {
    val <- switch(codes$field[i], author = author, short_title = short_title, title = title)
    if (!is.na(val) && str_detect(val, codes$match[i])) return(c(hit = TRUE, code = codes$code[i]))
  }
  c(hit = FALSE, code = NA_character_)
}

# compact a title into CamelCase, dropping small words and punctuation
compact_title <- function(x) {
  w <- str_split(stringi::stri_trans_general(x, "Latin-ASCII"), "[^A-Za-z0-9]+")[[1]]
  w <- w[w != "" & !tolower(w) %in% c("of", "the", "and", "de", "d", "du", "la", "in")]
  # capitalise each word unless it already has internal capitals (eFlora, iNaturalist)
  paste0(ifelse(str_detect(w, "^[a-z]+[A-Z]"), w, paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))),
         collapse = "")
}

first_surname <- function(author) {
  a <- str_trim(str_split_1(author, ";")[1])
  s <- str_trim(str_split_1(a, ",")[1])
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- str_replace_all(s, "[‐‑‒–]", "-")
  str_remove_all(s, "[^A-Za-z-]")
}

refs <- d |>
  distinct(reference, author, year, short_title, title) |>
  mutate(across(everything(), nz)) |>
  group_by(reference) |> slice(1) |> ungroup()

refs$base <- NA_character_
for (i in seq_len(nrow(refs))) {
  r <- refs[i, ]
  if (is.na(r$reference) || r$reference == "NA NA. NA. NA NA. NA.") next  # no matched reference -> blank
  lc <- lookup_code(r$author, r$short_title, r$title)
  stem <- if (lc[["hit"]] == "TRUE" && !is.na(lc[["code"]])) {
    lc[["code"]]
  } else if (lc[["hit"]] == "TRUE" || (is.na(r$author))) {
    compact_title(coalesce(r$short_title, r$title))
  } else {
    first_surname(r$author)
  }
  refs$base[i] <- if (is.na(r$year)) stem else paste0(stem, "_", r$year)
}

refs <- refs |>
  group_by(base) |> arrange(reference, .by_group = TRUE) |>
  mutate(source_id_new = if (n() > 1 && !is.na(first(base))) paste0(base, "_", row_number()) else base) |>
  ungroup()

d <- d |>
  left_join(refs |> select(reference, source_id_new), by = "reference") |>
  mutate(source_id = coalesce(source_id_new, "")) |>
  select(-source_id_new)

write_csv(d, f, na = "NA")
write_csv(refs |> select(source_id = source_id_new, reference, author, year, short_title) |> arrange(source_id),
          "data/Wenk_2025/raw/scripts/source_id_lookup.csv", na = "")
