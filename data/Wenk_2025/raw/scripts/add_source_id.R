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
#   4. Year missing -> the year the online source was consulted, 2026 (`recode`).
#   5. Rows with no matched reference (reference == "NA NA. NA. NA NA. NA.") are
#      linked from the scorer's note (`reference_OG`) where it names a work
#      (`unmatched`); otherwise blank, citing the dataset (custom_R_code puts the
#      note in `measurement_remarks`).
#   6. Distinct references sharing an id -> suffixes _1, _2, ... (ordered by reference).
#   7. Online floras take the keys AusTraits uses for them (`recode`): NTH, SAH,
#      CPBR (EUCLID), Efloras, PPD (Flora of Pakistan), PROTA; their entries copy
#      the flora dataset's reference with the year changed and no access date.
#   8. Keys are checked against every reference already in AusTraits
#      (scripts/source_references.R): the same work (same DOI, or same title and
#      year) takes its existing key and entry; a key naming a different work
#      takes the next free suffix (Brown_2019 -> Brown_2019_2).
#
# Writes the `source_id` column of data.csv, the
# lookup (source_id_lookup.csv) and data/Wenk_2025/sources.yml.
#
# Run from the repo root: Rscript data/Wenk_2025/raw/scripts/add_source_id.R

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble)})
source("scripts/source_references.R")

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
  # hyphens dropped too, as across AusTraits (Airy-Shaw -> AiryShaw)
  str_remove_all(s, "[^A-Za-z]")
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

# ---- 4, 7. AusTraits keys for online floras; a year for undated sources ----
recode <- c(
  FloraNT_2026 = "NTH_2026", FloraSA_2026 = "SAH_2026", EUCLID_2026 = "CPBR_2026",
  eFloras_2026 = "Efloras_2026", FloraPakistan_2026 = "PPD_2026",
  PlantResourcesTropicalAfrica_2026 = "PROTA_2026",
  AVH = "AVH_2026", iNaturalist = "iNaturalist_2026",
  JSTORGlobalPlants = "JSTORGlobalPlants_2026",   # a specimen page in Global Plants
  TasmaniaThreatenedSpeciesLink = "TasmaniaThreatenedSpeciesLink_2026",
  Mueller = "Mueller_1890",   # Victorian Naturalist 7: 67 (Sept. 1890), per IPNI
  FeddesRepertorium23_1926 = "Pfeiffer_1927",  # see `fixes`
  # the same paper under two reference strings; the entry with a DOI is kept
  Fitzgerald_1918_1 = "Fitzgerald_1918", Fitzgerald_1918_2 = "Fitzgerald_1918",
  Johnston_1997_1 = "Johnston_1997", Johnston_1997_2 = "Johnston_1997",
  # Lock 2001 is the Kew Bulletin review of Floodplain Flora; the values come from
  # the book (Cowie, Short & Osterkamp Madsen 2000), cited as in NTH_2014
  Lock_2001 = "Cowie_2000"
)
refs <- refs |> mutate(source_id_new = coalesce(unname(recode[source_id_new]), source_id_new))

# ---- 5. Rows with no matched reference: link where the scorer's note names a work ----
unmatched <- tribble(
  ~match,                                       ~key,
  # Althaea and Abutilon (Malvaceae) are in Part II of the 4th edition
  "Flora of South Australia [24]$",             "FloraSouthAustralia2_1986",
  "^Icones Plantarum Formosanarum 9 - Hayata",  "Hayata_1920",
  "^!!10\\.11646/phytotaxa\\.482\\.2\\.1$",     "Shtein_2021"
)
no_ref <- "NA NA. NA. NA NA. NA."

d <- d |>
  left_join(refs |> select(reference, source_id_new), by = "reference") |>
  mutate(source_id_new = ifelse(reference == no_ref, NA_character_, source_id_new))
for (i in seq_len(nrow(unmatched))) {
  hit <- d$reference == no_ref & str_detect(coalesce(nz(d$reference_OG), ""), unmatched$match[i])
  d$source_id_new[hit] <- unmatched$key[i]
}

# ---- References: one entry per key ----
fields <- d |>
  filter(reference != no_ref) |>
  distinct(reference, title, short_title, author, year, journal, doi, volume, ISBN, pages, type, url) |>
  mutate(across(everything(), nz)) |>
  group_by(reference) |> slice(1) |> ungroup() |>
  left_join(refs |> select(reference, key = source_id_new), by = "reference") |>
  # merged keys keep the entry with a DOI, or (JSTORGlobalPlants) the site's own
  arrange(key, !is.na(doi), !is.na(short_title) | !str_detect(coalesce(url, ""), "/stable/viewer/")) |>
  group_by(key) |> slice_tail(n = 1) |> ungroup()

index <- source_reference_index(exclude = "Wenk_2025")

# Online floras copy the flora dataset's reference, re-dated, without the access date
flora_templates <- c(
  ABRS = "ABRS_2023", NHNSW = "NHNSW_2024", RBGV = "RBGV_2023", WAH = "WAH_2023_1",
  NTH = "NTH_2023", SAH = "SAH_2023", CPBR = "CPBR_2002", TMAG = "TMAG_2009",
  ATRP = "ATRP_2020", Efloras = "Efloras_2010"
)

# Works with no author in the data, completed by hand
book_authors <- list(
  FloraKimberleyRegion_1992 = list(author = "J. R. Wheeler and B. L. Rye and B. L. Koch and A. J. G. Wilson",
    publisher = "Western Australian Herbarium, Department of Conservation and Land Management", place = "Perth"),
  FloraPerthRegion1_1987 = list(author = "N. G. Marchant and J. R. Wheeler and B. L. Rye and E. M. Bennett and N. S. Lander and T. D. Macfarlane",
    publisher = "Western Australian Herbarium, Department of Agriculture", place = "Perth"),
  FloraPerthRegion2_1987 = list(author = "N. G. Marchant and J. R. Wheeler and B. L. Rye and E. M. Bennett and N. S. Lander and T. D. Macfarlane",
    publisher = "Western Australian Herbarium, Department of Agriculture", place = "Perth"),
  FloraNSW4_1993 = list(author = "G. J. Harden", publisher = "New South Wales University Press", place = "Kensington"),
  FloraSouthAustralia4_1986 = list(author = "J. P. Jessop and H. R. Toelken",
    publisher = "South Australian Government Printing Division", place = "Adelaide"),
  FloraCapeRegion_2012 = list(author = "J. C. Manning and P. Goldblatt",
    publisher = "South African National Biodiversity Institute", place = "Pretoria")
)

# Authors the data gives as an institution, a surname alone or in a tangle
authors <- c(
  Andrews_1900 = "Charles William Andrews",
  MOIP_1859 = "{Moskovskoe obshchestvo liubitelei prirody}",
  NYBG_1900 = "{New York Botanical Garden}",
  Roxburgh_1814 = "William Roxburgh",
  Steenis_1948 = "C. G. G. J. van Steenis and M. J. van Steenis-Kruseman",
  Steenis_1974 = "C. G. G. J. van Steenis and M. J. van Steenis-Kruseman",
  Kern_1974 = "J. H. Kern",
  Moore_2025 = "Philip Moore",
  Turczaninow_1849 = "N. Turczaninow",
  Caruel_1879 = "T. Caruel",
  Hopper_1990 = "Stephen D. Hopper and Stephen van Leeuwen and Andrew P. Brown and Susan J. Patrick",
  Jepson_2026 = "{Jepson Flora Project}",
  QPWS_2000 = "{Queensland National Parks and Wildlife Service}",
  Mueller_1890 = "Ferdinand von Mueller",
  Mueller_1892_2 = "Ferdinand von Mueller and Ralph Tate"
)

# Fields the data leaves out or gets wrong
fixes <- list(
  Mueller_1890 = list(journal = "The Victorian Naturalist", volume = "7", pages = "67"),
  Hayasake_2009 = list(journal = "The Journal of Japanese Botany"),
  Mifsud_2026 = list(url = "https://www.maltawildplants.com"),
  # Oreobolus pfeifferianus Barros ex H.Pfeiff., Repert. Spec. Nov. Regni Veg. 23: 352 (1927), per IPNI
  Pfeiffer_1927 = list(year = 1927L, author = "H. Pfeiffer",
    title = "[Account of Oreobolus R.Br.]", journal = "Repertorium specierum novarum regni vegetabilis",
    volume = "23", note = "Title not verified; Oreobolus pfeifferianus is described on p. 352 (IPNI).")
)

manual <- list(
  list(key = "FloraSouthAustralia2_1986", bibtype = "Book", year = 1986L,
       author = "J. P. Jessop and H. R. Toelken",
       title = "Flora of South Australia. 2: Leguminosae - Rubiaceae",
       publisher = "South Australian Government Printing Division", place = "Adelaide"),
  list(key = "Hayata_1920", bibtype = "Book", year = 1920L, author = "Bunzo Hayata",
       title = "Icones Plantarum Formosanarum nec non et Contributiones ad Floram Formosanam. 9",
       publisher = "Bureau of Productive Industries, Government of Formosa", place = "Taihoku"),
  list(key = "Shtein_2021", bibtype = "Article", year = 2021L, author = "Ronen Shtein and Gideon F. Smith",
       title = "A revision of the climbing kalanchoes (Crassulaceae subfam. Kalanchooideae) of Madagascar including the description of Kalanchoe sect. Invasores and K. ser. Vilana",
       journal = "Phytotaxa", volume = "482", number = "2", doi = "10.11646/phytotaxa.482.2.1")
)

institution <- function(x) paste0("{", str_squish(str_remove_all(x, "[,.]+$|,")), "}")

build_entry <- function(r) {
  k <- r$key
  prefix <- str_remove(k, "_[0-9]{4}(_[0-9]+)?$")
  if (r$type %in% "webpage" && prefix %in% names(flora_templates) && str_detect(k, "_[0-9]{4}$")) {
    tk <- flora_templates[[prefix]]
    e <- index |> filter(key == tk) |> arrange(type != "dataset") |> pull(entry) |> (\(x) x[[1]])()
    e$key <- k; e$year <- as.integer(str_extract(k, "[0-9]{4}")); e$note <- NULL
    return(e)
  }
  coded <- lookup_code(r$author, r$short_title, r$title)[["hit"]] == "TRUE"
  author <- if (is.na(r$author)) {
    institution(coalesce(r$short_title, r$title))
  } else if (coded || !str_detect(r$author, ",")) {
    paste(vapply(str_split_1(r$author, ";"), institution, ""), collapse = " and ")
  } else {
    source_authors_from_semicolons(r$author)
  }
  year <- as.integer(coalesce(r$year, str_extract(k, "[0-9]{4}")))
  url <- ifelse(!is.na(r$url) & !str_detect(r$url, "^https?://") & str_detect(r$url, "\\."),
                paste0("https://", r$url), r$url)
  if (!is.na(url) && !str_detect(url, "\\.")) url <- NA_character_
  doi <- source_norm_doi(r$doi)
  e <- switch(
    r$type,
    journalArticle = list(bibtype = "Article", journal = str_remove(r$journal, "—.*$"),
                          volume = r$volume, pages = source_clean_pages(r$pages)),
    book = , bookSection = list(bibtype = "Book", volume = r$volume,
                                pages = if (r$type == "bookSection") source_clean_pages(r$pages)),
    list(bibtype = "Online")
  )
  # a volume already in the title is not repeated
  if (!is.null(e$volume) && !is.na(e$volume) && str_detect(r$title, fixed(paste0(" ", e$volume)))) e$volume <- NULL
  e <- c(list(key = k), e["bibtype"], list(year = year, author = author, title = source_clean_title(r$title)),
         e[setdiff(names(e), "bibtype")], list(isbn = r$ISBN, doi = doi, url = if (is.na(doi)) url))
  if (!is.null(book_authors[[k]])) e[names(book_authors[[k]])] <- book_authors[[k]]
  if (k %in% names(authors)) e$author <- authors[[k]]
  if (!is.null(fixes[[k]])) e[names(fixes[[k]])] <- fixes[[k]]
  if (e$bibtype == "Online" && is.null(e$url)) e$url <- url   # Online needs its url
  e$isbn <- str_extract(e$isbn %||% NA_character_, "[0-9][0-9Xx-]{8,}[0-9Xx]")
  for (f in c("author", "title", "journal", "publisher")) {
    if (!is.null(e[[f]]) && !is.na(e[[f]])) e[[f]] <- source_clean_text(e[[f]])
  }
  e
}

entries <- c(lapply(split(fields, seq_len(nrow(fields))), build_entry), manual)
entries <- lapply(entries, source_clean_entry)
names(entries) <- vapply(entries, `[[`, "", "key")

# ---- 8. Check every key against AusTraits ----
cand <- tibble(
  key = names(entries),
  year = vapply(entries, \(e) as.character(e$year %||% NA), ""),
  title = vapply(entries, \(e) as.character(e$title %||% NA), ""),
  doi = vapply(entries, \(e) as.character(e$doi %||% NA), ""),
  volume = vapply(entries, \(e) as.character(e$volume %||% NA), "")
)
# Not matched to the same work under another key: online floras keep the
# convention's key. (eFLOWER_2021's Airy_1980, SPRAT_2025_2's Crayzer_2004 and
# PlantNET_2025 were renamed AiryShaw_1980_1, Cayzer_2004_2 and NHNSW_2025 to match.)
# Both volumes of Flora of the South West match the Wheeler_2002 dataset (expected).
keep <- c(cand$key[str_detect(cand$key, paste0("^(", paste(names(flora_templates), collapse = "|"), ")_[0-9]{4}$")) &
                   vapply(entries, \(e) e$bibtype == "Online", TRUE)])
# The same work under a slightly different title or year, checked by eye
# (source_suggest_same_as() lists candidates)
same_as <- c(Cooper_2013 = "Cooper_2013", Chinnock_2007 = "Chinnock_2007", Dunlop_1995 = "Dunlop_1995",
             Stanley_1986 = "Stanley_1986", Bailey_1899 = "Bailey_1899",
             Orchard_1989 = "Orchard_1989",   # in SPRAT_2025_2 (its year was 2020 until fixed)
             ASBS_1981 = "Jessop_1981",       # Flora of Central Australia, in NTH_2014
             Cowie_2000 = "Cowie_2000")       # Floodplain Flora, in NTH_2014 (see `recode`)
resolved <- suppressWarnings(source_resolve_keys(cand, index, keep = keep, same_as = same_as))
if (interactive()) print(source_suggest_same_as(resolved, cand, index), n = Inf)
# The AVH dataset is the Australian Virtual Herbarium consulted here
resolved <- resolved |>
  mutate(action = ifelse(key == "AVH_2026", "copy", action),
         key_final = ifelse(key == "AVH_2026", "AVH_2026", key_final),
         copy_from = ifelse(key == "AVH_2026", "AVH_2026", copy_from),
         note = ifelse(key == "AVH_2026", NA_character_, note))

final <- lapply(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
final <- final[order(vapply(final, `[[`, "", "key"))]
final <- final[!duplicated(vapply(final, `[[`, "", "key"))]
key_map <- setNames(resolved$key_final, resolved$key)

problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

# ---- Write ----
d <- d |>
  mutate(
    source_id = coalesce(unname(key_map[source_id_new]), "")
  ) |>
  select(-source_id_new)

write_csv(d, f, na = "NA")
source_write_yml(final, "data/Wenk_2025/sources.yml")
write_csv(
  bind_rows(
    refs |> select(reference, author, year, short_title, candidate = source_id_new),
    tibble(reference = paste0("[reference_OG] ", unmatched$match), candidate = unmatched$key)
  ) |>
    filter(!is.na(candidate)) |>
    left_join(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note), by = "candidate") |>
    select(source_id, reference, author, year, short_title, action, copy_from, note) |>
    arrange(source_id),
  "data/Wenk_2025/raw/scripts/source_id_lookup.csv", na = "")
print(count(resolved, action))
