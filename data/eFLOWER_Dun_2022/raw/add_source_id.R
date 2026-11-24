# Add a `source_id` column to data/eFLOWER_Dun_2022/data.csv and write the
# references it names to data/eFLOWER_Dun_2022/sources.yml (#888).
#
# Run from the repo root, after `eFLOWER_Dun_2022 extra R code.R` has written
# data.csv (that script ends by sourcing this one):
#
#     Rscript data/eFLOWER_Dun_2022/raw/add_source_id.R
#
# Each PROTEUS record cites its source in `FullRef` (a free-text citation) or,
# for most records, only a website in `Reference`. `raw/source_lookup.csv`
# holds one row per distinct citation or website (`reference`) with either
#
#   - `site`: an online flora or database, keyed by its code and the year the
#     record was scored (`Date`), as eFLOWER_2021 and the flora datasets do:
#     florabase -> WAH_2022_2, PlantNET -> NHNSW_2021, eFloras -> Efloras_2014;
#     the entry is the site's reference elsewhere in AusTraits (`sites` below),
#     re-dated and without its access date; or
#   - `key` and the reference's fields (`bibtype`, `author`, ...): a published
#     work, keyed first-author surname + year. `origin` says where the fields
#     came from: `eFLOWER_2021` (the same citation is in that dataset),
#     `crossref` (CrossRef metadata for the citation, checked against it),
#     `parsed` (read from the citation) or `manual`; a second citation of the
#     same work gives only the key (`duplicate`); or
#   - `same_as`: the key of the same work already in AusTraits (eFLOWER_2021
#     for the 40 citations the two datasets share, Wenk_2025 for printed floras
#     such as `ABRS_1987`, Flora of Australia vol. 45), whose entry is copied; or
#   - nothing, `origin` `unidentified`: the work could not be identified, so
#     the record cites the dataset (the reason is in `note`).
#
# Every key is then checked against the references already in AusTraits
# (ignore/source_references.R): the same work takes its existing key and entry,
# and a key naming a different work takes the next free suffix. Records whose
# citation names no work (43 with no reference at all, and the `unidentified`
# ones) keep a blank `source_id`, citing the dataset.
#
# source_lookup.csv was drafted once from the citations (CrossRef queries, a
# parser for "Surname AB, Surname CD. 2001. Title. Journal 12: 1-20.", and
# hand-written rows), then checked by hand; edit it directly to fix a reference.

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/eFLOWER_Dun_2022"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE)
nz <- function(x) ifelse(x %in% c("NA", ""), NA_character_, x)

lookup <- read_csv(file.path(dir, "raw/source_lookup.csv"), col_types = cols(.default = col_character()),
                   na = "", trim_ws = FALSE, show_col_types = FALSE)

index <- source_reference_index(exclude = "eFLOWER_Dun_2022")
entry_of <- function(key) {
  e <- index |> filter(key == !!key) |> arrange(type != "dataset") |> pull(entry)
  if (length(e) == 0) stop("no reference with key ", key)
  e[[1]]
}

# ---- Online floras and databases: code -> the entry other years copy ----
# `from`: a key whose entry is copied (re-keyed, re-dated, access date dropped);
# otherwise the entry is written from `author`, `title`, `url`.
sites <- tribble(
  ~site,                  ~from,                      ~author,                                   ~title,                                                        ~url,
  "ABRS",                 "ABRS_2023",                NA, NA, NA,
  "NHNSW",                "NHNSW_2024",               NA, NA, NA,
  "RBGV",                 "RBGV_2023",                NA, NA, NA,
  "WAH",                  "WAH_2023_1",               NA, NA, NA,
  "NTH",                  "NTH_2023",                 NA, NA, NA,
  "SAH",                  "SAH_2023",                 NA, NA, NA,
  "TMAG",                 "TMAG_2009",                NA, NA, NA,
  "ATRP",                 "ATRP_2020",                NA, NA, NA,
  "Efloras",              "Efloras_2010",             NA, NA, NA,
  "APSA",                 "APSA_2017",                NA, NA, NA,
  "Dallwitz",             "Dallwitz_2020",            NA, NA, NA,
  "PPD",                  "PPD_2014",                 NA, NA, NA,
  "PlantIllustrations",   "PlantIllustrations_2014",  NA, NA, NA,
  "EOL",                  "EOL_2012",                 NA, NA, NA,
  "Palmweb",              "Palmweb_2011",             NA, NA, NA,
  "PFAF",                 "PFAF_2013",                NA, NA, NA,
  "PROTA",                "PROTA_2013",               NA, NA, NA,
  "TRIN",                 "TRIN_2013",                NA, NA, NA,
  "MNHN",                 "MNHN_2013",                NA, NA, NA,
  "Emonocot",             "Emonocot_2018",            NA, NA, NA,
  "Floramalesiana",       "Floramalesiana_2014",      NA, NA, NA,
  "TreeSA",               "TreeSA_2019",              NA, NA, NA,
  "KewEfloras",           "KewEfloras_2013",          NA, NA, NA,
  "NZPCN",                "NZPCN_2025",               NA, NA, NA,
  "FlowersIndia",         "FlowersIndia_2026",        NA, NA, NA,
  "Hyde",                 "Hyde_2026",                NA, NA, NA,
  "Jordan",               "Jordan_2026",              NA, NA, NA,
  "FloreAfriqueCentrale", "FloreAfriqueCentrale_2026", NA, NA, "https://www.floredafriquecentrale.be",
  "FloraMalawi",          "FloraMalawi_2026",         NA, NA, NA,
  "SingaporeFloraFaunaWeb", "SingaporeFloraFaunaWeb_2026", NA, NA, NA,
  "LucidWeedsAustralia",  "LucidWeedsAustralia_2026", NA, NA, NA,
  "PlantZAfrica",         "PlantZAfrica_2026",        NA, NA, NA,
  "TreesShrubsOnline",    "TreesShrubsOnline_2026",   NA, NA, NA,
  "ANPSA",                "ANPSA_2026",               NA, NA, NA,
  "iNaturalist",          "iNaturalist_2026",         NA, NA, NA,
  "JSTORGlobalPlants",    "JSTORGlobalPlants_2026",   NA, NA, NA,
  "deVogel",              "deVogel_2026",             NA, NA, NA,
  "Sharp",                "Sharp_2026",               NA, NA, NA,
  "HEAR",                 "HEAR_2026",                NA, NA, NA,
  "PlantFinder",          "PlantFinder_2026",         NA, NA, NA,
  "BRAIN",                NA, "{Brisbane Rainforest Action and Information Network}", "Brisbane Rainforest Action and Information Network", "http://www.brisrain.org.au",
  "AusGrass2",            NA, "{AusGrass2}",  "AusGrass2: Grasses of Australia",                          "https://ausgrass2.myspecies.info",
  "PNGTrees",             NA, "{PNGplants}",  "Trees of Papua New Guinea",                                "https://www.pngplants.org/PNGtrees",
  "ALA",                  NA, "{Atlas of Living Australia}", "Atlas of Living Australia",                 "https://www.ala.org.au",
  "PlantUse",             NA, "{Pl@ntUse}",   "Pl@ntUse",                                                 "https://uses.plantnet-project.org",
  "FloraGuianas",         NA, "{Flora of the Guianas}", "Flora of the Guianas",                           "https://portal.cybertaxonomy.org/flora-guianas",
  "FloraBotswana",        NA, "{Flora of Botswana}", "Flora of Botswana",                                 "https://www.botswanaflora.com",
  "FloraZambia",          NA, "{Flora of Zambia}", "Flora of Zambia",                                     "https://www.zambiaflora.com",
  "PlantesBotanique",     NA, "{Plantes et botanique}", "Plantes et botanique",                           "http://www.plantes-botanique.org",
  "CATEAraceae",          NA, "{CATE Araceae}", "CATE Araceae",                                           "https://cate-araceae.myspecies.info",
  "FloraNicaragua",       NA, "{Tropicos}", "Flora de Nicaragua",                                         "http://legacy.tropicos.org/Project/FN",
  "ConvolvulaceaePollenAtlas", NA, "{University of Arizona Herbarium}", "Convolvulaceae Pollen Atlas",    "https://cals.arizona.edu/herbarium/sites/cals.arizona.edu.herbarium/files/old_site/assoc/projects/convolv/Convolvulaceae_Pollen_Atlas.htm",
  "GlobalPollenProject",  NA, "{Global Pollen Project}", "The Global Pollen Project",                     "https://globalpollenproject.org",
  "Kyffhauser",           NA, "{Kyffhauser}", "Kyffhauser: plants of South Africa",                        "https://www.kyffhauser.co.za",
  "FloraPeninsulaIndica", NA, "{Flora of Peninsular India}", "Flora of Peninsular India",                 "http://flora-peninsula-indica.ces.iisc.ac.in",
  "MissouriPlants",       NA, "{Missouri Plants}", "Missouri Plants",                                     "http://www.missouriplants.com",
  "UCFArboretum",         NA, "{UCF Arboretum}", "UCF Arboretum Virtual Tour",                            "https://arboretum.ucf.edu/virtual",
  "Calscape",             NA, "{California Native Plant Society}", "Calscape",                            "https://calscape.org",
  "APSNSW",               NA, "{Australian Plants Society NSW}", "Australian Plants Society NSW",         "https://www.austplants.com.au",
  "MinnesotaWildflowers", NA, "{Minnesota Wildflowers}", "Minnesota Wildflowers",                         "https://www.minnesotawildflowers.info",
  "InfoFlora",            NA, "{Info Flora}", "Info Flora: the national data and information center on the Swiss flora", "https://www.infoflora.ch",
  "KeyBase",              NA, "{Royal Botanic Gardens Victoria}", "KeyBase",                              "https://keybase.rbg.vic.gov.au",
  "PalDat",               NA, "{PalDat}", "PalDat - a palynological database",                            "https://www.paldat.org"
)

# Keys that are datasets for some years: the flora's convention there
site_key_recode <- c(WAH_2022 = "WAH_2022_2", WAH_2023 = "WAH_2023_1")

site_entry <- function(site, key, year) {
  s <- sites |> filter(site == !!site)
  if (nrow(s) != 1) stop("no site ", site)
  if (!is.na(s$from)) {
    e <- entry_of(s$from)
    if (!is.na(s$url)) e$url <- s$url
  } else {
    e <- list(bibtype = "Online", author = s$author, title = s$title, url = s$url)
  }
  e$key <- key; e$year <- as.integer(year); e$note <- NULL
  source_clean_entry(e)
}

# ---- Work entries from the lookup ----
fields <- c("bibtype", "year", "author", "title", "booktitle", "editor", "journal", "volume",
            "number", "pages", "publisher", "place", "isbn", "doi", "url", "note")
work_entry <- function(r) {
  e <- as.list(r[intersect(fields, names(r))])
  e <- e[!is.na(unlist(e))]
  e$year <- as.integer(e$year)
  for (f in intersect(c("author", "title", "booktitle", "editor", "journal", "publisher"), names(e))) {
    e[[f]] <- source_clean_text(e[[f]])
  }
  if (!is.null(e$title)) e$title <- source_clean_title(e$title)
  source_clean_entry(c(list(key = r$key), e))
}

# ---- Each record's reference ----
d <- d |>
  mutate(
    .reference = coalesce(nz(FullRef), nz(Reference)),
    .year = as.integer(format(as.Date(Date, "%d-%b-%y"), "%Y"))
  ) |>
  # matched with white space squeezed: some citations carry stray line breaks
  mutate(.ref_sq = str_squish(.reference)) |>
  left_join(lookup |> transmute(.ref_sq = str_squish(reference), .site = site, .key = key, .same_as = same_as) |>
              distinct(.ref_sq, .keep_all = TRUE),
            by = ".ref_sq") |>
  mutate(
    .key = ifelse(!is.na(.site), paste0(.site, "_", .year), coalesce(.key, .same_as)),
    .key = coalesce(unname(site_key_recode[.key]), .key)
  )

# Rows the lookup leaves blank on purpose (origin `unidentified`) cite the dataset
unidentified <- str_squish(lookup$reference[lookup$origin %in% "unidentified"])
missing <- d |> filter(!is.na(.reference), is.na(.key), !.ref_sq %in% unidentified,
                       omit_duplicate %in% c("", "NA"))
if (nrow(missing) > 0) {
  print(distinct(missing, .reference))
  stop("references with no row in raw/source_lookup.csv")
}

site_keys <- d |> filter(!is.na(.site)) |> distinct(.key, .site, .year)
# One row per key holds its fields; other citations of the same work give only the key
work_rows <- lookup |> filter(is.na(site), is.na(same_as), !is.na(key)) |>
  arrange(is.na(bibtype)) |> distinct(key, .keep_all = TRUE)
same_as_keys <- unique(na.omit(lookup$same_as))

entries <- c(
  pmap(site_keys, \(.key, .site, .year) site_entry(.site, .key, .year)),
  map(split(work_rows, seq_len(nrow(work_rows))), work_entry),
  map(same_as_keys, entry_of)
)
names(entries) <- map_chr(entries, "key")

# ---- Check every key against AusTraits ----
cand <- tibble(
  key = names(entries),
  year = map_chr(entries, \(e) as.character(e$year %||% NA)),
  title = map_chr(entries, \(e) as.character(e$title %||% NA)),
  doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
  volume = map_chr(entries, \(e) as.character(e$volume %||% NA))
)
# A site's key already in AusTraits is the same site by convention (NHNSW_2022 is
# the PlantNET dataset); other site keys are new. Neither is matched by title.
resolved <- source_resolve_keys(cand, index, keep = site_keys$.key,
                                same_as = setNames(same_as_keys, same_as_keys))
existing_site <- resolved$key %in% site_keys$.key & resolved$key %in% index$key
resolved$action[existing_site] <- "copy"
resolved$key_final[existing_site] <- resolved$key[existing_site]
resolved$copy_from[existing_site] <- map_chr(resolved$key[existing_site], \(k)
  index |> filter(key == k) |> arrange(type != "dataset") |> pull(dataset_id) |> first())
resolved$note[existing_site] <- NA_character_

final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})
final <- final[!duplicated(map_chr(final, "key"))]
final <- final[order(map_chr(final, "key"))]

# Only references some kept record cites (records also in eFLOWER_2021 are
# dropped by custom_R_code)
key_map <- setNames(resolved$key_final, resolved$key)
used <- unique(unname(key_map[d$.key[d$omit_duplicate %in% c("", "NA")]]))
final <- final[map_chr(final, "key") %in% used]

problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

d <- d |>
  mutate(source_id = coalesce(unname(key_map[.key]), "")) |>
  select(-starts_with("."))

write_csv(d, file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(resolved |> select(candidate = key, source_id = key_final, action, copy_from, note) |> arrange(source_id),
          file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action))
