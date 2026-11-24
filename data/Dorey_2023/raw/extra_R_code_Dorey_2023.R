# Writes data/Dorey_2023/data.csv and data/Dorey_2023/locations.csv from raw/AusData_out.xlsx.
# Run from the repo root.
#   - raw/AusData_out.xlsx (27 MB, not in GitHub due to its size; supplied by James Dorey,
#     November 2026): Australian records from the BeeBDC global bee occurrence dataset
#     (Dorey et al. 2023, Sci Data 10:747), filtered to records with a host plant in
#     associatedTaxa. One row per pinned bee specimen; BeeBDC flags and de-duplication already
#     applied.
#   - raw/Dorey_2023_plant_lookup.csv: one row per verbatim associatedTaxa string (two rows when
#     a label names two host plants), giving the cleaned plant_name and a keep flag. Drafted by
#     regex (strip "host:"/"associated with:" and the family word; genus-only names become
#     "Genus sp."; family-only names become "<current family> sp."), then hand-checked.
#     keep = FALSE drops the Brassica rapa canola crop survey (S. Rao, 2008-2009), the
#     non-plant hosts (a Sceliphron nest, "Mites") and "host: none".
# Added columns:
#   - flower_visitor: AusTraits flower_visitor level for each bee, from genus or family.
#   - collection_date: from year/month/day where present. Otherwise parsed from the locality
#     string, which for many ALA/GBIF records holds the full specimen label (place, date,
#     collector, host). A label date range that agrees with year/month/day replaces the single
#     date; where the two disagree (BeeBDC mis-parsed years such as 2022 or 2026) the label wins.
#     Labels with a month but no year (e.g. L. Glauert's Rottnest Island series) are given
#     "unknown/2023", the year the BeeBDC dataset was compiled.
#   - date_source: "year/month/day", "locality label" or NA.
#   - location_name: one location per unique coordinate pair (rounded to 4 decimal places,
#     ~10 m), labelled with the place part of
#     the locality string (state, date, collector, host and museum code removed) most often
#     given for those coordinates. A place name given for more than one coordinate pair is
#     named "<place> (<lat>, <lon>)" for each. Records without coordinates get no location.
#   - taxon_name: plant_name, with genus-level names ("Eucalyptus sp.") suffixed by place
#     ("Eucalyptus sp. [Katherine; Dorey_2023]"), adding coordinates and/or collection date
#     only where needed to tell apart collections of that genus at different points under one
#     place name or on different days at one point ("Eucalyptus sp. [Katherine (-14.4667,
#     132.2667), 1974-11-20; Dorey_2023]"). Family-level names are excluded in metadata.yml.
#   - catalog_number: catalogNumber, or recordNumber for ALA records (e.g. "WAM 5466"),
#     prefixed with institutionCode when it lacks one.
#   - omit_duplicate: "Dorey_2021" for J.B. Dorey's specimens from the targeted Pharohylaeus
#     lactiferus observation events of Dorey (2021, Table S1), which Dorey_2021 holds with
#     the event's other visitors and counts; filtered out in custom_R_code.
# One row per bee specimen (two when the label names two host plants), keeping the
# specimen's catalogue number, occurrenceID, gbifID and BeeBDC database_id. Specimens present
# in both ALA and GBIF are kept once (see below).

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)

raw <- read_excel("data/Dorey_2023/raw/AusData_out.xlsx", col_types = "text", na = c("", "NA"))
plant_lookup <- read_csv("data/Dorey_2023/raw/Dorey_2023_plant_lookup.csv", col_types = cols(.default = "c"))

# ALA/GBIF duplicates -----------------------------------------------------------------------

# About 21,500 specimens are in the file twice, once from ALA and once from GBIF (same catalogue
# number, bee species, host, locality, date and coordinates; BeeBDC marks both "Unique", as the
# shared number is in recordNumber, which BeeBDC does not compare). The GBIF copy is kept, as
# it carries the gbifID and occurrenceID; the ALA copy's database_id is kept as
# duplicate_database_id. Shared numbers within one source (e.g. Dorey's field event codes such
# as "19JDEC61") are different specimens and are kept.
raw <- raw |>
  mutate(catalog_number = coalesce(catalogNumber, recordNumber),
         repository = str_extract(dataSource, "^[A-Za-z]+")) |>
  group_by(catalog_number, scientificName, repository) |>
  mutate(copy = row_number()) |>
  ungroup()

ala_copies <- raw |>
  filter(repository == "ALA", !is.na(catalog_number)) |>
  semi_join(raw |> filter(repository == "GBIF"), by = c("catalog_number", "scientificName", "copy"))

raw <- raw |>
  anti_join(ala_copies, by = "database_id") |>
  left_join(ala_copies |> select(catalog_number, scientificName, copy, duplicate_database_id = database_id) |>
              mutate(repository = "GBIF"),
            by = c("catalog_number", "scientificName", "copy", "repository"))

# Dates -------------------------------------------------------------------------------------

MONTHS <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
mon_re <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\.?"
yr_re <- "((?:18|19|20)\\d{2})"
month_num <- function(x) unname(MONTHS[tolower(substr(x, 1, 3))])

# ISO date at the finest valid precision; an impossible day (e.g. "30 Feb") falls back to year-month
iso_date <- function(y, m = NA, d = NA) {
  y <- suppressWarnings(as.integer(y)); m <- suppressWarnings(as.integer(m)); d <- suppressWarnings(as.integer(d))
  valid_day <- !is.na(d) & !is.na(as.Date(sprintf("%04d-%02d-%02d", y, m, d), optional = TRUE))
  case_when(
    is.na(y) ~ NA_character_,
    valid_day ~ sprintf("%04d-%02d-%02d", y, m, d),
    m %in% 1:12 ~ sprintf("%04d-%02d", y, m),
    TRUE ~ sprintf("%04d", y)
  )
}

date_range <- function(start, end) if_else(end < start, str_sub(start, 1, 4), paste0(start, "/", end))

parse_locality_date <- function(x) {
  x <- str_replace_all(x, "–", "-")
  m <- str_match(x, paste0("\\b", mon_re, ",?\\s+", yr_re, "\\s*-\\s*", mon_re, ",?\\s+", yr_re, "\\b"))  # Dec 1989 - Jan 1990
  if (!is.na(m[1])) return(date_range(iso_date(m[3], month_num(m[2])), iso_date(m[5], month_num(m[4]))))
  m <- str_match(x, paste0("\\b(\\d{1,2})\\s+", mon_re, "\\s*-\\s*(\\d{1,2})\\s+", mon_re, ",?\\s+", yr_re, "\\b"))  # 2 Feb - 4 Mar 1984
  if (!is.na(m[1])) return(date_range(iso_date(m[6], month_num(m[3]), m[2]), iso_date(m[6], month_num(m[5]), m[4])))
  m <- str_match(x, paste0("\\b(\\d{1,2})\\s*-\\s*(\\d{1,2})\\s+", mon_re, ",?\\s+", yr_re, "\\b"))  # 19 - 21 Jan 1990
  if (!is.na(m[1])) return(date_range(iso_date(m[5], month_num(m[4]), m[2]), iso_date(m[5], month_num(m[4]), m[3])))
  m <- str_match(x, paste0("\\b", mon_re, "\\s*-\\s*", mon_re, ",?\\s+", yr_re, "\\b"))  # Aug - Sept 1988
  if (!is.na(m[1])) return(date_range(iso_date(m[4], month_num(m[2])), iso_date(m[4], month_num(m[3]))))
  m <- str_match(x, paste0("\\b(\\d{1,2})\\s+", mon_re, ",?\\s+", yr_re, "\\b"))  # 23 Oct, 1990
  if (!is.na(m[1])) return(iso_date(m[4], month_num(m[3]), m[2]))
  m <- str_match(x, paste0("\\b", mon_re, ",?\\s+", yr_re, "\\b"))  # April 1899
  if (!is.na(m[1])) return(iso_date(m[3], month_num(m[2])))
  m <- str_match(x, paste0("\\b(\\d{1,2})\\s+(\\d{1,2})\\s+", yr_re, "$"))  # Orbost 6 03 1984
  if (!is.na(m[1])) return(iso_date(m[4], m[3], m[2]))
  m <- str_match(x, paste0("\\b(\\d{1,2})\\s+", yr_re, "$"))  # Mildura 02 1983
  if (!is.na(m[1])) return(iso_date(m[3], m[2]))
  m <- str_match(x, paste0("\\b", yr_re, "\\b"))  # Rao,S. ... 2009 ANIC
  if (!is.na(m[1])) return(iso_date(m[2]))
  NA_character_
}

# a date of b's precision (or vice versa) that matches
dates_agree <- function(a, b) str_starts(a, fixed(b)) | str_starts(b, fixed(a))

# Single labels whose year is an outlier against the same collector's other records from that
# collecting event; replaced with the event's year. The 1890s G. Turner records from Mackay are
# genuine and left as recorded.
LABEL_YEAR_FIXES <- tribble(
  ~pattern,                     ~wrong, ~right, # same event elsewhere in the file
  "10 Jan 1871, T.F. Houston",  "1871", "1971", # 5 mls NW of Hartley, 10 Jan 1971, on Bursaria spinosa
  "28 Oct 1894, K.L. Walker",   "1894", "1984", # Colquhoun State Forest, 28 Oct 1984
  "11 Oct 1195, K. Walker",     "1195", "1995", # 30km along Haasts Bluff Rd, 11 Oct 1995
  "22 Oct 1884, L. Jansen",     "1884", "1994"  # Jansen collected 1990-1995; Rocky River Hmsd Sep-Oct 1994
)

fix_label_years <- function(x) {
  for (i in seq_len(nrow(LABEL_YEAR_FIXES))) {
    hit <- str_detect(x, fixed(LABEL_YEAR_FIXES$pattern[i])) %in% TRUE
    x[hit] <- str_replace(x[hit], LABEL_YEAR_FIXES$wrong[i], LABEL_YEAR_FIXES$right[i])
  }
  x
}

# Locations ---------------------------------------------------------------------------------

STATES <- c("Western Australia" = "WA", "South Australia" = "SA", "Northern Territory" = "NT",
            "New South Wales" = "NSW", "Australian Capital Territory" = "ACT", "Queensland" = "Qld",
            "Victoria" = "Vic", "Tasmania" = "Tas")
month_word <- "(Jan|January|Feb|February|Mar|March|Apr|April|May|Jun|June|Jul|July|Aug|August|Sep|Sept|September|Oct|October|Nov|November|Dec|December)\\.?"

# the place name ends where the first date, collector, host or museum code starts
PLACE_END_PATTERNS <- c(
  paste0("\\b\\d{1,2}(st|nd|rd|th)?\\s*(-\\s*\\d{1,2}\\s*)?", month_word, "\\b"),    # 12 Jan, 19 - 21 Jan
  paste0("\\b", month_word, ",?\\s*(-\\s*", month_word, "\\s*)?(18|19|20)\\d{2}\\b"), # Dec 1899, Aug - Sept 1988
  paste0("\\s", month_word, "\\s+[A-Z][a-z]+,\\s?[A-Z]\\."),                          # Rottnest Island Dec Glauert, L.
  paste0(",?\\s", month_word, "(\\s+", month_word, ")*$"),                            # Roseville, Apr Sept Oct
  "\\s\\d{1,2}\\s+\\d{1,2}\\s+(18|19|20)\\d{2}$",                                     # Orbost 6 03 1984
  "\\s\\d{1,2}\\s+(18|19|20)\\d{2}$",                                                 # Mildura 02 1983
  ",?\\s(18|19|20)\\d{2}\\b",                                                         # lone year
  "\\s[A-Z][a-z]+,\\s?[A-Z]\\.",                                                      # Rao,S.  Houston, T.F.
  ",\\s*([A-Z]\\.\\s?)+(&\\s*([A-Z]\\.\\s?)+)?\\s?[A-Z][a-z]+",                        # , E.M. Exley  , J. & C.R. King
  ",?\\s(on|resting on|on flowers of|ex)\\s",                                         # host
  "\\s?\\((ANIC|NMV|SAM|QM|WAM|UQIC|BMNH|AM|MV)[^)]*\\)", "\\s(BMNH|ANIC)\\b"          # museum
)

clean_place <- function(x) {
  x <- str_squish(str_replace_all(x, " ", " "))
  end <- rep(NA_integer_, length(x))
  for (p in PLACE_END_PATTERNS) end <- pmin(end, str_locate(x, p)[, 1], na.rm = TRUE)
  x <- if_else(is.na(end), x, str_sub(x, 1, end - 1))
  state_re <- paste(c(names(STATES), STATES, toupper(STATES), "NA"), collapse = "|")
  x <- str_remove(x, paste0("^(", state_re, ")\\b\\.?,?\\s*"))
  x <- str_remove(x, paste0("^(", state_re, ")\\b\\.?,?\\s*"))  # "Australian Capital Territory NA NSW Ginninderra"
  x <- str_remove_all(x, "[\"']")
  x <- str_remove_all(x, "[\\s,;:.\\-(]+$")
  x <- str_remove(x, paste0(",?\\s", month_word, "((\\s|,\\s?|\\s?&\\s?)", month_word, ")*$"))  # Roseville, Apr Sept Oct
  x <- str_remove_all(x, "[\\s,;:.\\-(]+$")
  na_if(str_squish(x), "")
}

# Records ----------------------------------------------------------------------------------

records <- raw |>
  mutate(
    locality_dated = fix_label_years(locality),
    # year/month/day carry the same typo, so these records take the corrected label date
    ymd_date = if_else(locality_dated != locality & !is.na(locality), NA_character_, iso_date(year, month, day))
  )

label_dates <- records |>
  distinct(locality_dated) |>
  filter(!is.na(locality_dated)) |>
  mutate(label_date = map_chr(locality_dated, parse_locality_date))

records <- records |>
  left_join(label_dates, by = "locality_dated") |>
  mutate(
    label_start = word(label_date, 1, sep = "/"),
    label_end = word(label_date, -1, sep = "/"),
    label_agrees = dates_agree(label_start, ymd_date) | dates_agree(label_end, ymd_date),
    two_dates_on_label = str_detect(locality_dated, "&\\s*\\d"),
    collection_date = case_when(
      is.na(ymd_date) ~ label_date,
      is.na(label_date) ~ ymd_date,
      label_agrees ~ if_else(str_detect(label_date, "/"), label_date, ymd_date),
      two_dates_on_label ~ ymd_date,
      TRUE ~ label_date
    ),
    date_source = case_when(
      is.na(collection_date) ~ NA_character_,
      collection_date == ymd_date & !is.na(ymd_date) ~ "year/month/day",
      TRUE ~ "locality label"
    ),
    flower_visitor = case_when(
      species == "Apis mellifera" ~ "honeybee",
      genus %in% c("Tetragonula", "Austroplebeia") ~ "stingless_bee",
      genus %in% c("Exoneura", "Brevineura", "Exoneurella", "Braunsapis") ~ "allodapini",
      genus == "Amegilla" ~ "anthophorini",
      genus == "Xylocopa" ~ "carpenter_bee",
      genus == "Bombus" ~ "bumblebee",
      family == "Colletidae" ~ "colletidae",
      family == "Halictidae" ~ "sweat_bee",
      family == "Megachilidae" ~ "megachilidae",
      family == "Stenotritidae" ~ "stenotritid_bee",
      TRUE ~ "bee"
    ),
    state = unname(STATES[stateProvince]),
    place = clean_place(locality),
    latitude = as.numeric(decimalLatitude),
    longitude = as.numeric(decimalLongitude),
    # 4 decimal places (~10 m, GPS precision); also clears floating-point noise from the xlsx
    # (e.g. -17.268039999999999)
    latitude = round(latitude, 4),
    longitude = round(longitude, 4)
  ) |>
  inner_join(plant_lookup |> select(-n_raw_records), by = "associatedTaxa", relationship = "many-to-many") |>
  filter(keep == "TRUE")

# One location per unique coordinate pair, labelled with the place name most often given for it.
# A place name given for more than one coordinate pair carries the coordinates in each location
# name. Records without coordinates get no location (their label text stays in locality_verbatim).
locations <- records |>
  filter(!is.na(latitude)) |>
  count(latitude, longitude, state, place) |>
  arrange(desc(!is.na(place)), desc(n)) |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  group_by(state, place) |>
  mutate(n_coords = n()) |>
  ungroup() |>
  mutate(location_name = case_when(
    is.na(place) ~ sprintf("%s (%s, %s)", coalesce(state, "unknown state"), latitude, longitude),
    n_coords == 1 ~ place,
    TRUE ~ sprintf("%s (%s, %s)", place, latitude, longitude)
  )) |>
  # the same place name in two states
  group_by(location_name) |>
  mutate(n_states = n_distinct(state)) |>
  ungroup() |>
  mutate(location_name = if_else(n_states > 1, paste0(location_name, ", ", state), location_name)) |>
  # the location name without its coordinates, used to name genus-level host taxa
  group_by(place) |>
  mutate(place_states = n_distinct(state)) |>
  ungroup() |>
  mutate(place_label = case_when(
    is.na(place) ~ location_name,
    place_states > 1 ~ paste0(place, ", ", state),
    TRUE ~ place
  ))

stopifnot(!anyDuplicated(locations$location_name))

records <- records |>
  left_join(locations |> select(location_name, place_label, latitude, longitude), by = c("latitude", "longitude"))

locations <- locations |>
  select(location_name, `latitude (deg)` = latitude, `longitude (deg)` = longitude, state, locality = place) |>
  arrange(location_name)

# Specimens -------------------------------------------------------------------------------

# vials from Dorey (2021) Table S1, the targeted P. lactiferus observations on Stenocarpus
# sinuatus and Brachychiton acerifolius; 19JDEC96 is the Newrybar "Flame Tree Swept" event
DOREY_2021_VIALS <- c("18JDEC450-A", "18JDEC459-A", "18JDEC460", "18JDEC497-A", "19JDEC7", "19JDEC60",
                      "19JDEC61", "19JDEC66", "19JDEC68", "19JDEC72", "19JDEC96")

# genus-level names excluded in metadata.yml keep their plain name so the exclusion still matches
excluded_names <- yaml::read_yaml("data/Dorey_2023/metadata.yml")$exclude_observations |>
  keep(~ .x$variable == "taxon_name") |>
  map(~ str_squish(str_split_1(.x$find, ", "))) |>
  unlist()

# An unidentified "Genus sp." collected at one place on one day need not be the same species as
# one collected elsewhere or on another day. Each is named by its place, adding the coordinates
# only when that genus was collected at more than one point under the place name, and the date
# only when it was collected on more than one day at that point.
data <- records |>
  mutate(
    genus_level = str_detect(plant_name, "^[A-Z][a-z]+ sp\\.$") & !str_detect(plant_name, "aceae sp\\.$") &
      !plant_name %in% excluded_names & !is.na(location_name),
    date_label = coalesce(collection_date, "date unknown")
  ) |>
  group_by(plant_name, place_label) |>
  mutate(n_points = n_distinct(location_name)) |>
  group_by(plant_name, location_name) |>
  mutate(n_dates = n_distinct(date_label)) |>
  ungroup() |>
  mutate(
    taxon_name = if_else(genus_level,
                         paste0(plant_name, " [", place_label,
                                if_else(n_points > 1, sprintf(" (%s, %s)", latitude, longitude), ""),
                                if_else(n_dates > 1, paste0(", ", date_label), ""),
                                "; Dorey_2023]"),
                         plant_name),
    # catalogue numbers are held in catalogNumber or (ALA) recordNumber; prefix the institution
    # when the number does not already carry it
    catalog_number = if_else(!is.na(catalog_number) & !is.na(institutionCode) & !str_starts(catalog_number, fixed(institutionCode)),
                             paste(institutionCode, catalog_number), catalog_number),
    # AusTraits needs a collection_date; the BeeBDC dataset was compiled in 2023
    collection_date = coalesce(collection_date, "unknown/2023"),
    # how the pollinator count of a collecting event is determined (see metadata.yml contexts);
    # records with an individualCount are lots of preserved specimens, not field counts
    count_method = if_else(basisOfRecord == "HumanObservation", "count of photographic observations",
                           "count of preserved specimens"),
    omit_duplicate = if_else(catalog_number %in% paste("Dorey", DOREY_2021_VIALS), "Dorey_2021", NA_character_)
  ) |>
  select(taxon_name, plant_name, plant_name_verbatim = associatedTaxa, plant_family_verbatim, identification_uncertain,
         bee_family = family, bee_subfamily = subfamily, bee_tribe = tribe, bee_genus = genus,
         bee_subgenus = subgenus, bee_species = species, bee_scientific_name = scientificName, sex,
         flower_visitor, individualCount, count_method, basisOfRecord, location_name, state, locality_verbatim = locality,
         collection_date, date_source,
         catalog_number, occurrenceID, gbifID, database_id, duplicate_database_id, institutionCode,
         recordedBy, dataSource, omit_duplicate) |>
  arrange(taxon_name, bee_species, location_name, collection_date)

# locations whose only records are the Dorey_2021 duplicates are dropped
locations <- locations |>
  filter(location_name %in% data$location_name[is.na(data$omit_duplicate)])

write_csv(data, "data/Dorey_2023/data.csv", na = "")
write_csv(locations, "data/Dorey_2023/locations.csv", na = "")
