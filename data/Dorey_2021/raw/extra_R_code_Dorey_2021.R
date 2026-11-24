# Writes data/Dorey_2021/data.csv and data/Dorey_2021/locations.csv from the paper's
# Supplementary material 2 (raw/journal_of_hymenoptera_research-081-165-s002.xlsx).
# Run from the repo root.
#   - Table S1: the 42 targeted observation events on Stenocarpus sinuatus and Brachychiton
#     acerifolius, with the number and sex of Pharohylaeus lactiferus collected; one row per
#     event (one per vial where P. lactiferus were caught in more than one vial).
#   - Table S3: every vial of bees collected in NSW and Qld (Dec 2018 - Feb 2019 and
#     Nov - Dec 2019, plus 2015-16 specimens without host plants), with the number of bees
#     in the vial (NumInsect) and, where identified, the plant the bees were swept off.
#   - raw/Dorey_2021_note_visitors.csv: the other flower visitors named in the Notes of each
#     record (bees by genus/species, honeybees, stingless bees, wasps, flies, beetles, birds,
#     a butterfly), transcribed by hand with the source phrase in visitor_verbatim. Visitors
#     merely present at the site (e.g. "Lots of butterflies present") are not included.
# Rows of data.csv:
#   - one per Table S3 vial with a Plant ID: the vial's bees, as Anthophila (bee), with
#     visitor_count = NumInsect. Table S3's rows for the observation-only events of Table S1
#     ("Flame Tree Checked", ...) are replaced by the Table S1 rows, whose notes are fuller.
#   - one per Table S1 event with P. lactiferus, with visitor_count = # of P. lactiferus.
#   - one per visitor in raw/Dorey_2021_note_visitors.csv.
#   - one per Table S1 event with no visitor at all, with flower_visitor blank, so the
#     observation effort stays visible.
# Added columns:
#   - flower_visitor: the AusTraits flower_visitor level of the visitor. Blank for the two
#     records that were not flower visits (non_flower_visit gives the reason).
#   - plant_name / taxon_name: Plant ID as written, with stray whitespace removed; genus-level
#     names ("Leptospermum", "Eucalyptus spp.") given as "Genus sp." and suffixed with their
#     location ("Leptospermum sp. [Nattai; Dorey_2021]"), adding the date when that genus was
#     collected at one location on more than one day.
#   - basis_of_record: captive_cultivated for plants the notes place in a town, suburb, park,
#     garden, yard, street, school, native planting or on a private property, and for cultivated
#     exotic garden species; field otherwise, including weeds and plants in national parks.
#   - plant_name_from_notes: the plant for vials whose note names it but have no Plant ID
#     (NOTE_PLANTS below).
#   - location_name: one location per coordinate pair, named by the Location column, with the
#     coordinates added when a place name has more than one coordinate pair.

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

xlsx <- "data/Dorey_2021/raw/journal_of_hymenoptera_research-081-165-s002.xlsx"
s1 <- read_excel(xlsx, sheet = "Table S1", col_types = "text", na = c("", "NA"))
s3 <- read_excel(xlsx, sheet = "Table S3", col_types = "text", na = c("", "NA"))
note_visitors <- read_csv("data/Dorey_2021/raw/Dorey_2021_note_visitors.csv", col_types = cols(.default = "c"))

excel_date <- function(x) as.character(as.Date(as.numeric(x), origin = "1899-12-30"))
STATES <- c(QLD = "Qld", NSW = "NSW")

# Records were not flower visits
NON_FLOWER_VISITS <- c(
  "18JDEC450-A" = "female collected on foliage (Results: initial collection of a P. lactiferus female on foliage adjacent to Hallorans Hill Conservation Park)",
  "18JDEC178" = "bees nesting in a Xanthorrhoea flowering scape, not visiting flowers"
)

# Vials with no Plant ID whose note names the plant, or that come from the same tree on the
# same day as a vial with a Plant ID. The other vials without a Plant ID were collected at
# nests, over bare ground, grass or general vegetation, on snow, at a light trap, or on
# unidentified flowers; family-only notes ("pea plants", "yellow Asteraceae") are left out.
NOTE_PLANTS <- c(
  "19JDEC3" = "Eucalyptus sp.",                              # "General sweep over flowering euc"
  "19JDEC4" = "Eucalyptus sp.",
  "19JDEC8" = "Eucalyptus sp.",
  "19JDEC31" = "Brachychiton acerifolius",                   # "Sight-swept off flame tree"
  "19JDEC44" = "Grevillea sp.",                              # "General sweep over flowering Grevilleas"
  "19JDEC45" = "Grevillea sp.",
  "19JDEC101" = "Brachychiton populneus subsp. Populneus",   # same tree and note as 19JDEC100
  "19JDEC102" = "Brachychiton populneus subsp. Populneus",
  "19JDEC103" = "Brachychiton populneus subsp. Populneus",
  "19JDEC114" = "Angophora costata subsp. Costata",          # same tree and note as 19JDEC113
  "19JDEC121" = "Eucalyptus pauciflora",                     # "General sweeps over snowgum"
  "19JDEC129" = "Ranunculus sp.",                            # "Bees on Ranunculus sp."
  "18JDEC73" = "Ranunculus sp.",                             # "Seen resting on Ranunculus flower"
  "Dor26" = "Melaleuca sp."                                  # "Collected from one Melaleuca"
)

# Records ------------------------------------------------------------------------------------

s1_records <- s1 |>
  mutate(
    source_table = "Table S1",
    record_id = paste("Table S1 row", row_number()),
    vial_code = if_else(str_detect(Voucher, "^1[89]JDEC"), Voucher, NA_character_),
    event_name = if_else(is.na(vial_code), Voucher, NA_character_),
    collection_date = excel_date(`Data collected`),
    collection_time = format(as.POSIXct(as.numeric(`Collection time`) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"),
    p_lactiferus = as.numeric(`# of P. lactiferus`)
  ) |>
  select(source_table, record_id, vial_code, event_name, jbd_site = `JBD Site`, collection_date, collection_time,
         latitude = Latitude, longitude = Longitude, elevation = Elevation, state = State, locality = Location,
         plant_name_verbatim = `Plant ID`, plant_id_by = `Plant ID by`, plant_id_using = `Plant ID using`,
         sample_time_minutes = `Sample time (minutes)`, distance_to_rainforest_m = `distance (m)`,
         collector = Collector, notes = Notes, p_lactiferus, p_lactiferus_sex = Sex)

# Table S3 vials with a host plant; its rows for Table S1's observation-only events are dropped.
# A vial from a Table S1 event (18JDEC497 with 18JDEC497-A) takes the Table S1 date.
s1_dates <- s1_records |>
  filter(!is.na(vial_code)) |>
  distinct(vial_base = str_remove(vial_code, "-A$"), s1_date = collection_date)

s3_records <- s3 |>
  mutate(plant_name_from_notes = unname(NOTE_PLANTS[`Unique Ide`])) |>
  filter(!is.na(`Plant ID`) | !is.na(plant_name_from_notes), str_detect(`Unique Ide`, "^(1[89]JDEC|Dor)")) |>
  mutate(
    source_table = "Table S3",
    record_id = `Unique Ide`,
    vial_code = `Unique Ide`,
    vial_base = str_remove(`Unique Ide`, "-A$"),
    collection_date = excel_date(`Data collected`),
    bees_in_vial = suppressWarnings(as.numeric(NumInsect))
  ) |>
  left_join(s1_dates, by = "vial_base") |>
  mutate(collection_date = coalesce(s1_date, collection_date)) |>
  select(source_table, record_id, vial_code, jbd_site = `JBD Site`, collection_date,
         latitude = Latitude, longitude = Longitude, elevation = Elevation, state = State, locality = Location,
         plant_name_verbatim = `Plant ID`, plant_name_from_notes, plant_id_by = `Plant ID by`, plant_id_using = `Plant ID using`,
         sample_time_minutes = `Sample time`, distance_to_rainforest_m = `Distance from MVS2 m`,
         collector = Collector, notes = Notes, further_notes = `Further notes`, bees_in_vial)

stopifnot(all(note_visitors$record_id %in% c(s1_records$record_id, s3_records$record_id)))

records <- bind_rows(s1_records, s3_records) |>
  mutate(
    # coordinates are given to 5-8 decimal places; rounding to 8 only clears floating-point
    # noise from the xlsx (e.g. -17.268039999999999)
    latitude = round(as.numeric(latitude), 8),
    longitude = round(as.numeric(longitude), 8),
    distance_to_rainforest_m = round(as.numeric(distance_to_rainforest_m)),
    sample_time_minutes = round(as.numeric(sample_time_minutes), 1),
    state = unname(STATES[state]),
    locality = str_squish(locality)
  )

# Visitors -----------------------------------------------------------------------------------

# the "-A" vials, holding only the P. lactiferus of an event, have no NumInsect
vial_bees <- records |>
  filter(source_table == "Table S3", !is.na(bees_in_vial)) |>
  transmute(record_id, visitor_taxon_name = "Anthophila", visitor_sex = NA_character_,
            visitor_count = as.character(bees_in_vial),
            flower_visitor = if_else(bees_in_vial > 0, "bee", NA_character_))

p_lactiferus <- records |>
  filter(source_table == "Table S1", p_lactiferus > 0) |>
  transmute(record_id, visitor_taxon_name = "Pharohylaeus lactiferus", visitor_sex = p_lactiferus_sex,
            visitor_count = as.character(p_lactiferus), flower_visitor = "colletidae")

visitors <- bind_rows(vial_bees, p_lactiferus, note_visitors |> select(-visitor_verbatim))

no_visitors <- records |>
  filter(source_table == "Table S1", !record_id %in% visitors$record_id) |>
  transmute(record_id, visitor_count = "0")

data <- records |>
  select(-p_lactiferus, -p_lactiferus_sex, -bees_in_vial) |>
  inner_join(bind_rows(visitors, no_visitors), by = "record_id", relationship = "one-to-many") |>
  left_join(note_visitors |> select(record_id, visitor_taxon_name, visitor_verbatim), by = c("record_id", "visitor_taxon_name")) |>
  mutate(
    non_flower_visit = unname(NON_FLOWER_VISITS[vial_code]),
    flower_visitor = if_else(is.na(non_flower_visit), flower_visitor, NA_character_)
  )

# Host plants --------------------------------------------------------------------------------

GENUS_SPELLING <- c(Grevillia = "Grevillea", Syzigium = "Syzygium", Hibertia = "Hibbertia", Xanthorrea = "Xanthorrhoea")

data <- data |>
  mutate(
    # "Rubus¬†rosifolius": a non-breaking space garbled to "¬†" in the xlsx
    plant_name = str_squish(str_replace_all(coalesce(plant_name_verbatim, plant_name_from_notes), "¬†", " ")),
    # other names stay as the author wrote them and are mapped in metadata.yml
    # (taxonomic_updates, exclude_observations)
    plant_name = if_else(str_detect(plant_name, "^[A-Z][a-z]+( spp?\\.)?$"), paste(word(plant_name, 1), "sp."), plant_name),
    genus = word(plant_name, 1),
    genus_level = str_detect(plant_name, "^[A-Z][a-z]+ sp\\.$") & !str_detect(genus, "aceae$"),
    plant_name = if_else(genus_level & genus %in% names(GENUS_SPELLING),
                         paste(GENUS_SPELLING[genus], "sp."), plant_name)
  )

# Locations ----------------------------------------------------------------------------------

locations <- data |>
  distinct(latitude, longitude, state, locality, elevation, distance_to_rainforest_m, jbd_site) |>
  group_by(latitude, longitude) |>
  summarise(
    state = first(state),
    locality = first(na.omit(locality)),
    `elevation (m)` = first(na.omit(elevation)),
    `distance to tropical or subtropical rainforest (m)` = first(na.omit(distance_to_rainforest_m)),
    notes = paste("site", paste(sort(unique(na.omit(jbd_site))), collapse = ", ")),
    .groups = "drop"
  ) |>
  group_by(locality) |>
  mutate(n_coords = n()) |>
  ungroup() |>
  mutate(location_name = if_else(n_coords == 1, locality, sprintf("%s (%s, %s)", locality, latitude, longitude)))

stopifnot(!anyDuplicated(locations$location_name), !anyNA(locations$location_name))

data <- data |>
  left_join(locations |> select(latitude, longitude, location_name), by = c("latitude", "longitude"))

# Taxon names, basis of record -------------------------------------------------------------

CULTIVATED_EXOTICS <- c("Adenanthera pavonina", "Adenthera palonina", "Beaucarnea recurvata", "Caesalpinia ferrea",
                        "Cassia fistulata", "Duranta repens", "Durantas repens", "Koelreuteria paniculata",
                        "Ocimum basilicum")
PLANTED_RE <- regex(paste("garden|\\byards?\\b|street|native plantings|\\bproperty\\b|driveway|garage|school|hotel",
                          "\\btown|suburb|urban|(?<!national )\\bpark|council strip|carpark", sep = "|"), ignore_case = TRUE)
# weeds growing in towns and parks were not planted
WEEDS <- c("Solanum seaforthianum")
WEED_RE <- regex("weed", ignore_case = TRUE)

data <- data |>
  group_by(plant_name, location_name) |>
  mutate(n_dates = n_distinct(collection_date)) |>
  ungroup() |>
  mutate(
    taxon_name = if_else(genus_level,
                         paste0(plant_name, " [", location_name, if_else(n_dates > 1, paste0(", ", collection_date), ""), "; Dorey_2021]"),
                         plant_name),
    basis_of_record = case_when(
      plant_name_verbatim %in% CULTIVATED_EXOTICS ~ "captive_cultivated",
      plant_name %in% WEEDS | str_detect(notes, WEED_RE) %in% TRUE ~ "field",
      str_detect(notes, PLANTED_RE) %in% TRUE ~ "captive_cultivated",
      TRUE ~ "field"
    )
  ) |>
  # one collecting event (plant, location, date) is one set of plants, so one basis_of_record;
  # an empty vial or observation is kept only when nothing was caught or seen in the event
  group_by(taxon_name, location_name, collection_date) |>
  mutate(
    basis_of_record = if_else(any(basis_of_record == "captive_cultivated"), "captive_cultivated", "field"),
    event_has_visitors = any(!is.na(flower_visitor))
  ) |>
  ungroup() |>
  filter(!(is.na(flower_visitor) & is.na(non_flower_visit) & event_has_visitors)) |>
  select(taxon_name, plant_name, plant_name_verbatim, plant_name_from_notes, plant_id_by, plant_id_using, basis_of_record,
         flower_visitor, visitor_taxon_name, visitor_sex, visitor_count, visitor_verbatim, non_flower_visit,
         location_name, collection_date, collection_time, source_table, record_id, vial_code, event_name, jbd_site,
         sample_time_minutes, collector, notes, further_notes) |>
  arrange(taxon_name, location_name, collection_date, record_id, visitor_taxon_name)

locations <- locations |>
  select(location_name, `latitude (deg)` = latitude, `longitude (deg)` = longitude, `elevation (m)`,
         `distance to tropical or subtropical rainforest (m)`, locality, notes, state) |>
  arrange(location_name)

write_csv(data, "data/Dorey_2021/data.csv", na = "")
write_csv(locations, "data/Dorey_2021/locations.csv", na = "")
