# Writes data/Lamont_2026/data.csv from the author's Word table
# (raw/Table of flowering plant species and their pollinators in SW australia.docx).
# Run from the repo root.
#   - The table is copied cell for cell; paragraphs within a cell are joined with a
#     single space. The two "Family" columns are renamed Plant family / Animal family.
#   - source_id: the study that first published each record, from the Reference column.
#     "also see" references are secondary references, not sources.
#   - location_name, collection_date: sites and observation dates of the same records in
#     Brown_1997 and Elliott_2026 (filtered there, kept here); otherwise the publication
#     year of the source(s). Records seen at several sites become one row per site.
#   - taxon_name, visitor_taxon: plant name without the table's "*"; the animal named in
#     the table with common names removed and misspellings corrected.
#   - flower_visitor / pollination_vector_possible: rows whose Notes say the animal was
#     not inspected for pollen, or is an ineffective pollinator, are flower_visitor; all
#     others are pollination_vector_possible. visitor_group is the most specific
#     config/traits.yml term for the animal.
#   - flower_pollinator_reward, colour_stage_visited, pollen_grains_carried: from Notes
#     (and, for Diuris magnifica and Thelymitra crinita, the text of Lamont 2026).
#   - Plant-level rows: one extra row per taxon for traits of the plant rather than the
#     visitor (self-pollination, crepuscular anthesis, mammal-pollination syndrome), with
#     the visitor columns left blank so no visitor context attaches to them.

library(dplyr)
library(stringr)
library(xml2)

docx <- "data/Lamont_2026/raw/Table of flowering plant species and their pollinators in SW australia.docx"

tmp <- tempfile()
unzip(docx, files = "word/document.xml", exdir = tmp)
doc <- read_xml(file.path(tmp, "word/document.xml"))
ns <- xml_ns(doc)

rows <- xml_find_all(doc, "//w:tbl/w:tr", ns)
cells <- lapply(rows, function(r) {
  vapply(xml_find_all(r, "./w:tc", ns), function(tc) {
    paras <- vapply(xml_find_all(tc, ".//w:p", ns),
                    function(p) paste(xml_text(xml_find_all(p, ".//w:t", ns)), collapse = ""),
                    character(1))
    str_trim(paste(paras[paras != ""], collapse = " "))
  }, character(1))
})

header <- c("Plant species", "Plant family", "Animal species", "Animal family", "Notes", "Reference")
stopifnot(identical(cells[[1]][c(1, 3, 5, 6)], header[c(1, 3, 5, 6)]))

data <- as_tibble(do.call(rbind, cells[-1]), .name_repair = ~header) |>
  filter(`Plant species` != "")

data <- data |>
  mutate(
    source_id = case_when(
      str_starts(Reference, "George") ~ "George_1981",
      str_starts(Reference, "Van Leeuwen") ~ "vanLeeuwen_1997",
      str_starts(Reference, "Lamont, B. B. \\(2026\\)") ~ "Lamont_2026",
      str_starts(Reference, "Scaccabarozzi, D., Guzzetti") ~ "Scaccabarozzi_2020",
      str_starts(Reference, "Scaccabarozzi D et al. 2024") ~ "Scaccabarozzi_2024 Scaccabarozzi_2020",
      str_starts(Reference, "Scaccabarozzi, D. & Sletvold") ~ "Scaccabarozzi_2025",
      str_starts(Reference, "Lamont, B. 1982") ~ "Lamont_1982",
      str_starts(Reference, "Lamont, B.B. and Collins") ~ "Lamont_1988",
      str_starts(Reference, "Groom, P.K. and Lamont, B.B. 2015. Pollination strategies") ~ "Groom_2015",
      str_starts(Reference, "also see Groom, P.K. and Lamont, B.B. 2015. Pollination syndromes") ~ "Groom_2015_2",
      str_starts(Reference, "Houston") ~ "Houston_1993",
      str_starts(Reference, "Phillips") ~ "Phillips_2009",
      str_starts(Reference, "Peakall") ~ "Peakall_1989",
      str_starts(Reference, "Lamont, B.B 1985") ~ "Lamont_1985"
    ),
    collection_date = case_when(
      source_id == "Scaccabarozzi_2024 Scaccabarozzi_2020" ~ "2020/2024",
      TRUE ~ str_extract(source_id, "[0-9]{4}")
    )
  )

stopifnot(!any(is.na(data$source_id)))

# Sites and dates of the same records in Brown_1997 and Elliott_2026. The whole Lamont (1985)
# study was at Crystal Brook, and Lamont and Collins (1988) at Jandakot airport. Brown_1997
# day "15" means the day is not known, so those dates are kept to the month.
sites <- tribble(
  ~`Plant species`, ~animal, ~location_name, ~site_date,
  "Grevillea leucopteris", "Pachytricha", "Bull Creek", "1977-11",
  "Verticordia nitens", "Euryglossa", "12 km WNW of Harvey", "1991-01-04",
  "Verticordia nitens", "Euryglossa", "Moore River National Park", "1989-12-20",
  "Verticordia nitens", "Euryglossa", "Wanneroo_2", "1990-12-20",
  "Verticordia aurea", "Euryglossa", "S of Eneabba", "1990-11-14",
  "Leporella fimbriata", "Myrmecia", "Jandakot", "1984-05-03",
  "Leporella fimbriata", "Myrmecia", "Kwinana", "1984-05-03",
  "Leporella fimbriata", "Myrmecia", "Wanneroo_1", "1984-05-03",
  "Stylidium scandens", "Aluecasia", "Frankland River National Park", "1988-11"
)

data <- data |> mutate(row = row_number())
matched <- data |>
  inner_join(sites, by = "Plant species", relationship = "many-to-many") |>
  filter(str_starts(`Animal species`, animal)) |>
  select(-animal)

data <- bind_rows(matched, anti_join(data, matched, by = "row")) |>
  arrange(row) |>
  mutate(
    location_name = case_when(
      source_id == "Lamont_1985" ~ "Crystal Brook",
      source_id == "Lamont_1988" ~ "Jandakot airport",
      TRUE ~ location_name
    ),
    site_date = case_when(
      source_id == "Lamont_1985" ~ "1982-09",
      source_id == "Lamont_1988" ~ "1982-09/1984-11",
      TRUE ~ site_date
    ),
    collection_date = coalesce(site_date, collection_date)
  ) |>
  select(-site_date)

# Visitor-level columns
data <- data |>
  mutate(
    taxon_name = str_trim(str_remove(`Plant species`, "\\*")),
    visitor_taxon = str_trim(str_remove_all(`Animal species`, "\\((?![A-Z][a-z]+\\))[^)]*\\)")),
    visitor_taxon = if_else(visitor_taxon == "", `Animal family`, visitor_taxon),
    visitor_taxon = str_replace_all(
      visitor_taxon,
      c("Aluecasia" = "Aleucosia", "Circartetus" = "Cercartetus", "Scollid wasp" = "Scoliidae",
        "\\(Leiproctus\\)" = "(Leioproctus)", "kershawii" = "kershawi")
    ),
    visitor_group = case_when(
      str_detect(`Animal species`, "^Apis") ~ "honeybee",
      `Animal family` == "Meliphagidae" ~ "honeyeater",
      str_detect(`Animal species`, "^(Trichocolletes|Leioproctus|Euryglossa)") ~ "colletidae",
      str_detect(`Animal species`, "^(Tarsipes|Circartetus)") ~ "marsupial",
      str_detect(`Animal species`, "^Megaselia") ~ "fly",
      str_detect(`Animal species`, "^Aluecasia") ~ "bee_fly",
      str_detect(`Animal species`, "^(Pachytricha|Liparetrus|Stigmodera)|beetle") ~ "beetle",
      str_detect(`Animal species`, "^Gryllacris") ~ "orthoptera",
      str_detect(`Animal species`, "^(Phymatothynnus|Thynninae)") ~ "thynnine_wasp",
      str_detect(`Animal species`, "^Myrmecia") ~ "ant",
      str_detect(`Animal species`, "^(Vanessa|Geitoneura)") ~ "butterfly",
      str_detect(`Animal species`, "^Heliothis") ~ "moth",
      str_detect(`Animal species`, "^(Scollid|Cryptocheilus|Anthobosca)") ~ "wasp"
    ),
    visit_only = str_detect(Notes, "not inspected for pollen|ineffective pollinator"),
    flower_visitor = if_else(visit_only, visitor_group, NA_character_),
    pollination_vector_possible = if_else(visit_only, NA_character_, visitor_group),
    colour_stage_visited = case_when(
      str_detect(Notes, "Visits .+? stage rather than") ~ str_match(Notes, "Visits (.+? stage rather than [^,]+)")[, 2],
      str_detect(Notes, "yellow \\(visited\\) to red") ~ "yellow inflorescence stage rather than red"
    ),
    pollen_grains_carried = str_match(Notes, "([0-9]+) grains")[, 2],
    flower_pollinator_reward = case_when(
      str_detect(Notes, "Sexually deceptive") ~ "deceit",
      taxon_name %in% c("Diuris magnifica", "Thelymitra crinita") ~ "deceit",
      str_detect(Notes, "harvested pollen|ingests oily pollen") ~ "pollen"
    )
  )

stopifnot(!any(is.na(data$visitor_group)))

# Plant-level rows (one per taxon), without visitor columns
plant <- data |>
  filter(str_detect(Notes, "self-pollinate|Crepuscular anthesis|Mammal-pollinated syndrome")) |>
  distinct(taxon_name, .keep_all = TRUE) |>
  mutate(
    across(c(visitor_taxon, visitor_group, flower_visitor, flower_pollinator_reward,
             colour_stage_visited, pollen_grains_carried), ~ NA_character_),
    pollination_vector_possible = if_else(str_detect(Notes, "self-pollinate"), "autonomous", NA_character_),
    breeding_system = if_else(str_detect(Notes, "self-pollinate"), "autogamy", NA_character_),
    flowering_time_diel = if_else(str_detect(Notes, "Crepuscular anthesis"), "crepuscular", NA_character_),
    pollination_syndrome = if_else(str_detect(Notes, "Mammal-pollinated syndrome"), "mammal", NA_character_)
  )

data <- bind_rows(data, plant) |>
  arrange(row, !is.na(visitor_taxon)) |>
  select(-row, -visit_only)

write.csv(data, "data/Lamont_2026/data.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
