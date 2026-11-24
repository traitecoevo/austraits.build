# Link each record of data/Brown_1997/data.csv to its reference (#888):
#
#   source_id           the key of a published reference in data/Brown_1997/sources.yml;
#                       blank (the dataset) for unpublished or unidentified records
#   unpublished_source  the unpublished record a value comes from (observations,
#                       collections, manuscript notes, personal communications),
#                       mapped to `measurement_remarks`
#   reference_full      the reference written out in full, for looking up; not mapped
#   source_notes        how the reference number was read, and anything to check in
#                       the print book; not mapped
#
# and write raw/Brown_1997_references_to_check.csv, the records whose reference is
# missing, unreadable or inferred, for checking against the printed book.
#
#     Rscript data/Brown_1997/raw/add_source_id.R
#
# Each record's `Comments` reads "Reference in Brown et al. (1997):N", N being a
# reference in the book's Appendix Five, "Database References" (pp. 455-463).
# `raw/references.csv` is that list, one row per reference (`refnumber`):
#
#   - refs 135-160: references_135-160_standardised.bib (E. Wenk), matched to
#     OpenAlex/CrossRef;
#   - the others: transcribed from photographs of the printed pages, and matched
#     to CrossRef where the title, year (within one) and first author agree, or by
#     eye where only spelling differs (`origin`). Matched entries take the DOI,
#     volume, issue and pages from the record; `note` keeps any difference from
#     the printed reference;
#   - unpublished records (bibtype `Unpublished`: a person's observations,
#     collections, manuscript notes, an undergraduate project) are not sources:
#     as in Prendergast_2026_2, their records cite the dataset, with the record
#     named in `unpublished_source` -- except Keighery's (1979) titled notes (138),
#     which are cited;
#   - "X in Storr (unpubl. ms. notes 1)", "X in Sargent (1928)" and the like
#     belong to the reference the observation is reported in (`see`);
#   - 7 and 155 are personal communications to E. M. Brown that add "also see" a
#     paper: their records cite the paper (7b, 155b), and the personal
#     communication goes in `unpublished_source`.
#
# references.csv is built by raw/references_build/build_references.py (run in
# that folder) from appendix5_transcribed.txt, the 135-160 bib and the saved
# CrossRef results (crossref.json); edit references.csv directly to fix a reference.
#
# The `Comments` values were typed from a scan, and some are garbled; `ocr` below
# reads those, each checked against records of the same plant, place and year.

suppressMessages({library(dplyr); library(readr); library(stringr); library(tibble); library(purrr)})
source("ignore/source_references.R")

dir <- "data/Brown_1997"
d <- read_csv(file.path(dir, "data.csv"), col_types = cols(.default = col_character()),
              na = character(), trim_ws = FALSE, show_col_types = FALSE) |>
  select(-any_of(c("source_id", "unpublished_source", "reference_full", "source_notes")))
refs <- read_csv(file.path(dir, "raw/references.csv"), col_types = cols(.default = col_character()),
                 na = "", show_col_types = FALSE)

# ---- Comments -> reference number ----
# value read, and why (confidence: "digits" = the number is legible among the
# noise; "inferred" = read from records of the same plant, place and year)
ocr <- tribble(
  ~tail,    ~ref,  ~confidence, ~basis,
  ", 102",  "102", "digits",   "",
  ", 106",  "106", "digits",   "",
  ", 17",   "17",  "digits",   "",
  ",247",   "247", "digits",   "",
  ",87",    "87",  "digits",   "",
  ". 87",   "87",  "digits",   "",
  "/ 11",   "11",  "digits",   "other records 5 km S of Eneabba in 1984 cite 11",
  "/ 127",  "127", "digits",   "Livistona eastonii, Mitchell Plateau (Johnstone & Smith 1981)",
  "/ 16",   "16",  "digits",   "",
  "/ 33",   "33",  "digits",   "",
  "0.247",  "247", "digits",   "",
  "1 6 ;",  "16",  "digits",   "",
  "143 .",  "143", "digits",   "",
  "145/",   "145", "digits",   "",
  "155/",   "155", "digits",   "",
  "16/",    "16",  "digits",   "",
  "163>",   "163", "digits",   "",
  "205 1",  "205", "digits",   "Upper Liveringa Station (Shilling 1948)",
  "22 /",   "22",  "digits",   "",
  "22 1",   "22",  "digits",   "221 is a bee revision",
  "•22,", "22", "digits",  "",
  "247/",   "247", "digits",   "",
  "87 v",   "87",  "digits",   "",
  "98 _",   "98",  "digits",   "",
  "24&",    "246", "inferred", "Hovea / Tarsipes in 1996: Woldendorp (1996)",
  "24^",    "248", "inferred", "11 records of the same plant and visitor near Waychinicup River in 1980 cite 248",
  "42736",  "117", "inferred", "other records E of Mt Nossiter in 1983 cite 117",
  "44961",  "42",  "inferred", "the other Mus musculus records at Millbrook Nature Reserve in 1993 cite 42",
  "11>6",   "106", "inferred", "other Mt Gardner records cite 106",
  "IB",     "18",  "inferred", "Macropidia fuliginosa: E. M. Brown's (1989) thesis",
  "in",     "171", "inferred", "the other Ongerup 1978 bird records cite 170-172; see the 111 note",
  "n",      "72",  "inferred", "the other Pachyprosopis records 20 km S of Kununurra in 1973 cite 72"
)
unreadable <- c("10(5", "lOtf")   # cite the dataset

prefix <- "^Reference in Brown et al\\. \\(1997\\):"
d <- d |>
  mutate(
    .row = row_number() + 1L,   # row as a spreadsheet numbers it
    .tail = ifelse(str_detect(Comments, prefix), str_remove(Comments, prefix), NA_character_),
    .clean = str_detect(coalesce(.tail, ""), "^\\d+$") & suppressWarnings(as.integer(.tail)) %in% 1:255,
    .ref = case_when(.clean ~ .tail, TRUE ~ ocr$ref[match(.tail, ocr$tail)]),
    source_notes = case_when(
      is.na(Comments) | Comments %in% c("", "NA") ~
        "No reference given in Comments; cites the dataset. Check the print book.",
      .tail == "0" ~
        "Comments gives reference 0, which is not in Appendix Five; cites the dataset. Check the print book.",
      .tail %in% unreadable ~
        sprintf("Comments reads '%s', which cannot be read as a reference; cites the dataset. Check the print book.", .tail),
      !.clean & !is.na(.ref) ~ sprintf("Comments reads '%s', read as reference %s (%s%s).%s", .tail, .ref,
        ocr$confidence[match(.tail, ocr$tail)],
        ifelse(ocr$basis[match(.tail, ocr$tail)] == "", "", paste0(": ", ocr$basis[match(.tail, ocr$tail)])),
        ifelse(ocr$confidence[match(.tail, ocr$tail)] == "inferred", " Check the print book.", "")),
      TRUE ~ NA_character_
    ),
    # 111 (Houston 1981a, a revision of hylaeine bees) for birds: the Ongerup 1978
    # bird records otherwise cite 170-172 (Newbey); read as 171
    .bird111 = .ref %in% "111" & str_squish(`Animal class`) == "Aves",
    source_notes = ifelse(.bird111,
      "Comments gives reference 111 (Houston 1981a, a revision of hylaeine bees) for a bird record; read as 171 (Newbey & Newbey, unpublished observations), as the other Ongerup 1978 bird records cite 170-172. Check the print book.",
      source_notes),
    .ref = ifelse(.bird111, "171", .ref),
    source_notes = ifelse(.ref %in% "111" & str_squish(`Animal class`) == "Insecta" & str_squish(`Animal order`) == "Coleoptera",
      "Comments gives reference 111 (Houston 1981a), a taxonomic revision of hylaeine bees, for a beetle record; kept, but a revision is unlikely to report a beetle visitor. Check the print book.",
      source_notes),
    .ref = recode(.ref, "7" = "7b", "155" = "155b")   # pers. comm. "also see" a paper
  )
left <- setdiff(unique(d$.tail[!d$.clean & !is.na(d$.tail) & is.na(d$.ref)]), c("0", unreadable))
if (length(left) > 0) stop("Comments not read: ", paste(left, collapse = ", "))

# ---- references written out ----
fmt <- function(r) {
  au <- str_split_1(str_replace_all(r$author %||% "", "[{}]", ""), " and ")
  au <- if (length(au) > 1) paste(paste(head(au, -1), collapse = ", "), "and", tail(au, 1)) else au
  yr <- if (is.na(r$year) || r$year %in% c("", "n.d.")) "" else sprintf(" (%s)", r$year)
  head <- sprintf("%s%s. %s.", au, yr, str_remove(r$title, "\\.$"))
  body <- switch(r$bibtype,
    Article = paste0(" ", r$journal, if (!is.na(r$volume)) paste0(" ", r$volume) else "",
                     if (!is.na(r$number)) sprintf("(%s)", r$number) else "",
                     if (!is.na(r$pages)) paste0(": ", r$pages) else "", "."),
    InBook = paste0(" In: ", r$booktitle, if (!is.na(r$editor)) sprintf(" (ed. %s)", r$editor) else "",
                    if (!is.na(r$pages)) paste0(", pp. ", r$pages) else "",
                    if (!is.na(r$publisher)) paste0(". ", r$publisher) else "", "."),
    Book = paste0(" ", paste(na.omit(c(r$publisher, r$place)), collapse = ", "), if (r$year %in% "n.d.") ", undated" else "", "."),
    Thesis = paste0(" ", r$type, ", ", r$institution, "."),
    Report = paste0(" ", r$institution, "."),
    Unpublished = if (!is.na(r$note) && str_detect(r$note, "^unpublished|Murdoch")) paste0(" ", str_remove(r$note, ";.*$"), ".") else "",
    "")
  doi <- if (!is.na(r$doi)) paste0(" doi:", r$doi) else ""
  str_squish(str_replace_all(paste0(head, body, doi), "--", "-"))
}
refs <- refs |> mutate(.full = map_chr(seq_len(n()), \(i) fmt(refs[i, ])))
by_num <- refs |> filter(is.na(see) | see == "")
full_of <- setNames(by_num$.full, by_num$refnumber)
bibtype_of <- setNames(by_num$bibtype, by_num$refnumber)
# Unpublished works with enough detail to cite as sources: 138, Keighery's (1979)
# titled notes from King's Park and Botanic Gardens
cited_unpublished <- c("138")
bibtype_of[cited_unpublished] <- "Unpublished (cited)"
key_of <- setNames(by_num$key, by_num$refnumber)
see <- refs |> filter(!is.na(see), see != "")   # "X in <ref>", and the pers. comms 7 and 155
printed <- function(n) sprintf("Brown et al. (1997) ref. %s", n)

# Each record: its own reference number (as printed), and the work it sits in
d <- d |>
  mutate(
    .printed = recode(.ref, "7b" = "7", "155b" = "155"),
    .see_row = match(.printed, see$refnumber),
    .work = ifelse(!is.na(.see_row), see$see[.see_row], .ref),
    .unpub = bibtype_of[.work] %in% "Unpublished",
    reference_full = case_when(
      is.na(.ref) ~ NA_character_,
      !is.na(.see_row) & .printed %in% c("7", "155") ~
        sprintf("%s: %s, personal communication to E. M. Brown. Also see: %s", printed(.printed),
                see$author[.see_row], full_of[.work]),
      !is.na(.see_row) ~ sprintf("%s: %s, in %s", printed(.printed), see$author[.see_row], full_of[.work]),
      TRUE ~ sprintf("%s: %s", printed(.printed), full_of[.work])
    ),
    unpublished_source = case_when(
      .printed %in% c("7", "155") ~ sprintf("%s, personal communication to E. M. Brown (%s)",
                                             see$author[.see_row], printed(.printed)),
      .unpub & !is.na(.see_row) ~ sprintf("%s, in %s (%s)", see$author[.see_row],
                                           str_remove(full_of[.work], "\\.$"), printed(.printed)),
      .unpub ~ sprintf("%s (%s)", str_remove(full_of[.work], "\\.$"), printed(.printed)),
      TRUE ~ NA_character_
    ),
    .key = ifelse(is.na(.work) | .unpub, NA_character_, unname(key_of[.work]))
  )

# ---- published references: entries, checked against AusTraits ----
fields <- c("bibtype", "year", "author", "title", "booktitle", "editor", "journal", "volume", "number",
            "pages", "publisher", "place", "institution", "type", "doi", "url", "note")
entry_of <- function(r) {
  e <- as.list(r[fields])
  e <- e[!is.na(unlist(e))]
  if (!is.null(e$year) && grepl("^[0-9]+$", e$year)) e$year <- as.integer(e$year)
  for (f in intersect(c("author", "title", "booktitle", "editor", "journal", "publisher", "institution", "note"), names(e))) {
    e[[f]] <- source_clean_text(e[[f]])
  }
  source_clean_entry(c(list(key = r$key), e))
}
own <- by_num |> filter(bibtype != "Unpublished" | refnumber %in% cited_unpublished)
entries <- map(split(own, seq_len(nrow(own))), entry_of)
names(entries) <- map_chr(entries, "key")

index <- source_reference_index(exclude = "Brown_1997")
cand <- tibble(
  key = names(entries),
  year = map_chr(entries, \(e) as.character(e$year %||% NA)),
  title = map_chr(entries, \(e) as.character(e$title %||% NA)),
  doi = map_chr(entries, \(e) as.character(e$doi %||% NA)),
  volume = map_chr(entries, \(e) as.character(e$volume %||% NA))
)
# Part 1 of Walker (1995), held by Prendergast_2026_3 with its DOI (printed here as vol. 52)
resolved <- source_resolve_keys(cand, index, same_as = c(Walker_1995_1 = "Walker_1995"))
key_map <- setNames(resolved$key_final, resolved$key)

final <- map(seq_len(nrow(resolved)), function(i) {
  r <- resolved[i, ]
  if (r$action == "copy") return(source_copy_entry(index, r$key_final, r$copy_from))
  e <- entries[[r$key]]; e$key <- r$key_final; e
})

d <- d |> mutate(source_id = coalesce(unname(key_map[.key]), ""))

# only references some kept record cites (read_metadata() needs sources.yml to exist)
if (!file.exists(file.path(dir, "sources.yml"))) writeLines("[]", file.path(dir, "sources.yml"))
m <- traits.build::read_metadata(file.path(dir, "metadata.yml"))
kept <- traits.build:::process_custom_code(m$dataset$custom_R_code)(
  d |> select(-starts_with(".")) |> mutate(across(everything(), ~ na_if(na_if(.x, ""), "NA"))))
used <- unique(na.omit(kept$source_id))
final <- final[map_chr(final, "key") %in% used]
final <- final[order(map_chr(final, "key"))]

problems <- source_check_entries(final)
if (nrow(problems) > 0) { print(problems, n = Inf); stop("fix the references above") }

# ---- write ----
check <- d |>
  filter(!is.na(source_notes) & str_detect(source_notes, "Check the print book")) |>
  transmute(data_csv_row = .row, `Plant scientific name`, `Animal species binomial name`,
            `Nearest named place`, Date, Year, Comments,
            read_as = recode(.printed, .missing = "none (cites the dataset)"),
            reference_read = reference_full, source_notes,
            checked_reference_number = "", checker_comments = "")
write_csv(check, file.path(dir, "raw/Brown_1997_references_to_check.csv"), na = "")

write_csv(d |> select(-starts_with(".")), file.path(dir, "data.csv"), na = "NA")
source_write_yml(final, file.path(dir, "sources.yml"))
write_csv(
  refs |> select(refnumber, key, see, bibtype) |>
    left_join(resolved |> select(key, source_id = key_final, action, copy_from, note), by = "key") |>
    mutate(cited_as_source = source_id %in% used),
  file.path(dir, "raw/source_id_resolved.csv"), na = "")
print(count(resolved, action))
cat(length(final), "published references written;", sum(d$source_id == ""), "records cite the dataset, of which",
    sum(!is.na(d$unpublished_source) & d$source_id == ""), "name an unpublished source;", nrow(check), "records to check\n")
