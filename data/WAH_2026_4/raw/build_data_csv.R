# WAH_2026_4: Florabase "Management Notes (for the Swan NRM Region)" (K. Brown & K. Bettink et al.; 372 weed profiles in
# florabase_profiles.csv), read field by field (user 2026-11-04: one dataset per kind of Florabase text). Run from the repository
# root: Rscript data/WAH_2026_4/raw/build_data_csv.R
#
# The "General Biology" section is a list of labelled fields ("Growth form. Herb. Life form. Annual, caespitose. Reproduction. Seed.
# Dispersal. Water, soil. Photosynthetic Pathway. C4. Time to first flowering. 2 years. Seedbank persistence. Medium, 1-5 years. Fire
# response. Generally survives fire. Toxicity. ..."), some with a "Vegetative regeneration strategy. ..." and "Woody structure. ..."
# appended; the calendar field is "Flowering: Aug(O) Sep Oct Nov(O); Fruiting: ...; Germination: ...; Active Growth: ...; Dormant: ...;
# Optimum Treatment: ..." ("(O)" = occasionally). Each field's own words are the value_description; the mappings are the tables
# below. Management (herbicide, removal, treatment months) and the reference list are not traits and are left out.
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(tibble); library(tidyr); library(readr)
})
src <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra/florabase_profiles.csv"
mn <- "Management Notes (for the Swan NRM Region)"
d <- read_csv(src, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(!is.na(.data[[mn]]))

# names as in the other WAH_2026 datasets (data/ABRS_AtoM_2026/raw/build_data_csv.R, Florabase mode): the classification path's last
# element without its authority; phrase names kept as written (2026-11-05: informal infraspecific names, variants, vouchers too; other
# names keep every epithet and rank, hybrid formulae and intergrades included - the same function as there)
fb_ranks <- c("subsp.", "var.", "f.", "forma", "nothosubsp.", "nothovar.", "cv.")
fb_auth_words <- c("ex", "de", "du", "da", "van", "von", "der", "den", "la", "le", "in", "et", "and", "ms", "non", "sensu", "lato", "stricto", "auct")
fb_name <- function(n) map_chr(n, function(n) {
  n <- str_squish(str_replace_all(n, "×\\s*", "x "))
  if (str_detect(n, " sp\\. | (?:subsp\\.|var\\.|f\\.|forma) [A-Z']| variant\\b|\\([^()]*[0-9][^()]*\\)$")) return(n)
  tk <- str_split(n, " ")[[1]]
  g <- if (tk[1] == "x") 2 else 1
  epi <- function(k) k <= length(tk) && str_detect(tk[k], "^[a-z][a-z-]+$") && !tk[k] %in% fb_auth_words
  s <- if (length(tk) > g + 1 && tk[g + 1] == "x" && epi(g + 2)) g + 2 else g + 1
  if (length(tk) <= g || !str_detect(tk[g], "^[A-Z][a-z-]+$") || !epi(s)) return(word(n, g))
  out <- tk[seq_len(s)]; k <- s + 1
  while (k <= length(tk)) {
    t <- tk[k]
    if (t %in% fb_ranks && epi(k + 1)) { out <- c(out, if (t == "forma") "f." else t, tk[k + 1]); k <- k + 2 }
    else if (t %in% fb_ranks && k + 2 <= length(tk) && tk[k + 1] == "x" && epi(k + 2)) { out <- c(out, t, "x", tk[k + 2]); k <- k + 3 }
    else if (t %in% c("x", "-", "/") && epi(k + 1)) { out <- c(out, t, tk[k + 1]); k <- k + 2 }
    else if (t %in% c("x", "-", "/") && k + 2 <= length(tk) && str_detect(tk[k + 1], "^[A-Z][a-z-]+$") && epi(k + 2)) { out <- c(out, t, tk[k + 1], tk[k + 2]); k <- k + 3 }
    else k <- k + 1
  }
  paste(out, collapse = " ")
})
d <- d %>% mutate(cl_name = str_trim(str_extract(classification, "[^>]+$")), taxon_name = fb_name(coalesce(cl_name, taxon_name)),
                  cur = `Name Status` == "Current") %>%
  arrange(desc(cur)) %>% distinct(taxon_name, .keep_all = TRUE)

# ---------------------------------------------------------------- fields
labels <- c("Growth form", "Life form", "Reproduction", "Dispersal", "Photosynthetic [Pp]athway", "Time to first flowering", "Time of first flowering",
            "Time of first fruiting", "Seedbank persistence", "Fire response", "Toxicity", "Vegetative [Rr]egeneration [Ss]trategy", "Woody structure",
            "Type of stem", "Time of burrs produced")
lab_re <- paste0("(?:^|(?<=\\.)\\s+)(", paste(labels, collapse = "|"), ")\\.\\s*")
fields_of <- function(t) {
  gb <- str_split(t, "\\s*\\|\\s*")[[1]]; gb <- gb[str_detect(gb, "^General Biology")]
  if (!length(gb)) return(tibble(field = character(), text = character()))
  body <- str_remove(gb[1], "^General Biology\\.\\s*")
  loc <- str_locate_all(body, lab_re)[[1]]
  if (!nrow(loc)) return(tibble(field = character(), text = character()))
  lab <- str_match(str_sub(body, loc[, 1], loc[, 2]), paste0("(", paste(labels, collapse = "|"), ")"))[, 2]
  txt <- map_chr(seq_len(nrow(loc)), function(i) str_sub(body, loc[i, 2] + 1, if (i < nrow(loc)) loc[i + 1, 1] - 1 else nchar(body)))
  tibble(field = str_to_sentence(str_replace(lab, "Time of first flowering", "Time to first flowering")),
         text = str_squish(str_remove_all(txt, "^[.\\s]+|[.\\s]+$")))
}
fl <- d %>% transmute(taxon_name, f = map(.data[[mn]], fields_of)) %>% unnest(f) %>% filter(text != "")

rows <- list()
add <- function(taxon, trait, value, desc, q = NA_character_, cert = NA_character_, vt = "mode", region = NA_character_)
  rows[[length(rows) + 1]] <<- tibble(taxon_name = taxon, trait_name = trait, value = as.character(value), value_type = vt, value_description = desc,
                                     commonness_qualifier = q, scoring_certainty = cert, population_region = region)
# terms in text order, with the commonness / hedge word of their comma segment
terms <- function(text, dict) {
  seg <- str_split(text, ",\\s*|;\\s*|\\s+and\\s+|/")[[1]]
  map_dfr(seq_along(seg), function(i) {
    s <- seg[i]; v <- unique(unname(dict[map_lgl(names(dict), ~ str_detect(s, regex(.x, ignore_case = TRUE)))]))
    if (!length(v)) return(NULL)
    q <- str_to_lower(str_match(s, regex("\\b(primarily|mainly|largely|occasionally|rarely|very rarely|sometimes|usually)\\b", ignore_case = TRUE))[, 2])
    cw <- str_to_lower(str_match(s, regex("\\b(possibly|probably|likely)\\b", ignore_case = TRUE))[, 2])
    tibble(pos = i, value = v, q = q, cert = cw, term = str_squish(s))
  })
}

# ---------------------------------------------------------------- growth form, life form
gf <- c("^herb" = "herb", "^grass" = "graminoid", "^sedge" = "graminoid", "^geophyte" = "geophyte", "shrub" = "shrub", "tree" = "tree",
        # ("vine" = herbaceous climber unless stated woody: skill rule)
        "^vine" = "climber_herbaceous")
for (i in which(fl$field == "Growth form")) {
  h <- terms(fl$text[i], gf); if (nrow(h)) add(fl$taxon_name[i], "plant_growth_form", paste(unique(h$value), collapse = " "), fl$text[i])
}
for (i in which(fl$field == "Life form")) {
  t <- fl$text[i]; tx <- fl$taxon_name[i]
  lh <- c(if (str_detect(t, regex("\\bannual\\b(?!ly)", ignore_case = TRUE))) "annual",
          if (str_detect(t, regex("short[- ]lived perennial", ignore_case = TRUE))) "short_lived_perennial" else
            if (str_detect(t, regex("\\bperennial\\b|more than 2 years|annually renewed", ignore_case = TRUE))) "perennial")
  if (length(lh)) add(tx, "life_history", paste(lh, collapse = " "), t)
  sh <- c(if (str_detect(t, regex("caespitose", ignore_case = TRUE))) "caespitose", if (str_detect(t, regex("rhizom", ignore_case = TRUE))) "rhizomatous",
          if (str_detect(t, regex("stolon", ignore_case = TRUE))) "stoloniferous")
  # (in text order: "rhizomatous/caespitose")
  if (length(sh)) add(tx, "stem_growth_habit", paste(sh[order(map_int(c(caespitose = "caespitose", rhizomatous = "rhizom", stoloniferous = "stolon")[sh], ~ str_locate(str_to_lower(t), .x)[1]))], collapse = " "), t)
  so <- c(if (str_detect(t, regex("tuberous corm|\\bcorm", ignore_case = TRUE))) "corm", if (str_detect(t, regex("\\bbulb", ignore_case = TRUE))) "bulb",
          if (str_detect(t, regex("tuberous rhizome", ignore_case = TRUE))) "rhizome_fleshy" else if (str_detect(t, regex("rhizom", ignore_case = TRUE))) "rhizome",
          if (str_detect(t, regex("\\btuber\\b", ignore_case = TRUE))) "stem_tuber")
  if (length(so)) add(tx, "storage_organ", paste(so, collapse = " "), t)
}

# ---------------------------------------------------------------- reproduction
veg <- c("offsets?" = "bulb", "bulbils?" = "aboveground_clonal", "cormels?|corms?\\b" = "corm", "rhizomes?" = "rhizome", "stolons?" = "stolon",
         "root suckers?" = "root_buds", "root fragments?|roots\\b" = "root_buds", "(?<!root )suckers?|suckering" = "clonal",
         "stem layering|stem rooting|stems that come|take root" = "adventitious_roots", "stem (?:and root )?fragments?|stem segments|segments of the|fragmentation|crown fragments|vegetative fragments|stem nodes|cuttings" = "aboveground_clonal",
         "tubers?\\b" = "stem_tuber", "tuberous roots" = "root_tuber", "tillers?|tillering" = "clonal", "vegetative(?:ly| growth)?\\b" = "clonal")
for (i in which(fl$field == "Reproduction")) {
  t <- fl$text[i]; tx <- fl$taxon_name[i]
  h <- terms(t, veg)
  if (nrow(h)) {
    h <- h %>% group_by(value) %>% slice(1) %>% ungroup() %>% arrange(pos)
    for (k in split(h, paste(coalesce(h$q, ""), coalesce(h$cert, "")))) {
      add(tx, "clonal_spread_mechanism", paste(k$value, collapse = " "), paste(k$term, collapse = "; "), k$q[1], k$cert[1])
      add(tx, "vegetative_reproduction_ability", "vegetative", paste(k$term, collapse = "; "), k$q[1], k$cert[1])
    }
  }
  # ("This species reproduces only by seed"; "reproduces almost entirely by seeds" is not only)
  if (str_detect(t, regex("\\bonly by seed", ignore_case = TRUE))) add(tx, "vegetative_reproduction_ability", "not_vegetative", t)
}

# ---------------------------------------------------------------- dispersal (agents: dispersers; how: dispersal_syndrome)
disp <- c("\\bwater\\b|flood|run-off|tides|sea water" = "water", "\\bwind\\b" = "wind", "\\bbirds?\\b|galahs|cockatoos" = "birds", "\\bants\\b" = "ants",
          "rabbits|rats|mice|rodents" = "rodents", "foxes|possums|wildlife|other mammals|small mammals|\\bmammals?\\b|herbivorous mammals" = "mammals",
          "livestock|sheep|cattle|horses|grazing animals|\\bstock\\b|pigs" = "mammals_domestic", "\\banimals?\\b|fur\\b" = "animals",
          "garden refuse|garden waste|green waste|dumped vegetation|garden/other waste" = "garden_refuse", "vehicles?|tyres|vehical" = "vehicles",
          "skinks" = "vertebrates")
syn <- c("\\bwind\\b" = "anemochory", "\\bwater\\b|flood|run-off|tides|sea water" = "hydrochory", "ingestion|droppings|dung|seed in dung" = "endozoochory",
         "adhesion|on fur|\\bfur\\b|on fleece|awns on fur|on animals|attach" = "epizoochory", "\\bants\\b" = "myrmecochory",
         "explosive|explosively" = "ballistic", "machinery|vehicles?|tyres|clothing|footwear|shoes|slashing|mowing|hay|contaminat|planting|garden|soil movement|human|agricultur|harvest|ballast|boats|roadworks|road grading|nursery|mulch" = "anthrochory")
for (i in which(fl$field == "Dispersal")) {
  t <- str_remove(fl$text[i], "\\.\\s*(?:Vegetative regeneration strategy|Time of|Type of stem|Woody structure)\\b.*$"); tx <- fl$taxon_name[i]
  h <- terms(t, disp)
  if (nrow(h)) for (k in split(h, coalesce(h$cert, ""))) add(tx, "dispersers", paste(unique(k$value), collapse = " "), t, cert = k$cert[1])
  h <- terms(t, syn)
  if (nrow(h)) for (k in split(h, coalesce(h$cert, ""))) add(tx, "dispersal_syndrome", paste(unique(k$value), collapse = " "), t, cert = k$cert[1])
}

# ---------------------------------------------------------------- photosynthetic pathway (also written inside other fields)
pp <- d %>% transmute(taxon_name, m = str_match(.data[[mn]], "Photosynthetic [Pp]athway\\.\\s*(C3|C4|CAM)\\b")[, 2]) %>% filter(!is.na(m))
for (i in seq_len(nrow(pp))) add(pp$taxon_name[i], "photosynthetic_pathway", str_to_lower(pp$m[i]), paste("Photosynthetic pathway", pp$m[i]))

# ---------------------------------------------------------------- years: time to first flowering, seedbank persistence
num_words <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9, ten = 10, twelve = 12)
years <- function(t) {
  t <- str_to_lower(t) %>% str_replace_all("\\b(one|two|three|four|five|six|seven|eight|nine|ten|twelve)\\b", function(z) as.character(num_words[z])) %>%
    str_replace_all("\\b(\\d+) months?\\b", function(z) as.character(as.numeric(word(z, 1)) / 12))
  m <- str_match(t, "(?:^|[^0-9.])(<|up to|at least|less than|more than)?\\s*([0-9.]+)\\s*(?:-|to)\\s*([0-9.]+)\\s*(?:years?|$)")
  if (!is.na(m[1])) return(c(min = as.numeric(m[3]), max = as.numeric(m[4]), vt = NA))
  m <- str_match(t, "(?:^|[^0-9.])(<|up to|at least|less than|more than|within)?\\s*([0-9.]+)\\s*(\\+)?\\s*(?:years?|year)")
  if (is.na(m[1])) return(NULL)
  v <- as.numeric(m[3])
  if (!is.na(m[4]) || coalesce(m[2], "") %in% c("at least", "more than")) return(c(min = v, max = NA, vt = "minimum"))
  if (coalesce(m[2], "") %in% c("<", "up to", "less than", "within")) return(c(min = NA, max = v, vt = "maximum"))
  c(min = NA, max = v, vt = "mean")
}
add_years <- function(tx, trait, t, desc, cert = NA_character_) {
  y <- years(t); if (is.null(y)) return(invisible())
  if (is.na(y[["vt"]])) { add(tx, trait, y[["min"]], desc, cert = cert, vt = "minimum"); add(tx, trait, y[["max"]], desc, cert = cert, vt = "maximum") } else
    add(tx, trait, if (y[["vt"]] == "minimum") y[["min"]] else y[["max"]], desc, cert = cert, vt = y[["vt"]])
}
for (i in which(fl$field == "Time to first flowering")) {
  t <- fl$text[i]
  # ("3 years from seed, 2 years from suckers", "Normally 10-12 years, under irrigation 4-5 years": the first, seedling, value)
  if (str_detect(t, "^(?:Resprouts|Within)")) next
  add_years(fl$taxon_name[i], "reproductive_maturity", str_split(t, ",|;")[[1]][1], t,
            cert = str_to_lower(str_match(t, regex("\\b(possibly|probably)\\b", ignore_case = TRUE))[, 2]))
}
sb_class <- c("^(?:soil, |possibly |probably )?short(?![- /]*(?:to )?medium)" = "transient", "short[- /]*(?:to )?medium|^(?:soil, )?medium(?![- /]*(?:to )?long)" = "short_persistent",
              "medium[- /]*(?:to )?long" = "short_persistent long_persistent", "^(?:soil, |canopy, )?long|long term" = "long_persistent")
for (i in which(fl$field == "Seedbank persistence")) {
  t <- fl$text[i]; tx <- fl$taxon_name[i]
  cert <- str_to_lower(str_match(t, regex("^(possibly|probably)\\b", ignore_case = TRUE))[, 2])
  # (statements about the genus, other species or vegetative organs, and no seed set, give no seedbank values)
  if (str_detect(t, regex("Rubus|closely related|some species|tubers|cormels|no seed set|no viable seed|rarely sets seed|little viable seed|rarely matures|viability low|unknown|re-shoot", ignore_case = TRUE))) next
  if (str_detect(t, regex("^canopy", ignore_case = TRUE))) {
    add(tx, "seedbank_location", "canopy_seedbank soil_seedbank", t); add(tx, "seedbank_longevity_class", "long_persistent", t, region = "canopy seedbank")
    add(tx, "seedbank_longevity_class", "transient", t, region = "soil seedbank"); next }
  if (str_detect(t, regex("^soil\\b", ignore_case = TRUE))) add(tx, "seedbank_location", "soil_seedbank", t)
  cl <- unname(sb_class[map_lgl(names(sb_class), ~ str_detect(t, regex(.x, ignore_case = TRUE)))])
  if (length(cl)) add(tx, "seedbank_longevity_class", cl[1], t, cert = cert)
  # (the years of a class word are the class's own definition, "Medium, 1-5 years": a number is scored only when no class word is given)
  if (!length(cl) && !str_detect(t, regex("dormancy|dormant|in laboratory|if buried|salt water|depending|declining|survive past|top 2cm", ignore_case = TRUE)))
    add_years(tx, "seedbank_longevity", t, t, cert = cert)
}

# ---------------------------------------------------------------- fire response, vegetative regeneration strategy (vetted by reading)
# fire: "Generally survives fire" / "Resprouts" -> resprouts ("tolerant of fire" = resprouts, skill rule), with the source's commonness
# word; "Killed by fire" -> fire_killed; hot-fire-kills / cool-fire-resprouts statements -> fire_killed + resprouts; germination of
# soil-stored seed after fire -> post_fire_recruitment + germination_cues fire; enhanced flowering after fire -> post_fire_flowering
for (i in which(fl$field == "Fire response")) {
  t <- fl$text[i]; tx <- fl$taxon_name[i]; tl <- str_to_lower(t)
  cert <- str_match(tl, "\\b(possibly|probably|likely)\\b")[, 2]
  if (str_detect(tl, "aquatic species|does not seem to affect|unknown|frequent and/or intense fires create|^suckers")) next
  killed <- str_detect(tl, "killed by fire|kills? (?:mature|adult|most adult|young)|will kill|generally kills|kill adult|kill mature|are usually killed|plants are generally killed|mature plants (?:likely )?killed|seeder \\(|older plants may be killed|established trees may be killed|kill most|hot fire kills|hot fire can kill|kill plants")
  sprouts <- str_detect(tl, "resprout|survives|re-?shoot|reprout|respout|new shoots|regrowth|recover|top-killed|not killed|rarely killed|sprout from|regenerate from roots|stimulates new growth|tolerant to fire")
  vq <- if (str_detect(tl, "^generally survives|generally resprout")) "usually" else if (str_detect(tl, "occasionally resprout|some resprout|some resprouting")) "occasionally" else NA_character_
  if (killed && sprouts) { add(tx, "resprouting_capacity", "fire_killed", t, cert = cert); add(tx, "resprouting_capacity", "resprouts", t, q = coalesce(vq, "sometimes"), cert = cert) } else
    if (killed) add(tx, "resprouting_capacity", "fire_killed", t, cert = cert) else if (sprouts) add(tx, "resprouting_capacity", "resprouts", t, q = vq, cert = cert)
  bb <- c("rhizomes?" = "rhizome", "lignotuber" = "basal_stem_buds", "epicormic" = "epicormic_buds", "rootstock|root-crown|root crowns?" = "root_crown",
          "root suckers" = "bud-bearing_root", "from (?:the )?base|basal dormant buds" = "basal_buds", "corms" = "fleshy_underground_organ")
  if (sprouts) { b <- unique(unname(bb[map_lgl(names(bb), ~ str_detect(tl, .x))])); if (length(b)) add(tx, "bud_bank_location", paste(b, collapse = " "), t, cert = cert) }
  if (str_detect(tl, "germinat|seedling|colonise burned|regenerates well from seed|increase in (?:abundance|cover)|significant increase in juveniles|increases in cover")) {
    add(tx, "post_fire_recruitment", "post_fire_recruitment", t, cert = cert); add(tx, "germination_cues", "fire", t, cert = cert) }
  if (str_detect(tl, "smoke")) add(tx, "germination_cues", "smoke", t)
  # (enhanced flowering only where the text says so: "Prolific flowering ... follow summer fire", "Fire appears to stimulate flowering";
  # not "Rapidly resprouts, flowers and sets seeds")
  if (str_detect(tl, "prolific flowering|stimulate (?:prolific )?flowering|flowers particularly well|mass flowering|encourages flowering")) add(tx, "post_fire_flowering", "fire_enhanced_flowering", t, cert = coalesce(cert, if (str_detect(tl, "appears to")) "apparently" else NA_character_))
}
vrs <- d %>% transmute(taxon_name, s = str_match_all(.data[[mn]], "Vegetative [Rr]egeneration [Ss]trategy\\.\\s*([^.|]*)")) %>%
  mutate(s = map(s, ~ .x[, 2])) %>% unnest(s) %>% mutate(s = str_squish(s)) %>% filter(s != "") %>% distinct()
for (i in seq_len(nrow(vrs))) {
  t <- vrs$s[i]; tx <- vrs$taxon_name[i]; tl <- str_to_lower(t)
  if (str_detect(tl, "does not resprout or produce root suckers")) { add(tx, "vegetative_reproduction_ability", "not_vegetative", t); next }
  cl <- c(if (str_detect(tl, "root suckers|suckers")) "root_buds", if (str_detect(tl, "stem layering")) "adventitious_roots", if (str_detect(tl, "broken stems")) "aboveground_clonal")
  if (length(cl) && !str_detect(tl, "does not produce root suckers")) { add(tx, "clonal_spread_mechanism", paste(cl, collapse = " "), t, q = if (str_detect(tl, "can produces? root suckers")) "sometimes" else NA_character_)
    add(tx, "vegetative_reproduction_ability", "vegetative", t) }
  # (the regeneration strategy is not about fire alone: resprouting after disturbance in general)
  if (str_detect(tl, "resprout|coppice")) add(tx, "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", t,
                                             q = if (str_detect(tl, "occasionally")) "occasionally" else if (str_detect(tl, "young plants")) "sometimes" else NA_character_)
}
# woody structure (wood porosity), stem type, toxicity: candidate traits, the source's words
# (traits.yml levels diffuse-porous, ring-porous, semi-ring-porous; "Non porous" and "Soft" have none and stay in value_description)
wp <- c("semi[ -]ring[ -]porous" = "semi-ring-porous", "(?<!semi[ -])ring[ -]porous" = "ring-porous", "diffuse[ -]porous" = "diffuse-porous")
for (i in which(fl$field == "Woody structure")) {
  v <- unname(wp[map_lgl(names(wp), ~ str_detect(str_to_lower(fl$text[i]), .x))])
  add(fl$taxon_name[i], "wood_porosity", if (length(v)) paste(v[order(map_int(names(wp)[wp %in% v], ~ str_locate(str_to_lower(fl$text[i]), .x)[1]))], collapse = " ") else NA, fl$text[i])
}
# (user 2026-11-05: "Type of stem. Succulent" -> plant_succulence succulent_stems; any other stem type stays a candidate)
for (i in which(fl$field == "Type of stem")) {
  st <- str_to_lower(str_squish(fl$text[i]))
  if (st == "succulent") add(fl$taxon_name[i], "plant_succulence", "succulent_stems", fl$text[i]) else add(fl$taxon_name[i], "stem_succulence", st, fl$text[i])
}
for (i in which(fl$field == "Toxicity")) add(fl$taxon_name[i], "plant_toxicity_description", NA, str_remove(fl$text[i], "\\.\\s*Vegetative regeneration strategy\\b.*$"))

# ---------------------------------------------------------------- calendars ("Aug(O)" = occasionally)
# (user 2026-11-05: Germination months -> recruitment_time, the months of seed germination and seedling establishment; Active Growth and
# Dormant months -> active_growing_time and dormancy_time, two traits because they are not always complements: 57 of 87 taxa)
cal_trait <- c(Flowering = "flowering_time", Fruiting = "fruiting_time", Germination = "recruitment_time", `Active Growth` = "active_growing_time", Dormant = "dormancy_time")
mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
cal <- d %>% transmute(taxon_name, c = .data[[paste(mn, "calendar")]]) %>% filter(!is.na(c))
for (i in seq_len(nrow(cal))) for (p in str_split(cal$c[i], "\\s*;\\s*")[[1]]) {
  m <- str_match(p, "^([A-Za-z ]+):\\s*(.*)$"); if (is.na(m[1]) || !m[2] %in% names(cal_trait)) next
  tok <- str_split(str_squish(m[3]), " ")[[1]]
  main <- match(str_remove(tok[!str_detect(tok, "\\(O\\)")], "\\(.*$"), mon); occ <- match(str_remove(tok[str_detect(tok, "\\(O\\)")], "\\(O\\)"), mon)
  yn <- function(ms) paste(ifelse(1:12 %in% ms, "y", "n"), collapse = "")
  if (length(na.omit(main))) add(cal$taxon_name[i], cal_trait[[m[2]]], yn(main), str_squish(p), q = if (length(na.omit(occ))) "usually" else NA_character_)
  if (length(na.omit(occ))) add(cal$taxon_name[i], cal_trait[[m[2]]], yn(occ), str_squish(p), q = "occasionally")
}

# ---------------------------------------------------------------- write (long format, the WAH_2026 columns)
out <- bind_rows(rows) %>%
  left_join(d %>% transmute(taxon_name, family, taxon_rank = case_when(str_detect(taxon_name, " subsp\\. ") ~ "subspecies", str_detect(taxon_name, " var\\. ") ~ "variety",
                                                                      str_detect(taxon_name, " f\\. ") ~ "form", str_detect(taxon_name, " ") ~ "species", TRUE ~ "genus"),
                            florabase_url = url), by = "taxon_name") %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--"))) %>%
  distinct() %>%
  # (a trait scored from two fields, Reproduction and the Vegetative regeneration strategy: one row, the values in order, both texts)
  group_by(taxon_name, family, taxon_rank, florabase_url, trait_name, value_type, commonness_qualifier, population_region, scoring_certainty) %>%
  summarise(value = if (all(is.na(value))) NA_character_ else paste(unique(unlist(str_split(na.omit(value), " "))), collapse = " "),
            value_description = paste(unique(value_description), collapse = " | "), .groups = "drop") %>%
  select(taxon_name, family, taxon_rank, florabase_url, trait_name, value, value_type, value_description, commonness_qualifier, population_region, scoring_certainty) %>%
  arrange(family, taxon_name, trait_name, value_type)
out <- out %>% select(where(~ !all(is.na(.x))))
write_csv(out, "data/WAH_2026_4/data.csv", na = "")
cat(nrow(out), "rows,", n_distinct(out$taxon_name), "taxa\n"); print(sort(table(out$trait_name), decreasing = TRUE))
