# Builds data/WildNet_2026/data.csv from the scraped Queensland WildNet species profiles
# (wildnet_plant_profiles_with_text.csv, austraits.build-data.scraping.scripts/data_extra).
# Not run by the build -- kept for provenance / re-extraction.
# Categorical traits are written as a pair of columns: `<trait>_description`
# (verbatim source sentence(s)) and `<trait>` (best-effort mapping to config/traits.yml levels),
# so the mapping can be revised later without re-reading the source text.
# Unlike formulaic fact sheets, these profiles are free prose that mix mm, cm and m, so each
# numeric column is converted to a single unit here (stated in the column comments below);
# the matching `_description` column keeps the source phrase with its original units.

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)

src <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra/wildnet_plant_profiles_with_text.csv"
out <- "data/WildNet_2026/data.csv"

d <- readr::read_csv(src, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
d[is.na(d)] <- ""
d <- d %>% mutate(across(everything(), ~ str_squish(str_replace_all(str_replace_all(.x, "[\u2013\u2014\u2212]", "-"), "\u00ac", " "))))

# source typos
d <- d %>% mutate(
  # "growing to less than 0-5m tall" -> 0.5 m
  Description = ifelse(scientific_name == "Acacia porcata", str_replace(Description, "less than 0-5m tall", "less than 0.5m tall"), Description),
  # missing spaces between number and word
  Description = str_replace_all(Description, "(?<=[a-z])(?=[0-9])(?<!\\b[a-z])", " "),
  Description = str_replace_all(Description, "\\b(to|with|of)(?=[0-9])", "\\1 "),
  Reproduction = str_replace_all(Reproduction, "\\bSep\\b", "September"),
  # stray space inside decimals / units: "17. 5 cm", "6.5c m"
  Description = str_replace_all(Description, "(?<=[0-9])\\. (?=[0-9]+\\s?(?:mm|cm|m)\\b)", "."),
  Description = str_replace_all(Description, "(?<=[0-9])c m\\b", "cm"),
  # Arthraxon hispidus: leaf-blade length and width are swapped ("2 to 6 cm wide and 0.7 to 1.5 cm long")
  Description = ifelse(scientific_name == "Arthraxon hispidus", str_replace(Description, "2 to 6 cm wide and 0.7 to 1.5 cm long", "2 to 6 cm long and 0.7 to 1.5 cm wide"), Description),
  # Cycas ophiolitica: leaflets "0.6- 0.75mm wide" are 6-7.5 mm (cm intended)
  Description = ifelse(scientific_name == "Cycas ophiolitica", str_replace(Description, "0.6- 0.75mm wide", "0.6-0.75cm wide"), Description),
  # Acacia jackesiana: pods "8 m wide" (mm intended)
  Description = ifelse(scientific_name == "Acacia jackesiana", str_replace(Description, "and 8 m wide", "and 8 mm wide"), Description),
  # Macrozamia longispina: seeds "15-2mm wide" (15-20 mm intended; seeds are 20-25 mm long)
  Description = ifelse(scientific_name == "Macrozamia longispina", str_replace(Description, "15-2mm wide", "15-20mm wide"), Description),
  # glosses on "phyllode" that would otherwise be parsed as a petiole ("enlarged part of the leaf stalk taking the form of a leaf")
  Description = str_remove_all(Description, "\\s*\\((?:the |an )?(?:enlarged )?(?:part|section) of the leaf stalk[^)]*\\)")
)

na_if_empty <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
collapse_unique <- function(x, sep = " ") {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

# sentence splitter that protects abbreviations ("A. argentina", "subsp. baueri", "c. 14 cm", "Mt. Larcom")
sentences <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  x <- str_replace_all(x, "\\b([A-Z])\\.(?=\\s)", "\\1\u00a7")
  x <- str_replace_all(x, "\\b(subsp|var|sp|spp|Mt|St|ca|c|approx|al|cf|vs|no|pers|comm|ed|eds|dbh|DBH)\\.(?=\\s)", "\\1\u00a7")
  s <- str_split(x, "(?<=[.;])\\s+(?=[A-Z(])")[[1]]
  s <- str_replace_all(s, "\u00a7", ".")
  str_squish(s[s != ""])
}
grab_sentences <- function(x, pattern, exclude = NULL) {
  s <- sentences(x)
  keep <- str_detect(s, regex(pattern, ignore_case = TRUE))
  if (!is.null(exclude)) keep <- keep & !str_detect(s, regex(exclude, ignore_case = TRUE))
  collapse_unique(s[keep], " ")
}

# sentences comparing the taxon with another species ("differs from A. ruppii", "similar to M. macleayi")
# are dropped before scoring morphology, so other taxa's characters are not attributed to this one
comparison <- "differ(s|ing)? (from|in)|distinguish(ed|es|able)? (from|by)|similar to|related to|allied to|unlike|resembles|affinities with|differing features|confused"
own_sentences_full <- function(desc, taxon) own_sentences(desc, taxon, split_clauses = FALSE)
own_sentences <- function(desc, taxon, split_clauses = TRUE) {
  s <- sentences(desc)
  if (split_clauses) s <- unlist(map(s, ~ str_split(.x, ";\\s+")[[1]]))
  own_abbrev <- paste0(str_sub(taxon, 1, 1), ". ", word(taxon, 2))
  other <- str_extract_all(s, "\\b[A-Z]\\. [a-z-]{3,}")
  keep <- !str_detect(s, regex(comparison, ignore_case = TRUE)) &
    map_lgl(other, ~ all(.x == own_abbrev))
  s[keep]
}

dash <- "\\s*(?:-|to)\\s*"
num <- "[0-9]+(?:\\.[0-9]+)?"
unit <- "(?:mm|cm|m|metres|meters)\\b"
unit_mult <- c(mm = 1, cm = 10, m = 1000, metres = 1000, meters = 1000)
to_mm <- function(v, u) ifelse(is.na(v), NA_real_, as.numeric(v) * unname(unit_mult[u]))

# drop parenthetical extremes "(3-)5 to 12 (-15) mm" and "(rarely 5)" so the typical range is parsed
strip_extremes <- function(x) {
  x <- str_replace_all(x, paste0("\\(\\s*", num, "\\s*-\\s*\\)\\s*"), "")
  x <- str_replace_all(x, paste0("\\s*\\(\\s*-\\s*", num, "\\s*\\)"), "")
  str_replace_all(x, "\\s*\\((?:rarely|occasionally|sometimes|usually|mostly)[^)]*\\)", "")
}

# a measurement: "(up to) 3(-)5 cm", "3 cm to 5 cm", "c. 14 cm"
lo_unit <- "(?:\\s?(mm|cm|m)(?=\\s*(?:-|to)\\s*[0-9]))?"
meas <- paste0("(?:(?:up to|to|about|approximately|approx\\.|c\\.|ca\\.|less than|under|around|reaching)\\s+)?(", num, ")", lo_unit, "(?:", dash, "(", num, "))?\\s?(mm|cm|m)\\b")
parse_meas <- function(m) {
  # m: one str_match row (full, lo, unit_lo, hi, unit)
  u <- m[5]; u1 <- ifelse(is.na(m[3]), u, m[3])
  lo <- to_mm(m[2], u1); hi <- to_mm(m[4], u)
  c(min = ifelse(is.na(hi), NA, lo), max = ifelse(is.na(hi), lo, hi))
}

# first "<organ> ... <length> long (and|by|,) <width> wide" in a set of sentences;
# `organ` is the subject noun, `other_organs` are nouns that, when named just before a number,
# mean the number belongs to them ("on stalks 5 mm long", "central axis 5 to 14 cm long")
organ_dims <- function(s, organ, other_organs, length_words = "long|in length",
                       width_words = "wide|broad|across|in width|in diameter|diameter|diam\\.", gap_forbid = NULL, subject = NULL) {
  res <- c(length_min = NA, length_max = NA, width_min = NA, width_max = NA)
  other_tail <- regex(paste0("\\b(?:", other_organs, ")\\b[^,;]{0,15}$"), ignore_case = TRUE)
  # gap_forbid: organs whose mention anywhere before the number means it is theirs (leaflets of a compound leaf)
  gap_ok <- function(g) !is.na(g) && !str_detect(g, other_tail) && (is.null(gap_forbid) || !str_detect(g, regex(gap_forbid, ignore_case = TRUE)))
  for (x in unique(s)) {
    x <- strip_extremes(x)
    # optionally only clauses whose grammatical subject is the organ itself
    if (!is.null(subject) && !str_detect(x, regex(subject, ignore_case = TRUE))) next
    hit <- str_locate(x, regex(paste0("\\b(?:", organ, ")\\b(?![-][a-z])"), ignore_case = TRUE))
    if (is.na(hit[1])) next
    rest <- str_sub(x, hit[2] + 1)
    # every measurement followed by a length word, a width word, or "a by b", in text order
    cands <- bind_rows(
      str_locate_all(rest, paste0(meas, "\\s*(?:by|x|\u00d7)\\s*", meas))[[1]] %>% as_tibble() %>% mutate(kind = "by"),
      str_locate_all(rest, paste0("(?<![0-9.])(", num, ")(?:", dash, "(", num, "))?\\s*(?:by|x|\u00d7)\\s*", meas))[[1]] %>% as_tibble() %>% mutate(kind = "by0"),
      str_locate_all(rest, paste0(meas, "\\s*(?:", length_words, ")\\b"))[[1]] %>% as_tibble() %>% mutate(kind = "L"),
      str_locate_all(rest, paste0(meas, "\\s*(?:", width_words, ")"))[[1]] %>% as_tibble() %>% mutate(kind = "W")
    ) %>% arrange(start, kind)
    if (!nrow(cands)) next
    length_rejected <- FALSE
    for (i in seq_len(nrow(cands))) {
      gap <- str_sub(rest, 1, cands$start[i] - 1)
      txt <- str_sub(rest, cands$start[i], cands$end[i])
      ok <- gap_ok(gap)
      if (cands$kind[i] %in% c("by", "by0", "L") && !ok) { length_rejected <- TRUE; next }
      if (!ok) next
      if (cands$kind[i] == "W" && length_rejected) next
      if (cands$kind[i] == "by") {
        m <- str_match(txt, paste0(meas, "\\s*(?:by|x|\u00d7)\\s*", meas))
        L <- parse_meas(m[1:5]); W <- parse_meas(c(m[1], m[6:9]))
        res <- c(length_min = L[["min"]], length_max = L[["max"]], width_min = W[["min"]], width_max = W[["max"]])
      } else if (cands$kind[i] == "by0") {
        # first dimension written without a unit takes the unit of the second ("0.9 to 1.2 by 0.7 to 1 mm")
        m <- str_match(txt, paste0("(", num, ")(?:", dash, "(", num, "))?\\s*(?:by|x|\u00d7)\\s*", meas))
        L <- parse_meas(c(m[1], m[2], NA, m[3], m[7])); W <- parse_meas(c(m[1], m[4:7]))
        res <- c(length_min = L[["min"]], length_max = L[["max"]], width_min = W[["min"]], width_max = W[["max"]])
      } else if (cands$kind[i] == "L") {
        L <- parse_meas(str_match(txt, meas)[1:5])
        res[c("length_min", "length_max")] <- L
        after <- str_sub(rest, cands$end[i] + 1)
        mW2 <- str_match(after, paste0("^([^.;]{0,25}?)", meas, "\\s*(?:", width_words, ")"))
        if (!is.na(mW2[1]) && gap_ok(mW2[2])) {
          res[c("width_min", "width_max")] <- parse_meas(c(mW2[1], mW2[3:6]))
          txt <- paste0(txt, mW2[1])
        }
      } else {
        res[c("width_min", "width_max")] <- parse_meas(str_match(txt, meas)[1:5])
      }
      return(list(values = res, phrase = str_squish(str_sub(x, hit[1], hit[2] + cands$start[i] - 1 + nchar(txt)))))
    }
  }
  list(values = res, phrase = NA_character_)
}

# ---------------------------------------------------------------- height
height_pats <- c(
  paste0("(", num, ")", lo_unit, "(?:", dash, "(", num, "))?\\s?(mm|cm|m)\\b(?:\\s*\\([^)]{0,40}\\))?\\s*(?:tall|high|in height)\\b"),
  paste0("(?:grow(?:s|ing)?|reach(?:es|ing)?|height of|is an? [a-z ,-]{0,40}?(?:shrub|tree|herb|grass|sedge|subshrub|palm|plant|perennial|mallee))\\s+(?:from\\s+)?(?:up\\s+)?(?:to\\s+)?(?:a height of\\s+)?(?:about\\s+|approximately\\s+)?(", num, ")", lo_unit, "(?:", dash, "(", num, "))?\\s?(mm|cm|m)\\b(?!\\s*(?:long|in length|wide|in diameter|diameter|across))")
)
extra_max_pat <- paste0("(?:rarely|occasionally|sometimes)\\s+(?:up\\s+)?to\\s+(", num, ")\\s?(mm|cm|m)\\b")
parse_height_in <- function(cand, prefix) {
  lo <- c(); hi <- c(); phr <- c(); used <- c()
  for (x in cand) {
    # "grows to about 35cm in length" / "about 3cm in length, but can reach 8cm" are stem lengths, not heights
    pats <- if (str_detect(x, "in length")) height_pats[1] else height_pats
    for (p in pats) {
      m <- str_match_all(x, p)[[1]]
      if (nrow(m) == 0) next
      for (i in seq_len(nrow(m))) {
        u <- m[i, 5]; u1 <- ifelse(is.na(m[i, 3]), u, m[i, 3])
        a <- to_mm(m[i, 2], u1) / 1000; b <- to_mm(m[i, 4], u) / 1000
        if (!is.na(b)) { lo <- c(lo, a); hi <- c(hi, b) } else hi <- c(hi, a)
        phr <- c(phr, m[i, 1]); used <- c(used, x)
      }
    }
    e <- str_match_all(x, extra_max_pat)[[1]]
    if (nrow(e)) { hi <- c(hi, to_mm(e[, 2], e[, 3]) / 1000); phr <- c(phr, e[, 1]); used <- c(used, x) }
  }
  out <- tibble(min = if (length(lo)) min(lo) else NA_real_, max = if (length(hi)) max(hi) else NA_real_,
                description = collapse_unique(phr, "; "))
  names(out) <- paste0(prefix, c("_min", "_max", "_description"))
  out
}
parse_height <- function(s) {
  # plant-level sentences only: the first, plus early sentences about the plant as a whole;
  # sentences about the scape (flowering stem) are kept apart as a reproductive height
  idx <- seq_along(s)
  scape <- s[idx <= 4 & str_detect(s, "^(The scapes?|Scapes?) ")]
  cand <- s[idx == 1 | (idx <= 3 & str_detect(s, "^(It |The plant|Plants |This |The tree)"))]
  bind_cols(parse_height_in(cand, "height"), parse_height_in(scape, "scape_height"))
}

# stem / trunk diameter (DBH where stated)
parse_stem_diam <- function(s, is_fern) {
  # fern "stems" are rhizomes; not a stem diameter
  if (is_fern) return(tibble(stem_diam_min = NA_real_, stem_diam_max = NA_real_, stem_diam_description = NA_character_, dbh = NA))
  s <- s[seq_len(min(3, length(s)))]
  for (x in s) {
    m <- str_match(x, paste0("\\b(?:trunks?|stems?|underground stem|dbh|DBH)\\b(?![- ]like)([^.;]{0,60}?)", meas, "\\s*(?:in\\s+)?(?:diameter|diam\\.|DBH|dbh|thick)"))
    m2 <- str_match(x, paste0("(?:diameter|dbh|DBH) of\\s+", meas))
    m3 <- str_match(x, paste0("(?:tall|high)\\s+and\\s+", meas, "\\s*in diameter"))
    m4 <- str_match(x, paste0(meas, "\\s*(?:DBH|dbh)"))
    if (!is.na(m[1]) && !str_detect(m[2], "branch|root|leaf|leaves|frond|pinna|bulb")) {
      v <- parse_meas(c(m[1], m[3:6])); return(tibble(stem_diam_min = v[["min"]], stem_diam_max = v[["max"]], stem_diam_description = m[1], dbh = str_detect(x, regex("breast height|dbh|chest height", ignore_case = TRUE))))
    }
    for (mm in list(m2, m3, m4)) if (!is.na(mm[1])) {
      v <- parse_meas(c(mm[1], mm[2:5])); return(tibble(stem_diam_min = v[["min"]], stem_diam_max = v[["max"]], stem_diam_description = mm[1], dbh = str_detect(x, regex("breast height|dbh|chest height", ignore_case = TRUE))))
    }
  }
  tibble(stem_diam_min = NA_real_, stem_diam_max = NA_real_, stem_diam_description = NA_character_, dbh = NA)
}

# ---------------------------------------------------------------- months
month_re <- "\\b(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|June?|July?|Aug(?:ust)?|Sept?(?:ember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\b"
season_re <- "\\b((?:early|mid|mid-|late)[ -]?)?(spring|summer|autumn|winter)\\b"
season_months <- list(spring = 9:11, summer = c(12, 1, 2), autumn = 3:5, winter = 6:8)
month_index <- function(m) match(str_sub(str_to_lower(m), 1, 3), str_to_lower(month.abb))
cyc <- function(a, b) if (a <= b) a:b else c(a:12, 1:b)
year_round <- "throughout the year|all year|year[- ]round|any time of (?:the )?year|all months"

months_from_text <- function(x) {
  if (is.na(x) || x == "") return(integer(0))
  if (str_detect(x, regex(year_round, ignore_case = TRUE))) return(1:12)
  # "late November to early January": early/mid/late on a month name does not change the month
  x <- str_replace_all(x, paste0("\\b(?:early|mid|late)[- ]?(?=", str_remove(month_re, "^\\\\b"), ")"), "")
  toks <- bind_rows(
    str_locate_all(x, month_re)[[1]] %>% as_tibble() %>%
      mutate(txt = str_sub(x, start, end), a = month_index(txt), b = a),
    str_locate_all(x, regex(season_re, ignore_case = TRUE))[[1]] %>% as_tibble() %>%
      mutate(txt = str_to_lower(str_sub(x, start, end)),
             s = str_extract(txt, "spring|summer|autumn|winter"),
             mod = str_extract(txt, "early|mid|late"),
             a = map2_int(s, mod, ~ { m <- season_months[[.x]]; as.integer(if (is.na(.y)) m[1] else if (.y == "early") m[1] else if (.y == "mid") m[2] else m[3]) }),
             b = map2_int(s, mod, ~ { m <- season_months[[.x]]; as.integer(if (is.na(.y)) m[3] else if (.y == "early") m[1] else if (.y == "mid") m[2] else m[3]) })) %>%
      select(start, end, txt, a, b)
  ) %>% arrange(start)
  if (!nrow(toks)) return(integer(0))
  out <- integer(0); i <- 1
  connector <- "^\\s*\\)?\\s*(?:-|to|through to|through|until|into|till|,? (?:rarely|occasionally|sometimes) (?:extending )?(?:to|into|through to))\\s*\\(?\\s*$"
  while (i <= nrow(toks)) {
    a <- toks$a[i]; b <- toks$b[i]
    while (i < nrow(toks)) {
      gap <- str_sub(x, toks$end[i] + 1, toks$start[i + 1] - 1)
      before <- str_sub(x, max(1, toks$start[i] - 9), toks$start[i] - 1)
      is_range <- str_detect(gap, regex(connector, ignore_case = TRUE)) ||
        (str_detect(gap, "^\\s*and\\s*$") && str_detect(str_sub(x, max(1, toks$start[i] - 25), toks$start[i] - 1), regex("between (?:the months of )?$", ignore_case = TRUE)))
      if (!is_range) break
      i <- i + 1; b <- toks$b[i]
    }
    out <- c(out, cyc(a, b)); i <- i + 1
  }
  sort(unique(out))
}
yn <- function(m) if (!length(m)) NA_character_ else paste(ifelse(1:12 %in% m, "y", "n"), collapse = "")

# split Reproduction sentences into flowering vs fruiting clauses; "flowers and fruits" clauses count for both
# a hedge voids the rest of its sentence ("It is probably similar to A. baueri, flowering ...")
hedge <- "(?:,\\s*)?(?:\\b(?:and|but|although)\\s+)?\\b(?:possibly|probably|likely|almost certainly|it may also|may also|thought to be|is thought)\\b.*$"
kw_both <- "\\b(?:flower(?:s|ing)?,? (?:and |, )?(?:fruit(?:s|ing)?|seeds?)|flowers, fruit and seed|flowers and fruit)\\b"
kw_flower <- "\\b(?:flower(?:s|ing|ed)?|in flower|anthesis)\\b"
kw_bud <- "\\b(?:flower buds?|buds?)\\b"
kw_fruit <- "\\b(?:fruit(?:s|ing|ed)?|pods?|seed ?pods?|seeding|seeds (?=from|in )|seeds? (?:ripen|become ripe|are ripe)|ripe seeds?|mature seeds?|capsules?)\\b"
phenology_clauses <- function(rep) {
  fl <- c(); fr <- c(); fl_s <- c(); fr_s <- c()
  for (s in sentences(rep)) {
    s2 <- str_remove_all(s, regex(hedge, ignore_case = TRUE))
    hits <- bind_rows(
      str_locate_all(s2, regex(kw_both, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "both"),
      str_locate_all(s2, regex(kw_bud, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "bud"),
      str_locate_all(s2, regex(kw_flower, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "flower"),
      str_locate_all(s2, regex(kw_fruit, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "fruit")
    ) %>% arrange(start, desc(end))
    if (!nrow(hits)) next
    # drop hits nested inside an earlier, longer hit ("flowers and fruits" contains "flowers" and "fruits")
    keep <- rep(TRUE, nrow(hits)); last_end <- 0
    for (i in seq_len(nrow(hits))) { if (hits$start[i] <= last_end) keep[i] <- FALSE else last_end <- hits$end[i] }
    hits <- hits[keep, ]
    for (i in seq_len(nrow(hits))) {
      seg_end <- if (i < nrow(hits)) hits$start[i + 1] - 1 else nchar(s2)
      seg <- str_sub(s2, hits$start[i], seg_end)
      # months written before the first keyword ("In March, flowers ...") belong to it
      if (i == 1) seg <- paste(str_sub(s2, 1, hits$start[1] - 1), seg)
      m <- months_from_text(seg)
      if (!length(m)) next
      if (hits$type[i] %in% c("flower", "both")) { fl <- c(fl, m); fl_s <- c(fl_s, s) }
      if (hits$type[i] %in% c("fruit", "both")) { fr <- c(fr, m); fr_s <- c(fr_s, s) }
    }
  }
  tibble(flowering_time = yn(sort(unique(fl))), flowering_time_description = collapse_unique(fl_s),
         fruiting_time = yn(sort(unique(fr))), fruiting_time_description = collapse_unique(fr_s))
}

# ---------------------------------------------------------------- colours
colour_words <- "white|whitish|cream|creamy|ivory|yellow|yellowish|lemon|golden|gold|orange|apricot|straw|red|reddish|brown|brownish|maroon|bronze|crimson|rufous|fawn|rust|russet|copper|coppery|tan|scarlet|burgundy|chestnut|pink|pinkish|rose|salmon|magenta|blue|bluish|purple|purplish|mauve|lilac|violet|lavender|indigo|plum|green|greenish|black|blackish|grey|greyish|gray|silver|silvery"
colour_mod <- "pale|light|dark|deep|bright|dull|rich|dirty|bluish|greenish|purplish|reddish|yellowish|brownish|pinkish|creamy|whitish|blackish|greyish|golden|glossy|shiny"
cword <- paste0("(?:(?:", colour_mod, ")[- ])?(?:", colour_words, ")(?:[- ](?:", colour_words, "))*")
cphrase <- paste0(cword, "(?:\\s*(?:,|to|or|and|and/or|fading to|ageing|becoming)\\s+", cword, ")*")

flower_colour_map <- c(
  white = "white_cream", whitish = "white_cream", cream = "white_cream", creamy = "white_cream", ivory = "white_cream",
  yellow = "yellow_orange", yellowish = "yellow_orange", lemon = "yellow_orange", golden = "yellow_orange",
  gold = "yellow_orange", orange = "yellow_orange", apricot = "yellow_orange", straw = "yellow_orange",
  red = "red_brown", reddish = "red_brown", brown = "red_brown", brownish = "red_brown", maroon = "red_brown",
  bronze = "red_brown", crimson = "red_brown", rufous = "red_brown", fawn = "red_brown", rust = "red_brown",
  russet = "red_brown", copper = "red_brown", coppery = "red_brown", tan = "red_brown", scarlet = "red_brown",
  burgundy = "red_brown", chestnut = "red_brown",
  pink = "pink", pinkish = "pink", rose = "pink", salmon = "pink", magenta = "pink",
  blue = "blue_purple", bluish = "blue_purple", purple = "blue_purple", purplish = "blue_purple", mauve = "blue_purple",
  lilac = "blue_purple", violet = "blue_purple", lavender = "blue_purple", indigo = "blue_purple", plum = "blue_purple",
  green = "green", greenish = "green", black = "black", blackish = "black", grey = "grey", greyish = "grey", gray = "grey",
  silver = "grey", silvery = "grey")
fruit_colour_map <- c(
  white = "white", whitish = "white", cream = "cream", creamy = "cream", ivory = "cream",
  yellow = "yellow", yellowish = "yellow", lemon = "yellow", golden = "yellow", gold = "yellow", straw = "yellow",
  orange = "orange", apricot = "orange",
  red = "red", reddish = "red", crimson = "red", scarlet = "red", maroon = "red", burgundy = "red",
  brown = "brown", brownish = "brown", bronze = "brown", rufous = "brown", fawn = "brown", rust = "brown",
  russet = "brown", copper = "brown", coppery = "brown", tan = "brown", chestnut = "brown",
  pink = "pink", pinkish = "pink", rose = "pink", salmon = "pink", magenta = "pink",
  blue = "blue", bluish = "blue", purple = "purple", purplish = "purple", mauve = "purple", lilac = "purple",
  violet = "purple", lavender = "purple", indigo = "purple", plum = "purple",
  green = "green", greenish = "green", black = "black", blackish = "black", grey = "grey", greyish = "grey",
  gray = "grey", silver = "grey", silvery = "grey")

map_colours <- function(phrase, cmap) {
  if (is.na(phrase) || phrase == "") return(NA_character_)
  p <- str_to_lower(phrase)
  p <- str_remove_all(p, "\\b(?:fading|ageing|aging|becoming|turning|drying) to [a-z -]+")
  # for hyphenated compounds (greenish-yellow, creamy-white, yellow-brown) keep only the head colour
  p <- str_replace_all(p, "\\b[a-z]+-(?=[a-z]+\\b)", "")
  # colour modifiers that are themselves colour words ("golden yellow", "bluish green") are not separate colours
  p <- str_remove_all(p, paste0("\\b(?:", colour_mod, ") (?=(?:", colour_words, ")\\b)"))
  words <- str_extract_all(p, "[a-z]+")[[1]]
  collapse_unique(unname(cmap[words[words %in% names(cmap)]]))
}

# colour phrase tied to an organ noun: "white moderately conspicuous flowers" or "flowers ... white to cream in colour"
organ_colour <- function(s, organ, stop_organs, window = 60) {
  for (x in s) {
    # citations ("(White, 1936)") are not colours
    lx <- str_to_lower(str_remove_all(x, "\\(?[A-Z][A-Za-z]+(?: (?:and|&) [A-Z][a-z]+| et al\\.?)?,? (?:\\d{4}|n\\.d\\.)[a-z]?\\)?"))
    a <- str_match(lx, paste0("(?<![a-z-])(", cphrase, ")(?:,?\\s+(?!and\\b|the\\b|with\\b|or\\b|of\\b|to\\b)[a-z-]+){0,3}?,?\\s+(?:", organ, ")\\b"))
    b <- str_match(lx, paste0("\\b(?:", organ, ")\\b((?:[^.;]|\\.(?=[0-9])){0,", window, "}?)(?<![a-z-])(", cphrase, ")\\b"))
    ok_b <- !is.na(b[1]) && !str_detect(b[2], paste0("\\b(?:", stop_organs, ")\\b"))
    # a colour directly qualifying another organ ("release numerous fine white seeds") is not this organ's
    if (ok_b) {
      after <- str_sub(lx, str_locate(lx, fixed(b[1]))[2] + 1, str_locate(lx, fixed(b[1]))[2] + 30)
      if (str_detect(after, paste0("^\\s*(?:[a-z-]+\\s+){0,1}(?:", stop_organs, ")\\b"))) ok_b <- FALSE
    }
    pa <- if (!is.na(a[1])) str_locate(lx, fixed(a[1]))[1] else Inf
    pb <- if (ok_b) str_locate(lx, fixed(b[1]))[1] else Inf
    if (is.infinite(pa) && is.infinite(pb)) next
    phrase <- if (pa <= pb) a[2] else b[3]
    phrase <- str_remove(phrase, "\\s*(?:,|to|or|and|and/or)\\s*$")
    return(c(phrase = phrase, sentence = x))
  }
  c(phrase = NA_character_, sentence = NA_character_)
}

# ---------------------------------------------------------------- categorical vocabularies
leaf_shape_map <- c(
  "narrowly linear" = "narrowly_linear", "linear" = "linear", "narrowly lanceolate" = "narrowly_lanceolate",
  "lanceolate" = "lanceolate", "lance-shaped" = "lanceolate", "narrowly oblanceolate" = "narrowly_oblanceolate",
  "oblanceolate" = "oblanceolate", "narrowly elliptic" = "narrowly_elliptical", "narrowly elliptical" = "narrowly_elliptical",
  "broadly elliptic" = "widely_elliptical", "broadly elliptical" = "widely_elliptical", "elliptic" = "elliptical",
  "elliptical" = "elliptical", "oval" = "elliptical", "narrowly ovate" = "narrowly_ovate", "ovate" = "ovate",
  "egg-shaped" = "ovate", "narrowly obovate" = "narrowly_obovate", "broadly obovate" = "widely_obovate",
  "obovate" = "obovate", "narrowly oblong" = "narrowly_oblong", "oblong" = "oblong", "orbicular" = "orbicular",
  "circular" = "orbicular", "cordate" = "cordate", "heart-shaped" = "cordate", "reniform" = "reniform",
  "kidney-shaped" = "reniform", "terete" = "terete", "cylindrical" = "terete", "filiform" = "filiform",
  "thread-like" = "filiform", "falcate" = "falcate", "sickle-shaped" = "falcate", "spathulate" = "spathulate",
  "spatulate" = "spathulate", "subulate" = "subulate", "acicular" = "acicular", "needle-like" = "acicular",
  "strap-shaped" = "strap-shaped", "strap-like" = "strap-shaped", "peltate" = "peltate", "rhomboidal" = "rhomboidal",
  "diamond-shaped" = "rhomboidal", "deltate" = "deltate", "triangular" = "triangular", "obtriangular" = "obtriangular",
  "trullate" = "trullate", "ensiform" = "ensiform", "setaceous" = "setaceous")
map_by_dictionary <- function(p, dict) {
  if (is.na(p)) return(NA_character_)
  p <- str_to_lower(p)
  hits <- c()
  for (k in names(dict)[order(-nchar(names(dict)))]) {
    if (str_detect(p, paste0("(?<![a-z-])", k, "(?![a-z])"))) { hits <- c(hits, dict[[k]]); p <- str_replace_all(p, paste0("(?<![a-z-])", k, "(?![a-z])"), " ") }
  }
  collapse_unique(hits)
}

leaf_subject <- "^(?:The |Its |Each |Mature |Adult |All |Most |These |Both )?(?:[a-z-]+,? ){0,4}(?:leaves|leaf|lamina|laminae|laminas|leaf blades?|blades?|phyllodes?|fronds?|frond blade)\\b"
not_leaf_subject <- "^(?:The |Its |Each )?(?:[a-z-]+ ){0,3}(?:leaflets?|leaf[- ]?stalks?|leaf sheaths?|leaf scars?|leaf bases|seedling|juvenile|juvenille|young|floating|cataphylls?|sporophylls?|leafy internodes)"
leaf_other <- "axis|apex|tips?|petioles?|stalks?|sheaths?|ligules?|leaflets?|pinnae|pinna|segments?|scales?|stipules?|bracts?|spines?|prickles?|cataphylls?|rh?achis|teeth|hairs?|glands?|veins?|midribs?|flowers?|inflorescences?|lobes?|internodes|crowns?|pinnacanths|trunk"

extract_one <- function(r) {
  taxon <- r[["scientific_name"]]
  fam <- r[["family"]]
  desc <- r[["Description"]]; beh <- r[["Behaviour"]]; rep <- r[["Reproduction"]]
  hab <- r[["Habitat"]]; thr <- r[["Threatening process"]]
  all_txt <- paste(desc, beh, rep, hab, thr)
  s_all <- sentences(desc)
  s <- own_sentences(desc, taxon)
  s1 <- if (length(s_all)) s_all[1] else ""
  # growth-form sentence(s): the first, plus "It is a ..." / "<Genus> species are ..." follow-ups
  gf_s <- c(s1, s_all[seq_along(s_all) %in% 2:3 & str_detect(s_all, paste0("^(It is|It has|This (species|plant) is|", word(taxon, 1), " species are)"))])
  gf_txt <- str_to_lower(paste(gf_s, collapse = " "))

  # ---- growth form: scored from the "is a ... <noun>" phrase(s) only, so host trees, kangaroo grass
  # or "tree-like" elsewhere in the sentence are not picked up
  phrase_re <- regex("\\b(?:is|are) (?:a |an |the |one )?.*?(?=\\s(?:growing|grows|that|which|with|to [0-9]|[0-9]|up to|from [0-9])\\b|[.;]|$)", ignore_case = TRUE)
  gf_phr <- c(str_extract(s1, phrase_re),
              unlist(map(s_all[seq_along(s_all) %in% 2:3], ~ if (str_detect(.x, paste0("^(It is an? |", word(taxon, 1), " species are )"))) str_extract(.x, phrase_re) else NULL)))
  gf_phr <- gf_phr[!is.na(gf_phr)]
  # keep only phrases that name a growth form ("is yet to be formerly described" is not one)
  gf_noun <- "tree|shrub|sub-?shrub|herb|grass|sedge|vine|climber|liana|palm|cycad|fern|clubmoss|mallee|orchid|aquatic|tussock|plant|perennial|epiphyte"
  gf_phr <- gf_phr[str_detect(str_to_lower(gf_phr), paste0("\\b(?:", gf_noun, ")"))]
  gf_desc <- collapse_unique(str_squish(gf_phr), " | ")
  g <- str_to_lower(paste(gf_phr, collapse = " "))
  is_lyco <- fam %in% c("Lycopodiaceae", "Selaginellaceae")
  # each form is located in the phrase so values follow the text order ("shrub or tree" -> "shrub tree");
  # matched words are blanked (same length) so "tree fern" / "sub-shrub" are not matched again as tree / shrub
  form_rules <- tribble(
    ~pattern,                                                                 ~value,
    "tree fern",                                                              "fern palmoid",
    "banana tree",                                                            "palmoid",
    "\\bmallee\\b",                                                           "mallee",
    "\\btree\\b(?![- ](?:like|family|hosts?))",                                "tree",
    "\\bsub-?shrubs?\\b",                                                     "subshrub",
    "\\bshrubs?\\b",                                                          "shrub",
    "\\bherbs?\\b|aquatic (?:perennial )?plant|freshwater herb|\\bherbaceous perennial\\b|\\borchids?\\b", "herb",
    "\\bgrass(?:es)?\\b|grass[- ]like|\\bsedge\\b",                             "graminoid",
    "tussock",                                                                "tussock",
    "woody (?:vine|climber)|canopy vine|woody liana",                         "climber_woody",
    "herbaceous (?:vine|climber)",                                            "climber_herbaceous",
    "\\b(?:vine|climber|liana)\\b",                                             "climber",
    "\\bpalm\\b",                                                             "palmoid",
    "\\bcycad\\b",                                                            "palmoid",
    "\\bfern\\b|clubmoss",                                                    if (is_lyco) "lycophyte" else "fern"
  )
  # the qualifier must govern the form itself ("or rarely a small tree"), not an adjective ("rarely dioecious shrub")
  qualifier_re <- "(?:\\bor|,|\\bis|\\bare)\\s+(rarely|occasionally|sometimes|usually|mostly|generally|often|commonly|typically|mainly|normally)\\s+(?:as |becoming )?(?:an? )?(?:(?:small|tall|large|low|medium|medium-sized|slender|straggling) )?$"
  gm <- g; hits <- tibble(pos = integer(0), value = character(0), qualifier = character(0))
  for (i in seq_len(nrow(form_rules))) {
    loc <- str_locate(gm, form_rules$pattern[i])
    if (is.na(loc[1])) next
    q <- str_match(str_sub(g, max(1, loc[1] - 30), loc[1] - 1), qualifier_re)[, 2]
    hits <- add_row(hits, pos = loc[1], value = form_rules$value[i], qualifier = q)
    str_sub(gm, loc[1], loc[2]) <- strrep(" ", loc[2] - loc[1] + 1)
  }
  # orchids named only outside the "is a" phrase ("one of a group of Dendrobium species ... the Cooktown orchid")
  if (!"herb" %in% hits$value && str_detect(gf_txt, "\\borchids?\\b")) hits <- add_row(hits, pos = 1e6L, value = "herb", qualifier = NA)
  # "tufted" / "tufts" on a grass, sedge or grass-like plant is a tussock
  if ("graminoid" %in% hits$value && !"tussock" %in% hits$value && str_detect(gf_txt, "\\btuft(?:ed|s)?\\b")) {
    tp <- str_locate(g, "\\btuft(?:ed|s)?\\b")[1]
    hits <- add_row(hits, pos = if (is.na(tp)) hits$pos[hits$value == "graminoid"] + 1L else tp, value = "tussock", qualifier = NA)
  }
  # a generic "climber" adds nothing when a woody / herbaceous climber is already named ("canopy vine | woody climber")
  if (any(hits$value %in% c("climber_woody", "climber_herbaceous"))) hits <- hits %>% filter(value != "climber")
  hits <- hits %>% arrange(pos) %>% distinct(value, .keep_all = TRUE)
  gf <- hits$value
  # "tree or rarely a shrub": the qualified form keeps its qualifier, the unqualified alternative is "usually"
  gf_qualified <- if (any(!is.na(hits$qualifier))) paste0(hits$value, "=", coalesce(hits$qualifier, "usually"), collapse = ";") else NA_character_

  # ---- habit words
  habit_map <- c("erect" = "erect", "prostrate" = "prostrate", "prostate" = "prostrate", "sprawling" = "sprawling",
                 "spreading" = "spreading", "tufted" = "tufted", "creeping" = "creeping", "mat-forming" = "mat-forming",
                 "forms floating mats" = "floating mat-forming", "decumbent" = "decumbent", "pendulous" = "pendulous",
                 "slender" = "slender", "dense" = "dense", "open" = "open", "rhizomatous" = "rhizomatous",
                 "stoloniferous" = "stoloniferous", "submerged" = "submerged", "floating" = "floating", "trailing" = "prostrate")
  habit <- map_by_dictionary(paste(g, if (str_detect(s1, "forms floating mats")) "forms floating mats"), habit_map)
  branching_map <- c("single or multi-stemmed" = "single_basal_stem multi-stemmed", "multi-stemmed" = "multi-stemmed",
                     "single stemmed" = "single_basal_stem", "single-stemmed" = "single_basal_stem",
                     "solitary trunk" = "single_basal_stem", "many-branched" = "much-branched", "much branched" = "much-branched",
                     "extensively branched" = "much-branched", "extensive branching" = "much-branched",
                     "sparsely branched" = "sparsely-branched", "intricately branched" = "intricately-branched",
                     "densely branched" = "densely-branched", "lightly branched" = "sparsely-branched")
  branching <- map_by_dictionary(paste(c(gf_phr, s1), collapse = " "), branching_map)
  woody <- case_when(str_detect(gf_txt, "weakly woody") ~ "semi_woody",
                     str_detect(gf_txt, "woody base") ~ "woody_base",
                     str_detect(gf_txt, "\\bwoody\\b(?! (?:roots?|rhizomes?|culms?))") ~ "woody",
                     str_detect(gf_txt, "\\bherbaceous\\b") ~ "herbaceous", TRUE ~ NA_character_)
  woody_desc <- if (is.na(woody)) NA_character_ else str_extract(paste(gf_s, collapse = " "), regex("[^ ]*\\s?(?:weakly )?(?:woody|herbaceous)\\s?[^ ,.;]*", ignore_case = TRUE))

  # ---- life history
  life_txt <- paste(paste(gf_s, collapse = " "), beh, rep)
  life_txt <- str_remove_all(life_txt, regex("once considered an annual", ignore_case = TRUE))
  life <- collapse_unique(c(
    if (str_detect(life_txt, regex("\\bperennial\\b(?! (?:streams?|creeks?|water|pools?|springs?|swamps?|wetlands?|lagoons?|rivers?))", ignore_case = TRUE))) "perennial",
    if (str_detect(life_txt, regex("\\b(?:an annual|annual (?:herb|grass|plant|species))\\b", ignore_case = TRUE))) "annual"))
  life_desc <- grab_sentences(paste(paste(gf_s, collapse = " "), beh, rep), "\\bperennial\\b|\\ban annual\\b|annual (herb|grass|plant|species)|short-lived|life ?span|live for|survive (for )?at least|lives for",
                              exclude = "perennial (streams?|creeks?|water|pools?|springs?)")

  # ---- leaf phenology (whole-plant statements only, not "deciduous bracts/stipules")
  phen_s <- grab_sentences(paste(paste(gf_s, collapse = " "), beh, rep, desc), "\\b(?:semi-)?deciduous\\b|\\bevergreen\\b", exclude = "stipules|bracts|bracteoles|vine thicket|vine forest|vineforest|vinethicket|rainforest|communit")
  phen_core <- str_to_lower(paste(gf_txt, str_extract_all(phen_s %||% "", regex("plants are deciduous", ignore_case = TRUE))[[1]], collapse = " "))
  phen <- collapse_unique(c(if (str_detect(phen_core, "semi-deciduous")) "semi_deciduous",
                            if (str_detect(str_remove_all(phen_core, "semi-deciduous"), "\\bdeciduous\\b")) "deciduous",
                            if (str_detect(phen_core, "\\bevergreen\\b")) "evergreen"))

  # ---- substrate
  sub_txt <- paste(paste(gf_s, collapse = " "), hab)
  substrate <- collapse_unique(c(
    if (str_detect(sub_txt, regex("hemi-epiphyt", ignore_case = TRUE))) "hemiepiphyte",
    if (str_detect(str_remove_all(sub_txt, regex("hemi-epiphyt\\w*|other epiphytes|with other epiphytes", ignore_case = TRUE)), regex("\\bepiphyt|grows on the (?:scaly )?bark|on the (?:upper )?branches of|branches of (?:host )?trees|on trees|on hoop pines", ignore_case = TRUE))) "epiphyte",
    if (str_detect(str_remove_all(sub_txt, regex("lithophytic vegetation", ignore_case = TRUE)), regex("lithophyt|grows? (?:only )?on rocks|on rocks|on trees and rocks|rock faces|on boulders|cliff faces|on granite boulders", ignore_case = TRUE)) &
        !str_detect(taxon, "^(Crepidomanes)")) "lithophyte",
    if (str_detect(gf_txt, "\\bterrestrial\\b|ground orchid|grows on the ground")) "terrestrial",
    if (str_detect(gf_txt, "rooted, submerged|rooted to the substrate|floating or rooted")) "aquatic_rooted",
    if (str_detect(gf_txt, "floating mats|floating or rooted")) "aquatic_floating",
    if (str_detect(gf_txt, "\\baquatic\\b") & !str_detect(gf_txt, "rooted|floating")) "aquatic"))
  substrate_desc <- grab_sentences(paste(paste(gf_s, collapse = " "), hab), "epiphyt|lithophyt|on rocks|rock faces|boulders|cliff faces|on trees|bark of|branches of|terrestrial|aquatic|submerged|floating|on the ground|hoop pines")

  # ---- height and stem diameter
  ht <- parse_height(s_all)
  is_fern <- fam %in% c("Aspleniaceae", "Dryopteridaceae", "Thelypteridaceae", "Hymenophyllaceae", "Polypodiaceae", "Lycopodiaceae", "Selaginellaceae", "Cyatheaceae")
  sd <- parse_stem_diam(s_all, is_fern)

  # ---- leaves
  leaf_s <- s[str_detect(s, regex(leaf_subject, ignore_case = TRUE)) & !str_detect(s, regex(not_leaf_subject, ignore_case = TRUE))]
  # leaf measurements that sit in the growth-form sentence ("with flat leaves up to 95 cm long", "crown of 1-4 erect leaves 65-95 cm long")
  # then whole (unsplit) sentences, then any other clause whose subject is not another organ
  s_full <- own_sentences_full(desc, taxon)
  other_subject <- regex("^(?:The |Its |Each |A |An )?(?:[a-z-]+,? ){0,3}(?:stipules?|petioles?|bracts?|bracteoles?|scales?|flowers?|sepals?|calyx)\\b", ignore_case = TRUE)
  leaf_dim_s <- c(leaf_s, s_full[str_detect(s_full, regex(leaf_subject, ignore_case = TRUE)) & !str_detect(s_full, regex(not_leaf_subject, ignore_case = TRUE))],
                  s[!s %in% leaf_s & !str_detect(s, regex(not_leaf_subject, ignore_case = TRUE)) & !str_detect(s, other_subject)])
  ld <- organ_dims(leaf_dim_s, "leaves|leaf|lamina|laminae|laminas|leaf blades?|blades?|phyllodes?|fronds?", leaf_other, gap_forbid = "\\bleaflets?\\b|\\bpinnae\\b|\\bpinnules?\\b")
  lfd <- organ_dims(s, "leaflets?", "petiolules?|stalks?|rh?achis|teeth|hairs?|glands?|veins?|midribs?|flowers?|lobes?|margins?")
  ptd <- organ_dims(s, "petioles?|leaf[- ]stalks?|stalks \\(petioles\\)", "leaflets?|blades?|lamina|pinnae|segments?|flowers?|peduncles?|pedicels?|rh?achis|ligules?", width_words = "(?!x)x")
  leaf_txt <- paste(leaf_s, collapse = " ")
  leaf_shape_txt <- str_remove_all(leaf_txt, regex("[a-z-]+ (?:in|at the) (?:cross[- ]section|base|apex|tip)|(?:base|apex|tip|margins?|bases) (?:is|are) [a-z ,-]+|(?:cuneate|attenuate|rounded|acute|obtuse|acuminate|truncate|cordate|oblique) (?:at|towards?) (?:the )?(?:base|apex)|triangular in cross-section|(?:straight,? )?acicular,? prickles|subulate apex|stipules[^.;]*", ignore_case = TRUE))
  leaf_shape <- map_by_dictionary(leaf_shape_txt, leaf_shape_map)
  margin_txt <- paste(c(leaf_s[str_detect(leaf_s, regex("margin|toothed|serrat|dentate|crenat|entire", ignore_case = TRUE))],
                        s[str_detect(s, regex("^(?:The )?(?:leaf |lamina )?margins", ignore_case = TRUE))]), collapse = " ")
  margin <- collapse_unique(c(
    if (str_detect(margin_txt, regex("\\bentire\\b|margins? (?:are )?(?:flat|smooth)\\b(?! and)", ignore_case = TRUE)) & str_detect(margin_txt, regex("\\bentire\\b", ignore_case = TRUE))) "entire",
    if (str_detect(margin_txt, regex("dentate|denticulate", ignore_case = TRUE))) "toothed_dentate",
    if (str_detect(margin_txt, regex("serrat|serrulate", ignore_case = TRUE))) "toothed_serrate",
    if (str_detect(margin_txt, regex("crenat|crenulate", ignore_case = TRUE))) "toothed_crenate",
    if (str_detect(margin_txt, regex("\\btoothed\\b|\\bteeth\\b", ignore_case = TRUE)) & !str_detect(margin_txt, regex("lacking teeth|without teeth", ignore_case = TRUE))) "toothed"))
  # generic "toothed" only when no specific kind of toothing is named
  if (!is.na(margin) && str_detect(margin, "toothed_")) margin <- str_squish(str_remove(margin, "\\btoothed\\b(?!_)"))
  compound_txt <- paste(c(leaf_s, s[str_detect(s, regex("^(?:The |Its )?(?:adult |mature )?leaves (?:are|is) (?:compound|pinnate|bipinnate|simple|trifoliolate|trifoliate)|leaflets", ignore_case = TRUE)) & !str_detect(s, regex(not_leaf_subject, ignore_case = TRUE))],
                          s[seq_along(s) <= 2]), collapse = " ")
  compound_txt <- str_remove_all(compound_txt, regex("pinnately (?:veined|nerved)|pinnate venation|simple (?:hairs|trichomes)|simple,? (?:uncoloured )?trichomes|simple straight|simple curved|lateral branches simple|predominately simple", ignore_case = TRUE))
  compound <- collapse_unique(c(
    if (str_detect(compound_txt, regex("\\bsimple\\b|in one piece", ignore_case = TRUE))) "simple",
    if (str_detect(compound_txt, regex("\\b(?:bi|tri|[0-9]-)?pinnate\\b|trifoliate|trifoliolate|\\bcompound\\b|leaflets|\\bpinnae\\b|composed of [0-9-]+ leaflets|palmately|unifoliolate", ignore_case = TRUE))) "compound"))
  compound_desc <- collapse_unique(str_extract_all(compound_txt, regex("[^.;]*\\b(?:simple|in one piece|pinnate|bipinnate|trifoliate|trifoliolate|compound|leaflets|pinnae|palmately)\\b[^.;]*", ignore_case = TRUE))[[1]] %>% str_squish() %>% str_remove("[;,]$") %>% unique() %>% head(2), " | ")
  phyllo_txt <- str_remove_all(leaf_txt, regex("leaf-opposed|leaflets?[^.;]*", ignore_case = TRUE))
  phyllotaxis <- collapse_unique(c(if (str_detect(phyllo_txt, regex("\\balternate(?:ly)?\\b", ignore_case = TRUE))) "alternate",
                                   if (str_detect(phyllo_txt, regex("\\bopposite\\b|\\bsubopposite\\b|in pairs", ignore_case = TRUE))) "opposite",
                                   if (str_detect(phyllo_txt, regex("\\bwhorl", ignore_case = TRUE))) "whorled"))
  arrangement <- collapse_unique(c(if (str_detect(paste(leaf_txt, gf_txt), regex("spirally arranged|arranged spirally", ignore_case = TRUE))) "spiral",
                                   if (str_detect(paste(leaf_txt, gf_txt), regex("\\brosette\\b", ignore_case = TRUE))) "rosette",
                                   if (str_detect(paste(leaf_txt, gf_txt), regex("cluster of leaves at its base|leaves mainly occur at the base", ignore_case = TRUE))) "clustered_basal",
                                   if (str_detect(leaf_txt, regex("\\bcrowded\\b", ignore_case = TRUE))) "crowded"))
  glaucous <- if (str_detect(leaf_txt, regex("(?<!not )\\bglaucous\\b", ignore_case = TRUE))) "glaucous" else NA_character_
  phyllode <- if (str_detect(desc, regex("\\bphyllodes?\\b", ignore_case = TRUE))) "phyllode" else NA_character_

  # ---- reduced leaves / photosynthetic organ
  reduced_desc <- grab_sentences(desc, "leaves (are )?reduced to|apparently leafless|leafless|photosynthetic roots")
  leaf_len_type <- case_when(str_detect(desc, regex("leaves are reduced to (?:a )?(?:narrow )?(?:cylindrical )?sheath", ignore_case = TRUE)) ~ "reduced_to_sheath",
                             str_detect(desc, regex("leaves are reduced to [^.]*scales", ignore_case = TRUE)) ~ "scale_leaves",
                             str_detect(desc, regex("apparently leafless", ignore_case = TRUE)) ~ "leafless",
                             TRUE ~ NA_character_)
  photo_organ <- case_when(str_detect(desc, regex("photosynthetic roots", ignore_case = TRUE)) ~ "root",
                           leaf_len_type %in% c("reduced_to_sheath", "scale_leaves") ~ "stem",
                           !is.na(phyllode) ~ phyllode, TRUE ~ NA_character_)

  # ---- flowers
  flower_nouns <- "flowers|flower|petals|corolla|perianth|tepals|florets|ray florets|flower heads|heads|spikes|inflorescences"
  stop_organs <- "sepals?|calyx|bracts?|bracteoles?|anthers?|stamens?|filaments?|styles?|ovary|disc|eye|throat|stripes?|markings?|spots?|veins?|buds?|hairs?|glands?|pedicels?|peduncles?|stalks?|column|labellum|lip|tongue|hood|appendages|base|tips?|leaves|fruits?|pods?|seeds?"
  fc <- organ_colour(s, flower_nouns, stop_organs)
  # flower size only when nothing but descriptive words sits between "flower(s)" and the number
  # (pedicels, petals, spikes, clusters etc. named in between mean the number is theirs)
  flower_not <- "petals?|sepals?|calyx|corolla|tubes?|pedicels?|stalks?|bracts?|stamens?|anthers?|filaments?|styles?|ovary|lobes?|hypanthium|spurs?|labellum|lip|column|disc|peduncles?|bracteoles?|racemes?|inflorescences?|leaves|clusters?|spikes?|heads?|panicles?|umbels?|cymes?|grouped|borne|produced|arranged|occur|clumps?|branch(?:es)?|shoots?|perianth|tepals?|parts|on|stems?|bundles?|consists?|capsules?|fruits?|seeds?|pods?"
  fdim <- organ_dims(s, "flowers?(?! (?:heads?|stalks?|buds?|stems?|clusters?|spikes?))", flower_not, length_words = "long", width_words = "in diameter|diameter|across|wide",
                     gap_forbid = paste0("\\b(?:", flower_not, ")\\b"),
                     subject = "^(?:The |Each |Its |Individual |Male |Female |Mature |Single )?(?:(?!have\\b|has\\b|with\\b|of\\b)[a-z-]+,? ){0,2}flowers?\\b(?! (?:heads?|stalks?|buds?|stems?|clusters?|spikes?))")
  scent_s <- grab_sentences(desc, "(?<![A-Za-z-])(?:fragran|perfume|scented|sweet smelling|odour resembling)", exclude = "leaves|leaflets|bark|crushed|foliage|cones")
  scent <- if (is.na(scent_s)) NA_character_ else "scent_produced"

  # ---- fruit and seed
  fruit_nouns <- "fruits?|pods?|capsules?|berr(?:y|ies)|drupes?|follicles?|legumes?|achenes?|nuts?|cypselas?|mericarps?|samaras?|nutlets?"
  fruit_other <- "petioles?|stalks?|pedicels?|wings?|calyx|calyx lobes|stipes?|arils?|caruncles?|hairs?|bristles|spines?|scales?|appendages?|elaiosomes?|pappus|beaks?|valves?|seeds?|segments?|lobes?|flowers?|bracts?|strophiole"
  fruit_s <- s[str_detect(s, regex(paste0("\\b(?:", fruit_nouns, ")\\b"), ignore_case = TRUE)) &
                 !str_detect(s, regex("fruiting calyx|have not been seen|undescribed|fruiting period|fruits? (?:have|has) been recorded", ignore_case = TRUE))]
  frd <- organ_dims(fruit_s, fruit_nouns, fruit_other, width_words = "wide|broad|across|in width|in diameter|diameter",
                    gap_forbid = "\\bwings?\\b",
                    subject = paste0("^(?:The |Each |Its |Mature |Ripe |Young )?(?:(?!have\\b|has\\b|with\\b|of\\b|in\\b)[a-z-]+,? ){0,3}(?:", fruit_nouns, "|cypselas?|fruit capsules)\\b"))
  seed_s <- s[str_detect(s, regex("\\bseeds?\\b", ignore_case = TRUE)) & !str_detect(s, regex("seed pods?|seed[- ]bearing|have not been seen", ignore_case = TRUE))]
  sdd <- organ_dims(seed_s, "seeds?(?! pods?)", "arils?|caruncles?|elaiosomes?|appendages?|wings?|hairs?|strophiole|pappus|fruits?|pods?|sporophylls?|cones?",
                    gap_forbid = "\\b(?:fruits?|pods?|capsules?|cones?|sporophylls?|arils?)\\b",
                    subject = "^(?:The |Each |Its |Mature |Ripe )?(?:(?!have\\b|has\\b|with\\b|of\\b|in\\b)[a-z-]+,? ){0,2}seeds?\\b")
  ftype_map <- c("berry" = "berry", "berries" = "berry", "drupe" = "drupe", "drupes" = "drupe", "drupaceous" = "drupe",
                 "capsule" = "capsule", "capsules" = "capsule", "capsular" = "capsule", "follicle" = "follicle",
                 "follicles" = "follicle", "achene" = "achene", "achenes" = "achene", "nutlet" = "nutlet", "nutlets" = "nutlet",
                 "nut" = "nut", "samara" = "samara", "schizocarp" = "schizocarp", "mericarps" = "schizocarp",
                 "caryopsis" = "caryopsis", "legume" = "legume", "pod" = "legume", "pods" = "legume")
  ftype_txt <- paste(fruit_s, collapse = " ")
  if (fam != "Leguminosae") ftype_txt <- str_remove_all(ftype_txt, regex("\\bpods?\\b", ignore_case = TRUE))
  fruit_type <- map_by_dictionary(ftype_txt, ftype_map)
  fruit_ctx <- paste(c(fruit_s, sentences(beh)[str_detect(sentences(beh), regex("fruit|pod|capsule", ignore_case = TRUE))],
                       sentences(rep)[str_detect(sentences(rep), regex("fruit|pod|capsule", ignore_case = TRUE))]), collapse = " ")
  fleshy <- collapse_unique(c(if (str_detect(fruit_ctx, regex("\\bfleshy\\b|\\bsucculent\\b", ignore_case = TRUE))) "fleshy",
                              if (str_detect(fruit_ctx, regex("(?<!when )\\bdry\\b(?! season)|\\bwoody fruits?\\b|fruits? (?:is|are) woody", ignore_case = TRUE))) "dry"))
  dehisc <- collapse_unique(c(if (str_detect(fruit_ctx, regex("(?<!in)dehiscent|split(?:s|ting)? (?:open|lengthways)|splitting when ripe|opens? naturally|open spontaneously", ignore_case = TRUE))) "dehiscent",
                              if (str_detect(fruit_ctx, regex("\\bindehiscent\\b", ignore_case = TRUE))) "indehiscent"))
  fruit_col <- organ_colour(fruit_s, "fruits?|pods?|capsules?|berr(?:y|ies)|drupes?|follicles?", "seeds?|calyx|wings?|arils?|hairs?|bristles|valves?|lobes?|flowers?|stalks?|pedicels?|margins?", window = 90)
  seed_col <- organ_colour(seed_s[str_detect(seed_s, regex("^(?:The |Each |Its |Mature |Ripe )?(?:(?!have\\b|has\\b|with\\b|of\\b|in\\b)[a-z-]+,? ){0,2}seeds?\\b", ignore_case = TRUE))], "seeds?", "arils?|caruncles?|elaiosomes?|strophiole|hairs?|fruits?|pods?|capsules?|wings?|testa", window = 80)
  appendage_txt <- paste(seed_s, fruit_s, collapse = " ")
  appendage <- collapse_unique(c(if (str_detect(appendage_txt, regex("\\barils?\\b|\\barillate", ignore_case = TRUE))) "aril",
                                 if (str_detect(appendage_txt, regex("elaiosome", ignore_case = TRUE))) "elaiosome",
                                 if (str_detect(appendage_txt, regex("\\bpappus\\b", ignore_case = TRUE))) "pappus",
                                 if (str_detect(appendage_txt, regex("winged seeds?|seeds? (?:is|are) winged", ignore_case = TRUE))) "wings"))
  appendage_desc <- grab_sentences(paste(desc, rep, beh), "\\barils?\\b|arillate|elaiosome|\\bpappus\\b|winged seeds?|seeds? (?:is|are) winged")

  # ---- sex type
  sex_txt <- paste(desc, rep, beh)
  sex <- collapse_unique(c(
    if (str_detect(sex_txt, regex("\\bmonoecious\\b|male and female (?:flowers|cones) (?:occur )?on the same (?:plant|tree)|separate male and female flowers on the same plant", ignore_case = TRUE))) "monoecious",
    if (str_detect(sex_txt, regex("\\bdioecious\\b|male and female [a-z ]*?(?:develop|are produced|occur|grow)? ?on separate (?:plants|trees|individuals)", ignore_case = TRUE))) "dioecious",
    if (str_detect(sex_txt, regex("andromonoecious", ignore_case = TRUE))) "andromonoecious",
    if (str_detect(sex_txt, regex("\\b\\d+ (?:or \\d+ )?bisexual flowers\\b", ignore_case = TRUE)) & !str_detect(sex_txt, regex("male|female", ignore_case = TRUE))) "hermaphrodite"))
  sex_desc <- grab_sentences(sex_txt, "monoecious|dioecious|male and female|bisexual|unisexual|hermaphrodit|andromonoecious|female florets|female or hermaphrodite|bisexual or male")
  if (!is.na(sex) && str_detect(sex, "dioecious") && str_detect(sex, "monoecious") && !str_detect(sex_txt, regex("monoecious or (?:rarely )?dioecious", ignore_case = TRUE))) sex <- sex # both stated separately; checked in QA

  # ---- storage organs and roots
  st_txt <- paste(desc, rep, beh)
  storage <- collapse_unique(c(
    if (str_detect(st_txt, regex("pseudobulb", ignore_case = TRUE))) "pseudobulb",
    if (str_detect(st_txt, regex("lignotuber", ignore_case = TRUE))) "lignotuber",
    if (str_detect(st_txt, regex("\\bcaudex\\b", ignore_case = TRUE))) "caudex",
    if (str_detect(st_txt, regex("tuberous roots|tuber-like roots|roots are tuberous|tuber roots|tuber \\(swollen root\\)", ignore_case = TRUE))) "root_tuber",
    if (str_detect(str_remove_all(st_txt, regex("tuberous roots|tuber-like roots|roots are tuberous|tuber roots|tuber \\(swollen root\\)|tuberous bases|tubercul\\w*|tuberous herb", ignore_case = TRUE)), regex("\\btubers?\\b|\\btuberous\\b", ignore_case = TRUE)) |
        str_detect(st_txt, regex("tuberous herb", ignore_case = TRUE))) "tuber",
    if (!is_fern && !str_detect(taxon, "^Bulbophyllum") && str_detect(st_txt, regex("woody rhizomes?", ignore_case = TRUE))) "rhizome_woody",
    if (!is_fern && !str_detect(taxon, "^Bulbophyllum") && str_detect(st_txt, regex("rhizome[^.]{0,40}fleshy", ignore_case = TRUE))) "rhizome_fleshy",
    if (!is_fern && !str_detect(taxon, "^Bulbophyllum") && !str_detect(st_txt, regex("woody rhizomes?|rhizome[^.]{0,40}fleshy", ignore_case = TRUE)) &&
        str_detect(st_txt, regex("\\brhizomes?\\b|\\brhizomatous\\b", ignore_case = TRUE))) "rhizome"))
  storage_desc <- grab_sentences(st_txt, "pseudobulb|lignotuber|caudex|\\btubers?\\b|tuberous|tuber-like|rhizom", exclude = "tubercul")
  sto <- organ_dims(s, "tubers?|pseudobulbs?|tuber-like roots|rhizome|bulb-like structure", "leaves|leaf|flowers?|hairs?|scales?|roots(?! )|bracts?|stems?")
  sto_p <- str_to_lower(sto$phrase %||% "")
  storage_entity <- case_when(str_detect(sto_p, "pseudobulb") ~ "pseudobulb", str_detect(sto_p, "tuber-like roots") ~ "root_tuber",
                              str_detect(sto_p, "tuber") ~ "tuber", str_detect(sto_p, "rhizome") ~ "rhizome", TRUE ~ NA_character_)
  # dimensions are only kept for an organ that is itself scored (not Phaius' "bulb-like structure", fern rhizomes)
  if (is.na(storage_entity) || is.na(storage) || (storage_entity == "rhizome" && !str_detect(storage, "rhizome"))) { sto$values[] <- NA; sto$phrase <- NA_character_; storage_entity <- NA_character_ }
  # "tuberous" describing tuberous roots is the root tuber, not a second organ
  if (!is.na(storage) && str_detect(storage, "root_tuber")) storage <- str_squish(str_remove(storage, "(?<!root_)\\btuber\\b"))
  root_type <- collapse_unique(c(
    if (str_detect(desc, regex("fleshy taproot", ignore_case = TRUE))) "taproot_fleshy",
    if (str_detect(desc, regex("thick taproot|stout taproot", ignore_case = TRUE))) "taproot_stout",
    if (str_detect(desc, regex("roots are fibrous", ignore_case = TRUE))) "fibrous_roots",
    if (str_detect(desc, regex("fleshy roots", ignore_case = TRUE))) "fleshy_roots",
    if (str_detect(paste(desc, rep), regex("adventitious roots", ignore_case = TRUE))) "adventitious_roots"))
  root_desc <- grab_sentences(paste(desc, rep), "taproot|roots are fibrous|fleshy roots|adventitious roots")
  climbing <- collapse_unique(c(if (str_detect(desc, regex("\\btwining\\b", ignore_case = TRUE))) "twining",
                                if (str_detect(desc, regex("attached to rocks or soils by adventitious roots", ignore_case = TRUE))) "adventitious_roots"))
  climbing_desc <- grab_sentences(desc, "twining|by adventitious roots")
  alt_strategy <- if (str_detect(gf_txt, "saprophytic")) "saprophyte" else NA_character_
  parasite_desc <- grab_sentences(desc, "parasitic")

  # ---- habitat
  hab_s <- sentences(hab)
  soil_words <- "sandy-clay|sandy clay|clay-loam|clay loam|clayey sand|sandy loam|sandy|sand|clayey|clay|loamy|loam|gravelly|gravel|lateritic|laterite|granitic|granite|basalt|basaltic|sandstone|rhyolite|rhyolitic|volcanic|serpentinite|serpentine|krasnozem|euchrozem|alluvial|colluvial|limestone|ironstone|quartzite|skeletal|stony|rocky|shallow|deep|red|black|brown|grey|yellow|white|peaty|peat|humus|silty|heavy|cracking|acidic|infertile|well-drained|free-draining|poorly drained|siliceous|metamorphic|mudstone|andesitic|greywacke"
  soil_phr <- str_extract_all(str_to_lower(hab), "(?:[a-z/-]+(?:,| and| or| to)? ){0,6}(?:soils?|sands?|loams?|clays?|krasnozem|substrates?)\\b")[[1]]
  soil_t <- unlist(str_extract_all(soil_phr, paste0("\\b(", soil_words, ")\\b")))
  soil_t <- str_replace_all(soil_t, c("^sand$" = "sandy", "^clayey$" = "clay", "^laterite$" = "lateritic", "^peat$" = "peaty",
                                      "^loam$" = "loamy", "^gravel$" = "gravelly", "^basaltic$" = "basalt", "^granitic$" = "granite",
                                      "^rhyolitic$" = "rhyolite", "^serpentine$" = "serpentinite"))
  elev <- str_match(hab, paste0("(?:altitudes?|elevations?)[^.;]{0,40}?(", num, ")\\s?(?:m)?\\s*(?:-|to|and)\\s*(", num, ")\\s?m\\b"))

  # ---- ecology: verbatim sentences only; the values are scored in the vetted tribble below
  eco_txt <- paste(desc, beh, rep, hab, thr)
  eco <- tibble(
    pollination_description = grab_sentences(eco_txt, "pollinat|honeyeater|nectivorous|weevil|thrips|feeding at the flowers|self-fertilis"),
    dispersal_description = grab_sentences(eco_txt, "dispers|drift seed|seeds? (?:drop|is shed|are shed)|wind-dispersed|enable dispersal"),
    fire_response_description = grab_sentences(eco_txt, "resprout|re-sprout|sprouts? |re-shoot|coppice|lignotuber|obligate seeder|killed (?:outright )?by fire|kill(?:s)? (?:individuals|the above|small seedlings|juveniles)|fire[- ]sensitive|fire tolerant|resistant to most fires|survive fire|regenerat\\w* (?:after|from)|germinat\\w* after fire|promoted by fire|fire promotes|fire-triggered|requires fire|suckers following damage", exclude = "weed species|Lantana"),
    seedbank_description = grab_sentences(eco_txt, "seed ?bank|viable in the soil|stored in the soil|seed is held on the tree|in the soil at the time"),
    germination_description = grab_sentences(eco_txt, "germinat|dormancy|scarif|heat application"),
    vegetative_reproduction_description = grab_sentences(eco_txt, "vegetative|sucker|spreads? (?:mainly )?by|stolons? (?:form|enable)|live-bearing|proliferous|new plants are produced|ring of plantlets|creeping underground root"),
    lifespan_description = grab_sentences(eco_txt, "life ?span|lives? for|live for|survive for|short-lived plant|reproductive maturity|juvenile period|first seeds"),
    physical_defence_description = grab_sentences(desc, "spines?\\b|spinescent|prickl|thorns?", exclude = "sporophyll|cone|megasporophyll|similar to|differs"),
    flower_lifespan_description = grab_sentences(paste(desc, beh, rep), "flowers? (?:lasting|last|remained open)|last only one or two days|open for")
  )

  bind_cols(
    tibble(taxon_name = taxon, family = fam, common_name = na_if_empty(r[["accepted_common_name"]]),
           wildnet_taxon_id = r[["taxon_id"]], wildnet_url = r[["url"]],
           wildnet_name_superseded = r[["superseded"]], nca_status = na_if_empty(r[["nca_status"]]),
           epbc_status = na_if_empty(r[["epbc_status"]]),
           plant_growth_form_description = na_if_empty(gf_desc), plant_growth_form = collapse_unique(gf), growth_form_qualified = gf_qualified,
           stem_growth_habit = habit, stem_branching_form = branching,
           woodiness_description = na_if_empty(woody_desc), woodiness_detailed = woody,
           life_history_description = life_desc, life_history = life,
           leaf_phenology_description = phen_s, leaf_phenology = phen,
           plant_growth_substrate_description = substrate_desc, plant_growth_substrate = substrate,
           plant_alternative_energy_and_nutrient_acquisition_strategy = alt_strategy,
           parasitic_description = parasite_desc),
    ht, sd,
    tibble(leaf_description = collapse_unique(leaf_s),
           leaf_dimensions_description = ld$phrase,
           leaf_length_min = ld$values[["length_min"]], leaf_length_max = ld$values[["length_max"]],
           leaf_width_min = ld$values[["width_min"]], leaf_width_max = ld$values[["width_max"]],
           leaflet_dimensions_description = lfd$phrase,
           leaflet_length_min = lfd$values[["length_min"]], leaflet_length_max = lfd$values[["length_max"]],
           leaflet_width_min = lfd$values[["width_min"]], leaflet_width_max = lfd$values[["width_max"]],
           petiole_length_description = ptd$phrase,
           petiole_length_min = ptd$values[["length_min"]], petiole_length_max = ptd$values[["length_max"]],
           leaf_shape = leaf_shape, leaf_margin = margin,
           leaf_compoundness_description = compound_desc, leaf_compoundness = compound,
           leaf_phyllotaxis = phyllotaxis, leaf_arrangement = arrangement, leaf_glaucousness = glaucous,
           leaf_length_type_description = reduced_desc, leaf_length_type = leaf_len_type,
           plant_photosynthetic_organ = photo_organ,
           flower_colour_description = na_if_empty(fc[["phrase"]]), flower_colour = map_colours(fc[["phrase"]], flower_colour_map),
           flower_colour_sentence = na_if_empty(fc[["sentence"]]),
           flower_dimensions_description = fdim$phrase,
           flower_length_min = fdim$values[["length_min"]], flower_length_max = fdim$values[["length_max"]],
           flower_diameter_min = fdim$values[["width_min"]], flower_diameter_max = fdim$values[["width_max"]],
           flower_scent_description = scent_s, flower_scent_production = scent,
           sex_type_description = sex_desc, sex_type = sex,
           fruit_description = collapse_unique(fruit_s),
           fruit_dimensions_description = frd$phrase,
           fruit_length_min = frd$values[["length_min"]], fruit_length_max = frd$values[["length_max"]],
           fruit_width_min = frd$values[["width_min"]], fruit_width_max = frd$values[["width_max"]],
           fruit_type = fruit_type, fruit_fleshiness = fleshy, fruit_dehiscence = dehisc,
           fruit_colour_description = na_if_empty(fruit_col[["phrase"]]), fruit_colour = map_colours(fruit_col[["phrase"]], fruit_colour_map),
           seed_dimensions_description = sdd$phrase,
           seed_length_min = sdd$values[["length_min"]], seed_length_max = sdd$values[["length_max"]],
           seed_width_min = sdd$values[["width_min"]], seed_width_max = sdd$values[["width_max"]],
           seed_colour_description = na_if_empty(seed_col[["phrase"]]), seed_colour = map_colours(seed_col[["phrase"]], flower_colour_map),
           dispersal_appendage_description = appendage_desc, dispersal_appendage = appendage,
           storage_organ_description = storage_desc, storage_organ = storage,
           storage_organ_dimensions_description = sto$phrase, storage_entity = storage_entity,
           storage_organ_length_min = sto$values[["length_min"]], storage_organ_length_max = sto$values[["length_max"]],
           storage_organ_diameter_min = sto$values[["width_min"]], storage_organ_diameter_max = sto$values[["width_max"]],
           root_system_type_description = root_desc, root_system_type = root_type,
           plant_climbing_mechanism_description = climbing_desc, plant_climbing_mechanism = climbing),
    phenology_clauses(rep),
    eco,
    tibble(habitat_description = na_if_empty(hab), distribution_description = na_if_empty(r[["Distribution"]]),
           soil_terms = collapse_unique(soil_t, "; "),
           elevation_min_m = as.numeric(elev[2]), elevation_max_m = as.numeric(elev[3]))
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

wide <- map_dfr(seq_len(nrow(d)), ~ extract_one(d[.x, ]))
if (interactive() || Sys.getenv("WILDNET_QA") != "") saveRDS(wide, Sys.getenv("WILDNET_QA"))

# ---------------------------------------------------------------- explicit taxon fixes
yn_months <- function(m) paste(ifelse(1:12 %in% m, "y", "n"), collapse = "")
wide <- wide %>% mutate(
  # Acacia sp. (Castletower): "Green unripe fruit and remains of flower spikes ... in October" -- fruit, not flowering
  flowering_time = ifelse(taxon_name == "Acacia sp. (Castletower N.Gibson TOI345)", NA, flowering_time),
  fruiting_time = ifelse(taxon_name == "Acacia sp. (Castletower N.Gibson TOI345)", yn_months(10), fruiting_time),
  # Arytera dictyoneura: "Flowering and plants with mature fruit have been observed ... in December and February" (both)
  flowering_time = ifelse(taxon_name == "Arytera dictyoneura", yn_months(c(2, 12)), flowering_time),
  # Cadellia pentastylis: flowers mainly October-December, "occasionally flowering extends through to early April"
  flowering_time = ifelse(taxon_name == "Cadellia pentastylis", yn_months(c(10:12, 1:4)), flowering_time),
  # cycads and Callitris bear cones, not fruit: seed-ripening / "fruiting cone" months are left in the description only
  fruiting_time = ifelse(family %in% c("Zamiaceae", "Cycadaceae", "Cupressaceae"), NA, fruiting_time),
  # Chamaecrista maritima: "leaflets are attached to a 2-4 cm long rachis" was read as leaflet length
  leaflet_length_min = ifelse(taxon_name == "Chamaecrista maritima", 3.3, leaflet_length_min),
  leaflet_length_max = ifelse(taxon_name == "Chamaecrista maritima", 6.6, leaflet_length_max),
  leaflet_width_min = ifelse(taxon_name == "Chamaecrista maritima", 1, leaflet_width_min),
  leaflet_width_max = ifelse(taxon_name == "Chamaecrista maritima", 1.6, leaflet_width_max),
  leaflet_dimensions_description = ifelse(taxon_name == "Chamaecrista maritima", "leaflets are oblong, sometimes overlapping, 3.3-6.6mm long, 1-1.6mm wide", leaflet_dimensions_description),
  # "flowers ... 20 cm long" (Dansiea elliptica, Dioclea hexandra) is not credible for a single flower; left unscored
  flower_length_min = ifelse(taxon_name %in% c("Dansiea elliptica", "Dioclea hexandra"), NA, flower_length_min),
  flower_length_max = ifelse(taxon_name %in% c("Dansiea elliptica", "Dioclea hexandra"), NA, flower_length_max)
)

# ---------------------------------------------------------------- vetted ecological scores
# Pollination, dispersal, fire response, seed bank, clonality, defence and lifespan statements are free prose
# (often hedged), so values are scored per taxon here from the sentences kept in the matching *_description
# columns. Hedged statements ("thought to be", "probably", "appear to be") go to pollination_vector_possible
# or are left unscored; genus-wide unhedged statements printed in a species' own profile are scored.
eco_scores <- tribble(
  ~taxon_name,                      ~trait,                                      ~value,
  # pollination
  "Bowenia serrulata",              "pollination_vector_possible",               "beetle",                 # "appear to be pollinated by ... Miltotranes weevil"
  "Bowenia spectabilis",            "pollination_vector_possible",               "beetle",
  "Bulbophyllum globuliforme",      "pollination_vector_possible",               "fly",                    # genus: "commonly pollinated by flies"
  "Bulbophyllum weinthalii",        "pollination_vector_known",                  "fly",                    # "Pollination is by blowflies"
  "Bertya opponens",                "pollination_vector_possible",               "wind",                   # "speculated ... wind pollinated"
  "Corchorus cunninghamii",         "pollination_vector_known",                  "bee honeybee stingless_bee wasp",
  "Corchorus cunninghamii",         "pollination_vector_possible",               "ant",                    # "and possibly ants"
  "Cycas cairnsiana",               "pollination_vector_possible",               "beetle",                 # "thought to be effected by small beetles"
  "Cycas candida",                  "pollination_vector_known",                  "beetle",
  "Cycas couttsiana",               "pollination_vector_known",                  "beetle",
  "Cycas desolata",                 "pollination_vector_known",                  "beetle",
  "Cycas megacarpa",                "pollination_vector_known",                  "beetle",
  "Cycas ophiolitica",              "pollination_vector_known",                  "beetle",
  "Cycas platyphylla",              "pollination_vector_known",                  "beetle",
  "Cycas semota",                   "pollination_vector_known",                  "beetle",
  "Gastrodia crebriflora",          "pollination_syndrome",                      "self",                   # "flowers which are self-pollinating"
  "Gossia gonoclada",               "pollination_vector_possible",               "bee",                    # "likely to be pollinated by native bees"
  "Graptophyllum excelsum",         "pollination_vector_possible",               "bird",                   # "Birds are thought to be the pollinators"
  "Grevillea hockingsii",           "flower_visitor",                            "bird",                   # nectivorous birds observed feeding at the flowers
  "Hydrocharis dubia",              "pollination_vector_possible",               "insect",                 # "appears to be insect pollinated"
  "Livistona nitida",               "pollination_vector_possible",               "wind bee",               # "probably by wind and perhaps ... bees"
  "Lysiana filifolia",              "pollination_vector_possible",               "honeyeater",             # "probably pollinated almost exclusively by honeyeaters"
  "Macadamia jansenii",             "pollination_vector_possible",               "bee",                    # "thought ... pollinated by native bees"
  "Macrozamia conferta",            "pollination_vector_known",                  "beetle",                 # Tranes weevil, obligate mutualism
  "Macrozamia cranei",              "pollination_vector_known",                  "beetle",
  "Macrozamia crassifolia",         "pollination_vector_known",                  "beetle",
  "Macrozamia lomandroides",        "pollination_vector_known",                  "beetle",
  "Macrozamia machinii",            "pollination_vector_known",                  "beetle",
  "Macrozamia pauli-guilielmi",     "pollination_vector_possible",               "beetle",                 # "likely to be a species of Tranes weevil"
  "Macrozamia platyrhachis",        "pollination_vector_known",                  "thrips",                 # "pollinated by Cycadothrips thrips"
  "Macrozamia longispina",          "flower_visitor",                            "thrips",                 # cones "attended by thrips"
  "Pterostylis cobarensis",         "pollination_vector_known",                  "fly",                    # males of small gnats
  "Pterostylis cobarensis",         "flower_scent_production",                   "scent_produced",         # "pseudosexual perfume"
  # dispersal
  "Acacia attenuata",               "dispersal_syndrome",                        "barochory",              # "primarily ... effected by gravity" (ejection only "possibly")
  "Corchorus cunninghamii",         "dispersal_syndrome",                        "barochory",              # seeds drop to the ground, not forcibly ejected
  "Cyperus cephalotes",             "dispersal_syndrome",                        "hydrochory",             # stolons "enable dispersal by water"
  "Dioclea hexandra",               "dispersal_syndrome",                        "hydrochory",             # "drift seed species"
  "Grevillea hockingsii",           "dispersal_syndrome",                        "myrmecochory",           # elaiosome which ants eat
  "Homopholis belsonii",            "dispersal_syndrome",                        "anemochory",             # dried panicle breaks off in the wind
  "Homopholis belsonii",            "dispersal_appendage",                       "tumbleweed",
  "Olearia gravis",                 "dispersal_syndrome",                        "anemochory",             # "The seed is wind-dispersed"
  "Livistona nitida",               "dispersal_syndrome",                        "endozoochory",           # "undoubtedly facilitated by birds and fruit bats"
  # fire response and seed bank
  "Acacia eremophiloides",          "resprouting_capacity",                      "fire_killed",            # "obligate seeder"
  "Acacia porcata",                 "resprouting_capacity",                      "fire_killed",
  "Acacia porcata",                 "post_fire_recruitment",                     "post_fire_recruitment",
  "Acacia ramiflora",               "resprouting_capacity",                      "resprouts",
  "Acacia ramiflora",               "post_fire_recruitment",                     "post_fire_recruitment",
  "Banksia plagiocarpa",            "resprouting_capacity",                      "resprouts",
  "Boronia keysii",                 "resprouting_capacity",                      "fire_killed",
  "Boronia keysii",                 "post_fire_recruitment",                     "post_fire_recruitment",
  "Boronia keysii",                 "seedbank_location",                         "soil_seedbank",
  "Boronia repanda",                "resprouting_capacity",                      "fire_killed resprouts",  # DPIE 2018 "killed by fire" vs 2020 observed resprouting
  "Boronia repanda",                "seedbank_location",                         "soil_seedbank",
  "Caustis blakei subsp. macrantha", "resprouting_capacity",                     "fire_killed",            # fire sensitive, regenerates from soil-stored seed
  "Caustis blakei subsp. macrantha", "seedbank_location",                        "soil_seedbank",
  "Commersonia argentea",           "resprouting_capacity",                      "resprouts",              # fire promotes recruitment from suckering rootstocks
  "Commersonia beeronensis",        "resprouting_capacity",                      "resprouts",
  "Commersonia beeronensis",        "post_fire_recruitment",                     "post_fire_recruitment",
  "Daviesia discolor",              "resprouting_capacity",                      "resprouts",
  "Hakea trineura",                 "resprouting_capacity",                      "resprouts",
  "Macrozamia lomandroides",        "resprouting_capacity",                      "resprouts",              # "Adult Macrozamia plants ... are able to resprout"
  "Macrozamia longispina",          "resprouting_capacity",                      "resprouts",
  "Macrozamia parcifolia",          "resprouting_capacity",                      "resprouts",
  "Macrozamia pauli-guilielmi",     "resprouting_capacity",                      "resprouts",
  "Macrozamia viridis",             "resprouting_capacity",                      "resprouts",
  "Macrozamia lomandroides",        "resprouting_capacity_juvenile",             "juvenile_fire_killed",   # seedlings "usually killed by fire"
  "Macrozamia longispina",          "resprouting_capacity_juvenile",             "juvenile_fire_killed",
  "Macrozamia occidua",             "resprouting_capacity_juvenile",             "juvenile_fire_killed",
  "Macrozamia parcifolia",          "resprouting_capacity_juvenile",             "juvenile_fire_killed",
  "Macrozamia pauli-guilielmi",     "resprouting_capacity_juvenile",             "juvenile_fire_killed",
  "Macrozamia viridis",             "resprouting_capacity_juvenile",             "juvenile_fire_killed",
  "Marsdenia hemiptera",            "resprouting_capacity",                      "resprouts",              # "basal resprouter"
  "Melaleuca sylvana",              "resprouting_capacity",                      "resprouts",
  "Olearia gravis",                 "resprouting_capacity",                      "fire_killed",
  "Olearia gravis",                 "seedbank_location",                         "soil_seedbank",
  "Olearia gravis",                 "post_fire_recruitment",                     "post_fire_recruitment",
  "Paspalidium grandispiculatum",   "resprouting_capacity",                      "resprouts",              # regenerates from the rhizome after fire
  "Solanum graniticum",             "resprouting_capacity",                      "resprouts",              # "herbaceous resprouter"
  "Acacia attenuata",               "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance",
  "Cadellia pentastylis",           "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", # rootstock / coppice from stumps
  "Gossia gonoclada",               "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", # stem suckers after damage
  "Acacia deuteroneura",            "seedbank_location",                         "soil_seedbank",          # "fire may deplete the soil seed bank"
  "Acacia storyi",                  "seedbank_location",                         "soil_seedbank",
  "Zieria verrucosa",               "seedbank_location",                         "soil_seedbank",
  "Gonocarpus urceolatus",          "seedbank_location",                         "soil_seedbank",
  "Trioncinia retroflexa",          "seedbank_location",                         "soil_seedbank",          # seeds viable in the soil for at least 18 months
  "Eucalyptus conglomerata",        "seedbank_location",                         "canopy_seedbank",        # seed held on the tree until the branch dies
  "Eucalyptus conglomerata",        "serotiny",                                  "serotinous",
  # germination
  "Dioclea hexandra",               "seed_germination_treatment",                "scarify",
  "Cadellia pentastylis",           "seed_germination_treatment",                "heat",
  "Corchorus cunninghamii",         "seed_germination_treatment",                "heat",
  "Homopholis belsonii",            "seed_dormancy_class",                       "non_dormant",            # germinates readily without a dormancy period
  # vegetative reproduction
  "Acacia attenuata",               "vegetative_reproduction_ability",           "vegetative",
  "Acacia attenuata",               "clonal_spread_mechanism",                   "root_buds",              # regeneration from surface roots
  "Acacia tenuinervis",             "vegetative_reproduction_ability",           "vegetative",
  "Acacia tenuinervis",             "clonal_spread_mechanism",                   "root_buds",              # "often with root suckers"
  "Aponogeton prolifer",            "vegetative_reproduction_ability",           "vegetative",
  "Aponogeton prolifer",            "clonal_spread_mechanism",                   "viviparous",             # live-bearing, plantlets at stalk tips
  "Cerbera dumicola",               "vegetative_reproduction_ability",           "vegetative",
  "Cerbera dumicola",               "clonal_spread_mechanism",                   "clonal",                 # "capable of suckering"
  "Commersonia argentea",           "vegetative_reproduction_ability",           "vegetative",
  "Commersonia argentea",           "clonal_spread_mechanism",                   "rhizome root_buds",
  "Commersonia beeronensis",        "vegetative_reproduction_ability",           "vegetative",
  "Commersonia beeronensis",        "clonal_spread_mechanism",                   "rhizome",                # stems suckering from rhizomes
  "Corchorus cunninghamii",         "vegetative_reproduction_ability",           "not_vegetative",
  "Cossinia australiana",           "vegetative_reproduction_ability",           "vegetative",
  "Cossinia australiana",           "clonal_spread_mechanism",                   "root_buds",
  "Cyperus cephalotes",             "vegetative_reproduction_ability",           "vegetative",
  "Cyperus cephalotes",             "clonal_spread_mechanism",                   "stolon",
  "Digitaria porrecta",             "vegetative_reproduction_ability",           "vegetative",
  "Digitaria porrecta",             "clonal_spread_mechanism",                   "clonal",                 # tussock ring fragments into plantlets
  "Eriocaulon carsonii",            "vegetative_reproduction_ability",           "vegetative",
  "Eriocaulon carsonii",            "clonal_spread_mechanism",                   "clonal",
  "Gossia gonoclada",               "vegetative_reproduction_ability",           "vegetative",
  "Gossia gonoclada",               "clonal_spread_mechanism",                   "stem_suckers",
  "Homopholis belsonii",            "vegetative_reproduction_ability",           "vegetative",
  "Homopholis belsonii",            "clonal_spread_mechanism",                   "stolon",
  "Lissanthe brevistyla",           "vegetative_reproduction_ability",           "vegetative",
  "Lissanthe brevistyla",           "clonal_spread_mechanism",                   "belowground_clonal",     # creeping underground root system
  "Neoroepera buxifolia",           "vegetative_reproduction_ability",           "vegetative",
  "Phebalium distans",              "vegetative_reproduction_ability",           "not_vegetative",
  # other
  "Dendrobium bigibbum",            "leaf_phenology",                            "facultative_drought_deciduous", # deciduous in exposed sites in the dry season
  "Dendrobium lithocola",           "leaf_phenology",                            "facultative_drought_deciduous",
  "Dendrobium phalaenopsis",        "leaf_phenology",                            "facultative_drought_deciduous",
  "Eriocaulon carsonii",            "sex_type",                                  "monoecious",             # heads with female and male flowers
  "Rutidosis glandulosa",           "sex_type",                                  "gynomonoecious",         # bisexual florets plus a few female outer florets
  "Lysiana filifolia",              "parasitic",                                 "hemiparasitic stem_parasitic",
  "Thesium australe",               "parasitic",                                 "root_parasitic",
  # physical defence
  "Solanum adenophorum",            "plant_physical_defence_structures",         "prickle",
  "Solanum dissectum",              "plant_physical_defence_structures",         "prickle",
  "Solanum elachophyllum",          "plant_physical_defence_structures",         "prickle",
  "Solanum graniticum",             "plant_physical_defence_structures",         "prickle",
  "Solanum sporadotrichum",         "plant_physical_defence_structures",         "prickle",
  "Solanum stenopterum",            "plant_physical_defence_structures",         "prickle",
  "Solanum johnsonianum",           "plant_physical_defence_structures",         "absent",                 # "without prickles"
  "Capparis humistrata",            "plant_physical_defence_structures",         "spine",
  "Discaria pubescens",             "plant_physical_defence_structures",         "spine",
  "Graptophyllum excelsum",         "plant_physical_defence_structures",         "spine",                  # axillary spines
  "Bursaria reevesii",              "plant_physical_defence_structures",         "thorn",                  # spinescent short shoots
  "Acacia saxicola",                "plant_physical_defence_structures",         "pungent_leaf_apex",
  "Pultenaea setulosa",             "plant_physical_defence_structures",         "pungent_leaf_apex",
  "Cyathea celebica",               "plant_physical_defence_structures",         "spine",                  # frond stalk spines
  "Cyathea exilis",                 "plant_physical_defence_structures",         "spine prickle",
  "Cycas cairnsiana",               "plant_physical_defence_structures",         "spine",                  # petiole spines (pinnacanths)
  "Cycas couttsiana",               "plant_physical_defence_structures",         "spine",
  "Cycas platyphylla",              "plant_physical_defence_structures",         "spine",
  "Cycas megacarpa",                "plant_physical_defence_structures",         "spine",                  # lowest leaflets reduced to spines
  "Cycas ophiolitica",              "plant_physical_defence_structures",         "spine",
  "Macrozamia serpentina",          "plant_physical_defence_structures",         "spine",
  "Livistona concinna",             "plant_physical_defence_structures",         "spine",                  # petiole spines
  "Livistona lanuginosa",           "plant_physical_defence_structures",         "prickle",
  "Livistona drudei",               "plant_physical_defence_structures",         "thorn",                  # petiole marginal thorns
  "Livistona fulva",                "plant_physical_defence_structures",         "thorn",
  "Livistona nitida",               "plant_physical_defence_structures",         "thorn"
)
# numeric ecological values (years / days), with the same provenance
eco_numeric <- tribble(
  ~taxon_name,              ~trait,                  ~min, ~max,
  "Acacia attenuata",       "lifespan",              5,    10,   # "life span of between five and ten years"
  "Acacia eremophiloides",  "lifespan",              8,    12,   # cultivated specimens
  "Corchorus cunninghamii", "lifespan",              3,    4,
  "Trioncinia retroflexa",  "lifespan",              NA,   5,    # "approximately 5 years"
  "Acacia attenuata",       "reproductive_maturity", 2,    3,    # juvenile period ~2 years (3 at most)
  "Acacia eremophiloides",  "reproductive_maturity", 3,    4,
  "Marsdenia hemiptera",    "reproductive_maturity", 6,    20,   # "first seeds 6-20 years"
  "Gastrodia crebriflora",  "flower_lifespan",       1,    2,    # days
  "Dendrobium bigibbum",    "flower_lifespan",       NA,   14,   # "flowers lasting for two weeks"
  "Dendrobium lithocola",   "flower_lifespan",       NA,   14,
  "Dendrobium phalaenopsis", "flower_lifespan",      NA,   14,
  "Newcastelia velutina",   "flower_lifespan",       4,    5
)
stopifnot(all(eco_scores$taxon_name %in% wide$taxon_name), all(eco_numeric$taxon_name %in% wide$taxon_name))

eco_wide <- eco_scores %>% pivot_wider(names_from = trait, values_from = value)
eco_num_wide <- eco_numeric %>% pivot_longer(c(min, max), names_to = "bound") %>%
  mutate(col = paste0(trait, "_", bound)) %>% select(taxon_name, col, value) %>%
  pivot_wider(names_from = col, values_from = value)
# traits that are also scripted (scent, phenology, sex, appendage): the vetted value replaces the scripted one where given
for (tr in intersect(names(eco_wide)[-1], names(wide))) {
  v <- eco_wide[[tr]][match(wide$taxon_name, eco_wide$taxon_name)]
  wide[[tr]] <- coalesce(v, wide[[tr]])
  eco_wide[[tr]] <- NULL
}
wide <- wide %>% left_join(eco_wide, by = "taxon_name") %>% left_join(eco_num_wide, by = "taxon_name")

# ---------------------------------------------------------------- heights to their trait
# orchid heights in the profiles are to the top of the flowering stem; climber heights depend on support;
# a separately described scape is a reproductive height
is_orchid <- wide$family == "Orchidaceae"
is_climber <- str_detect(coalesce(wide$plant_growth_form, ""), "climber")
wide <- wide %>% mutate(
  plant_height_reproductive_min = case_when(is_orchid ~ height_min, TRUE ~ scape_height_min),
  plant_height_reproductive_max = case_when(is_orchid ~ coalesce(height_max, scape_height_max), TRUE ~ scape_height_max),
  plant_height_reproductive_description = case_when(is_orchid ~ coalesce(height_description, scape_height_description), TRUE ~ scape_height_description),
  plant_height_climbing_plant_min = ifelse(is_climber, height_min, NA),
  plant_height_climbing_plant_max = ifelse(is_climber, height_max, NA),
  plant_height_min = ifelse(is_orchid | is_climber, NA, height_min),
  plant_height_max = ifelse(is_orchid | is_climber, NA, height_max),
  plant_height_description = height_description,
  plant_diameter_breast_height_min = ifelse(dbh %in% TRUE, stem_diam_min, NA),
  plant_diameter_breast_height_max = ifelse(dbh %in% TRUE, stem_diam_max, NA),
  stem_diameter_min = ifelse(dbh %in% TRUE, NA, stem_diam_min),
  stem_diameter_max = ifelse(dbh %in% TRUE, NA, stem_diam_max),
  stem_diameter_description = stem_diam_description
) %>% select(-starts_with("height_"), -starts_with("scape_height_"), -starts_with("stem_diam_"), -dbh)

# ---------------------------------------------------------------- context rows and output
# Contexts in traits.build apply to a whole row, so storage-organ dimensions (entity_measured = the organ)
# get their own rows; every other value sits on the single main row per taxon.
id_cols <- c("taxon_name", "family", "common_name", "wildnet_taxon_id", "wildnet_url")
# growth forms with a frequency qualifier ("tree or rarely a shrub") become one row per form with
# commonness_qualifier = usually / rarely ...; unqualified alternatives ("shrub or tree") stay on the main row
gf_q <- wide %>% filter(!is.na(growth_form_qualified))
gf_rows <- gf_q %>% select(all_of(id_cols), plant_growth_form_description, growth_form_qualified) %>%
  separate_rows(growth_form_qualified, sep = ";") %>%
  separate(growth_form_qualified, c("plant_growth_form", "commonness_qualifier"), sep = "=")
wide <- wide %>% mutate(plant_growth_form = ifelse(is.na(growth_form_qualified), plant_growth_form, NA)) %>% select(-growth_form_qualified)

storage_rows <- wide %>% filter(!is.na(storage_organ_length_max) | !is.na(storage_organ_diameter_max)) %>%
  select(all_of(id_cols), entity_measured = storage_entity, storage_organ_dimensions_description,
         starts_with("storage_organ_length_"), starts_with("storage_organ_diameter_"))
main <- wide %>% select(-storage_entity, -storage_organ_dimensions_description,
                        -starts_with("storage_organ_length_"), -starts_with("storage_organ_diameter_"))

out_df <- bind_rows(main, gf_rows, storage_rows)
if (!"commonness_qualifier" %in% names(out_df)) out_df$commonness_qualifier <- NA_character_
out_df <- out_df %>%
  relocate(entity_measured, commonness_qualifier, .after = wildnet_url) %>%
  mutate(row_order = case_when(!is.na(commonness_qualifier) ~ 1, !is.na(entity_measured) ~ 2, TRUE ~ 0)) %>%
  arrange(taxon_name, row_order, commonness_qualifier) %>% select(-row_order)

# numeric columns: plant heights in m, all other lengths/diameters in mm, lifespan / maturity in years, flower lifespan in days
out_df <- out_df %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
# numeric ranges inside text written as "a--b" (a hyphen or en dash between numbers is read as a formula/date by Excel)
out_df <- out_df %>% mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--")))

readr::write_csv(out_df, out, na = "")
