# Builds data/LucidFerns_2026/data.csv from the scraped Lucid "Australian Tropical Ferns and
# Lycophytes" fact sheets (lucid_tropical_ferns_fact_sheets.csv, austraits.build-data.scraping.scripts/data_extra).
# Not run by the build -- kept for provenance / re-extraction.
#
# Descriptions are Flora-of-Australia style: clauses led by an organ ("Rhizome ...", "Fronds ...",
# "Stipe ...", "Lamina ...", "Primary pinnae ...", "Ultimate segments ..."). Each clause is assigned to
# the organ that leads it; clauses led by "margins", "apex", "upper surface" ... inherit the previous organ.
# Fern terms follow AusTraits usage: stipe = petiole, frond / lamina = leaf, pinnae / pinnules /
# segments = leaflets. Which division a size or count refers to is kept in context columns
# (as in Wenk_2025): `leaf_type` (fronds, lamina, pinnae, primary pinnae, ultimate segments, ...) and
# `plant_organ_measured` (stipe, rhizome, trunk, branches, fronds).
# Categorical traits are written as `<trait>_description` (verbatim) + `<trait>` (mapped to traits.yml).
# Numeric columns: plant height in m, all other lengths/diameters in mm (source mixes mm, cm and m);
# a range is written to `_min` / `_max`, "to X" / "up to X" to `_max`, "X or more" / "more than X" to `_min`,
# and any other single value ("c. 35 cm", "3 pairs") to `_mean`.
# Pinna counts given as pairs are kept as pairs in `leaflet_count_pairs_*` (unit "pairs", converted x2 by
# traits.build), counts of individual pinnae in `leaflet_count_*`.

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)

src <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra/lucid_tropical_ferns_fact_sheets.csv"
out <- "data/LucidFerns_2026/data.csv"

d <- readr::read_csv(src, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
d[is.na(d)] <- ""
d <- d %>% mutate(across(everything(), ~ str_squish(str_replace_all(.x, "[\u2013\u2014\u2212]", "-"))))

# source typos
d <- d %>% mutate(
  `Habit and habitat` = str_replace_all(`Habit and habitat`, regex("\\blitophyt|\\blithophyhyt", ignore_case = TRUE), "lithophyt"),
  `Habit and habitat` = str_replace_all(`Habit and habitat`, "\\bsomtimes\\b", "sometimes"),
  Description = str_replace_all(Description, "(?<=[0-9])\\. (?=[0-9]+\\s?(?:mm|cm|m)\\b)", "."),
  # Lastreopsis walleri: "340-100 cm long" (40-100 cm intended)
  Description = ifelse(taxon_name == "Lastreopsis walleri", str_replace(Description, "340-100 cm long", "40-100 cm long"), Description)
)

na_if_empty <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
collapse_unique <- function(x, sep = " ") {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# sentence / clause splitter protecting abbreviations ("c. 2 mm", "diam.", "C. contigua", "Fl. Australia")
sentences <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  x <- str_replace_all(x, "\\b([A-Z])\\.(?=\\s)", "\\1\u00a7")
  # "diam." / "mm." often end a sentence, so they are not protected
  x <- str_replace_all(x, "\\b(c|ca|approx|sp|spp|subsp|var|Fl)\\.(?=\\s)", "\\1\u00a7")
  s <- str_split(x, "(?<=[.])\\s+(?=[A-Z(])")[[1]]
  s <- str_replace_all(s, "\u00a7", ".")
  str_squish(s[s != ""])
}
clauses <- function(x) {
  s <- sentences(x)
  str_squish(unlist(map(s, ~ str_split(.x, ";\\s*")[[1]]))) %>% str_remove("\\.$") %>% .[. != ""]
}
grab_sentences <- function(x, pattern, exclude = NULL) {
  s <- sentences(x)
  keep <- str_detect(s, regex(pattern, ignore_case = TRUE))
  if (!is.null(exclude)) keep <- keep & !str_detect(s, regex(exclude, ignore_case = TRUE))
  collapse_unique(s[keep], " ")
}

# ---------------------------------------------------------------- measurements
num <- "[0-9]+(?:\\.[0-9]+)?"
dash <- "\\s*(?:-|to)\\s*"
unit_mult <- c(mm = 1, cm = 10, m = 1000)
to_mm <- function(v, u) ifelse(is.na(v), NA_real_, as.numeric(v) * unname(unit_mult[u]))
# unit on the lower bound only when a range follows, otherwise "0.8 mm" is read as 0.8 "m" + "m"
lo_unit <- "(?:\\s?(mm|cm|m)(?=\\s*(?:-|to)\\s*[0-9]))?"
meas <- paste0("(?:(?:up to|to|about|approximately|c\\.|ca\\.|less than|more than|commonly|usually|mostly)\\s+)?(", num, ")", lo_unit, "(?:", dash, "(", num, "))?\\s?(mm|cm|m)\\b")
# a range -> min / max; a single value -> max ("to X", "up to X", "less than X"), min ("X or more",
# "more than X") or mean (any other single value); `txt` is the matched text, qualifiers included
range_val <- function(lo, hi, txt) {
  if (!is.na(hi)) return(c(min = lo, max = hi, mean = NA))
  if (str_detect(txt, regex("\\bor more\\b|^more than\\b", ignore_case = TRUE))) return(c(min = lo, max = NA, mean = NA))
  if (str_detect(txt, regex("^(?:(?:in|with|of|bearing|into)\\s+)?(?:up to|to|less than)\\b", ignore_case = TRUE))) return(c(min = NA, max = lo, mean = NA))
  c(min = NA, max = NA, mean = lo)
}
parse_meas <- function(m, txt = m[1]) {
  u <- m[5]; u1 <- ifelse(is.na(m[3]), u, m[3])
  range_val(to_mm(m[2], u1), to_mm(m[4], u), str_squish(txt))
}
count_val <- function(lo, hi, txt) range_val(as.numeric(lo), as.numeric(hi), str_squish(txt))
# drop parenthetical extremes "(1.5-) 2-3 (-3.5) cm" and "(rarely ...)" so the typical range is parsed
strip_extremes <- function(x) {
  x <- str_replace_all(x, paste0("\\(\\s*", num, "\\s*-?\\s*\\)\\s*"), "")
  x <- str_replace_all(x, paste0("\\s*\\(\\s*-\\s*", num, "\\s*\\)"), "")
  str_replace_all(x, "\\s*\\((?:rarely|occasionally|sometimes)[^)]*\\)", "")
}
# first measurement followed by one of `words` that is not preceded (closely) by another organ noun
first_meas <- function(x, words, other = NULL) {
  m <- str_locate_all(x, paste0(meas, "(?:\\s+or more)?\\s*(?:", words, ")\\b"))[[1]]
  for (i in seq_len(nrow(m))) {
    gap <- str_sub(x, 1, m[i, 1] - 1)
    if (!is.null(other) && str_detect(gap, regex(paste0("\\b(?:", other, ")\\b[^,;]{0,25}$"), ignore_case = TRUE))) next
    txt <- str_sub(x, m[i, 1], m[i, 2])
    return(list(v = parse_meas(str_match(txt, meas)[1:5], txt), txt = txt))
  }
  NULL
}
# "to 33 cm long, 3 cm wide": the "to" of the first dimension also bounds an unqualified second one
carry_to <- function(first, second, cl) {
  if (is.null(first) || is.null(second)) return(second)
  if (is.na(first$v[["min"]]) && !is.na(first$v[["max"]]) && !is.na(second$v[["mean"]]) &&
      str_detect(first$txt, regex("^(?:up to|to)\\b", ignore_case = TRUE))) {
    gap <- str_sub(cl, str_locate(cl, fixed(first$txt))[2] + 1, str_locate(cl, fixed(second$txt))[1] - 1)
    if (!is.na(gap) && str_detect(gap, "^\\s*(?:,|and|,\\s*and)?\\s*$")) second$v <- c(min = NA, max = second$v[["mean"]], mean = NA)
  }
  second
}
# "6 by 1.4 cm" / "4-5 x 2-3 mm"
by_meas <- function(x) {
  m <- str_match(x, paste0("(", num, ")(?:", dash, "(", num, "))?\\s?(mm|cm|m)?\\s*(?:by|x|\u00d7)\\s*", meas))
  if (is.na(m[1])) return(NULL)
  u <- m[8]; u1 <- coalesce(m[4], u)
  L <- range_val(to_mm(m[2], u1), to_mm(m[3], u1), m[1])
  W <- parse_meas(c(m[1], m[5:8]))
  list(L = L, W = W, txt = m[1])
}

# ---------------------------------------------------------------- organ subjects
lvl_mod <- "(?:primary|secondary|tertiary|quaternary|basal|lower|lowest|middle|median|mid|upper|longest|largest|larger|smaller|fertile|sterile|lateral|terminal|apical|ultimate|reduced basal|reduced|central|main|higher order|accessory|sterile and fertile)"
leaflet_noun <- "(?:pinnae|pinna|pinnules?|segments?|lobes?|leaflets?|divisions?)"
leaflet_subject <- regex(paste0("^(?:the |next few pairs of |each )?((?:", lvl_mod, "[ ,]+(?:and |or )?){0,3})", leaflet_noun, "\\b"), ignore_case = TRUE)
frond_subject <- regex("^((?:sterile and fertile|sterile|fertile|nest|foliage|base|basal|mature|juvenile|simple|dissected)\\s+)?(fronds?)\\b(?!\\s+laminae?)", ignore_case = TRUE)
lamina_subject <- regex("^((?:sterile|fertile)\\s+)?(?:fronds?\\s+)?(laminae?)\\b(?:\\s+of\\s+(sterile|fertile)\\s+fronds?)?", ignore_case = TRUE)
leaves_subject <- regex("^((?:lateral|median|dorsal|ventral|axillary|sterile|fertile|vegetative|mature|juvenile|ordinary)\\s+)?leaves\\b", ignore_case = TRUE)
stipe_subject <- regex("^(?:common\\s+)?stipes?\\b", ignore_case = TRUE)
rhizome_subject <- regex("^rhizomes?\\b(?!\\s+scales)", ignore_case = TRUE)
plant_subject <- regex("^plants?\\b", ignore_case = TRUE)
branch_subject <- regex("^((?:main|aerial|erect|lateral|fertile|sterile)\\s+)?(branches|branchlets?(?: systems?)?|stems?|shoots)\\b", ignore_case = TRUE)
# clauses about parts of the current organ (inherit it)
part_subject <- regex("^(?:margins?|apex|apices|bases?|upper surface|lower surface|surfaces|both surfaces|texture|midribs?|costae?|veins?)\\b", ignore_case = TRUE)

normalise_level <- function(mod, noun) {
  mod <- str_squish(str_to_lower(str_replace_all(coalesce(mod, ""), "[,]", " ")))
  noun <- str_to_lower(noun)
  noun <- case_when(str_detect(noun, "^pinn(a|ae)$") ~ "pinnae", str_detect(noun, "^pinnules?$") ~ "pinnules",
                    str_detect(noun, "^segments?$") ~ "segments", str_detect(noun, "^lobes?$") ~ "lobes",
                    str_detect(noun, "^leaflets?$") ~ "leaflets", str_detect(noun, "^divisions?$") ~ "divisions", TRUE ~ noun)
  str_squish(paste(mod, noun))
}

classify_clause <- function(cl, current) {
  if (str_detect(cl, rhizome_subject)) return(list(kind = "organ", level = "rhizome"))
  if (str_detect(cl, stipe_subject)) return(list(kind = "organ", level = "stipe"))
  if (str_detect(cl, plant_subject)) return(list(kind = "organ", level = "plants"))
  m <- str_match(cl, lamina_subject)
  if (!is.na(m[1])) return(list(kind = "leaf", level = str_squish(paste(str_to_lower(coalesce(m[2], m[4], "")), "lamina"))))
  m <- str_match(cl, frond_subject)
  if (!is.na(m[1])) return(list(kind = "leaf", level = str_squish(paste(str_to_lower(coalesce(m[2], "")), "fronds"))))
  m <- str_match(cl, leaflet_subject)
  if (!is.na(m[1])) return(list(kind = "leaflet", level = normalise_level(m[2], str_match(cl, regex(paste0(leaflet_noun, "\\b"), ignore_case = TRUE))[1])))
  m <- str_match(cl, leaves_subject)
  if (!is.na(m[1])) return(list(kind = "leaf", level = str_squish(paste(str_to_lower(coalesce(m[2], "")), "leaves"))))
  m <- str_match(cl, branch_subject)
  if (!is.na(m[1])) return(list(kind = "organ", level = str_replace_all(str_to_lower(m[3]), c("^branchlet system$" = "branchlet systems", "^branchlet$" = "branchlets", "^stem$" = "stems"))))
  # "longest 13-60 cm long" / "basal ones ..." refer to the division just described
  m <- str_match(cl, regex("^(longest|largest|larger|basal|lowest|lower|terminal|middle|apical|upper)\\b(?!\\s+(?:sori|scales?|hairs?|veins?))(?=\\s+(?:[0-9]|c\\.|to |ones?\\b|pairs?\\b))", ignore_case = TRUE))
  if (!is.na(m[1]) && !is.null(current)) {
    base <- if (current$kind == "leaflet") current$level else "pinnae"
    return(list(kind = "leaflet", level = str_squish(paste(str_to_lower(m[2]), str_remove(base, "^(?:longest|largest|larger|basal|lowest|lower|terminal|middle|apical|upper) ")))))
  }
  if (str_detect(cl, part_subject) && !is.null(current)) return(c(current, part = TRUE))
  list(kind = "other", level = NA_character_)
}

# ---------------------------------------------------------------- categorical vocabularies
map_by_dictionary <- function(p, dict) {
  if (is.na(p) || p == "") return(NA_character_)
  p <- str_to_lower(p)
  hits <- tibble(pos = integer(0), v = character(0))
  for (k in names(dict)[order(-nchar(names(dict)))]) {
    loc <- str_locate(p, paste0("(?<![a-z-])", k, "(?![a-z])"))
    if (!is.na(loc[1])) {
      hits <- add_row(hits, pos = loc[1], v = dict[[k]])
      p <- str_replace_all(p, paste0("(?<![a-z-])", k, "(?![a-z])"), function(z) strrep(" ", nchar(z)))
    }
  }
  if (!nrow(hits)) return(NA_character_)
  collapse_unique(hits$v[order(hits$pos)])
}
leaf_shape_map <- c(
  "narrowly linear" = "narrowly_linear", "linear" = "linear", "narrowly lanceolate" = "narrowly_lanceolate",
  "lanceolate" = "lanceolate", "lance-shaped" = "lanceolate", "narrowly oblanceolate" = "narrowly_oblanceolate",
  "oblanceolate" = "oblanceolate", "narrowly elliptic" = "narrowly_elliptical", "broadly elliptic" = "widely_elliptical",
  "elliptic" = "elliptical", "elliptical" = "elliptical", "narrowly ovate" = "narrowly_ovate", "ovate" = "ovate",
  "narrowly obovate" = "narrowly_obovate", "broadly obovate" = "widely_obovate", "obovate" = "obovate",
  "narrowly oblong" = "narrowly_oblong", "oblong" = "oblong", "orbicular" = "orbicular", "round" = "orbicular",
  "circular" = "orbicular", "cordate" = "cordate", "reniform" = "reniform", "terete" = "terete",
  "filiform" = "filiform", "falcate" = "falcate", "spathulate" = "spathulate", "subulate" = "subulate",
  "acicular" = "acicular", "strap-shaped" = "strap-shaped", "ligulate" = "strap-shaped", "peltate" = "peltate",
  "rhomboid" = "rhomboidal", "rhombic" = "rhomboidal", "deltate" = "deltate", "deltoid" = "deltate",
  "narrowly triangular" = "narrowly_triangular", "triangular" = "triangular", "ensiform" = "ensiform")
division_map <- c(
  "1-pinnate-pinnatifid" = "pinnately_compound", "1-pinnate" = "pinnately_compound", "once pinnate" = "pinnately_compound",
  "pinnate" = "pinnately_compound", "2-pinnate-pinnatifid" = "bipinnate", "2-pinnate" = "bipinnate", "bipinnate" = "bipinnate",
  "3-pinnate-pinnatifid" = "tripinnate", "3-pinnate" = "tripinnate", "tripinnate" = "tripinnate",
  "1-pinnatifid" = "pinnatifid", "pinnatifid" = "pinnatifid", "2-pinnatifid" = "bipinnatifid", "bipinnatifid" = "bipinnatifid",
  "pinnatisect" = "pinnatisect", "2-pinnatisect" = "bipinnatisect", "bipinnatisect" = "bipinnatisect",
  "pinnatipartite" = "pinnatipartite", "trifoliate" = "trifoliate", "palmatifid" = "palmately_lobed",
  "palmately divided" = "palmately_lobed", "palmately lobed" = "palmately_lobed", "dichotomously" = "dichotomously_lobed",
  "pinnately lobed" = "pinnately_lobed", "bipartite" = "bipartite", "tripartite" = "tripartite")
# "2-3-pinnate" / "3- or 4-pinnate": expand degree ranges so each degree is mapped
expand_degrees <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\b([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)", function(z) {
    m <- str_match(z, "([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)")
    paste(paste0(seq(as.integer(m[2]), as.integer(m[3])), "-", m[4]), collapse = " ")
  })
  str_replace_all(x, "\\b([1-4])\\s*-?\\s*or\\s*([1-4])-(pinnate)", "\\1-\\3 \\2-\\3")
}
colour_free <- function(x) x

# ---------------------------------------------------------------- per-taxon extraction
extract_one <- function(r) {
  taxon <- r[["taxon_name"]]; fam <- r[["Family"]]
  desc <- r[["Description"]]; hab <- r[["Habit and habitat"]]
  cls <- clauses(desc)
  current <- NULL
  recs <- list()
  for (cl in cls) {
    cc <- classify_clause(cl, current)
    if (cc$kind != "other" && is.null(cc$part)) current <- cc[c("kind", "level")]
    recs[[length(recs) + 1]] <- tibble(kind = cc$kind, level = cc$level, part = !is.null(cc$part), clause = cl)
  }
  recs <- bind_rows(recs)

  rows <- list()   # context rows: one per (context, level)
  add_val <- function(ctx, level, trait, v, txt) {
    rows[[length(rows) + 1]] <<- tibble(context = ctx, level = level, trait = trait,
                                       min = unname(v[["min"]]), max = unname(v[["max"]]), mean = unname(v[["mean"]]), txt = txt)
  }
  cat_vals <- list()
  add_cat <- function(ctx, level, trait, value, txt) {
    if (is.na(value)) return(invisible())
    cat_vals[[length(cat_vals) + 1]] <<- tibble(context = ctx, level = level, trait = trait, value = value, txt = txt)
  }

  is_treefern <- fam %in% c("Cyatheaceae", "Dicksoniaceae")
  for (i in seq_len(nrow(recs))) {
    k <- recs$kind[i]; lv <- recs$level[i]; cl <- strip_extremes(recs$clause[i])
    if (k == "other") next
    if (!recs$part[i]) {
      # ---- sizes
      if (k == "organ" && lv == "rhizome") {
        trunk <- is_treefern || str_detect(cl, "trunk")
        h <- first_meas(cl, "tall|high|long", other = "scales?|hairs?|phyllopodia|roots|stipes?|bristles")
        if (!is.null(h)) {
          if (trunk) add_val("plant_organ_measured", "trunk", "plant_height", h$v / 1000, h$txt)
          else add_val("plant_organ_measured", "rhizome", "stem_length", h$v, h$txt)
        }
        dm <- carry_to(h, first_meas(cl, "diam\\.?|diameter|thick|wide", other = "scales?|hairs?|phyllopodia|roots|stipes?|bristles|base"), cl)
        if (!is.null(dm)) add_val("plant_organ_measured", if (trunk) "trunk" else "rhizome", "stem_diameter", dm$v, dm$txt)
      }
      if (k == "organ" && lv == "stipe") {
        L <- first_meas(cl, "long", other = "scales?|hairs?|wings?|grooves?|tubercles|spines")
        if (!is.null(L)) add_val("plant_organ_measured", "stipe", "petiole_length", L$v, L$txt)
        W <- carry_to(L, first_meas(cl, "diam\\.?|diameter|thick|wide", other = "scales?|hairs?|wings?|grooves?|tubercles|spines"), cl)
        if (!is.null(W)) add_val("plant_organ_measured", "stipe", "petiole_width", W$v, W$txt)
      }
      # "Plants ascending or erect, to 12 cm tall": a plant height with no organ context
      if (k == "organ" && lv == "plants") {
        tall <- first_meas(cl, "tall|high")
        if (!is.null(tall)) add_val("whole_plant", NA_character_, "plant_height", tall$v / 1000, tall$txt)
      }
      if (k == "organ" && lv %in% c("branches", "branchlet systems", "branchlets", "stems", "shoots")) {
        tall <- first_meas(cl, "tall|high")
        if (!is.null(tall)) add_val("plant_organ_measured", lv, "plant_height", tall$v / 1000, tall$txt)
        L <- first_meas(cl, "long", other = "leaves|hairs?|scales?|spikes?|strobil[iu]s?|sporophylls?|zones?")
        if (!is.null(L)) add_val("plant_organ_measured", lv, "stem_length", L$v, L$txt)
        dm <- carry_to(L, first_meas(cl, "diam\\.?|diameter|thick|wide", other = "leaves|hairs?|scales?|spikes?|strobil[iu]s?|sporophylls?"), cl)
        if (!is.null(dm)) add_val("plant_organ_measured", lv, "stem_diameter", dm$v, dm$txt)
      }
      if (k %in% c("leaf", "leaflet")) {
        other <- "stipes?|stalks?|scales?|hairs?|sori|sorus|indusi[ua]m?|veins?|costae?|midribs?|wings?|auricles?|teeth|lobes?|segments?|pinnae|pinnules?|bulbils?|spores?|sporangia|apex|tips?|bases?|rachis|areoles|glands?|margins?"
        if (k == "leaflet") other <- str_remove_all(other, "\\|lobes\\?|\\|segments\\?|\\|pinnae|\\|pinnules\\?")
        pre <- if (k == "leaf") "leaf" else "leaflet"
        b <- by_meas(cl)
        L <- first_meas(cl, "long|tall|in length", other = other)
        W <- carry_to(L, first_meas(cl, "wide|broad|across|diam\\.?|in width", other = other), cl)
        if (!is.null(b) && (is.null(L) || str_locate(cl, fixed(b$txt))[1] < str_locate(cl, fixed(L$txt))[1])) {
          add_val("leaf_type", lv, paste0(pre, "_length"), b$L, b$txt); add_val("leaf_type", lv, paste0(pre, "_width"), b$W, b$txt)
        } else {
          if (!is.null(L)) {
            if (k == "leaf" && str_detect(L$txt, "tall") && str_detect(lv, "fronds")) add_val("plant_organ_measured", "fronds", "plant_height", L$v / 1000, L$txt)
            else add_val("leaf_type", lv, paste0(pre, "_length"), L$v, L$txt)
          }
          if (!is.null(W)) add_val("leaf_type", lv, paste0(pre, "_width"), W$v, W$txt)
        }
        # a stipe described inside a frond clause ("Fronds with a distinct stipe 5-10 cm long")
        if (k == "leaf" && str_detect(lv, "fronds")) {
          st <- str_match(cl, paste0("\\bstipes?\\b[^,;]{0,30}?(", meas, ")(?:\\s+or more)?\\s*long"))
          if (!is.na(st[1])) add_val("plant_organ_measured", "stipe", "petiole_length", parse_meas(str_match(st[2], meas)[1:5]), st[2])
        }
        # counts: fronds per plant; pinnae (individual or in pairs), lobes
        if (k == "leaf" && str_detect(lv, "fronds")) {
          fc <- str_match(cl, regex("^(?:sterile |fertile )?fronds?\\s+(?:usually\\s+|commonly\\s+)?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?=,|\\s|$)(?!\\s*(?:mm|cm|m|by|x)\\b)(?!\\s*-?\\s*pinnate)", ignore_case = TRUE))
          if (!is.na(fc[1])) add_val("leaf_type", lv, "leaf_count", count_val(fc[2], fc[3], fc[1]), fc[1])
        }
        pr <- str_match(cl, regex(paste0("(?:in|with|of|bearing)?\\s*(?:up to |c\\. |about )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?\\s+pairs(?: of ((?:", lvl_mod, " )?", leaflet_noun, "))?"), ignore_case = TRUE))
        if (!is.na(pr[1])) {
          tgt <- if (!is.na(pr[4])) normalise_level(str_match(pr[4], paste0("^(", lvl_mod, ")\\s"))[2], str_extract(pr[4], paste0(leaflet_noun, "$"))) else if (k == "leaflet") lv else "pinnae"
          add_val("leaf_type", tgt, "leaflet_count_pairs", count_val(pr[2], pr[3], pr[1]), pr[1])
        } else {
          pc <- str_match(cl, regex(paste0("(?:with|of|bearing|into)\\s+(?:up to |c\\. |about )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?\\s+(?:[a-z-]+,?\\s+){0,6}?((?:", lvl_mod, " )?", leaflet_noun, ")\\b"), ignore_case = TRUE))
          if (!is.na(pc[1]) && !str_detect(pc[1], "rows?|veins")) {
            tgt <- normalise_level(str_match(pc[4], paste0("^(", lvl_mod, ")\\s"))[2], str_extract(pc[4], paste0(leaflet_noun, "$")))
            add_val("leaf_type", tgt, "leaflet_count", count_val(pc[2], pc[3], pc[1]), pc[1])
          }
          if (k == "leaflet") {
            pc2 <- str_match(cl, regex(paste0("^(?:[a-z ,]+?)?", leaflet_noun, "\\s+([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?=[, ]|$)(?!\\s*(?:mm|cm|m|pairs)\\b)"), ignore_case = TRUE))
            if (!is.na(pc2[1])) add_val("leaf_type", lv, "leaflet_count", count_val(pc2[2], pc2[3], pc2[1]), pc2[1])
          }
        }
      }
    }
    # ---- leaf-part attributes at the current division: margin, apex shape, hairs
    if (k %in% c("leaf", "leaflet")) {
      lc <- str_to_lower(cl)
      ctx <- "leaf_type"
      mg <- collapse_unique(c(
        if (str_detect(lc, "margins?[^,;]{0,20}\\bentire\\b|\\bentire\\b(?! segments)")) "entire",
        if (str_detect(lc, "crenat|crenulate")) "toothed_crenate",
        if (str_detect(lc, "serrat|serrulate")) "toothed_serrate",
        if (str_detect(lc, "dentate|denticulate")) "toothed_dentate",
        if (str_detect(lc, "\\btoothed\\b|\\bteeth\\b") && !str_detect(lc, "crenat|serrat|dentat")) "toothed"))
      if (str_detect(lc, "margin|entire|toothed|crenat|serrat|dentat")) add_cat(ctx, lv, "leaf_margin", mg, recs$clause[i])
      # every apex term in the apex phrase ("apex acute to acuminate" -> acute acuminate), in text order
      ap_phr <- str_match(lc, "\\bap(?:ex|ices)\\s+([^,;(]*)")[2]
      if (!is.na(ap_phr)) {
        ap_phr <- str_remove_all(ap_phr, "(?:rarely|occasionally|sometimes) [a-z]+")
        apx <- map_by_dictionary(ap_phr, c(acuminate = "acuminate", acute = "acute", obtuse = "obtuse", rounded = "rounded",
                                           apiculate = "apiculate", attenuate = "acuminate", caudate = "acuminate", mucronate = "apiculate"))
        add_cat(ctx, lv, "leaf_apex_shape", apx, recs$clause[i])
      }
      hr <- collapse_unique(c(if (str_detect(lc, "\\bglabrous\\b(?! except| apart)")) "glabrous",
                              if (str_detect(lc, "\\bhairy\\b|\\bhairs\\b|pubescent|tomentose|pilose")) "hairy"))
      if (!recs$part[i] || str_detect(lc, "surface")) if (str_detect(lc, "glabrous|hairy|hairs|pubescent|tomentose|pilose")) add_cat(ctx, lv, "leaf_hairs_adult_leaves", hr, recs$clause[i])
      if (str_detect(lc, "(?<!not |sub)glaucous")) add_cat(ctx, lv, "leaf_glaucousness", "glaucous", recs$clause[i])
    }
  }
  rows <- bind_rows(rows); cat_vals <- bind_rows(cat_vals)

  # ---- whole-frond categorical traits (main row) from Fronds / Lamina clauses
  frond_txt <- paste(recs$clause[recs$kind == "leaf" & !recs$part & str_detect(coalesce(recs$level, ""), "fronds|lamina")], collapse = " | ")
  lam_txt <- paste(recs$clause[recs$kind == "leaf" & !recs$part & str_detect(coalesce(recs$level, ""), "lamina|fronds")], collapse = " | ")
  div_txt <- str_remove_all(expand_degrees(lam_txt), "pinnae|pinnules|pinna\\b|pinnate venation")
  division <- map_by_dictionary(div_txt, division_map)
  division_desc <- collapse_unique(str_extract_all(lam_txt, regex("[^,|]*\\b(?:[1-4](?:\\s*(?:-|or)\\s*[1-4])?-)?(?:pinnate|pinnatifid|pinnatisect|pinnatipartite|bipinnate|tripinnate|trifoliate|palmatifid|palmately|dichotomously|simple)\\b[^,|]*", ignore_case = TRUE))[[1]] %>% str_squish(), " | ")
  compound <- collapse_unique(c(
    if (str_detect(str_to_lower(lam_txt), "\\bsimple\\b|\\bentire\\b|\\bundivided\\b") || (!is.na(division) && str_detect(division, "pinnatifid|lobed|pinnatisect|bipartite|tripartite") && !str_detect(division, "compound|bipinnate$|tripinnate|trifoliate"))) "simple",
    if (!is.na(division) && str_detect(division, "pinnately_compound|bipinnate|tripinnate|trifoliate|palmately_compound")) "compound"))
  lam_shape_txt <- str_remove_all(str_to_lower(paste(recs$clause[recs$kind == "leaf" & !recs$part & str_detect(coalesce(recs$level, ""), "lamina")], collapse = " | ")),
                                  "pinnae[^,|]*|pinnules[^,|]*|segments[^,|]*|lobes[^,|]*|in (?:cross[- ])?section|at (?:the )?(?:base|apex)|bases?[^,|]*|ap(?:ex|ices)[^,|]*")
  shape <- map_by_dictionary(lam_shape_txt, leaf_shape_map)
  hetero <- case_when(str_detect(str_to_lower(frond_txt), "not dimorphic|monomorphic|isophyllous|homophyllous") ~ "isophyllous",
                      str_detect(str_to_lower(frond_txt), "dimorphic|anisophyllous|heterophyllous") ~ "anisophyllous", TRUE ~ NA_character_)
  hetero_desc <- grab_sentences(desc, "dimorphic|monomorphic|nest fronds|anisophyll")
  arr_txt <- str_to_lower(paste(recs$clause[(recs$kind == "leaf" & str_detect(coalesce(recs$level, ""), "fronds")) | recs$level %in% "stipe"], collapse = " | "))
  arrangement <- map_by_dictionary(arr_txt, c("tufted" = "clustered", "clustered" = "clustered", "crowded" = "crowded",
                                              "scattered" = "scattered", "spaced" = "scattered", "distant" = "scattered",
                                              "in 2 rows" = "distichous", "in two rows" = "distichous", "rosette" = "rosette",
                                              "spiral" = "spiral"))
  arrangement_desc <- collapse_unique(str_extract_all(arr_txt, "[^,|]*\\b(?:tufted|clustered|crowded|scattered|spaced|distant|in 2 rows|in two rows|rosette|spiral)\\b[^,|]*")[[1]] %>% str_squish(), " | ")

  # ---- rhizome
  rh_txt <- paste(recs$clause[recs$level %in% "rhizome" & !recs$part], collapse = " | ")
  rl <- str_to_lower(rh_txt)
  rhizome_form <- collapse_unique(c(
    if (str_detect(rl, "short[- ]?(?:\\[?to long\\]?[- ])?(?:to [a-z]+[- ])?creeping|shortly creeping|very short[- ]creeping|short- to (?:medium|moderately long|long)-creeping")) "short_creeping",
    if (str_detect(rl, "long[- ]creeping|widely creeping|wide[- ]creeping|to long-creeping|\\[to long\\]|moderately long-creeping")) "long_creeping",
    if (str_detect(rl, "\\bslender\\b|\\bwiry\\b|\\bthin\\b")) "slender",
    if (str_detect(rl, "\\bstout\\b|\\bthick\\b|\\brobust\\b|massive")) "stout",
    if (str_detect(rl, "branched|branching") && !str_detect(rl, "unbranched")) "branched",
    if (str_detect(rl, "\\bwoody\\b")) "woody"))
  rh_habit <- collapse_unique(c(
    if (nchar(rh_txt) > 0) "rhizomatous",
    if (str_detect(rl, "\\berect\\b|forming (?:an? )?(?:[a-z]+ )*trunk")) "erect",
    if (str_detect(rl, "creeping")) "creeping",
    if (str_detect(rl, "climbing|scandent")) "climbing",
    if (str_detect(rl, "decumbent")) "decumbent",
    if (str_detect(rl, "stoloniferous") && !str_detect(rl, "stolons lacking")) "stoloniferous",
    if (str_detect(rl, "subterranean|underground")) "subterranean"))

  # ---- substrate from the opening of "Habit and habitat" (and the description's own opening)
  hab_s <- sentences(hab)
  sub_txt <- str_to_lower(paste(c(hab_s[1], str_extract(desc, "^[^.]*(?:epiphyt|lithophyt|terrestrial|aquatic)[^.]*")), collapse = " "))
  sub_rules <- tribble(
    ~pattern,                                                           ~value,
    "hemi-?epiphyt\\w*",                                                "hemiepiphyte",
    "(?<!hemi-|hemi)\\bepiphyt\\w*|\\bas an epiphyte",                  "epiphyte",
    "lithophyt\\w*|\\bon (?:[a-z]+ (?:and |or )?){0,3}(?:rocks?|boulders?|sandstone|cliffs?|cliff faces|rock faces|rock walls|ledges)\\b|in (?:damp )?crevices", "lithophyte",
    "\\bterrestrial\\b",                                                "terrestrial",
    "free-floating|floating aquatic",                                   "aquatic_floating",
    "semi-?aquatic",                                                    "semiaquatic",
    "(?<!semi-|semi)\\baquatic\\b",                                     "aquatic",
    # growing in tarns (deeper standing water) is aquatic; swamps, marshes, rockpools and "tarn situations" are not
    "\\b(?:in|from) (?:[a-z]+ ){0,2}tarns?\\b",                         "aquatic",
    "amphibious",                                                       "semiaquatic")
  qualifier_re <- "(?:\\bor|,|\\band|^)\\s*(rarely|occasionally|sometimes|usually|often|commonly|mostly|mainly)\\s+(?:as an? )?$"
  gm <- sub_txt; sh <- tibble(pos = integer(0), value = character(0), qualifier = character(0))
  for (j in seq_len(nrow(sub_rules))) {
    loc <- str_locate(gm, sub_rules$pattern[j])
    if (is.na(loc[1])) next
    q <- str_match(str_sub(sub_txt, max(1, loc[1] - 20), loc[1] - 1), qualifier_re)[, 2]
    sh <- add_row(sh, pos = loc[1], value = sub_rules$value[j], qualifier = q)
    str_sub(gm, loc[1], loc[2]) <- strrep(" ", loc[2] - loc[1] + 1)
  }
  if ("aquatic_floating" %in% sh$value) sh <- sh %>% filter(value != "aquatic")
  # "grows on rocks and on trees", "mats on rocks, logs and tree trunks": epiphyte when no substrate adjective names it
  if (!"epiphyte" %in% sh$value && !"hemiepiphyte" %in% sh$value && !str_detect(sub_txt, "climb") &&
      str_detect(sub_txt, "\\bon (?:[a-z]+,? (?:and |or )?){0,3}(?:trees|tree trunks|trunks)\\b"))
    sh <- add_row(sh, pos = str_locate(sub_txt, "\\btrees|tree trunks|trunks")[1], value = "epiphyte", qualifier = NA)
  # no substrate word, but rooted in soil ("often in sandy soils")
  if (!nrow(sh) && str_detect(sub_txt, "\\bsoils?\\b|\\bloam\\b")) sh <- add_row(sh, pos = 1L, value = "terrestrial", qualifier = NA)
  sh <- sh %>% arrange(pos) %>% distinct(value, .keep_all = TRUE)
  substrate <- collapse_unique(sh$value)
  substrate_qualified <- if (any(!is.na(sh$qualifier))) paste0(sh$value, "=", coalesce(sh$qualifier, "usually"), collapse = ";") else NA_character_
  substrate_desc <- na_if_empty(coalesce(hab_s[1], str_extract(desc, "^[^.]*(?:epiphyt|lithophyt|terrestrial|aquatic)[^.]*")))

  # ---- growth form (taxonomic group is definitional) + climbing
  climbing <- str_detect(str_to_lower(paste(hab, desc)), "climbing|climber|scandent|high-climbing|twining")
  gf <- collapse_unique(c(
    if (fam %in% c("Lycopodiaceae", "Selaginellaceae", "Isoetaceae")) "lycophyte" else "fern",
    if (is_treefern) "palmoid",
    if (climbing) "climber_herbaceous"))
  gf_desc <- collapse_unique(c(grab_sentences(paste(hab, desc), "climbing|climber|scandent|twining"), if (is_treefern) grab_sentences(desc, "trunk|rhizome erect|rhizome to")), " ")
  if (climbing) rh_habit <- collapse_unique(c(str_split(coalesce(rh_habit, ""), " ")[[1]], "climbing"))

  # ---- plant_height_type: why the plant's length is not a plant height (a self-supporting, ground-rooted plant with
  # upright fronds or shoots). Frond length is never used as plant height for these taxa, and plant_height_type is
  # written for those left with no plant height. Evidence (verbatim) in plant_height_type_description.
  #   climbing: climbers and scramblers (Gleicheniaceae: indeterminate fronds in tiers, scrambling over vegetation)
  #   epiphyte: epiphytic or hemiepiphytic, terrestrial absent or only occasional
  #   aquatic / aquatic_floating: frond length follows water depth
  #   leaf_tuft: pendulous fronds, lamina or shoots
  #   prostrate: lycophytes whose main stems are creeping / appressed to the substrate (and not also erect)
  terr_ok <- "terrestrial" %in% sh$value[is.na(sh$qualifier) | !sh$qualifier %in% c("rarely", "occasionally", "sometimes")]
  sub_ev <- coalesce(substrate_desc, paste("substrate:", substrate))
  ht <- list(
    climbing = if (climbing || str_detect(str_to_lower(paste(hab, desc)), "scrambl") || fam == "Gleicheniaceae")
      coalesce(grab_sentences(paste(hab, desc), "climb|scandent|scrambl|twining"), "Gleicheniaceae: fronds of indeterminate growth, in tiers"),
    epiphyte = if (any(sh$value %in% c("epiphyte", "hemiepiphyte")) && !terr_ok) sub_ev,
    aquatic_floating = if ("aquatic_floating" %in% sh$value) sub_ev,
    aquatic = if (!"aquatic_floating" %in% sh$value && any(sh$value %in% c("aquatic", "semiaquatic"))) sub_ev,
    leaf_tuft = grab_sentences(desc, "\\b(?:fronds?|laminae?|shoots?|branches|stems?|plants?)\\b[^.;]{0,60}\\b(?:pendulous|pendent|hanging)\\b"),
    prostrate = if (fam %in% c("Lycopodiaceae", "Selaginellaceae"))
      grab_sentences(desc, "^(?:main |primary )?stems?\\b[^.]*\\b(?:creeping|prostrate|trailing|appressed to (?:the )?substrat)", exclude = "\\berect\\b"))
  ht <- discard(ht, ~ is.null(.x) || is.na(.x))
  height_type <- if (length(ht)) paste(names(ht), collapse = " ") else NA_character_
  height_type_desc <- if (length(ht)) paste(unique(unlist(ht)), collapse = " | ") else NA_character_

  # ---- vegetative reproduction (explicit only)
  veg_txt <- str_to_lower(desc)
  veg_desc <- grab_sentences(desc, "stoloniferous|stolons?\\b|bulbiferous|bulbils?|proliferous|plantlets?|suckering|suckers|buds?\\b|gemmae", exclude = "stolons lacking|not proliferous|non-proliferous")
  # frond-tip rooting ("apex prolonged into a whiplike stolon rooting at tip") is above-ground, not a rhizome stolon;
  # "roots proliferous" / proliferous root tubers are root buds
  root_prolif <- str_detect(veg_txt, "roots? (?:with [a-z ]*)?proliferous|roots? proliferous|proliferous tubers")
  veg_noroot <- str_remove_all(veg_txt, "roots? (?:with [a-z ]*)?proliferous[a-z ]*|roots? proliferous")
  clonal <- collapse_unique(c(
    if (str_detect(veg_txt, "stoloniferous|stolons?\\b") && !str_detect(veg_txt, "stolons lacking|whiplike stolon")) "stolon",
    if (str_detect(veg_noroot, "bulbiferous|bulbils?|proliferous|plantlets?|gemmae|whiplike stolon") && !str_detect(veg_noroot, "not proliferous|non-proliferous")) "aboveground_clonal",
    if (root_prolif) "root_buds",
    if (str_detect(veg_txt, "suckering|suckers")) "rhizome"))
  veg <- if (!is.na(clonal)) "vegetative" else NA_character_

  # ---- reproductive structures kept as text (no AusTraits traits for sori, indusia, spores)
  sori_desc <- collapse_unique(recs$clause[str_detect(recs$clause, regex("^(?:sori|sorus|sporangia|indusi|receptacle|sporogenous|spikes?|strobil|sporophylls?|fertile)", ignore_case = TRUE))], " | ")
  spore_desc <- collapse_unique(recs$clause[str_detect(recs$clause, regex("^(?:spores|megaspores|microspores|exospores|perispores)", ignore_case = TRUE))], " | ")

  main <- tibble(
    taxon_name = taxon, family = fam, common_name = na_if_empty(r[["Common name"]]),
    lucid_url = r[["url"]], apni_url = na_if_empty(r[["apni_url"]]),
    plant_growth_form_description = gf_desc, plant_growth_form = gf,
    plant_growth_substrate_description = substrate_desc, plant_growth_substrate = substrate, substrate_qualified = substrate_qualified,
    rhizome_description = na_if_empty(rh_txt), rhizome_form = rhizome_form, stem_growth_habit = rh_habit,
    frond_description = na_if_empty(frond_txt),
    leaf_heterogeneity_description = hetero_desc, leaf_heterogeneity = hetero,
    leaf_arrangement_description = arrangement_desc, leaf_arrangement = arrangement,
    lamina_description = na_if_empty(paste(recs$clause[recs$kind == "leaf" & !recs$part & str_detect(coalesce(recs$level, ""), "lamina")], collapse = " | ")),
    leaf_lamina_division_description = division_desc, leaf_lamina_division = division, leaf_compoundness = compound,
    leaf_shape = shape,
    vegetative_reproduction_description = veg_desc, vegetative_reproduction_ability = veg, clonal_spread_mechanism = clonal,
    sori_description = sori_desc, spore_description = spore_desc,
    habitat_description = na_if_empty(hab), distribution_description = na_if_empty(r[["Distribution"]]),
    natural_history = na_if_empty(r[["Natural history"]]), similar_species = na_if_empty(r[["Similar species"]]),
    plant_height_type_description = height_type_desc, plant_height_type = height_type
  )
  list(main = main, rows = rows, cats = cat_vals)
}

res <- map(seq_len(nrow(d)), ~ extract_one(d[.x, ]))
main <- map_dfr(res, "main")
num_rows <- map2_dfr(res, d$taxon_name, ~ if (nrow(.x$rows)) mutate(.x$rows, taxon_name = .y) else NULL)
cat_rows <- map2_dfr(res, d$taxon_name, ~ if (nrow(.x$cats)) mutate(.x$cats, taxon_name = .y) else NULL)
if (Sys.getenv("LUCID_QA") != "") saveRDS(list(main = main, num = num_rows, cats = cat_rows), Sys.getenv("LUCID_QA"))

# ---------------------------------------------------------------- explicit fixes
# Ophioderma pendulum: "Common stipe and sterile lamina forming a continuous ... blade, 25-200 cm long" is the
# whole blade, not the stipe
num_rows <- num_rows %>% filter(!(taxon_name == "Ophioderma pendulum" & level == "stipe"))

# ---------------------------------------------------------------- assemble context rows
# one row per (taxon, context, division/organ); first statement wins when a division is described twice
num_rows <- num_rows %>% group_by(taxon_name, context, level, trait) %>% slice(1) %>% ungroup()
cat_rows <- cat_rows %>% group_by(taxon_name, context, level, trait) %>% slice(1) %>% ungroup()

# rows flagged as wrong (exclude_row_reason; filtered out by custom_R_code in metadata.yml). The flag is per context
# row, so only rows holding nothing but the wrong value are flagged; flagged rows are not used for derived heights
flag_rows <- tribble(
  ~taxon_name, ~context, ~level, ~exclude_row_reason,
  # "small aborted fronds <12 cm long and large fully developed fronds 30-85 cm long": "<12 cm" read as the frond length
  "Crepidomanes aphlebioides", "leaf_type", "fronds", "frond length read from the small aborted fronds (<12 cm long)")
num_rows <- num_rows %>% left_join(flag_rows, by = c("taxon_name", "context", "level"))

# plant_height (plant_organ_measured = fronds) when the sheet gives no explicit height (as Wenk_2025): the longest frond
# length given (fronds, sterile / fertile fronds, ...), else stipe + the longest lamina length. Not for taxa with a
# plant_height_type (epiphytes, climbers, aquatics, pendulous fronds), whose fronds do not give a height.
has_height <- unique(num_rows$taxon_name[num_rows$trait == "plant_height"])
no_frond_height <- main$taxon_name[!is.na(main$plant_height_type)]
usable <- num_rows %>% filter(is.na(exclude_row_reason), !taxon_name %in% c(has_height, no_frond_height))
longest <- function(df) df %>% mutate(top = coalesce(max, mean, min)) %>% group_by(taxon_name) %>%
  slice_max(top, n = 1, with_ties = FALSE) %>% ungroup() %>% select(-top)
frond_h <- usable %>% filter(context == "leaf_type", str_detect(level, "fronds$"), trait == "leaf_length") %>% longest() %>%
  mutate(txt = paste0("[", level, " length] ", txt), context = "plant_organ_measured", level = "fronds", trait = "plant_height",
         across(c(min, max, mean), ~ .x / 1000))
# stipe + lamina: lower bounds (min or mean) and upper bounds (max or mean) are summed; equal sums are a mean,
# a missing lower or upper bound leaves a maximum or a minimum
stipe_lamina_h <- inner_join(
  usable %>% filter(level == "stipe", trait == "petiole_length"),
  usable %>% filter(context == "leaf_type", str_detect(level, "lamina$"), trait == "leaf_length") %>% longest(),
  by = "taxon_name", suffix = c("_s", "_l")) %>%
  filter(!taxon_name %in% frond_h$taxon_name) %>%
  mutate(lo = coalesce(min_s, mean_s) + coalesce(min_l, mean_l), hi = coalesce(max_s, mean_s) + coalesce(max_l, mean_l)) %>%
  transmute(taxon_name, context = "plant_organ_measured", level = "fronds", trait = "plant_height",
            min = ifelse(is.na(hi) | (!is.na(lo) & lo < hi), lo, NA) / 1000,
            max = ifelse(is.na(lo) | (!is.na(hi) & lo < hi), hi, NA) / 1000,
            mean = ifelse(!is.na(lo) & !is.na(hi) & lo == hi, lo, NA) / 1000,
            txt = paste0("[stipe + ", level_l, " length] ", txt_s, " + ", txt_l))
num_rows <- bind_rows(num_rows, frond_h, stipe_lamina_h)

# plant_height_type only where no plant height is recorded
height_taxa <- unique(num_rows$taxon_name[num_rows$trait == "plant_height" & is.na(num_rows$exclude_row_reason)])
main <- main %>% mutate(plant_height_type = ifelse(taxon_name %in% height_taxa, NA, plant_height_type),
                        plant_height_type_description = ifelse(is.na(plant_height_type), NA, plant_height_type_description))

num_wide <- num_rows %>%
  mutate(across(c(min, max, mean), ~ round(.x, 4))) %>%
  group_by(taxon_name, context, level) %>%
  mutate(measurement_description = paste(unique(txt), collapse = "; ")) %>% ungroup() %>%
  select(-txt) %>%
  pivot_wider(names_from = trait, values_from = c(min, max, mean), names_glue = "{trait}_{.value}")
# keep each trait's _min, _max and _mean together; drop value columns no taxon fills
num_cols <- names(num_wide)[str_detect(names(num_wide), "_(min|max|mean)$")]
num_cols <- num_cols[map_lgl(num_cols, ~ any(!is.na(num_wide[[.x]])))]
num_traits <- sort(unique(str_remove(num_cols, "_(min|max|mean)$")))
num_wide <- num_wide %>% select(taxon_name, context, level, exclude_row_reason, measurement_description,
                                all_of(intersect(as.vector(t(outer(num_traits, c("_min", "_max", "_mean"), paste0))), num_cols)))
cat_wide <- cat_rows %>% select(taxon_name, context, level, trait, value, txt) %>%
  pivot_wider(names_from = trait, values_from = c(value, txt), names_glue = "{trait}{ifelse(.value == 'txt', '_description', '')}")
ctx_rows <- full_join(num_wide, cat_wide, by = c("taxon_name", "context", "level")) %>%
  mutate(leaf_type = ifelse(context == "leaf_type", level, NA), plant_organ_measured = ifelse(context == "plant_organ_measured", level, NA)) %>%
  select(-context, -level) %>%
  relocate(starts_with("leaf_margin"), starts_with("leaf_apex"), starts_with("leaf_hairs"), starts_with("leaf_glauc"), .after = last_col())

# substrate with a frequency qualifier ("Terrestrial or rarely lithophytic") -> one row per substrate
sub_rows <- main %>% filter(!is.na(substrate_qualified)) %>%
  select(taxon_name, plant_growth_substrate_description, substrate_qualified) %>%
  separate_rows(substrate_qualified, sep = ";") %>%
  separate(substrate_qualified, c("plant_growth_substrate", "commonness_qualifier"), sep = "=")
main <- main %>% mutate(plant_growth_substrate = ifelse(is.na(substrate_qualified), plant_growth_substrate, NA)) %>% select(-substrate_qualified)

id_cols <- c("taxon_name", "family", "common_name", "lucid_url")
ids <- main %>% select(all_of(id_cols))
out_df <- bind_rows(main, inner_join(ids, ctx_rows, by = "taxon_name"), inner_join(ids, sub_rows, by = "taxon_name")) %>%
  relocate(leaf_type, plant_organ_measured, commonness_qualifier, exclude_row_reason, .after = lucid_url) %>%
  mutate(row_order = case_when(!is.na(commonness_qualifier) ~ 1, !is.na(plant_organ_measured) ~ 2, !is.na(leaf_type) ~ 3, TRUE ~ 0)) %>%
  arrange(taxon_name, row_order, plant_organ_measured, leaf_type) %>% select(-row_order)

# numeric ranges inside text written as "a--b" (a hyphen or en dash between numbers is read as a formula/date by Excel)
out_df <- out_df %>% mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--")))
readr::write_csv(out_df, out, na = "")
