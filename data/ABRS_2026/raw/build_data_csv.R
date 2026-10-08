# Builds data/ABRS_2026/data.csv from the Flora of Australia (ABRS) online profiles scraped to
# foa_profiles.csv (austraits.build-data.scraping.scripts/data_extra).
# Not run by the build -- kept for provenance / re-extraction.
#
# The source is large (~20,600 profiles with text, 306 families), so it is processed family by family:
# `families_done` lists the families extracted and checked so far (env FOA_FAMILIES="A,B" overrides it
# for QA runs). Taxa are sorted by family, then scientific name; genus- and family-rank profiles are
# not written (their descriptions summarise variation across species).
#
# Descriptions are Flora-of-Australia organ-led prose ("Shrub to 2 m high. Leaves: petiole ...; blade ...").
# Each sentence is split into clauses (";", ":") and comma tokens; a token led by an organ noun sets the
# current organ, tokens led by a part noun ("base", "apex", "margins", "tube", "lobes") belong to that part
# only, other tokens inherit the current organ. Traits are then read from the organ "units".
# Categorical traits are written as `<trait>_description` (verbatim unit text) + `<trait>` (mapped to
# traits.yml levels, space-delimited, in the order they appear in the text).
# Commonness modifiers ("tree or rarely a shrub", "white, more rarely pink") split a trait onto rows
# with `commonness_qualifier` (unqualified alternatives = "usually"), for every categorical trait.
# Numbers: plant height / width in m, all other lengths in mm (source mixes mm, cm and m); parenthetical
# extremes "(0.9-) 10 (-30) m" are dropped so the typical range is recorded.
# Male / female flower measurements go on `entity_measured` rows; split calendars on `population_region` rows.
# Traits not (yet) in traits.yml are kept under descriptive candidate names (see report / metadata notes).

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(tibble); library(tidyr); library(readr)
})

src <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra/foa_profiles.csv"
out <- "data/ABRS_2026/data.csv"
families_done <- c("Acanthaceae", "Achariaceae", "Actinidiaceae", "Agapanthaceae", "Aizoaceae", "Akaniaceae", "Alismataceae",
                   "Alliaceae", "Alseuosmiaceae", "Alstroemeriaceae", "Amaranthaceae", "Amaryllidaceae", "Anacardiaceae",
                   "Anarthriaceae", "Annonaceae", "Apiaceae", "Apocynaceae", "Apodanthaceae", "Aponogetonaceae",
                   "Aquifoliaceae", "Araceae", "Araliaceae", "Araucariaceae", "Arecaceae", "Argophyllaceae", "Aristolochiaceae",
                   "Asparagaceae", "Asphodelaceae", "Aspleniaceae", "Asteliaceae",
                   # Asteraceae (765 taxa) skipped for now (user's choice); resumed with the smaller families after it
                   "Atherospermataceae", "Austrobaileyaceae", "Balanopaceae", "Balanophoraceae", "Balsaminaceae", "Basellaceae", "Bataceae",
                   "Berberidaceae", "Berberidopsidaceae", "Betulaceae", "Bignoniaceae", "Bixaceae", "Blandfordiaceae", "Blechnaceae",
                   "Boraginaceae", "Boryaceae", "Brassicaceae", "Bromeliaceae", "Burseraceae", "Byblidaceae", "Cabombaceae", "Cactaceae",
                   "Calycanthaceae", "Campanulaceae", "Campynemataceae", "Cannabaceae", "Cannaceae", "Capparaceae", "Cardiopteridaceae",
                   # Caryophyllaceae - Celastraceae etc. not yet done; Chenopodiaceae done out of order (user's request)
                   "Chenopodiaceae",
                   "Caryophyllaceae", "Casuarinaceae", "Celastraceae", "Centrolepidaceae", "Cephalotaceae", "Ceratophyllaceae",
                   "Chrysobalanaceae", "Cistaceae", "Cleomaceae", "Clusiaceae", "Colchicaceae", "Combretaceae", "Commelinaceae",
                   "Connaraceae", "Convolvulaceae", "Cornaceae", "Corsiaceae", "Corynocarpaceae", "Costaceae", "Cucurbitaceae",
                   "Cupressaceae", "Cyatheaceae", "Cycadaceae", "Cymodoceaceae", "Cyperaceae", "Dasypogonaceae", "Datiscaceae",
                   "Davalliaceae", "Dennstaedtiaceae", "Dichapetalaceae", "Dicksoniaceae", "Dilleniaceae",
                   "Dioscoreaceae", "Dipteridaceae", "Doryanthaceae", "Droseraceae", "Dryopteridaceae", "Ecdeiocoleaceae", "Elaeagnaceae",
                   "Elaeocarpaceae", "Elatinaceae", "Equisetaceae", "Ericaceae",
                   "Eriocaulaceae", "Erythroxylaceae", "Escalloniaceae", "Euphorbiaceae", "Eupomatiaceae", "Fagaceae", "Flagellariaceae",
                   "Frankeniaceae", "Gelsemiaceae", "Gentianaceae", "Geraniaceae", "Gesneriaceae", "Gleicheniaceae", "Goodeniaceae",
                   "Grammitidaceae", "Griseliniaceae", "Grossulariaceae", "Gunneraceae", "Gyrostemonaceae", "Haemodoraceae", "Haloragaceae",
                   "Hamamelidaceae", "Hanguanaceae", "Hemerocallidaceae", "Hernandiaceae", "Himantandraceae", "Hydatellaceae", "Hydrangeaceae",
                   "Hydrocharitaceae")
fams <- if (Sys.getenv("FOA_FAMILIES") != "") str_split(Sys.getenv("FOA_FAMILIES"), ",")[[1]] else families_done

d_all <- read_csv(src, show_col_types = FALSE, col_types = cols(.default = "c"), guess_max = 1e5)
# ~230 scraped profiles lack rank and family: rank from the name, family from the genus (other profiles, else taxon_list.csv)
gen_fam <- d_all %>% filter(!is.na(family)) %>% transmute(genus = word(scientific_name, 1), family) %>% distinct(genus, .keep_all = TRUE)
tl_fam <- read_csv("config/taxon_list.csv", show_col_types = FALSE, col_types = cols(.default = "c")) %>% distinct(genus, family) %>% filter(!is.na(genus)) %>% distinct(genus, .keep_all = TRUE)
d_all <- d_all %>%
  mutate(rank = case_when(!is.na(rank) ~ rank, str_detect(scientific_name, " subsp\\. ") ~ "subspecies", str_detect(scientific_name, " var\\. ") ~ "variety",
                          str_detect(scientific_name, " f\\. ") ~ "form", str_detect(scientific_name, "^[A-Z][a-z-]+ (?:[a-z-]+|sp\\. .+)$") ~ "species", TRUE ~ NA_character_),
         genus = word(scientific_name, 1)) %>%
  left_join(gen_fam %>% rename(fam_g = family), by = "genus") %>% left_join(tl_fam %>% rename(fam_t = family), by = "genus") %>%
  mutate(family = coalesce(family, fam_g, fam_t)) %>% select(-genus, -fam_g, -fam_t)
# genus and family profiles: used only for copy-down of categorical traits that hold throughout the group
d_higher <- d_all %>% filter(family %in% fams, rank %in% c("genus", "family"), !is.na(Description),
                             !str_detect(Description, "^\\s*Pending\\b"))
d <- d_all %>%
  filter(family %in% fams, rank %in% c("species", "subspecies", "variety", "form")) %>%
  mutate(Description = ifelse(str_detect(coalesce(Description, ""), "^\\s*Pending\\b"), NA, Description)) %>%
  filter(!is.na(Description) | !is.na(Phenology)) %>%
  arrange(family, scientific_name)

# ---------------------------------------------------------------- source typos / fixes
fix_text <- tribble(
  ~taxon, ~find, ~replace,
  # "Tree or shrub to 2-7 mm high" (2-7 m intended; a mangrove tree)
  "Avicennia integra", "2–7 mm high", "2–7 m high",
  # "Prostate" for prostrate (family description; kept for completeness)
  "Acanthaceae", "Prostate to erect", "Prostrate to erect",
  # Brunoniella spiciflora: blade "1.2--2.5 mm wide" on a 38-91 mm long ovate blade (cm intended)
  "Brunoniella spiciflora", "1.2--2.5 mm wide", "1.2--2.5 cm wide",
  # Dyschoriste depressa: seed "c. 2 mm long, c. 1.5 cm wide" (mm intended)
  "Dyschoriste depressa", "c. 1.5 cm wide", "c. 1.5 mm wide",
  # Crossandra infundibuliformis: an undershrub "to 50 (–120) mm high" with leaves 3–10 cm long (cm intended)
  "Crossandra infundibuliformis", "to 50 (–120) mm high", "to 50 (–120) cm high",
  # Pachystachys coccinea: garbled "filame clavatents"
  "Pachystachys coccinea", "filame clavatents", "filaments",
  # Allium triquetrum: perianth "segments 10–18 cm long" on a 1-2 cm bulb plant (mm intended)
  "Allium triquetrum", "segments 10–18 cm long", "segments 10–18 mm long",
  # Achyranthes bidentata: lamina "(15–) 25–100 (–220) m long" (mm intended)
  "Achyranthes bidentata", "(–220) m long", "(–220) mm long",
  # Ichnocarpus frutescens: corolla "lobes lanceolate, 2–6 m long" (mm intended)
  "Ichnocarpus frutescens", "lobes lanceolate, 2–6 m long", "lobes lanceolate, 2–6 mm long",
  # Aponogeton distachyos: "Perianth segment 1 ... 10–15 (–30) cm long" on a 5.5 cm inflorescence (mm intended)
  "Aponogeton distachyos", "10–15 (–30) cm long", "10–15 (–30) mm long",
  # Rhaphidophora australasica: "conic style to c. 0.6 m long" (mm intended)
  "Rhaphidophora australasica", "style to c. 0.6 m long", "style to c. 0.6 mm long",
  # Aponogetonaceae family description: words run together ("monoecious ordioecious", "perfect orimperfect")
  "Aponogetonaceae", "ordioecious", "or dioecious",
  "Aponogetonaceae", "orimperfect", "or imperfect",
  "Aponogetonaceae", "floatingand", "floating and",
  "Aponogetonaceae", "white topink", "white to pink",
  # Sisymbrium irio: reversed range "Sepals erect, 3.5–3.0 mm long"
  "Sisymbrium irio", "3.5–3.0 mm long", "3.0–3.5 mm long",
  # Silene flos-cuculi: cauline leaves "40–90 cm long" (mm intended); Centrolepis inconspicua heads "1–2 m wide" (mm intended)
  "Silene flos-cuculi", "linear-lanceolate, 40–90 cm long", "linear-lanceolate, 40–90 mm long",
  "Centrolepis inconspicua", "oblong, 1–2 m wide", "oblong, 1–2 mm wide",
  # Mesua sp. Boonjie: fruit "25–3 (–35) mm wide" (25–30 intended, as the length "25 (–35) mm long")
  "Mesua sp. Boonjie (A.K.Irvine 1218)", "25–3 (–35) mm wide", "25–30 (–35) mm wide",
  # Trichosanthes subvelutina: "Male flowers in 4–8-flowered racemes, pubescent, 12–20 cm long" (the racemes are 12–20 cm)
  "Trichosanthes subvelutina", "racemes, pubescent, 12–20 cm long", "racemes; racemes pubescent, 12–20 cm long",
  # Hibbertia: garbled ranges ("0-5-0.8 mm" = 0.5-0.8; "(2.3-) 35-8 (-14.6)" = 3.5-8; "(6-) 10-5 (-25)" = 10-15, flagged)
  "Hibbertia praestans", "anthers ovate, 0-5-0.8 mm long", "anthers ovate, 0.5–0.8 mm long",
  "Hibbertia exutiacies", "(2.3–) 35–8 (–14.6) mm long", "(2.3–) 3.5–8 (–14.6) mm long",
  "Hibbertia hypericoides subsp. hypericoides", "(6–) 10–5 (–25) mm long", "(6–) 10–15 (–25) mm long",
  # Einadia hastata: "Spreading perennial to 1.5 mm high" (m intended)
  "Einadia hastata", "to 1.5 mm high", "to 1.5 m high",
  # Cardamine corymbosa: fruits "10–30 mm long, 0.5–1 (–2) m wide" (mm intended)
  "Cardamine corymbosa", "0.5–1 (–2) m wide", "0.5–1 (–2) mm wide"
)
for (i in seq_len(nrow(fix_text))) {
  fx <- function(z) z %>% mutate(Description = ifelse(scientific_name == fix_text$taxon[i],
                                                     str_replace(Description, fixed(fix_text$find[i]), fix_text$replace[i]), Description))
  d <- fx(d); d_higher <- fx(d_higher)
}

# ---------------------------------------------------------------- helpers
fixed_re <- function(x) str_replace_all(x, "([.()\\[\\]{}+*?^$|\\\\])", "\\\\\\1")
na_if_empty <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
collapse_unique <- function(x, sep = " ") {
  x <- unique(unlist(str_split(x[!is.na(x) & x != ""], " ")))
  if (sep != " ") x <- unique(x)
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}
collapse_text <- function(x, sep = " | ") {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

# Some profiles stitch together several cited treatments ("... (Prescott 1984: 37-38). Prostrate or scrambling herb ...
# (Green 1994: 77) See also ..."). The newest is kept; uncited trailing text is the current FoA treatment.
pick_treatment <- function(x) {
  if (is.na(x)) return(list(text = NA_character_, used = NA_character_))
  y <- str_replace_all(x, "\\s*\\[[^\\]]*\\]", "")
  cit <- str_locate_all(y, "\\((?:[A-Z][^()]*?)\\b(?:1[89][0-9]{2}|20[0-9]{2})[a-z]?(?::[^()]*)?\\)\\.?(?=\\s+[A-Z]|\\s*$)")[[1]]
  if (nrow(cit) < 1) return(list(text = x, used = NA_character_))
  starts <- c(1, cit[, 2] + 1); ends <- c(cit[, 2], nchar(y))
  chunks <- str_squish(str_sub(y, starts, ends))
  cites <- c(str_sub(y, cit[, 1], cit[, 2]), NA)
  keep <- nchar(chunks) > 80 & !str_detect(chunks, "^See ")
  # an in-text citation ("... (Zich et al. 2020). Shoots ...") is not a treatment boundary: the next chunk must restart
  # a description (habit word, or the same opening word as the first chunk)
  opener <- "^(?:Herbs?|Shrubs?|Trees?|Annuals?|Perennials?|Prostrate|Erect|Decumbent|Climbers?|Vines?|Lianas?|Plants?|Stout|Robust|Small|Bulbous|Bulbaceous|Succulent|Undershrubs?|Subshrubs?|Tufted|Rhizomatous|Glabrous|Aquatic|Epiphytic|Dioecious|Monoecious)\\b"
  first_word <- word(chunks[1], 1)
  restart <- c(TRUE, str_detect(chunks[-1], opener) | word(chunks[-1], 1) == first_word)
  if (!all(restart[keep])) {
    # merge chunks that do not restart into the preceding one
    grp <- cumsum(restart)
    chunks <- map_chr(split(chunks, grp), ~ paste(.x, collapse = " "))
    cites <- map_chr(split(cites, grp), ~ tail(.x, 1))
    keep <- nchar(chunks) > 80 & !str_detect(chunks, "^See ")
  }
  if (sum(keep) < 2) return(list(text = x, used = NA_character_))
  yr <- ifelse(is.na(cites), Inf, suppressWarnings(as.numeric(map_chr(str_extract_all(coalesce(cites, ""), "(?:1[89]|20)[0-9]{2}"), ~ if (length(.x)) max(.x) else NA_character_))))
  i <- which(keep)[which.max(yr[keep] + seq_along(yr[keep]) * 1e-6)]
  list(text = str_remove(chunks[i], "\\s*\\([^()]*\\b(?:1[89]|20)[0-9]{2}[^()]*\\)\\.?$"),
       used = ifelse(is.na(cites[i]), "current FoA text (after earlier cited treatments)", str_remove_all(cites[i], "^\\(|\\)\\.?$")))
}

prep <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- str_replace_all(x, "[\u2013\u2014\u2212\u2011\u2010]", "-")
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_replace_all(x, "\\bFlowersand\\b", "Flowers and")
  x <- str_replace_all(x, "\\bInilorescence", "Inflorescence")
  # PDF glitches: soft hyphens inside numbers ("1\u00ad.5"), a letter l for the digit 1 ("l.4-1.5 mm")
  x <- str_remove_all(x, "\u00ad")
  x <- str_replace_all(x, "(?<![A-Za-z])l\\.(?=[0-9])", "1.")
  x <- str_replace_all(x, "(?<=[0-9])\\s*-\\s*-\\s*(?=[0-9])", "-")
  # square brackets hold other regions' values or references ("[3-5 in India]", "[See Barker (1986)]")
  x <- str_replace_all(x, "\\s*\\[[^\\]]*\\]", "")
  # missing space after a full stop ("forming a lignotuber.Bark usually smooth")
  x <- str_replace_all(x, "(?<=[a-z)])\\.(?=[A-Z][a-z])", ". ")
  x <- str_replace_all(x, "\u00b1\\s*", "")
  # "1.5-2. 5 cm" -> "1.5-2.5 cm"
  x <- str_replace_all(x, "(?<=[0-9])\\. (?=[0-9]+\\s?(?:mm|cm|m)\\b)", ".")
  # uncertain terms ("?Erect, ?tuberous herb", "?November"): the term is dropped; "rarely? white" keeps the qualifier
  x <- str_replace_all(x, "(?<![A-Za-z0-9])\\?[A-Za-z][A-Za-z-]*", "")
  x <- str_replace_all(x, "\\?", "")
  # words run together before a month or in phenology phrases ("FlowersJan.-Mar.", "fruitsSept.", "andfruits", "allmonths")
  x <- str_replace_all(x, "(?<=[a-z])(?=(?:Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)[a-z]*(?=[.,;\\s-]|$))", " ")
  x <- str_replace_all(x, "(?i)\\b(fruits|flowers|and)(?=(?:all|fruits|flowers|sporadically)\\b)", "\\1 ")
  x <- str_replace_all(x, "\\ballmonths\\b", "all months")
  # missing space after a comma ("erect stem,with short lactifers")
  x <- str_replace_all(x, ",(?=[a-z])", ", ")
  # states that do not occur in Australia ("acaulescent (not in Australia)", "spiny (not in Australia)") are dropped
  # "convex or (not in Australia) truncate or conical", "simple (not in Australia) or imparipinnate", "acaulescent (not in Australia),"
  x <- str_remove_all(x, "\\s*\\bor \\(\\s*not in Australia\\s*\\) [a-z-]+(?: (?:or|and) [a-z-]+)*")
  x <- str_remove_all(x, "[^,;:.()]*\\(\\s*not in Australia\\s*\\)\\s+or\\s+")
  x <- str_remove_all(x, "(?:,\\s*|\\bor\\s+)?[^,;:.()]*\\(\\s*not in Australia\\s*\\)")
  x <- str_replace_all(x, ",\\s*(?=[,;])", "")
  str_squish(x)
}
# hedged statements are not scored ("probably white with purple markings", "apparently up to 1 m high")
dehedge <- function(x) {
  if (is.na(x)) return(x)
  x <- str_remove_all(x, regex("\\b(?:probably|possibly|perhaps) (?:dependent|depending) on [a-z ]+", ignore_case = TRUE))
  # ("Annual or possibly ephemeral herb": the hedge covers "ephemeral", not the growth-form noun)
  str_squish(str_remove_all(x, regex("\\b(?:probably|possibly|perhaps|apparently|presumably)\\b(?:[^,;.)]*?(?=\\s+(?:herbs?|shrubs?|subshrubs?|trees?|climbers?|vines?|palms?|grass(?:es)?|sedges?)\\b)|[^,;.)]*)", ignore_case = TRUE)))
}

sentences <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  x <- str_replace_all(x, "\\b([A-Z])\\.(?=\\s)", "\\1\u00a7")
  x <- str_replace_all(x, "\\b(subsp|var|sp|spp|ssp|cv|Mt|St|ca|c|approx|al|cf|vs|pers|comm|ed|eds|e\\.g|i\\.e|fig|figs|Fl|Herb|Austral|Bot|J|Proc|Soc|Mus|incl)\\.(?=\\s)", "\\1\u00a7")
  s <- str_split(x, "(?<=[.])\\s+(?=[A-Z(])")[[1]]
  s <- str_replace_all(s, "\u00a7", ".")
  s <- str_remove(str_squish(s), "\\.$")
  s[s != ""]
}

# ---------------------------------------------------------------- measurements
# Parenthetical extremes ("(3.7-) 10 (-30) m", "1.5-3 (rarely to 4) cm") are kept: they are turned into markers
# (<lo:N>, <hi:N>, <q:WORD:N>) that the measurement pattern carries along, then parsed into extreme_min / extreme_max.
num <- "[0-9]+(?:\\.[0-9]+)?"
dash <- "\\s*(?:-|to|or)\\s*"
unit_mult <- c(mm = 1, cm = 10, dm = 100, m = 1000)
to_mm <- function(v, u) ifelse(is.na(v), NA_real_, as.numeric(v) * unname(unit_mult[u]))
lo_unit <- "(?:\\s?(mm|cm|dm|m)(?=\\s*(?:-|to)\\s*[0-9]))?"
mk_lo <- paste0("(?:\u27e8(?:lo|q:[a-z ]+):", num, "\u27e9\\s*)?")
mk_hi <- paste0("(?:\\s*\u27e8(?:hi|q:[a-z ]+):", num, "\u27e9)?")
meas <- paste0("(?:(?:up to|to|about|approximately|c\\.|ca\\.|less than|more than|under|mostly|usually|commonly|generally|often|frequently|reaching)\\s+)?",
               mk_lo, "(", num, ")", lo_unit, "(?:", dash, "(", num, "))?", mk_hi, "\\s?(mm|cm|dm|m)\\b", mk_hi)
mark_extremes <- function(x) {
  if (is.na(x)) return(x)
  x <- str_replace_all(x, paste0("\\(\\s*(", num, ")\\s*-?\\s*\\)\\s*-?\\s*(?=[0-9])"), "\u27e8lo:\\1\u27e9")
  x <- str_replace_all(x, paste0("\\s*\\(\\s*-\\s*(", num, ")\\s*(?:mm|cm|dm|m)?\\s*\\)"), "\u27e8hi:\\1\u27e9")
  x <- str_replace_all(x, paste0("(?<=[0-9])\\s*\\(\\s*(", num, ")\\s*\\)(?=\\s*(?:mm|cm|dm|m)\\b)"), "\u27e8hi:\\1\u27e9")
  x <- str_replace_all(x, paste0("\\s*\\((very rarely|rarely|occasionally|sometimes|exceptionally|or)\\s+(?:to |up to |as (?:much|long|large|high) as )?(", num, ")\\s*(?:mm|cm|dm|m)?\\s*\\)"), "\u27e8q:\\1:\\2\u27e9")
  x <- str_replace_all(x, "\\s*\\((?:rarely|occasionally|sometimes|very rarely|exceptionally|or more|more)[^)]*\\)", "")
  str_squish(x)
}
# markers back to the source wording, for the *_description columns
unmark <- function(x) {
  x <- str_replace_all(x, "\u27e8lo:([0-9.]+)\u27e9\\s*", "(\\1-) ")
  x <- str_replace_all(x, "\u27e8hi:([0-9.]+)\u27e9", " (-\\1)")
  str_squish(str_replace_all(x, "\u27e8q:([a-z ]+):([0-9.]+)\u27e9", " (\\1 \\2)"))
}
# extremes in a matched measurement, in mm (unit `u`), placed below / above the typical range
extremes_of <- function(txt, u, typ_min, typ_max, scale_unit = TRUE) {
  f <- function(v) if (scale_unit) to_mm(v, u) else as.numeric(v)
  lo <- str_match(txt, "\u27e8lo:([0-9.]+)\u27e9")[2]; hi <- str_match(txt, "\u27e8hi:([0-9.]+)\u27e9")[2]
  q <- str_match_all(txt, "\u27e8q:[a-z ]+:([0-9.]+)\u27e9")[[1]][, 2]
  emin <- if (!is.na(lo)) f(lo) else NA_real_; emax <- if (!is.na(hi)) f(hi) else NA_real_
  for (v in q) { vv <- f(v); ref <- coalesce(typ_min, typ_max)
    if (!is.na(ref) && vv < ref) emin <- vv else emax <- vv }
  c(extreme_min = emin, extreme_max = emax)
}
parse_meas <- function(m) {
  u <- m[5]; u1 <- ifelse(is.na(m[3]), u, m[3])
  lo <- to_mm(m[2], u1); hi <- to_mm(m[4], u)
  v <- c(min = ifelse(is.na(hi), NA, lo), max = ifelse(is.na(hi), lo, hi))
  c(v, extremes_of(m[1], u, v[["min"]], v[["max"]]))
}
meas_of <- function(txt) parse_meas(str_match(txt, regex(meas, ignore_case = TRUE))[1:5])
# first measurement followed by one of `words`, not closely preceded by another organ noun
first_meas <- function(x, words, other = NULL) {
  if (is.na(x) || x == "") return(NULL)
  x <- mark_extremes(x)
  m <- str_locate_all(x, regex(paste0(meas, "(?:\\s+or more)?\\s*(?:", words, ")\\b"), ignore_case = TRUE))[[1]]
  for (i in seq_len(nrow(m))) {
    gap <- str_sub(x, 1, m[i, 1] - 1)
    if (!is.null(other) && str_detect(gap, regex(paste0("\\b(?:", other, ")\\b[^,;|]{0,25}$"), ignore_case = TRUE))) next
    # surface outgrowths anywhere earlier in the segment own the number ("studded with papillae ... to 0.2 mm long")
    if (!is.null(other) && str_detect(gap, regex("\\b(?:papillae|hairs?|scales?|spines?|bristles|glands?|prickles|warts|tubercles)\\b[^,;|]*$", ignore_case = TRUE))) next
    # "0.5-6 cm long peduncles": the number belongs to the organ named right after it
    after <- str_sub(x, m[i, 2] + 1, m[i, 2] + 25)
    if (!is.null(other) && !str_detect(after, regex("^\\s*(?:including|incl\\.?|excluding|excl\\.?|with|without)\\b", ignore_case = TRUE)) &&
        str_detect(after, regex(paste0("^\\s*(?:[a-z-]+\\s+)?(?:", other, ")\\b"), ignore_case = TRUE))) next
    txt <- str_sub(x, m[i, 1], m[i, 2])
    return(list(v = meas_of(txt), txt = unmark(txt), pos = m[i, 1]))
  }
  NULL
}
# "6-9 x 5.2-6 mm", "c. 10 x 7 mm", "15-30 cm x 4-6 mm"
by_meas <- function(x, other = NULL) {
  if (is.na(x) || x == "") return(NULL)
  x <- mark_extremes(x)
  pat <- paste0("(?<![0-9.])", mk_lo, "(", num, ")(?:", dash, "(", num, "))?", mk_hi, "\\s?(mm|cm|dm|m)?\\s*(?:by|x|\u00d7)\\s*", meas)
  loc <- str_locate_all(x, pat)[[1]]
  for (i in seq_len(nrow(loc))) {
    gap <- str_sub(x, 1, loc[i, 1] - 1)
    if (!is.null(other) && str_detect(gap, regex(paste0("\\b(?:", other, ")\\b[^,;|]{0,25}$"), ignore_case = TRUE))) next
    m <- str_match(str_sub(x, loc[i, 1], loc[i, 2]), pat)
    u <- m[8]; u1 <- coalesce(m[4], u)
    L <- c(min = ifelse(is.na(m[3]), NA, to_mm(m[2], u1)), max = to_mm(coalesce(m[3], m[2]), u1))
    first_part <- str_split_fixed(m[1], "\\s*(?:by|x|\u00d7)\\s*", 2)[1]
    L <- c(L, extremes_of(first_part, u1, L[["min"]], L[["max"]]))
    W <- parse_meas(c(m[1], m[5:8]) %>% { .[1] <- str_split_fixed(m[1], "\\s*(?:by|x|\u00d7)\\s*", 2)[2]; . })
    return(list(L = L, W = W, txt = unmark(m[1]), pos = loc[i, 1]))
  }
  NULL
}
# length / width of an organ unit: "a x b" or "a long, b wide"
dims <- function(x, other = NULL, len_words = "long|in length", wid_words = "wide|broad|across|diam|diameter|in diameter|in width") {
  b <- by_meas(x, other); L <- first_meas(x, len_words, other); W <- first_meas(x, wid_words, other)
  if (!is.null(b) && (is.null(L) || b$pos < L$pos)) {
    return(list(L = list(v = b$L, txt = b$txt), W = list(v = b$W, txt = b$txt)))
  }
  list(L = L, W = W)
}
count_range <- function(m_lo, m_hi) {
  lo <- suppressWarnings(as.numeric(m_lo)); hi <- suppressWarnings(as.numeric(m_hi))
  # counts are alternatives, in any order ("Sepals 5 or 3")
  if (!is.na(lo) && !is.na(hi) && lo > hi) { t <- lo; lo <- hi; hi <- t }
  c(min = ifelse(is.na(hi), NA, lo), max = ifelse(is.na(hi), lo, hi))
}
# a count with optional extremes: "(12-) 28", "2 or 3 (rarely 4)"
cnt <- paste0(mk_lo, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?", mk_hi)
parse_count <- function(txt) {
  core <- str_remove_all(txt, "\u27e8[^\u27e9]*\u27e9")
  m <- str_match(core, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?")
  v <- count_range(m[2], m[3])
  c(v, extremes_of(txt, NA, v[["min"]], v[["max"]], scale_unit = FALSE))
}

# ---------------------------------------------------------------- term scanning with commonness qualifiers
qual_words <- c("very rarely", "more rarely", "less often", "less commonly", "less frequently", "more often", "more commonly",
                "rarely", "occasionally", "sometimes", "often", "usually", "mostly", "mainly", "commonly", "frequently",
                "seldom", "generally", "infrequently", "typically", "predominantly", "normally", "uncommonly",
                "exceptionally", "sporadically", "chiefly", "rarely also", "also")
qual_words <- setdiff(qual_words, c("rarely also", "also"))
rare_quals <- c("very rarely", "more rarely", "rarely", "occasionally", "sometimes", "seldom", "infrequently",
                "uncommonly", "exceptionally", "sporadically", "less often", "less commonly", "less frequently")
qual_re <- paste0("\\b(", paste(qual_words, collapse = "|"), ")\\b")
filler_words <- c("a", "an", "the", "or", "and", "to", "even", "small", "tall", "large", "low", "dwarf", "robust", "slender",
                  "compact", "weak", "stout", "sprawling", "scrambling", "climbing", "erect", "prostrate", "spreading",
                  "woody", "herbaceous", "perennial", "annual", "multi-stemmed", "much-branched", "tufted", "becoming",
                  "forming", "as", "more", "less", "somewhat", "slightly", "shortly", "very", "densely", "sparsely",
                  "minutely", "diffusely", "pale", "dark", "deep", "bright", "light", "faintly", "also", "almost", "nearly", "quite",
                  "rather", "finely", "thinly", "moderately", "scattered", "sparingly", "in", "with", "be", "being", "is",
                  "are", "may", "can", "a", "rather", "strongly", "weakly", "narrowly", "broadly", "only", "partly",
                  "partially", "distinctly", "obscurely", "conspicuously", "dull", "glossy", "shiny", "much", "well", "variously", "irregularly")
# growth form: a commonness word governs the form only across articles / size words ("tree or rarely a small shrub");
# in "Creeping or sometimes erect herb" it governs "erect", not the herb
gf_fillers <- c("a", "an", "the", "or", "even", "small", "tall", "large", "low", "dwarf", "robust", "slender", "stout", "compact", "weak", "as", "becoming", "forming", "be", "being", "is", "are", "may", "can", "also",
                "woody", "herbaceous", "sprawling", "scrambling", "climbing", "twining", "scandent")
region_words <- "far|extreme|south|north|east|west|central|coastal|inland|upland|lowland|montane|alpine|arid|tropical|subtropical|temperate|southern|northern|eastern|western|north-eastern|north-western|south-eastern|south-western|northeastern|northwestern|southeastern|southwestern|north-east|north-west|south-east|south-west"
places <- "qld|queensland|nsw|new south wales|victoria|vic|tasmania|tas|western australia|south australia|northern territory|nt|cape york(?: peninsula)?|arnhem land|kimberley|pilbara|top end|nullarbor|central australia|the (?:north|south|east|west)|lord howe island|norfolk island|christmas island"
region_re <- paste0("\\b(?:in|on|at|from|towards|throughout|across)\\s+(?:the\\s+)?((?:(?:", region_words, ")\\b[a-z -]*?\\b(?:parts?|populations?|range|areas?|regions?|districts?|forms?|plants?|specimens?|collections?|states?|coast|ranges)\\b(?:\\s+of\\s+(?:its|the)\\s+(?:range|distribution))?|(?:", region_words, ")\\b(?:\\s+(?:of\\s+(?:its|the)\\s+(?:range|distribution)))?|(?:[a-z]+ )?(?:", places, ")\\b))")
negators <- "\\b(?:not|non|without|lacking|never|scarcely|hardly|devoid of|free of)\\b[^,;|.)]*"

scan_terms <- function(text, dict, neg = NULL, generic_neg = TRUE, prep_fun = NULL, fillers = filler_words) {
  empty <- tibble(pos = integer(), end = integer(), value = character(), term = character(), qual = character(), region = character())
  if (is.na(text) || !nzchar(text)) return(empty)
  t0 <- str_to_lower(text)
  if (!is.null(prep_fun)) t0 <- prep_fun(t0)
  t <- t0
  hits <- list()
  find <- function(dd) {
    if (is.null(dd)) return(invisible())
    keys <- names(dd)[order(-nchar(names(dd)))]
    for (k in keys) {
      loc <- str_locate_all(t, paste0("(?<![a-z])(?:", k, ")(?![a-z]|-(?:like|shaped))"))[[1]]
      for (i in seq_len(nrow(loc))) {
        hits[[length(hits) + 1]] <<- tibble(pos = loc[i, 1], end = loc[i, 2], value = unname(dd[[k]]), term = str_sub(t0, loc[i, 1], loc[i, 2]))
        str_sub(t, loc[i, 1], loc[i, 2]) <<- strrep("\u0001", loc[i, 2] - loc[i, 1] + 1)
      }
    }
  }
  find(neg)
  if (generic_neg) t <- str_replace_all(t, negators, function(z) strrep("\u0002", nchar(z)))
  find(dict)
  if (!length(hits)) return(empty)
  h <- bind_rows(hits) %>% filter(!is.na(value)) %>% arrange(pos)
  if (!nrow(h)) return(empty)
  # qualifier: a commonness word earlier in the same comma segment, with only filler words between it and the term
  tq <- str_replace_all(t, "[\u0001\u0002]", " ")
  h$qual <- map_chr(h$pos, function(p) {
    pre <- str_sub(tq, 1, p - 1)
    b <- str_locate_all(pre, "[,;:(|]")[[1]]
    seg <- if (nrow(b)) str_sub(pre, max(b[, 1]) + 1) else pre
    q <- str_locate_all(seg, qual_re)[[1]]
    if (!nrow(q)) return(NA_character_)
    qi <- nrow(q)
    gap <- str_sub(seg, q[qi, 2] + 1)
    gap_words <- str_extract_all(gap, "[a-z0-9-]+")[[1]]
    if (all(gap_words %in% fillers)) str_sub(seg, q[qi, 1], q[qi, 2]) else NA_character_
  })
  # regional qualifier after the term, before the next term or comma ("becoming a shrub in southern part of its range")
  nxt <- c(h$pos[-1], nchar(tq) + 1L)
  h$region <- map2_chr(h$end, nxt, function(e, n) {
    post <- str_sub(tq, e + 1, n - 1)
    post <- str_split_fixed(post, "[,;|.]", 2)[1]
    str_squish(str_match(post, region_re)[2])
  })
  h
}

# records: one row per (trait, context) value
rec_cat <- function(trait, h, desc = NA_character_, ctx_type = NA_character_, ctx_value = NA_character_) {
  if (is.null(h) || !nrow(h)) return(NULL)
  if (!"region" %in% names(h)) h$region <- NA_character_
  if (!"term" %in% names(h)) h$term <- desc
  h <- h %>% mutate(src_term = ifelse(is.na(qual), term, paste(qual, term)),
                    src_term = ifelse(is.na(region), src_term, paste(src_term, "in", region)))
  # "tree or rarely a shrub": the unqualified alternative is the usual one; when the only qualifiers are
  # "usually"/"often"-type ("terminal spikes, usually with axillary clusters") unqualified values stay on the main row
  # "paripinnate or sometimes imparipinnate": alternatives that map to one level carry no commonness at that level
  if (n_distinct(h$value[h$value != ""]) <= 1 && all(is.na(h$region)) && any(is.na(h$qual)) && nrow(h) > 1) h$qual <- NA_character_
  qq <- h$qual[!is.na(h$qual)]
  default_q <- if (length(qq) && all(qq %in% rare_quals)) "usually" else NA_character_
  h <- h %>% mutate(qual = ifelse(is.na(qual) & is.na(region), default_q, qual)) %>%
    group_by(qual, region) %>% mutate(first = min(pos)) %>% ungroup() %>% arrange(first, pos)
  h %>% group_by(qual, region, first) %>%
    summarise(alts = n_distinct(value), value = collapse_unique(value), desc = paste(unique(src_term), collapse = "; "), .groups = "drop") %>% arrange(first) %>%
    transmute(trait = trait, value, desc, min = NA_real_, max = NA_real_, ctx_type = ctx_type, ctx_value = ctx_value,
              qualifier = qual, region, kind = "cat", alts)
}
rec_num <- function(trait, v, desc, ctx_type = NA_character_, ctx_value = NA_character_, scale = 1) {
  if (is.null(v) || all(is.na(v))) return(NULL)
  # a reversed range left after the source fixes is not written (listed in the QA output instead)
  if (!is.na(v[["min"]]) && !is.na(v[["max"]]) && v[["min"]] > v[["max"]]) { message("reversed range dropped: ", trait, " ", desc); return(NULL) }
  ex <- function(k) if (k %in% names(v)) unname(v[[k]]) * scale else NA_real_
  tibble(trait = trait, value = NA_character_, desc = desc, min = unname(v[["min"]]) * scale, max = unname(v[["max"]]) * scale,
         extreme_min = ex("extreme_min"), extreme_max = ex("extreme_max"),
         ctx_type = ctx_type, ctx_value = ctx_value, qualifier = NA_character_, kind = "num")
}

# ---------------------------------------------------------------- organ / part classification
rx <- function(x) regex(x, ignore_case = TRUE)
sec_rules <- tribble(
  ~sec, ~pat,
  "juvenile", "^(?:the )?(?:juvenile|seedling|young|intermediate|coppice)\\b[^,;:]{0,30}\\bleaves|^juvenile growth|^seedlings?\\b",
  "bark", "^(?:the )?bark\\b",
  "petiole", "^(?:the )?petioles?\\b",
  "lamina", "^(?:the )?(?:leaf |phyllode |adult |mature )?(?:blades?|laminae?|laminas)\\b",
  "leaflet", "^(?:the )?(?:lateral |terminal |basal |upper |lower |distal |proximal |primary |secondary |tertiary |ultimate |sterile |fertile |middle |median |central )?(?:leaflets?|pinnules?|pinnae|pinna|foliolules?)\\b",
  "leaf", "^(?:the )?(?:adult |mature |cauline |basal |stem |upper |lower |vegetative |rosette |radical |sterile |fertile |all |emergent |floating |submerged |aerial |floral |lateral )?(?:leaves|leaf|phyllodes?|cladodes?|phylloclades?|fronds?)\\b",
  "stipule", "^(?:the )?(?:stipules?|ochreae?|ochreas|stipular)\\b",
  "stem", "^(?:the )?(?:young |older |ultimate |upper |lower |main |flowering |fertile |sterile |lateral |primary |secondary |aerial |vegetative |mature )?(?:branchlets?|branches|branch|stems?|twigs?|culms?|shoots?|trunks?|canes?|internodes|axes)\\b",
  "underground", "^(?:the )?(?:rhizomes?|rootstocks?|stolons?|roots?|tubers?|bulbs?|corms?|lignotubers?|caudex|caudices|pseudobulbs?|taproots?)\\b",
  "peduncle", "^(?:the )?(?:common )?(?:peduncles?|scapes?)\\b",
  # Araceae spadix zones: not flowers or anthers ("male zone c. 8 cm long", "appendix ... to c. 15 cm long")
  "spadix_part", "^(?:the )?(?:appendix|appendices|(?:female|male|sterile|staminate|pistillate|neuter|fertile) zones?|(?:sterile |naked )?interstices?|neuter organs|staminodes zone)\\b",
  "inflorescence", "^(?:the )?(?:male |female |staminate |pistillate |terminal |axillary |lateral |fruiting )?(?:inflorescences?|racemes?|spikes?|panicles?|heads?|capitul(?:a|um)|umbels?|cymes?|corymbs?|thyrses?|fascicles?|synflorescences?|conflorescences?|spadix|spadices|glomerules?|cymules?|flower[- ]heads?|rachis|rachises)\\b",
  # Chenopodiaceae dispersal units: the fruiting bracteoles (Atriplex) or fruiting perianth (Maireana, Sclerolaena) enclose
  # the fruit and are what the flora measures (user's choice: fruit_* sizes on entity_measured rows)
  "diaspore", "^(?:the )?(?:fruiting|fruit-bearing) (?:bracteoles?|perianths?|perianth-tubes?|articles?|spikes?)\\b",
  "bract", "^(?:the )?(?:floral |flower |inner |outer |subtending |involucral |lower |upper |basal |lowest |uppermost )?(?:bracts?|bracteoles?|prophylls?|spathes?|involucres?|phyllaries)\\b",
  "pedicel", "^(?:the )?(?:pedicels?|pedicles?)\\b",
  "bud", "^(?:the )?(?:mature |flower )?buds?\\b",
  "flower", "^(?:the )?(?:males|females|staminate ones|pistillate ones)\\b|^(?:the )?(?:male |female |staminate |pistillate |bisexual |hermaphrodite |functionally (?:male|female) |sterile |fertile |ray |disc |marginal |central |outer |inner |lateral |terminal |chasmogamous |cleistogamous |mature |open )?(?:flowers?|florets?|spikelets?)\\b",
  "corona", "^(?:the )?(?:coronas?|paracorolla)\\b",
  "corolla", "^(?:the )?(?:(?:inner and outer|outer and inner|outer|inner|lateral|upper|lower|larger|smaller)(?: [0-9]+| two| three)? )?(?:sepals and petals|petals and sepals|corollas?|petals?|perianth|perianths|tepals?|standard|keel|labellum|dorsal sepal|lateral sepals|operculum|opercula)\\b",
  "calyx", "^(?:the )?(?:calyx|calyces|sepals?|hypanthium|hypanthia|outer perianth whorl)\\b",
  "androecium", "^(?:the )?(?:(?:abaxial|adaxial|longer|shorter|outer|inner|upper|lower|fertile|staminal) )?(?:pair of )?(?:stamens?|filaments?|anthers?|staminodes?|androecium|pollen|staminal (?:column|tube))\\b",
  "gynoecium", "^(?:the )?(?:ovary|ovaries|styles?|stigmas?|carpels?|pistils?|gynoecium|ovules?|placentae?|disc|column|indusi(?:a|um))\\b",
  "fruit", "^(?:the )?(?:(?:mature|ripe|fruiting|intact|hygroscopic|woody|papery|fleshy|dry|indehiscent|dehiscent|globose|ovoid|ellipsoid|young|submature|single|solitary|small|large|winged) )?(?:fruiting carpels?|apocarps?|monocarps?|syncarpi(?:a|um)|fruits?|capsules?|pods?|legumes?|drupes?|berr(?:y|ies)|nuts?|nutlets?|achenes?|cypselas?|cypselae|follicles?|mericarps?|samaras?|siliques?|siliquas?|siliculas?|silicles?|utricles?|infructescences?|cones?|syncarps?|caryops[ie]s|grains?|endocarps?|pericarps?|valves|cocci|schizocarps?|pyrenes?|anthocarps?|fruitlets?|syconi(?:a|um)|figs?|loments?|diaspores?)\\b",
  "seed", "^(?:the )?(?:seeds?|arils?|embryos?|endosperm|testa)\\b",
  "habit", "^(?:the )?(?:plants?|habit)\\b"
) %>% mutate(re = map(pat, rx))
part_re <- rx("^(?:the )?((?:upper |lower |abaxial |adaxial |both |outer |inner |dorsal |ventral )?(?:surfaces?|undersurface|under-surface|margins?|apex|apices|tips?|bases?|midribs?|mid-?veins?|veins?|venation|lateral veins|side-veins|reticulation|glands?|oil glands|indumentum|hairs|texture|teeth|lobes?|tube|throat|limb|lips?|wings?|beak|attachment(?: scar)?|appendages?|bladder appendages?|tubercles?|radicular tubercle|stipe|radicle|sheaths?|ligules?|auricles?|claws?|awns?|segments?|rays?|cotyledons|spines?|thorns|prickles|axes|axis|ribs?|nerves|pulvinus|gland|papillae|bladder cells|trichomes|scales|sheathing bases?))\\b")
# sub-parts written with their organ ("Corolla tube 2-3 mm long", "Calyx lobes ...")
organ_part_re <- rx("^(?:the )?(corolla|calyx|perianth|leaf|lamina|blade) (tube|lobes?|limb|segments?|teeth|throat|base|apex|margins?|surfaces?|upper surface|lower surface|undersurface)\\b")
entity_re <- rx("^(?:the )?(male|female|staminate|pistillate|functionally male|functionally female|bisexual|hermaphrodite)(?:s\\b|\\b)")

# Ferns and lycophytes (user's rules, as in LucidFerns_2026 / Wenk_2025): stipe = petiole, frond / lamina = leaf, pinnae /
# pinnules = leaflets with the division recorded in leaf_division; the fern rhizome is the stem (rhizome_form,
# stem_growth_habit), not a storage organ
fern_families <- c("Aspleniaceae", "Athyriaceae", "Blechnaceae", "Cyatheaceae", "Cystopteridaceae", "Davalliaceae", "Dennstaedtiaceae",
                   "Dicksoniaceae", "Diplaziopsidaceae", "Dipteridaceae", "Dryopteridaceae", "Equisetaceae", "Gleicheniaceae",
                   "Hymenophyllaceae", "Hypodematiaceae", "Isoetaceae", "Lindsaeaceae", "Lomariopsidaceae", "Lycopodiaceae", "Lygodiaceae",
                   "Marattiaceae", "Marsileaceae", "Matoniaceae", "Nephrolepidaceae", "Oleandraceae", "Ophioglossaceae", "Osmundaceae",
                   "Plagiogyriaceae", "Polypodiaceae", "Psilotaceae", "Pteridaceae", "Saccolomataceae", "Salviniaceae", "Schizaeaceae",
                   "Selaginellaceae", "Tectariaceae", "Thelypteridaceae", "Woodsiaceae", "Cibotiaceae", "Culcitaceae", "Loxsomataceae",
                   "Didymochlaenaceae", "Lonchitidaceae", "Azollaceae", "Anemiaceae")
lyco_families <- c("Lycopodiaceae", "Selaginellaceae", "Isoetaceae")
fern_mode <- FALSE
chenopod_mode <- FALSE
casuarina_mode <- FALSE
classify_token <- function(tok) {
  # Casuarinaceae: the woody "cone" is the infructescence (sizes on entity_measured = cone rows, as for chenopod dispersal units);
  # the samara is the fruit
  if (casuarina_mode && str_detect(tok, rx("^(?:the )?(?:mature )?cones?(?: body| bodies)?\\b"))) return(list(kind = "organ", sec = "diaspore", part = NA_character_, subj = "cone"))
  # Chenopodiaceae (Tecticornia, Salicornia): the stem "articles" (in other families articles are fruit segments)
  if (chenopod_mode && str_detect(tok, rx("^(?:the )?(?:vegetative |flowering |fertile |sterile )?articles?\\b"))) return(list(kind = "organ", sec = "stem", part = NA_character_, subj = "articles"))
  if (fern_mode && str_detect(tok, rx("^(?:the )?stipes?\\b"))) return(list(kind = "organ", sec = "petiole", part = NA_character_, subj = "stipe"))
  m <- str_match(tok, organ_part_re)
  if (!is.na(m[1])) {
    sec <- switch(str_to_lower(m[2]), corolla = "corolla", calyx = "calyx", perianth = "corolla", "lamina")
    return(list(kind = "part", sec = sec, part = str_to_lower(m[3]), subj = str_to_lower(m[2])))
  }
  for (i in seq_len(nrow(sec_rules))) {
    m <- str_match(tok, sec_rules$re[[i]])
    if (!is.na(m[1])) return(list(kind = "organ", sec = sec_rules$sec[i], part = NA_character_, subj = str_to_lower(str_squish(m[1]))))
  }
  m <- str_match(tok, part_re)
  if (!is.na(m[1])) return(list(kind = "part", sec = NA_character_, part = str_to_lower(m[2]), subj = NA_character_))
  list(kind = "none", sec = NA_character_, part = NA_character_, subj = NA_character_)
}

split_tokens <- function(cl) {
  # comma tokens, not splitting inside parentheses
  depth <- 0; out <- character(0); cur <- ""
  chars <- str_split(cl, "")[[1]]
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- max(0, depth - 1)
    if (ch == "," && depth == 0) { out <- c(out, cur); cur <- "" } else cur <- paste0(cur, ch)
  }
  str_squish(c(out, cur)) %>% .[. != ""]
}

units_of <- function(desc) {
  s <- sentences(desc)
  rows <- list()
  # "Similar to subsp. variabilis but leaves ... 8-10 mm wide": a comparison sentence is not the habit
  cmp_first <- length(s) > 0 && str_detect(s[1], rx("^(?:similar to|differs? from|differing from|as for|like)\\b"))
  # ("Similar to Allocasuarina paludosa. Usually monoecious shrub to 3 m high": the habit is the second sentence)
  habit_si <- if (cmp_first) 2L else 1L
  for (si in seq_along(s)) {
    # ";" and ":" inside parentheses do not end a clause ("(perianth lobes c. 6-8 mm long fide ...; perianth 10-15 mm long)")
    sx <- s[si]; depth <- 0; chars <- str_split(sx, "")[[1]]
    for (k in seq_along(chars)) { if (chars[k] == "(") depth <- depth + 1; if (chars[k] == ")") depth <- max(0, depth - 1)
      if (depth > 0 && chars[k] %in% c(";", ":")) chars[k] <- "\u00b6" }
    cls <- str_split(paste(chars, collapse = ""), ";\\s*|:\\s+")[[1]] %>% str_replace_all("\u00b6", ";") %>% str_squish() %>% .[. != ""]
    cur <- NULL; entity <- NA_character_
    for (ci in seq_along(cls)) {
      if (!is.null(cur)) cur$part <- NA_character_
      toks <- split_tokens(cls[ci])
      for (ti in seq_along(toks)) {
        tk <- toks[ti]
        cc <- classify_token(tk)
        em <- str_match(tk, entity_re)[2]
        if (!is.na(em) && cc$kind == "organ" && cc$sec %in% c("flower", "inflorescence")) entity <- str_to_lower(em)
        # "Panicle drooping, exserted, open, flowers rather widely spaced, usually 30-50 cm long": a flower-spacing phrase
        # inside an inflorescence clause does not change the organ
        if (cc$kind == "organ" && cc$sec == "flower" && !is.null(cur) && cur$sec == "inflorescence" && ti > 1 &&
            str_detect(tk, rx("\\b(?:spaced|crowded|dense(?:ly)?|arranged|borne|scattered|distant|congested)\\b"))) cc$kind <- "none"
        # parts of a chenopod dispersal unit ("Fruiting bracteoles ...; valves free ...", "Fruiting perianth ...; upper perianth erect")
        if (cc$kind == "organ" && !is.null(cur) && cur$sec == "diaspore" && cc$sec %in% c("fruit", "corolla") &&
            str_detect(coalesce(cc$subj, ""), "^(?:the )?(?:valves?|upper perianth|lower perianth|perianth(?: lobes)?|lobes|perianth segments)$")) {
          cc <- list(kind = "part", sec = "diaspore", part = cc$subj, subj = cur$subj)
        }
        # "spines 2 (rarely 4) ...; lateral pair 20-30 mm long": spine pairs of a dispersal unit
        if (!is.null(cur) && cur$sec == "diaspore" && str_detect(tk, rx("^(?:[a-z-]+ ){0,2}pairs?\\b"))) cc <- list(kind = "part", sec = "diaspore", part = "spines", subj = cur$subj)
        # "Inflorescences unisexual ...; females largest, to 2 m long": bare "males" / "females" after an inflorescence are inflorescences
        if (cc$kind == "organ" && cc$sec == "flower" && str_detect(coalesce(cc$subj, ""), "^(?:the )?(?:fe)?males$") && !is.null(cur) && cur$sec == "inflorescence") {
          cc <- list(kind = "organ", sec = "inflorescence", part = NA_character_, subj = cur$subj)
        }
        if (cc$kind == "organ") {
          cur <- cc; sec <- cc$sec; part <- NA_character_; subj <- cc$subj
        } else if (cc$kind == "part") {
          sec <- coalesce(cc$sec, cur$sec, if (si == habit_si) "habit" else "other"); part <- cc$part
          subj <- coalesce(cc$subj, cur$subj, NA_character_)
          # flower parts ("tube", "lobes", "lower lip") and named sub-organs ("radicle", "beak") persist to the end of the
          # clause; leaf-blade details listed inline ("base cuneate, concolorous, glossy green") apply to their token only
          # (parts of a chenopod dispersal unit persist too: "spines 2, rarely 3, opposite, 3-20 mm long")
          persist <- !is.na(cc$sec) || isTRUE(cur$sec == "diaspore") || !str_detect(part, "^(?:base|bases|apex|apices|tips?|margins?|teeth|midribs?|mid-?veins?|veins?|venation|lateral veins|side-veins|reticulation|glands?|oil glands|gland|nerves|ribs?|pulvinus|spines?|thorns|prickles|axis|axes|texture|sheathing bases?)$")
          if (persist) cur <- list(kind = "organ", sec = sec, part = part, subj = subj)
        } else {
          if (is.null(cur)) cur <- list(kind = "organ", sec = if (si == habit_si) "habit" else "other", part = NA_character_, subj = NA_character_)
          sec <- cur$sec; part <- cur$part; subj <- cur$subj
        }
        rows[[length(rows) + 1]] <- tibble(si = si, ci = ci, ti = ti, sec = sec, part = part, subj = subj, entity = entity, text = tk)
      }
    }
  }
  if (!length(rows)) return(tibble(si = integer(), ci = integer(), ti = integer(), sec = character(), part = character(),
                                   subj = character(), entity = character(), text = character(), unit = integer()))
  u <- bind_rows(rows)
  key <- paste(u$si, u$sec, coalesce(u$part, ""), coalesce(u$subj, ""), coalesce(u$entity, ""))
  u$unit <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
  u
}
unit_text <- function(u, secs, parts = NA, subj_re = NULL, entity = "any") {
  x <- u %>% filter(sec %in% secs)
  if (!identical(parts, "any")) x <- x %>% filter(if (all(is.na(parts))) is.na(part) else (is.na(part) | part %in% parts))
  if (!is.null(subj_re)) x <- x %>% filter(str_detect(coalesce(subj, ""), rx(subj_re)))
  if (entity == "none") x <- x %>% filter(is.na(entity))
  if (!nrow(x)) return(character(0))
  x %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
}
join_units <- function(x) if (!length(x)) NA_character_ else paste(x, collapse = " | ")

# ---------------------------------------------------------------- vocabularies
growth_form_dict <- c(
  "treeferns?" = "fern palmoid",
  "climbing herbs?|herbaceous (?:perennial |annual )?(?:climbers?|vines?|twiners?)|twining herbs?" = "climber_herbaceous",
  "woody (?:perennial )?(?:climbers?|vines?|twiners?|scramblers?)|lianas?|lianes?" = "climber_woody",
  # (user's rules: "vine" is herbaceous unless stated woody, "liana" woody, "twiner" stays generic)
  "vines?" = "climber_herbaceous",
  "climbers?|twiners?|scramblers?" = "climber",
  # "with woody base" -> subshrub (as in ABRS_2022)
  "(?:with |having )?(?:a )?woody base|woody at (?:the )?base|base woody" = "subshrub",
  "mallees?" = "mallee",
  "trees?|treelets?" = "tree",
  "sub-?shrubs?|under-?shrubs?|shrublets?" = "subshrub",
  "shrubs?|bush(?:es)?" = "shrub",
  "herbs?|forbs?|herbaceous perennials?|herbaceous annuals?" = "herb",
  "grass(?:es)?|sedges?|rush(?:es)?|grass-like plants?" = "graminoid",
  "tussocks?|tussock-forming" = "tussock",
  "hummocks?|hummock-forming" = "hummock",
  "palms?|cycads?" = "palmoid",
  "ferns?" = "fern",
  "geophytes?" = "geophyte"
)
life_history_dict <- c("short-lived perennials?" = "short_lived_perennial", "annuals?" = "annual", "biennials?" = "biennial",
                       "perennials?" = "perennial", "ephemerals?" = "ephemeral")
stem_habit_dict <- c(
  "erect|upright" = "erect", "prostrate|procumbent|prostate" = "prostrate", "decumbent" = "decumbent",
  "spreading" = "spreading", "sprawling" = "sprawling", "creeping" = "creeping", "trailing" = "prostrate",
  "scandent|climbing|twining|scrambling|clambering" = "climbing", "mat-forming|forming mats|mats" = "mat-forming",
  "cushion-forming|cushions?" = "cushion-forming", "tufted" = "tufted", "caespitose|cespitose" = "caespitose",
  "rhizomatous" = "rhizomatous", "stoloniferous" = "stoloniferous", "rosette|rosetted|basal-rosetted|rosulate" = "rosette",
  "pendulous|pendent|weeping" = "pendulous", "bushy" = "bushy", "open(?=,? (?:[a-z-]+ )?(?:shrubs?|trees?|subshrubs?|crowns?|habit|herbs?))" = "open", "(?:dense|compact)(?=,? (?:[a-z-]+ )?(?:shrubs?|subshrubs?|herbs?|mats?|cushions?|tussocks?|clumps?|habit|crowns?))" = "dense", "lax" = "lax",
  "low-growing" = "low-growing", "acaulescent|stemless" = "acaulescent", "arborescent" = "arborescent",
  "suffrutescent" = "suffrutescent", "submerged" = "submerged", "floating" = "floating")
branching_dict <- c(
  "multi-?stemmed" = "multi-stemmed", "many-stemmed" = "many-stemmed", "few-stemmed" = "few-stemmed",
  "single-stemmed|single stemmed|with a single stem" = "single_basal_stem", "unbranched" = "unbranched",
  "much[- ]branched|richly branched|profusely branched" = "much-branched",
  "sparsely[- ]branched|sparingly[- ]branched|few-branched" = "sparsely-branched", "densely[- ]branched" = "densely-branched",
  "openly[- ]branched" = "openly-branched", "intricately[- ]branched" = "intricately-branched",
  "divaricate(?:ly[- ]branched)?|divaricately[- ]branched|divaricating" = "divaricately-branched", "virgate" = "virgate",
  "branched|branching" = "branched")
defence_dict <- c(
  "spiny|spinose|spinescent|armed|axillary spines|spines|stipular spines|thorny|thorns" = "spine",
  "prickly|prickles" = "prickle", "stinging(?: hairs)?|urticating" = "stinging_or_irritant_hairs")
defence_neg <- c("unarmed|spineless|without (?:[a-z-]+ )?spines|lacking spines|spines absent" = "absent")
leaf_defence_dict <- c("pungent(?:-pointed)?|spine-tipped|spinose-tipped|pungent-tipped|sharply pointed" = "pungent_leaf_apex",
                       "spiny-toothed|spinose-dentate|spinulose-dentate|(?:the )?teeth (?:often |usually |sometimes )?(?:spine-tipped|spinose|pungent)|spiny teeth|spine-tipped teeth" = "sharp_pointed_defence")
storage_dict <- c(
  "lignotuber(?:ous)?|forming a lignotuber" = "lignotuber", "tuberous roots?|roots? tuberous|root[- ]tubers?|tuberous-rooted|tuberous rootstock" = "root_tuber",
  "stem[- ]tubers?" = "stem_tuber", "tuberous|tubers?" = "tuber", "bulbs?|bulbous" = "bulb", "corms?|cormous" = "corm",
  "pseudobulbs?" = "pseudobulb", "caudex|caudices" = "caudex", "fleshy rhizomes?|rhizomes? (?:thick, )?fleshy|succulent rhizomes?" = "rhizome_fleshy",
  "woody rhizomes?|rhizomes? woody" = "rhizome_woody", "rhizomes?|rhizomatous" = "rhizome")
storage_neg <- c("without (?:a )?lignotuber|lignotuber absent|lacking (?:a )?lignotuber|not forming a lignotuber|non-lignotuberous|lignotuber (?:not|lacking)" = "lignotuber_absent")
rhizome_form_dict <- c("short[- ]?(?:to long[- ])?creeping|shortly creeping|very short[- ]creeping" = "short_creeping",
                       "long[- ]creeping|widely creeping|wide[- ]creeping" = "long_creeping", "slender|wiry" = "slender",
                       "stout|thick|robust|massive" = "stout", "branched|branching" = "branched", "woody" = "woody")
rhizome_form_neg <- c("unbranched" = "")
phenology_dict <- c("semi-deciduous|partly deciduous|briefly deciduous|brevi-?deciduous" = "semi_deciduous", "deciduous" = "deciduous", "evergreen" = "evergreen")
sex_type_dict <- c("dioecious|plants (?:male or female|unisexual)" = "dioecious", "monoecious" = "monoecious", "andromonoecious" = "andromonoecious",
                   "gynodioecious" = "gynodioecious", "androdioecious" = "androdioecious", "polygamodioecious" = "polygamodioecious",
                   "polygamomonoecious|polygamo-monoecious" = "polygamonoecious", "polygamous" = "polygamous")
flower_sex_dict <- c("bisexual|hermaphrodite|perfect" = "bisexual", "unisexual|imperfect" = "unisexual")
parasitic_dict <- c("root hemiparasites?|root-hemiparasit\\w*|hemiparasitic on roots" = "hemiparasitic root_parasitic",
                    "(?:aerial |stem |branch )(?:hemi-?)?parasites?|parasitic on (?:the )?(?:branches|stems)" = "stem_parasitic",
                    "hemi-?parasit\\w*" = "hemiparasitic", "holoparasit\\w*" = "holoparasitic", "root parasit\\w*" = "root_parasitic",
                    "parasit(?:e|es|ic)" = "parasitic")
climbing_dict <- c("twining|twiners?|twines" = "twining", "tendrils?|tendrillar" = "tendrils", "scrambling|scramblers?|scrambles|clambering" = "scrambling",
                   "hooks|hooked prickles|recurved prickles" = "hooks", "adventitious roots|climbing by roots|root-climbing|aerial roots" = "adventitious_roots")
substrate_dict <- c("hemi-?epiphyt\\w*" = "hemiepiphyte", "epiphyt\\w*" = "epiphyte", "lithophyt\\w*" = "lithophyte",
                    "terrestrial" = "terrestrial", "free-floating|floating aquatic" = "aquatic_floating", "semi-aquatic|amphibious" = "semiaquatic",
                    "aquatic|submerged" = "aquatic", "marine" = "marine")

phyllotaxis_dict <- c("alternate|alternately arranged|spirally arranged|spiral|distichous" = "alternate",
                      "opposite|subopposite|decussate|opposite pairs|in (?:[a-z]+ )?(?:unequal |subequal |equal )?pairs" = "opposite",
                      "whorled|verticillate|in whorls|whorls of (?:[0-9]+|three|four|five|six)|pseudo-whorls" = "whorled")
arrangement_dict <- c("decussate" = "decussate", "crowded" = "crowded", "clustered|in clusters" = "clustered",
                      "fascicled|fasciculate|in fascicles|tufted" = "fasciculate", "basal rosettes?|rosettes?|rosulate" = "rosette",
                      "basal|radical" = "clustered_basal", "imbricate|overlapping" = "imbricate",
                      "distichous|in 2 rows|in two rows|two-ranked|2-ranked" = "distichous", "spirally arranged|spiral" = "spiral",
                      "scattered" = "scattered")
compound_dict <- c("simple|undivided" = "simple",
                   # fan-palm leaves: no leaf_compoundness level (recorded, not mapped)
                   "costapalmate|palmate(?! ?compound)" = "",
                   "compound|pinnate|imparipinnate|paripinnate|bipinnate|tripinnate|pinnately compound|trifoliolate|trifoliate|[0-9]-foliolate|palmately compound|digitate|unifoliolate|leaflets?" = "compound")
division_dict <- c("bipinnate|2-pinnate" = "bipinnate", "tripinnate|3-pinnate" = "tripinnate",
                   "(?<![2-4]-)pinnate|imparipinnate|paripinnate|pinnately compound|1-pinnate" = "pinnately_compound",
                   "trifoliolate|trifoliate|3-foliolate" = "trifoliate", "palmately compound|digitate|palmate" = "palmately_compound",
                   "bipinnatifid" = "bipinnatifid", "pinnatifid" = "pinnatifid", "bipinnatisect" = "bipinnatisect", "pinnatisect" = "pinnatisect",
                   "pinnatipartite" = "pinnatipartite", "pinnately lobed" = "pinnately_lobed", "palmately lobed|palmatifid|palmatisect" = "palmately_lobed",
                   "dichotomously (?:divided|lobed|forked)" = "dichotomously_lobed")
lobation_dict <- c("shallowly to deeply (?:[0-9]-)?lobed" = "lobed_shallow lobed_deep", "deeply (?:[0-9]-)?lobed" = "lobed_deep", "shallowly (?:[0-9]-)?lobed" = "lobed_shallow", "(?:[0-9]-)?lobed" = "lobed")
lobation_neg <- c("unlobed|not lobed|without lobes" = "unlobed")
leaf_shape_dict <- c(
  "narrow(?:ly)? linear" = "narrowly_linear", "linear" = "linear", "narrow(?:ly)? lanceolate" = "narrowly_lanceolate",
  "broad(?:ly)? lanceolate|wide(?:ly)? lanceolate" = "lanceolate", "lanceolate|lance-shaped" = "lanceolate",
  "narrow(?:ly)? oblanceolate" = "narrowly_oblanceolate", "oblanceolate" = "oblanceolate",
  "narrow(?:ly)? elliptic(?:al)?" = "narrowly_elliptical", "broad(?:ly)? elliptic(?:al)?|wide(?:ly)? elliptic(?:al)?" = "widely_elliptical",
  "elliptic(?:al)?|oval" = "elliptical", "narrow(?:ly)? ovate" = "narrowly_ovate", "broad(?:ly)? ovate|wide(?:ly)? ovate" = "widely_ovate",
  "ovate|egg-shaped" = "ovate", "narrow(?:ly)? obovate" = "narrowly_obovate", "broad(?:ly)? obovate|wide(?:ly)? obovate" = "widely_obovate",
  "obovate" = "obovate", "narrow(?:ly)? oblong" = "narrowly_oblong", "oblong" = "oblong",
  "orbicular|suborbicular|circular|rotund|round" = "orbicular", "obcordate" = "obcordate", "cordate|heart-shaped" = "cordate",
  "reniform|kidney-shaped" = "reniform", "terete|cylindrical|subterete" = "terete", "filiform|thread-like" = "filiform",
  "falcate|sickle-shaped" = "falcate", "spathulate|spatulate" = "spathulate", "subulate|awl-shaped" = "subulate",
  "acicular|needle-like|needle-shaped" = "acicular", "strap-shaped|strap-like|ligulate|lorate" = "strap-shaped", "peltate" = "peltate",
  "narrow(?:ly)? rhombic|narrow(?:ly)? rhomboid(?:al)?" = "narrowly_rhomboidal", "rhombic|rhomboid(?:al)?|diamond-shaped" = "rhomboidal",
  "deltate|deltoid" = "deltate", "obtriangular|obdeltate|obdeltoid" = "obtriangular", "narrow(?:ly)? triangular" = "narrowly_triangular",
  "triangular" = "triangular", "narrow(?:ly)? obtrullate" = "narrowly_obtrullate", "obtrullate" = "obtrullate", "trullate" = "trullate",
  "ensiform|sword-shaped" = "ensiform", "setaceous|bristle-like" = "setaceous", "oblate" = "oblate",
  # no leaf_shape level: recorded, not mapped
  "sagittate|hastate|triquetrous|rhomboid-ovate|clavate|semi-?terete|subulate-terete|flabellate|cuneiform" = "")
shape_words <- "ovate|obovate|elliptic|elliptical|lanceolate|oblanceolate|oblong|linear|orbicular|suborbicular|spathulate|cordate|deltate|deltoid|triangular|rhombic|rhomboid|obtriangular|obdeltate|falcate|terete|subulate|obcordate|reniform|trullate|obtrullate|circular"
shape_prep <- function(x) {
  x <- str_replace_all(x, paste0("(?<=[a-z])-(?=(?:", shape_words, ")\\b)"), " ")
  x <- str_replace_all(x, "\\bnarrow-(?=[a-z])", "narrowly ")
  x <- str_replace_all(x, "\\bbroad-(?=[a-z])", "broadly ")
  x
}
base_dict <- c("cuneate|wedge-shaped" = "cuneate", "attenuate|long-attenuate|tapering|tapered|narrowed|narrowing" = "attenuate",
               "rounded" = "rounded", "truncate" = "truncate", "cordate" = "cordate", "auriculate|auricled|eared" = "auriculate",
               "oblique|asymmetric" = "oblique", "obtuse" = "obtuse", "acute" = "acute", "sagittate" = "sagittate", "hastate" = "hastate",
               "sheathing|sheath(?:ing)? at (?:the )?base" = "sheathing",
               "decurrent|amplexicaul|connate|perfoliate|clasping|peltate|subcordate|unequal" = "")
apex_dict <- c("acuminate|long-acuminate|caudate|attenuate|cuspidate" = "acuminate", "acute|pointed" = "acute", "obtuse|blunt" = "obtuse",
               "rounded" = "rounded", "apiculate|mucronate|mucronulate|mucro|apiculum" = "apiculate",
               "emarginate|retuse|truncate|notched|bifid|aristate" = "")
margin_dict <- c("entire" = "entire", "dentate|denticulate|spiny-toothed" = "toothed_dentate", "serrate|serrulate|serrated" = "toothed_serrate",
                 "crenate|crenulate" = "toothed_crenate", "toothed|teeth" = "toothed")
margin_posture_dict <- c("revolute|recurved" = "revolute", "involute|incurved" = "involute", "undulate|wavy|crisped|crispate" = "undulate", "flat" = "flat")
lamina_posture_dict <- c("flat" = "flat", "concave|channelled|canaliculate" = "concave", "convex" = "convex", "conduplicate|folded" = "conduplicate",
                         "plicate" = "plicate", "incurved" = "incurved", "recurved" = "recurved", "undulate|wavy" = "undulate")
hairs_dict <- c("glandular-(?:pubescent|hairy|pilose|puberulous|puberulent|villous|tomentose|setose)|glandular hairs|glandular-hairs" = "glandular_pubescent",
                "glabrous|glabrescent|hairless|subglabrous" = "glabrous",
                "eglandular-(?:hairy|pubescent)|hairy|pubescent|puberulous|puberulent|tomentose|tomentellous|villous|pilose|hirsute|hispid|hispidulous|strigose|strigulose|sericeous|silky|setose|lanate|woolly|velutinous|velvety|floccose|araneose|canescent|hoary|stellate-hairy|pilosulous|scabrid|hirtellous|hairs|indumentum" = "hairy",
                "indumentum (?:lacking|absent|0)|without indumentum" = "glabrous")
glaucous_dict <- c("subglaucous|slightly glaucous|somewhat glaucous|faintly glaucous" = "subglaucous", "glaucous|pruinose" = "glaucous")
glaucous_neg <- c("not glaucous|non-glaucous|not pruinose" = "not_glaucous")
leaf_colour_dict <- c(
  "dark grey[- ]green|dark greyish[- ]green" = "dark_grey_green", "(?:pale|light) grey[- ]green|(?:pale|light) greyish[- ]green" = "pale_grey_green",
  "grey[- ]green|greyish[- ]green|gray[- ]green|glaucous[- ]green" = "grey_green", "blue[- ]green|bluish[- ]green" = "blue_green",
  "yellow[- ]green|yellowish[- ]green" = "green_yellow", "olive(?:[- ]green)?" = "green_olive",
  "silvery[- ]green|silver[- ]green|silvery|silver" = "green_silvery", "brownish[- ]green|brown[- ]green" = "green_brown",
  "(?:dark|deep)[- ](?:glossy |shiny |satiny |dull )?green" = "dark_green", "(?:pale|light)[- ](?:glossy |shiny |dull )?green" = "pale_green",
  "green" = "green", "purple|purplish" = "purple", "red|reddish" = "red", "white|whitish" = "white", "yellow|yellowish" = "yellow",
  "brown|brownish" = "brown", "blue|bluish" = "blue")
discolor_dict <- c("discolou?rous|paler (?:below|beneath|underneath|on (?:the )?(?:lower|under) ?surface|abaxially)|(?:lower surface|undersurface|under-surface) paler" = "discolorous",
                   "concolou?rous" = "concolorous")
reflect_dict <- c("shiny|glossy|lustrous|satiny|polished|shining" = "shiny", "dull|matt|matte" = "dull")
texture_dict <- c("coriaceous|leathery|subcoriaceous" = "coriaceous", "chartaceous|papery" = "chartaceous",
                  "membranous|membranaceous|thin-textured" = "membranous", "fleshy|succulent" = "fleshy", "rigid|stiff" = "rigid")
attachment_dict <- c("sessile|subsessile|amplexicaul|stem-clasping|clasping|stalkless" = "sessile", "petiolate|petioled|shortly petiolate|stalked" = "petiolate")
stipule_dict <- c("exstipulate|estipulate|stipules absent|without stipules|stipules lacking|stipules 0" = "absent", "stipulate" = "present",
                  # within a stipule clause: "stipules minute, obsolete or absent"
                  "absent|obsolete|lacking" = "absent",
                  "minute|small|tiny|caducous|persistent|deciduous|fugacious|interpetiolar|intrapetiolar|scale-like|setaceous|subulate|filiform|triangular|lanceolate|ovate|linear|spinescent|foliaceous|membranous|scarious|present" = "present")
stem_shape_dict <- c("terete|subterete|round(?:ed)? in (?:cross-)?section" = "terete", "(?:[0-9]-|four-|six-)?angled|angular|quadrangular|tetragonal|square" = "angular",
                     "ribbed|ridged|striate|sulcate|grooved" = "ribbed", "winged" = "winged", "flattened|compressed|angular-compressed" = "flattened")
bark_texture_dict <- c("smooth" = "smooth", "rough" = "rough", "furrowed|fissured" = "furrowed", "tessellated" = "tessellated",
                       "fibrous" = "fibrous", "stringy" = "stringy", "flaky|flaking|scaly|in flakes|in scales" = "flaky",
                       "papery|paperbark" = "papery", "corky" = "corky", "spiny" = "spiny")
bark_colour_dict <- c("dark brown" = "brown_dark", "(?:light|pale) brown" = "brown_light", "chalky white|white|whitish|cream" = "white",
                      "grey|greyish|gray" = "grey", "brown|brownish" = "brown", "green|greenish" = "green", "orange" = "orange",
                      "pink|pinkish" = "pink", "red|reddish" = "red", "yellow|yellowish" = "yellow", "black|blackish" = "black")

infl_type_dict <- c("solitary|single (?:axillary |terminal )?flowers?|flowers? (?:borne )?singly" = "solitary", "racemes?|racemose|raceme-like" = "raceme",
                    "spikes?|spicate|spike-like|spiciform" = "spike", "panicles?|paniculate" = "panicle",
                    "cymes?|cymose|dichasi(?:a|um|al)|monochasi(?:a|um|al)|cymules?" = "cyme", "corymbs?|corymbose" = "corymb",
                    "umbels?|umbellate|umbelliform|umbelliforms" = "umbel", "heads?|capitate|capitul(?:a|um)|glomerules?" = "head",
                    "terminal|apical" = "terminal", "axillary|in (?:the )?(?:upper )?(?:leaf )?axils" = "axillary")
infl_shape_dict <- c("globular|globose|spherical|subglobose" = "spherical", "cylindrical|cylindric" = "cylindrical", "elongated?|oblong" = "elongated")
symmetry_dict <- c("actinomorphic|regular|radially symmetric(?:al)?" = "actinomorphic_general",
                   "zygomorphic|irregular(?! (?:dichasi|cymes?|clusters?|panicles?|racemes?|inflorescences?|heads?|umbels?|spikes?|whorls?|fascicles?|thyrses?))|bilaterally symmetric(?:al)?|2-lipped|two-lipped|bilabiate" = "zygomorphic")
flower_shape_dict <- c("tubular" = "tubulate", "campanulate|bell-shaped" = "campanulate", "funnel-shaped|funnelform|infundibuliform" = "funnelform",
                       "salverform|salver-shaped|hypocrateriform" = "salverform", "rotate|wheel-shaped" = "rotate", "urceolate|urn-shaped" = "urceolate",
                       "cup-shaped|cupular|cupuliform" = "cup-shaped", "2-lipped|two-lipped|bilabiate" = "bilabiate", "cruciform" = "cruciform")
orientation_dict <- c("pendulous|nodding|pendent|drooping|deflexed|pendant" = "down", "erect|upright" = "up", "horizontal" = "lateral")
scent_dict <- c("scented|fragrant|perfumed|sweet-smelling|sweetly smelling|aromatic flowers|odoriferous|foetid|fetid|malodorous|unpleasant(?:ly)? (?:smell|odour)|strongly smelling|smelling|smell|odour|odor|scent|fragrance|perfume" = "scent_produced")
scent_neg <- c("unscented|not scented|scentless|without (?:a )?scent|odourless" = "scent_absent")
nectar_dict <- c("nectar-producing|nectariferous|nectar|nectaries|nectary|nectariferous disc" = "nectar_produced")
ovary_dict <- c("half-inferior|semi-inferior|partly inferior|half inferior" = "half_inferior", "inferior" = "inferior", "superior" = "superior")

fruit_type_dict <- c(
  "capsules?|capsular" = "capsule", "drupes?|drupaceous|drupelets?" = "drupe", "berr(?:y|ies)|baccate|berry-like" = "berry",
  "legumes?" = "legume", "follicles?|follicular" = "follicle", "achenes?|cypselas?|cypselae" = "achene", "nutlets?" = "nutlet", "nuts?" = "nut",
  "samaras?" = "samara", "schizocarps?" = "schizocarp", "mericarps?" = "mericarp", "utricles?" = "utricle",
  "caryops[ie]s|grains?" = "caryopsis", "siliques?|siliquas?|siliculas?|silicles?|siliculae" = "silique", "syconi(?:a|um)" = "syconium",
  "pomes?" = "pome", "pepos?" = "pepo", "syncarps?|syncarpi(?:a|um)" = "syncarp", "apocarps?|monocarps?" = "", "anthocarps?" = "anthocarp", "multiple fruits?" = "multiple_fruit",
  "pyrenes?" = "pyrene", "strobil(?:i|us)" = "strobilus")
dehisc_dict <- c("indehiscent|not opening|not splitting" = "indehiscent",
                 "dehiscent|dehiscing|loculicidal(?:ly)?|septicidal(?:ly)?|opening|splitting|explosively|explod(?:es|ing)|[0-9]-valved|valves?|bivalved|circumsciss\\w*|dehisces" = "dehiscent")
fleshy_dict <- c("fleshy|succulent|juicy|pulpy|drupaceous|baccate" = "fleshy", "dry|woody|papery|chartaceous|crustaceous|coriaceous|membranous|bony" = "dry")
fruit_shape_dict <- c("globose|globular|spherical|subglobose" = "globose", "obovoid" = "obovoid", "ovoid" = "ovoid", "ellipsoid(?:al)?" = "ellipsoid",
                      "obloid|oblong" = "oblong", "cylindrical|cylindric|terete" = "cylindrical", "clavate|club-shaped" = "clavate", "fusiform" = "fusiform",
                      "linear" = "linear", "obconical|obconic" = "obconical", "conical|conic" = "conical", "pyriform|pear-shaped" = "pyriform",
                      "turbinate" = "turbinate", "lenticular" = "lenticular", "reniform" = "reniform", "cup-shaped|cupular" = "cup-shaped",
                      "barrel-shaped" = "barrel-shaped", "hemispherical" = "hemispherical", "compressed|flattened|flat" = "flattened")
seed_shape_dict <- c("ovoid|egg-shaped|obovoid" = "ovoid", "ellipsoid(?:al)?" = "ellipsoid", "globose|globular|spherical|subglobose" = "globoid",
                     "discoid|disc-shaped|discoidal" = "discoid", "lenticular|lens-shaped" = "lenticular", "reniform|kidney-shaped" = "reniform",
                     "orbicular|circular|subcircular" = "orbicular", "cylindrical|cylindric" = "cylindrical", "fusiform" = "fusiform",
                     "conical|conic" = "conical", "cuneate|wedge-shaped" = "cuneate", "polyhedral|angular" = "polyhedral",
                     "hemispherical" = "hemispheric", "comma-shaped" = "comma-shaped", "winged" = "winged")
seed_texture_dict <- c("smooth" = "smooth", "rugose|rugulose|wrinkled" = "wrinkled", "tuberculate|tubercled|verrucose|warty|papillose|muricate|colliculate" = "bumpy",
                       "pitted|foveolate|foveate|dimpled|punctate" = "pitted", "reticulate|net-veined|alveolate" = "netted",
                       "ribbed|striate|ridged|costate" = "ribbed", "grooved|furrowed|sulcate" = "grooved", "rough|scabrous|scabrid" = "rough",
                       "spiny|echinate|spines|spinose|spinulose" = "spiny", "scaly" = "scaly")
seed_hairs_dict <- c("glabrous" = "glabrous", "hairy|pubescent|hairs|pilose|villous|tomentose|comose|hispid|sericeous" = "hairs")
appendage_dict <- c("arils?|arillate|arillode|arillodes" = "aril", "elaiosomes?|strophioles?|strophiolate|caruncles?|carunculate" = "elaiosome",
                    "wings?|winged" = "wings", "pappus" = "pappus", "plumose" = "plumose",
                    "coma|comose|tuft of (?:silky |long )?hairs|hairs [0-9.]+(?:\\s*-\\s*[0-9.]+)? (?:mm|cm) long" = "hairs",
                    "hooks?|hooked" = "hooks", "sarcotesta" = "sarcotesta")
appendage_neg <- c("exarillate|without (?:an )?aril|aril absent|estrophiolate|without (?:a )?wing|wingless|not winged" = "none")

# colours
cw <- "white|whitish|cream|creamy|ivory|yellow|yellowish|lemon|golden|gold|orange|apricot|straw|red|reddish|brown|brownish|maroon|bronze|crimson|rufous|fawn|rust|russet|copper|coppery|tan|scarlet|burgundy|chestnut|pink|pinkish|rose|salmon|magenta|blue|bluish|purple|purplish|mauve|lilac|violet|lavender|indigo|plum|green|greenish|black|blackish|grey|greyish|gray|silver|silvery|cerise|vermilion|ochre|carmine|claret|wine-red|sky|lime|olive|buff|amber"
cw_mod <- "bluish|greenish|purplish|reddish|yellowish|brownish|pinkish|creamy|whitish|blackish|greyish|golden|lime|sky|olive"
colour_prep <- function(x, ageing = TRUE) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "straw-colou?red", "straw")
  # colour changes with age are not the flower colour; for fruits the ripening colour is kept (ageing = FALSE)
  if (ageing) x <- str_remove_all(x, "\\b(?:fading|ageing|aging|becoming|turning|drying|maturing|changing|darkening)\\b(?: to)?(?: [a-z-]+){1,3}")
  x <- str_remove_all(x, "\\bdrying [a-z-]+")
  x <- str_remove_all(x, "\\bexcept\\b[^,;|]*")
  x <- str_remove_all(x, "\\b(?:with|having)\\s+(?:[a-z-]+\\s+){0,4}?(?:markings?|marks|patches|spots?|spotting|stripes?|striations?|striae|streaks?|lines|veins?|mid-?veins?|venation|blotch(?:es)?|dots|centres?|centers?|throat|eye|tips?|apex|apices|margins?|bands?|midribs?|nerves|hairs|glands|base|flecks?|guides?|anthers?|stamens?|bases|areas?|zones?|rings?|palate|tinge|flush)\\b[^,;|]*")
  # colours of hairs ("white-pubescent", "short brown bristles"), of the tube interior, or before maturity
  x <- str_remove_all(x, paste0("\\b(?:", cw, ")[- ](?:pubescent|hairy|tomentose|villous|pilose|sericeous|puberulous|ciliate|setose)\\b"))
  x <- str_remove_all(x, paste0("\\b(?:", cw, ")\\s+(?:[a-z-]+\\s+)?(?:bristles|hairs|setae|scales|papillae|trichomes)\\b"))
  x <- str_remove_all(x, paste0("(?:\\b(?:pale|dark|deep|light|bright) )?\\b(?:", cw, ")(?:[- ](?:", cw, "))*\\s+(?:in bud|when immature|when young|(?:inside|within|in) (?:the )?(?:tube|throat|mouth)|at first|at (?:the )?base|towards (?:the )?base|near (?:the )?base)\\b"))
  # tinges and markings: "tinged with red", "purple tinged red" (the base colour stays), "pink- or reddish-tinged", "red-striped"
  # (hyphenated markings first: "often purple-veined" goes whole)
  x <- str_remove_all(x, "\\b[a-z]+-? (?:or|and|to) [a-z]+-(?=(?:tinged|striped|spotted|veined|dotted)\\b)")
  x <- str_remove_all(x, "\\b[a-z]+-(?:tinged|striped|spotted|veined|marked|flushed|blotched|streaked|dotted|lined|mottled|edged|tipped|banded|speckled|suffused)\\b")
  x <- str_remove_all(x, "\\b(?:tinged|flushed|suffused|striped|spotted|streaked|marked|veined|dotted|blotched|mottled|edged|tipped|banded|speckled)\\b(?: with)?(?: (?:and|or|to|pale|dark|deep|light|bright|[a-z]+ish|[a-z]+-[a-z]+|[a-z]+)){0,3}")
  x <- str_remove_all(x, "\\bin bud\\b|\\bwhen dry\\b|\\bon drying\\b|\\bwhen young\\b|\\bwhen immature\\b|\\bimmature\\b|\\bin herbarium\\b|\\b(?:pulp|flesh)\\b[^,;|]*")
  x
}
flower_colour_map <- c(
  "white|whitish|cream|creamy|ivory" = "white_cream",
  "yellow|yellowish|lemon|golden|gold|orange|apricot|straw|amber|buff|ochre" = "yellow_orange",
  "red|reddish|brown|brownish|maroon|bronze|crimson|rufous|fawn|rust|russet|copper|coppery|tan|scarlet|burgundy|chestnut|vermilion|carmine|claret|wine-red" = "red_brown",
  "pink|pinkish|rose|salmon|magenta|cerise" = "pink",
  "blue|bluish|purple|purplish|mauve|lilac|violet|lavender|indigo|plum" = "blue_purple",
  "green|greenish|lime|olive" = "green", "black|blackish" = "black", "grey|greyish|gray|silver|silvery" = "grey")
fruit_colour_map <- c(
  "white|whitish" = "white", "cream|creamy|ivory" = "cream", "yellow|yellowish|lemon|golden|gold|straw|amber" = "yellow",
  "orange|apricot" = "orange", "red|reddish|crimson|scarlet|maroon|burgundy|vermilion|carmine|claret|wine-red" = "red",
  "brown|brownish|bronze|rufous|fawn|rust|russet|copper|coppery|tan|chestnut|buff|ochre" = "brown",
  "pink|pinkish|rose|salmon|magenta|cerise" = "pink", "blue|bluish" = "blue",
  "purple|purplish|mauve|lilac|violet|lavender|indigo|plum" = "purple", "green|greenish|lime|olive" = "green",
  "black|blackish" = "black", "grey|greyish|gray|silver|silvery" = "grey")
seed_colour_map <- flower_colour_map
colour_mods <- "pale|light|dark|deep|bright|dull|rich|dirty|vivid|intense|brilliant|glossy|shiny|clear|pure|soft|mid|medium|creamy|whitish|bluish|greenish|purplish|reddish|yellowish|brownish|pinkish|blackish|greyish|golden"
colour_dict <- function(cmap) {
  out <- character(0)
  for (k in names(cmap)) for (w in str_split(k, "\\|")[[1]]) {
    key <- paste0("(?:(?:", colour_mods, "|", cw, ")[- ])*", w, "(?![- ](?:", cw, ")\\b)")
    out[key] <- cmap[[k]]
  }
  out
}
flower_colour_dict <- colour_dict(flower_colour_map)
fruit_colour_dict <- colour_dict(fruit_colour_map)
seed_colour_dict <- colour_dict(seed_colour_map)

# ---------------------------------------------------------------- generic organ x character matrix (user's rule: every
# documentable character is extracted, trait or not). Each organ (and organ part: abaxial / adaxial surface, margin,
# tube, lobes, apex) is scanned for hairs, shape, colour, texture, surface, orientation, fusion and persistence; counts
# and sizes not covered by an existing trait are added too. Column names are <organ>[_<part>]_<character>; the mapped
# column holds a suggested value from these draft vocabularies, to be refined once many families are done.
gen_hairs_dict <- c(
  "glabrous|hairless" = "glabrous", "glabrate|glabrescent|becoming glabrous|soon glabrous|glabrous with age|ageing glabrous|aging glabrous" = "glabrescent",
  "subglabrous|almost glabrous|nearly glabrous|± glabrous|more or less glabrous" = "subglabrous",
  "puberulous|puberulent|puberula" = "puberulous", "pubescent|pubescence|downy" = "pubescent", "hairy|hairs|indumentum|pilosity" = "hairy",
  "pilose|pilosulous" = "pilose", "tomentose|tomentum|tomentellous|tomentulose" = "tomentose", "villous|villose" = "villous",
  "hirsute|hirsutulous" = "hirsute", "hispid|hispidulous" = "hispid", "sericeous|silky" = "sericeous", "strigose|strigillose" = "strigose",
  "velutinous|velvety" = "velutinous", "woolly|lanate|lanuginose|floccose|arachnoid|cobwebby" = "woolly",
  "stellate|stellate-hairy|stellate-pubescent|stellate-tomentose" = "stellate",
  "glandular-hairy|glandular hairs|glandular-pubescent|glandular-pilose|stipitate-glandular|glandular-puberulous" = "glandular_hairy",
  "ciliate|ciliolate|fringed with hairs" = "ciliate", "setose|setulose|bristly" = "setose", "scabrous|scabrid|scabridulous" = "scabrous",
  "lepidote|scaly" = "lepidote")
gen_hairs_neg <- c("hairs absent|without hairs|lacking hairs|devoid of hairs|not hairy|not pubescent" = "glabrous")
gen_shape_dict <- c(
  "linear" = "linear", "lanceolate" = "lanceolate", "oblanceolate" = "oblanceolate", "ovate" = "ovate", "obovate" = "obovate",
  "elliptic|elliptical" = "elliptic", "oblong" = "oblong", "orbicular|circular" = "orbicular", "spathulate|spatulate" = "spathulate",
  "triangular|deltoid|deltate" = "triangular", "subulate|awl-shaped" = "subulate", "rhomboid|rhombic" = "rhomboid",
  "reniform|kidney-shaped" = "reniform", "cordate|heart-shaped" = "cordate", "falcate|sickle-shaped" = "falcate",
  "acicular|needle-like|needle-shaped" = "acicular", "filiform|thread-like" = "filiform", "terete" = "terete",
  "flattened|compressed|dorsiventrally flattened|laterally compressed" = "flattened", "ovoid|subovoid" = "ovoid", "obovoid" = "obovoid",
  "globose|subglobose|globular|spherical" = "globose", "ellipsoid|ellipsoidal" = "ellipsoid", "cylindrical|cylindric|subcylindrical" = "cylindrical",
  "clavate|club-shaped" = "clavate", "fusiform|spindle-shaped" = "fusiform", "conical|conic" = "conical", "obconical|obconic" = "obconical",
  "turbinate|top-shaped" = "turbinate", "campanulate|bell-shaped" = "campanulate", "tubular" = "tubular",
  "funnel-shaped|infundibuliform|funnelform" = "funnel-shaped", "rotate|wheel-shaped" = "rotate", "urceolate|urn-shaped" = "urceolate",
  "salverform|salver-shaped|hypocrateriform" = "salverform", "cup-shaped|cupular|cupuliform" = "cup-shaped",
  "boat-shaped|cymbiform|navicular" = "boat-shaped", "peltate" = "peltate", "hooded|cucullate" = "hooded", "capitate" = "capitate",
  "trigonous|triquetrous|3-angled|three-angled" = "trigonous", "quadrangular|4-angled|four-angled" = "quadrangular",
  "saccate|pouched" = "saccate", "hemispherical|hemispheric" = "hemispherical", "lenticular" = "lenticular",
  "pyriform|pear-shaped" = "pyriform", "discoid|disc-shaped" = "discoid", "semicircular" = "semicircular", "square" = "square",
  "bifid|2-fid|bilobed|2-lobed|two-lobed" = "bilobed", "trifid|3-fid|3-lobed|three-lobed" = "trilobed", "punctiform" = "punctiform",
  "plumose|feathery" = "plumose", "filamentous" = "filiform", "lobed" = "lobed", "sagittate" = "sagittate", "hastate" = "hastate")
gen_texture_dict <- c(
  "coriaceous|subcoriaceous|leathery" = "coriaceous", "chartaceous|papery|papyraceous" = "chartaceous", "membranous|membranaceous" = "membranous",
  "scarious" = "scarious", "hyaline" = "hyaline", "fleshy|succulent|carnose" = "fleshy", "woody|lignified" = "woody", "corky|suberose" = "corky",
  "spongy" = "spongy", "herbaceous" = "herbaceous", "cartilaginous" = "cartilaginous", "crustaceous|bony|stony|osseous" = "hard",
  "rigid|stiff" = "rigid", "petaloid" = "petaloid", "sepaloid" = "sepaloid", "foliaceous|leaf-like" = "foliaceous", "fibrous" = "fibrous")
gen_surface_dict <- c(
  "smooth" = "smooth", "rugose|rugulose|wrinkled" = "rugose", "tuberculate|tubercled|tuberculed" = "tuberculate",
  "verrucose|warty|verruculose" = "verrucose", "muricate" = "muricate", "papillose|papillate" = "papillose",
  "pitted|foveolate|foveate" = "pitted", "reticulate" = "reticulate", "striate|striated" = "striate", "ribbed|costate" = "ribbed",
  "sulcate|grooved|furrowed" = "grooved", "keeled|carinate" = "keeled", "winged|alate" = "winged",
  "glandular-punctate|gland-dotted|glandular-dotted|punctate|pellucid-dotted|dotted with glands" = "gland_dotted", "glandular" = "glandular",
  "viscid|sticky|glutinous|resinous" = "viscid", "glossy|shiny|shining|lustrous" = "shiny", "dull|matt|matte" = "dull",
  "glaucous|pruinose" = "glaucous", "spiny|spinose|prickly|aculeate" = "spiny", "echinate" = "echinate", "lenticellate" = "lenticellate")
gen_orient_dict <- c(
  "erect|suberect" = "erect", "spreading|patent" = "spreading", "reflexed|deflexed|bent back" = "reflexed", "recurved" = "recurved",
  "incurved|inflexed" = "incurved", "ascending" = "ascending", "pendulous|pendent|drooping|nodding" = "pendulous",
  "appressed|adpressed" = "appressed", "imbricate" = "imbricate", "twisted|contorted" = "twisted", "straight" = "straight", "curved" = "curved")
gen_fusion_dict <- c("free" = "free", "connate|fused|united|joined|coalescent|gamosepalous|gamopetalous|sympetalous" = "connate",
                     "adnate" = "adnate", "polypetalous|polysepalous" = "free")
gen_persist_dict <- c("persistent|persisting" = "persistent", "caducous|early deciduous|early-deciduous|soon falling|fugacious" = "caducous",
                      "deciduous|falling" = "deciduous", "accrescent|enlarging in fruit|enlarged in fruit" = "accrescent", "marcescent" = "marcescent")
gen_exsert_dict <- c("exserted|protruding|exsert" = "exserted", "included|enclosed|inserted within" = "included")
gen_attach_dict <- c("basifixed" = "basifixed", "dorsifixed" = "dorsifixed", "versatile" = "versatile", "medifixed" = "medifixed")
gen_dehisc_dict <- c("longitudinal slits?|longitudinally dehiscent|dehiscing longitudinally|opening longitudinally|by slits|longitudinally" = "slits",
                     "poricidal|by (?:apical |terminal )?pores?|opening by pores?|apical pores?" = "pores", "by valves|valvate|valvular" = "valves",
                     "introrse" = "introrse", "extrorse" = "extrorse", "latrorse" = "latrorse")
gen_colour_dict <- colour_dict(fruit_colour_map)
# phrases naming another organ ("enclosed in the persistent calyx", "longer than the sepals") are not the organ's own character
gen_other <- c("leaves", "leaf", "laminae?", "blades?", "petioles?", "stipules?", "bracts?", "bracteoles?", "pedicels?", "peduncles?",
               "inflorescences?", "flowers?", "calyx", "calyces", "sepals?", "corollas?", "petals?", "tepals?", "perianths?", "stamens?",
               "filaments?", "anthers?", "staminodes?", "ovary", "ovaries", "styles?", "stigmas?", "carpels?", "ovules?", "fruits?",
               "capsules?", "seeds?", "arils?", "stems?", "branches", "branchlets?", "bark", "roots?", "hypanthium", "disc",
               "columellae?", "columns?", "receptacles?", "septa", "septum", "valves?", "beaks?", "wings?", "hilum", "caruncles?",
               "elaiosomes?", "claws?", "spurs?", "glands?", "keels?", "sheaths?", "spines?", "awns?")
gen_own <- list(leaf = "leaves|leaf|laminae?|blades?", leaflet = "", petiole = "petioles?", stipule = "stipules?", inflorescence = "inflorescences?",
                peduncle = "peduncles?", pedicel = "pedicels?", bud = "", bract = "bracts?", bracteole = "bracteoles?", involucral_bract = "bracts?",
                sepal = "sepals?|calyx", calyx = "calyx|calyces|sepals?", hypanthium = "hypanthium|calyx", perianth = "perianths?|tepals?",
                petal = "petals?|corollas?", corolla = "corollas?|petals?", anther = "anthers?|stamens?", filament = "filaments?|stamens?",
                staminode = "staminodes?", stamen = "stamens?|filaments?|anthers?", ovary = "ovary|ovaries|carpels?", style = "styles?",
                stigma = "stigmas?|styles?", carpel = "carpels?|ovary", disc = "disc", fruit = "fruits?|capsules?", seed = "seeds?",
                aril = "arils?|seeds?", stem = "stems?|branches|branchlets?", branchlet = "branchlets?|branches|stems?")
gen_strip_other <- function(ok) {
  own <- str_split(gen_own[[ok]] %||% "", "\\|")[[1]]
  oth <- setdiff(gen_other, own)
  re <- paste0("\\b(?:(?:in|by|within|with|on|from|than|to|as|of|and|exceeding|enclosing|enclosed) )?(?:(?:the|a|an) )?(?:[a-z-]+ ){0,2}(?:", paste(oth, collapse = "|"), ")\\b[^,;|]*")
  function(x) str_remove_all(x, re)
}
# a combined pattern per dictionary: a unit with none of its words is skipped (speed)
dict_any <- function(d) rx(paste0("(?<![a-z])(?:", paste(names(d), collapse = "|"), ")"))
gen_dicts <- list(hairs = gen_hairs_dict, shape = gen_shape_dict, colour = gen_colour_dict, texture = gen_texture_dict, surface = gen_surface_dict,
                  orientation = gen_orient_dict, fusion = gen_fusion_dict, persistence = gen_persist_dict, exsertion = gen_exsert_dict,
                  attachment = gen_attach_dict, dehiscence = gen_dehisc_dict, apex_shape = apex_dict)
gen_any <- map(gen_dicts, dict_any)
# organ (from the classified unit's section and subject) and part class
gen_okey <- function(sec, part, subj) {
  s <- str_remove(str_to_lower(coalesce(subj, "")), "^the ")
  case_when(
    sec == "stem" & str_detect(s, "branchlet|twig") ~ "branchlet", sec == "stem" ~ "stem",
    sec %in% c("leaf", "lamina") ~ "leaf", sec == "leaflet" ~ "leaflet", sec == "petiole" ~ "petiole", sec == "stipule" ~ "stipule",
    sec == "inflorescence" ~ "inflorescence", sec == "peduncle" ~ "peduncle", sec == "pedicel" ~ "pedicel", sec == "bud" ~ "bud",
    sec == "bract" & str_detect(s, "bracteole|prophyll") ~ "bracteole", sec == "bract" & str_detect(s, "involuc|phyllar") ~ "involucral_bract",
    sec == "bract" & str_detect(s, "spathe") ~ "spathe", sec == "bract" ~ "bract",
    sec == "calyx" & str_detect(s, "sepal") ~ "sepal", sec == "calyx" & str_detect(s, "hypanth") ~ "hypanthium", sec == "calyx" ~ "calyx",
    sec == "corolla" & str_detect(s, "tepal|perianth|sepals and petals|petals and sepals") ~ "perianth",
    sec == "corolla" & str_detect(s, "labellum") ~ "labellum", sec == "corolla" & str_detect(s, "petal|standard|keel") ~ "petal",
    sec == "corolla" & str_detect(s, "corolla") ~ "corolla", sec == "corona" ~ "corona",
    sec == "androecium" & str_detect(s, "anther") ~ "anther", sec == "androecium" & str_detect(s, "filament") ~ "filament",
    sec == "androecium" & str_detect(s, "staminode") ~ "staminode", sec == "androecium" & str_detect(s, "stamen|staminal") ~ "stamen",
    sec == "gynoecium" & str_detect(s, "ovar") ~ "ovary", sec == "gynoecium" & str_detect(s, "style") ~ "style",
    sec == "gynoecium" & str_detect(s, "stigma") ~ "stigma", sec == "gynoecium" & str_detect(s, "carpel|pistil") ~ "carpel",
    sec == "gynoecium" & str_detect(s, "disc") ~ "disc", sec == "gynoecium" & str_detect(s, "indusi") ~ "indusium",
    sec == "fruit" ~ "fruit", sec == "seed" & str_detect(s, "aril") ~ "aril", sec == "seed" & str_detect(s, "seed") ~ "seed",
    TRUE ~ NA_character_)
}
gen_pclass <- function(part) {
  p <- str_to_lower(coalesce(part, ""))
  case_when(p == "" ~ "", str_detect(p, "^(?:lower|abaxial) surfaces?$|^under-?surface$") ~ "abaxial",
            str_detect(p, "^(?:upper|adaxial) surfaces?$") ~ "adaxial", str_detect(p, "^margins?$") ~ "margin",
            p == "tube" ~ "tube", str_detect(p, "^(?:lobes?|segments?|teeth)$") ~ "lobe", str_detect(p, "^(?:apex|apices|tips?)$") ~ "apex",
            TRUE ~ NA_character_)
}
# characters scanned per part class
gen_chars <- list(organ = c("hairs", "shape", "colour", "texture", "surface", "orientation", "fusion", "persistence"),
                  abaxial = c("hairs", "colour", "surface"), adaxial = c("hairs", "colour", "surface"), margin = c("hairs"),
                  tube = c("hairs", "shape", "colour"), lobe = c("hairs", "shape", "colour", "orientation", "fusion"), apex = c("apex_shape"))
gen_extra <- list(anther = c("attachment", "dehiscence", "exsertion"), stamen = "exsertion", style = "exsertion", stigma = "exsertion",
                  filament = "exsertion", staminode = character(0))
# organ x character pairs already extracted under an existing (or earlier candidate) trait
gen_skip <- c("leaf__hairs", "leaf__shape", "leaf__colour", "leaf__texture", "leaf__surface", "leaf_apex_apex_shape", "leaflet__shape",
              "leaflet_apex_apex_shape", "stem__hairs", "stem__shape", "branchlet__shape", "fruit__hairs", "fruit__shape", "fruit__colour",
              "seed__shape", "seed__colour", "seed__surface", "seed__hairs", "corolla__colour", "petal__colour", "perianth__colour",
              "corolla_lobe_colour", "perianth_lobe_colour", "inflorescence__shape")
# counts: <trait> = organ noun at the start of a token ("Petals 5", "Staminodes 2 or 3") or "N <noun>"
gen_counts <- tribble(
  ~trait, ~okeys, ~noun,
  "petal_count", "petal|corolla", "petals?",
  "sepal_count", "sepal|calyx", "sepals?",
  "tepal_count", "perianth", "tepals?|perianth (?:segments|lobes|parts)",
  "calyx_lobe_count", "calyx", "(?:calyx )?(?:lobes|teeth)",
  "corolla_lobe_count", "corolla", "(?:corolla )?lobes",
  "staminode_count", "staminode|stamen", "staminodes?",
  "carpel_count", "carpel|ovary", "carpels?|pistils?",
  "style_count", "style|ovary", "styles?",
  "stigma_count", "stigma|style|ovary", "stigmas?|stigmatic lobes|style branches|style-branches",
  "ovule_count", "ovary|carpel", "ovules?",
  "bract_count", "bract|involucral_bract", "bracts?",
  "bracteole_count", "bracteole", "bracteoles?|prophylls?")
# sizes for organs with no existing length / width trait: <trait prefix> = organ, nouns excluded from "other organ" checks
gen_sizes <- tribble(
  ~okey, ~len, ~wid, ~own,
  "stipule", "stipule_length", "stipule_width", "stipules?",
  "bracteole", "bracteole_length", "bracteole_width", "bracteoles?",
  "involucral_bract", "involucral_bract_length", "involucral_bract_width", "bracts?",
  "spathe", "spathe_length", "spathe_width", "spathes?",
  "bract", NA, "flower_bract_width", "bracts?",
  "sepal", NA, "flower_sepal_width", "sepals?|calyx|lobes?|segments?",
  "petal", NA, "flower_petal_width", "petals?",
  "perianth", NA, "tepal_width", "tepals?|perianth|lobes?|segments?",
  "labellum", NA, "labellum_width", "labellum",
  "hypanthium", "hypanthium_length", "hypanthium_width", "hypanthium|tube|calyx",
  "staminode", "staminode_length", NA, "staminodes?",
  "ovary", "ovary_length", "ovary_width", "ovary",
  "stigma", "stigma_length", NA, "stigmas?",
  "aril", "aril_length", NA, "arils?",
  "corona", "corona_length", NA, "coronas?",
  "disc", NA, "disc_diameter", "disc",
  "indusium", "indusium_length", "indusium_width", "indusi(?:a|um)")

# ---------------------------------------------------------------- phenology (months)
month_re <- "\\b(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|June?|July?|Aug(?:ust)?|Sept?(?:ember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\b"
season_re <- "\\b((?:early|mid|mid-|late)[ -]?)?(spring|summer|autumn|winter)\\b"
season_months <- list(spring = 9:11, summer = c(12, 1, 2), autumn = 3:5, winter = 6:8)
month_index <- function(m) match(str_sub(str_to_lower(m), 1, 3), str_to_lower(month.abb))
cyc <- function(a, b) if (a <= b) a:b else c(a:12, 1:b)
year_round <- "throughout the year|through the year|all year|year[- ]round|any time of (?:the )?year|all months|all seasons|every month"
months_from_text <- function(x) {
  if (is.na(x) || x == "") return(integer(0))
  if (str_detect(x, regex(year_round, ignore_case = TRUE))) return(1:12)
  # "early to late spring", "mid to late summer" -> "early spring to late spring"
  x <- str_replace_all(x, regex("\\b(early|mid)[- ]?(?:to|-)\\s*(mid|late)[- ]?(spring|summer|autumn|winter)\\b", ignore_case = TRUE), "\\1 \\3 to \\2 \\3")
  x <- str_replace_all(x, paste0("\\b(?:early|mid|late)[- ]?(?=", str_remove(month_re, "^\\\\b"), ")"), "")
  toks <- bind_rows(
    str_locate_all(x, month_re)[[1]] %>% as_tibble() %>% mutate(txt = str_sub(x, start, end), a = month_index(txt), b = a),
    str_locate_all(x, regex(season_re, ignore_case = TRUE))[[1]] %>% as_tibble() %>%
      mutate(txt = str_to_lower(str_sub(x, start, end)), s = str_extract(txt, "spring|summer|autumn|winter"),
             mod = str_extract(txt, "early|mid|late"),
             a = map2_int(s, mod, ~ { m <- season_months[[.x]]; as.integer(if (is.na(.y)) m[1] else if (.y == "early") m[1] else if (.y == "mid") m[2] else m[3]) }),
             b = map2_int(s, mod, ~ { m <- season_months[[.x]]; as.integer(if (is.na(.y)) m[3] else if (.y == "early") m[1] else if (.y == "mid") m[2] else m[3]) })) %>%
      select(start, end, txt, a, b)
  ) %>% arrange(start)
  if (!nrow(toks)) return(integer(0))
  out <- integer(0); i <- 1
  connector <- "^\\.?\\s*\\)?\\s*(?:,?\\s*(?:and |an |with |but )?(?:may )?(?:flowering )?continu(?:es|ing|e)\\b[^.;]*?\\b(?:until|to|into|through)(?: until| to)?|-|to|through to|through|until|into|till|,? (?:rarely|occasionally|sometimes|mainly|mostly) (?:extending )?(?:to|into|through to)|,? (?:or )?(?:occasionally |rarely |sometimes )?(?:as late as|as early as))\\s*\\(?\\s*$"
  while (i <= nrow(toks)) {
    a <- toks$a[i]; b <- toks$b[i]
    while (i < nrow(toks)) {
      gap <- str_sub(x, toks$end[i] + 1, toks$start[i + 1] - 1)
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
hedge <- "(?:,\\s*)?(?:\\b(?:and|but|although)\\s+)?\\b(?:possibly|probably|likely|almost certainly|it may also|may also|could|might|may occur|thought to be|is thought|appears to be|appear to be|seems to be|presumably)\\b.*$"
kw_both <- "\\b(?:flower(?:s|ing)?,? (?:and |, )?(?:fruit(?:s|ing)?|seeds?)|flowers, fruit and seed|flowers and fruit|flowering and fruiting)\\b"
kw_flower <- "\\b(?:flower(?:s|ing|ed)?|in flower|anthesis)\\b"
kw_bud <- "\\b(?:flower buds?|buds?)\\b"
kw_fruit <- "\\b(?:fruit(?:s|ing|ed)?|pods?|seeding|seeds? (?:ripen|become ripe|are ripe|mature)|ripe seeds?|mature seeds?|capsules?|cones?)\\b"
# months per flowering / fruiting clause of one sentence
pheno_hits <- function(s2) {
  out <- list(fl = integer(0), fr = integer(0))
  hits <- bind_rows(
    str_locate_all(s2, regex(kw_both, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "both"),
    str_locate_all(s2, regex(kw_bud, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "bud"),
    str_locate_all(s2, regex(kw_flower, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "flower"),
    str_locate_all(s2, regex(kw_fruit, ignore_case = TRUE))[[1]] %>% as_tibble() %>% mutate(type = "fruit")
  ) %>% arrange(start, desc(end))
  if (!nrow(hits)) return(out)
  keep <- rep(TRUE, nrow(hits)); last_end <- 0
  for (i in seq_len(nrow(hits))) { if (hits$start[i] <= last_end) keep[i] <- FALSE else last_end <- hits$end[i] }
  hits <- hits[keep, ]
  # "The first flowers appear in late July and flowering continues to January": one flowering clause
  hits <- hits[c(TRUE, hits$type[-1] != hits$type[-nrow(hits)]), ]
  for (i in seq_len(nrow(hits))) {
    seg_end <- if (i < nrow(hits)) hits$start[i + 1] - 1 else nchar(s2)
    seg <- str_sub(s2, hits$start[i], seg_end)
    if (i == 1) seg <- paste(str_sub(s2, 1, hits$start[1] - 1), seg)
    m <- months_from_text(seg)
    if (!length(m)) next
    if (hits$type[i] %in% c("flower", "both")) out$fl <- c(out$fl, m)
    if (hits$type[i] %in% c("fruit", "both")) out$fr <- c(out$fr, m)
  }
  out
}
# Parenthetical extremes ("Flowers (Oct.-) Dec.-Feb. (-July)"): the typical months are read without them, the full
# span with them; the extra months go on a separate "rarely" row (user's choice), the typical ones on a "usually" row
mon_pat <- "(?:Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)[a-z]*\\.?"
# parenthetical extremes can be seasons too ("(late winter-) spring")
ext_pat <- "(?:(?:Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)[a-z]*\\.?|(?:early |mid |late )?(?:spring|summer|autumn|winter))"
phenology_parse <- function(txt) {
  fl <- c(); fr <- c(); fl_s <- c(); fr_s <- c(); fl_x <- c(); fr_x <- c(); q_extra <- list(); main_q <- c(fl = NA_character_, fr = NA_character_)
  txt <- str_replace_all(txt, regex("\\bthroughout year\\b", ignore_case = TRUE), "throughout the year")
  # "mostly June-Nov., with a few records from Jan. and Apr.": the stray records go on a "rarely" row
  txt <- str_replace_all(txt, regex(",?\\s*with occasional (?:records|collections) (?:from |in )?", ignore_case = TRUE), ", occasionally ")
  txt <- str_replace_all(txt, regex(",?\\s*with (?:a few|few|isolated|scattered) (?:records|collections) (?:from |in )?", ignore_case = TRUE), ", rarely ")
  # citations / access dates are not flowering months ("(Australasian Virtual Herbarium, accessed 21 July 2023)")
  txt <- str_remove_all(txt, "\\([^()]*\\b(?:1[89]|20)[0-9]{2}\\b[^()]*\\)")
  # "(buds appear in January)", "(buds develop February-August)" are not flowering months
  txt <- str_remove_all(txt, regex("\\(\\s*(?:flower )?buds?\\b[^()]*\\)", ignore_case = TRUE))
  # "Dec.-Feb. (-July)": the abbreviation's full stop is not a sentence end
  txt <- str_replace_all(txt, "(?<=\\b(?:Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec))\\.\\s+(?=\\(\\s*-)", " ")
  for (s in sentences(txt)) {
    s2 <- s
    # "Flowers most of the year, primarily June-Oct.": the months on a "primarily" row; "most (months) of the year"
    # alone is not a calendar
    mq <- str_match(s2, regex("(?:for |during )?(?:sporadically )?most (?:months )?of (?:the )?year,?\\s*(?:with (?:an? )?)?(primarily|mainly|mostly|particularly|especially|peak(?:s|ing)?)\\s+(?:(?:in|from|during)\\s+)?", ignore_case = TRUE))
    if (!is.na(mq[1])) {
      pre <- str_sub(s2, 1, str_locate(s2, fixed(mq[1]))[1, 1] - 1)
      kk <- if (str_detect(pre, regex(kw_both, ignore_case = TRUE))) c("fl", "fr") else if (str_detect(pre, regex(kw_fruit, ignore_case = TRUE))) "fr" else "fl"
      main_q[kk] <- str_replace(str_to_lower(mq[2]), "^peak(?:s|ing)$", "peak"); s2 <- str_replace(s2, fixed(mq[1]), "")
    }
    s2 <- str_remove_all(s2, regex(",?\\s*(?:for |during )?(?:sporadically )?most (?:months )?of (?:the )?year,?", ignore_case = TRUE))
    s2 <- str_remove_all(s2, regex("\\b(?:probably|possibly|perhaps) (?:dependent|depending) on [a-z ]+", ignore_case = TRUE))
    s2 <- str_remove_all(s2, regex(hedge, ignore_case = TRUE))
    lo_re <- paste0("\\(\\s*(", ext_pat, ")\\s*-\\s*\\)"); hi_re <- paste0("\\(\\s*-\\s*(", ext_pat, ")\\s*\\)")
    # "Flowers and fruits at any time but mainly September-March": X on a "mainly" row, the whole year on an
    # "occasionally" row (user's choice)
    # (also "all year round, but mainly in spring", "all year, but predominantly July-December")
    # (and "throughout the year with peak in November-December", "year round, prolifically from c. November-March")
    am <- str_match(s2, regex("\\b(?:(?:at )?any time(?: of (?:the )?year)?|all year(?: round| around)?|year[- ]round|throughout the year),?\\s*(?:but\\s+|with (?:an? )?)?(mainly|mostly|chiefly|usually|especially|particularly|primarily|predominantly|peak(?:s|ing)?|prolifically|most frequently|most commonly)\\s+(?:(?:in|from|during)\\s+)?(?:c\\.\\s*)?", ignore_case = TRUE))
    # (only when months or seasons follow: "all year round, most commonly in association with seasonal rainfall" is all year)
    if (!is.na(am[1]) && !str_detect(str_sub(s2, str_locate(s2, fixed(am[1]))[1, 2] + 1), regex(paste0(mon_pat, "|spring|summer|autumn|winter|wet season|dry season"), ignore_case = TRUE))) am[1] <- NA
    if (!is.na(am[1])) {
      s2 <- str_replace(s2, fixed(am[1]), "")
      allyr <- pheno_hits(str_replace(s2, regex(paste0(mon_pat, "|(?:early |mid |late )?(?:spring|summer|autumn|winter)"), ignore_case = TRUE), "throughout the year"))
      for (k in c("fl", "fr")) if (length(allyr[[k]]) == 12) main_q[k] <- str_replace(str_to_lower(am[2]), "^peak(?:s|ing)$", "peak") %>% str_replace("^most (?:frequently|commonly)$", "mostly")
      for (k in c("fl", "fr")) if (length(allyr[[k]]) == 12) q_extra[[length(q_extra) + 1]] <- tibble(k = k, q = "occasionally", m = list(1:12), s = s)
    }
    # "mainly Oct.-May, can flower sporadically throughout year": an all-year row with that commonness word
    sm <- str_match(s2, regex(",?\\s*(?:and |but )?(?:can |may )?(?:flower(?:s|ing)? )?(sporadically|occasionally|rarely|intermittently) (?:throughout (?:the )?year|all year(?: round)?|at any time)\\b", ignore_case = TRUE))
    if (!is.na(sm[1]) && str_detect(str_remove(s2, fixed(sm[1])), regex(paste0(mon_pat, "|spring|summer|autumn|winter"), ignore_case = TRUE))) {
      pre <- str_sub(s2, 1, str_locate(s2, fixed(sm[1]))[1, 2])
      s2 <- str_replace(s2, fixed(sm[1]), "")
      allyr <- pheno_hits(pre)
      for (k in c("fl", "fr")) if (length(allyr[[k]]) == 12) q_extra[[length(q_extra) + 1]] <- tibble(k = k, q = str_to_lower(sm[2]), m = list(1:12), s = s)
      if (str_detect(s2, regex("\\b(?:mainly|mostly|chiefly)\\b", ignore_case = TRUE))) for (k in c("fl", "fr")) if (length(allyr[[k]]) == 12) main_q[k] <- str_to_lower(str_extract(s2, regex("\\b(?:mainly|mostly|chiefly)\\b", ignore_case = TRUE)))
    }
    # "Flowers November-February, occasionally August-May"; "October-March, sometimes until June": the qualified
    # alternative goes on its own commonness row (the months it adds to the typical calendar)
    qm <- str_match(s2, regex(paste0("^(.*?" , mon_pat, ".*?),\\s*(?:but\\s+|and\\s+)?(occasionally|rarely|sometimes|sporadically|infrequently)\\s+(?:from\\s+)?(?:(?:extending|continuing)\\s+)?([^;]*", mon_pat, "[^;]*?)(;.*)?$"), ignore_case = TRUE))
    if (!is.na(qm[1])) {
      main_s <- paste0(qm[2], coalesce(qm[5], ""))
      lead <- str_extract(qm[2], regex("^[^0-9]*?\\b(?:flower(?:s|ing)?|fruit(?:s|ing)?)\\b(?: and (?:fruits?|flowers?))?", ignore_case = TRUE))
      last_m <- tail(str_extract_all(qm[2], regex(mon_pat, ignore_case = TRUE))[[1]], 1)
      q_txt <- qm[4]
      if (str_detect(q_txt, regex("^(?:until|to|into|till|through)\\b", ignore_case = TRUE)) && length(last_m)) q_txt <- paste(last_m, "-", str_remove(q_txt, regex("^(?:until|to|into|till|through)\\s+", ignore_case = TRUE)))
      typ_q <- pheno_hits(str_squish(str_remove_all(str_remove_all(main_s, lo_re), hi_re)))
      alt <- pheno_hits(paste(coalesce(lead, ""), q_txt))
      for (k in c("fl", "fr")) {
        add_m <- setdiff(alt[[k]], typ_q[[k]])
        if (length(add_m)) q_extra[[length(q_extra) + 1]] <- tibble(k = k, q = str_to_lower(qm[3]), m = list(add_m), s = s)
      }
      s2 <- main_s
    }
    has_x <- str_detect(s2, lo_re) || str_detect(s2, hi_re)
    typ <- pheno_hits(str_squish(str_remove_all(str_remove_all(s2, lo_re), hi_re)))
    if (length(typ$fl)) { fl <- c(fl, typ$fl); fl_s <- c(fl_s, s) }
    if (length(typ$fr)) { fr <- c(fr, typ$fr); fr_s <- c(fr_s, s) }
    if (has_x) {
      full <- pheno_hits(str_replace_all(str_replace_all(s2, lo_re, "\\1 -"), hi_re, "- \\1"))
      fl_x <- c(fl_x, setdiff(full$fl, typ$fl)); fr_x <- c(fr_x, setdiff(full$fr, typ$fr))
    }
  }
  fl_x <- setdiff(fl_x, fl); fr_x <- setdiff(fr_x, fr)
  list(fl = yn(sort(unique(fl))), fl_s = collapse_text(fl_s, " "), fr = yn(sort(unique(fr))), fr_s = collapse_text(fr_s, " "),
       fl_x = yn(sort(unique(fl_x))), fr_x = yn(sort(unique(fr_x))), q_extra = bind_rows(q_extra), main_q = main_q)
}

# ---------------------------------------------------------------- per-taxon extraction
other_organs <- "shoots?|petioles?|petiolate|petiolules?|pedicels?|pedicles?|papillae|peduncles?|stalks?|stipes?|stipules?|bracts?|bracteoles?|hairs?|scales?|glands?|spines?|thorns?|prickles?|teeth|lobes?|tube|throat|limb|lips?|calyx|corolla|sepals?|petals?|stamens?|filaments?|anthers?|styles?|stigmas?|ovary|seeds?|arils?|wings?|beak|radicle|rachis|axis|axes|internodes?|sheaths?|ligules?|veins?|midribs?|leaflets?|pinnae|pinnules?|leaves|leaf|blades?|lamina|branchlets?|branches|stems?|trunks?|roots?|pneumatophores?|heads?|spikes?|racemes?|inflorescences?|flowers?|fruits?|capsules?|pods?|buds?|cotyledons?|tubers?|rhizomes?|bulbs?|corms?|segments?|apex|tips?|base|margins?|disc|hypanthium|operculum|valves|spikelets?|awns?|glumes?|lemmas?|paleas?|tepals?|scapes?|culms?|cones?|bulbils?|tendrils?|cladodes?|phyllodes?"
excl <- function(...) paste(setdiff(str_split(other_organs, "\\|")[[1]], c(...)), collapse = "|")

extract_one <- function(r, group_filter = TRUE) {
  taxon <- r[["scientific_name"]]; fam <- r[["family"]]
  trt <- pick_treatment(r[["Description"]])
  desc <- dehedge(prep(trt$text))
  # "Similar to subsp. variabilis but leaves broadly orbicular, ...": what follows "but" is this taxon's own description
  if (!is.na(desc)) desc <- str_replace(desc, regex("^((?:similar to|differs? from|differing from|as for|like) (?:[^.]|\\.(?=\\s[a-z]))*?),? but (?=[a-z])", ignore_case = TRUE), "\\1. \u00b6") %>%
    str_replace("\u00b6([a-z])", function(z) str_to_upper(str_sub(z, 2)))
  # Aizoaceae: the "operculum" is the lid of the circumscissile capsule, not a petal cap as in Myrtaceae
  if (fam == "Aizoaceae" && !is.na(desc)) desc <- str_replace_all(desc, "\\bOperculum\\b", "Capsule operculum")
  # genus / family descriptions summarise variation: sentences scoped to some species ("or introduced basal-rosetted
  # species", "most species ...") and comma segments carrying a commonness word are not universal, so they are dropped
  if (r[["rank"]] %in% c("genus", "family") && group_filter) {
    ss <- sentences(desc)
    ss <- ss[!str_detect(ss, rx("\\b(?:species|spp|some|most|many|several|others?|introduced|naturalised|cultivated|exotic)\\b"))]
    ss <- map_chr(ss, function(x) {
      # ranges ("herbaceous to coriaceous", "globose to obloid-ovoid") are variation across the group: the range words are
      # removed, the rest of the segment kept ("Fruit a globose to obloid-ovoid, syncarpous fleshy berry")
      adv <- "(?:(?:slightly|weakly|strongly|very|more|less|shortly|narrowly|broadly|widely|deeply|shallowly|densely|sparsely|somewhat|minutely|finely|irregularly)\\s+)?"
      x <- str_remove_all(x, paste0(adv, "\\b(?!(?:up|reduced|similar|adnate|attached|fused|close|due|next|equal|subequal|inserted|appressed|confined|restricted|tapering|tapered|narrowed|narrowing|extending|leading|according|relative|opposite|decurrent|connate|joined)\\b)[a-z-]+(?: to ", adv, "(?!the\\b|an?\\b|[0-9])[a-z-]+)+"))
      seg <- str_split(x, "(?<=[,;:])\\s+")[[1]]
      # segments naming a particular genus ("glandular-pubescent (Hydrocleys)") describe only that genus
      # ... and alternatives ("smooth or variously winged") are variation across the group
      drop <- str_detect(seg, rx(qual_re)) | str_detect(seg, "\\([A-Z][a-z]+") | str_detect(seg, "\\bor\\b")
      # a commonness word at the head of a list ("Flowers mostly hermaphroditic, hypogynous, actinomorphic", "Leaves usually
      # petiolate, fleshy, coriaceous ...") may govern the whole list: the rest of the clause is dropped too
      # (only for the clause's opening segment, and not when the commonness word governs a count: "Seeds usually 3-6, angled, black")
      clause_end <- str_detect(seg, ";$|:$")
      opens <- c(TRUE, head(clause_end, -1))
      # ("Deciduous or sometimes evergreen trees": a commonness word after "or" governs only that alternative)
      lead_q <- opens & str_detect(map_chr(str_split(str_remove_all(seg, "\\([^)]*\\)"), "\\s+"), ~ paste(head(.x, 3), collapse = " ")), rx(qual_re)) &
        !str_detect(seg, rx(paste0("\\bor\\s+", qual_re))) &
        !str_detect(seg, rx(paste0(qual_re, "\\s+(?:c\\.\\s*)?[0-9(]")))
      for (k in which(lead_q)) { j <- k; while (j <= length(seg)) { drop[j] <- TRUE; if (clause_end[j]) break; j <- j + 1 } }
      # a dropped segment keeps its organ noun, so the segments after it still belong to that organ ("Seeds ... or ..., black")
      seg[drop] <- map_chr(seg[drop], function(z) {
        cc <- classify_token(z)
        # the kept head carries a marker so that it is not read as a statement ("stipules absent or small" is not "stipules present")
        if (cc$kind == "organ") paste0(str_sub(z, 1, nchar(cc$subj)), " \u2205,") else ""
      })
      paste(seg[seg != ""], collapse = " ") %>% str_replace_all(",\\s*,", ",") %>% str_remove("[,;:]\\s*$")
    })
    desc <- paste(paste0(ss[ss != ""], "."), collapse = " ")
  }
  fern_mode <<- fam %in% fern_families
  chenopod_mode <<- fam %in% c("Chenopodiaceae", "Casuarinaceae")
  casuarina_mode <<- fam == "Casuarinaceae"
  u <- units_of(desc)
  # kept heads of dropped group segments only carry the organ context forward; they are not statements
  u <- u %>% filter(!str_detect(text, "\u2205"))
  recs <- list()
  add <- function(x) if (!is.null(x) && nrow(x)) recs[[length(recs) + 1]] <<- x
  addc <- function(trait, txt, dict, neg = NULL, generic_neg = TRUE, prep_fun = NULL, ctx_type = NA_character_, ctx_value = NA_character_) {
    if (is.na(txt) || txt == "") return(invisible(NULL))
    h <- scan_terms(txt, dict, neg, generic_neg, prep_fun)
    add(rec_cat(trait, h, txt, ctx_type, ctx_value))
    invisible(h)
  }
  addn <- function(trait, v, txt, ctx_type = NA_character_, ctx_value = NA_character_, scale = 1) {
    if (is.null(v)) return(invisible(NULL))
    add(rec_num(trait, v, txt, ctx_type, ctx_value, scale))
  }
  # first measurement over a set of units; returns list(v, txt) or NULL
  m_first <- function(texts, words, other) {
    for (x in texts) { m <- first_meas(x, words, other); if (!is.null(m)) return(m) }
    NULL
  }
  d_first <- function(texts, other) {
    res <- list(L = NULL, W = NULL)
    for (x in texts) {
      dd <- dims(x, other)
      if (is.null(res$L) && !is.null(dd$L)) res$L <- dd$L
      if (is.null(res$W) && !is.null(dd$W)) res$W <- dd$W
      if (!is.null(res$L)) break
    }
    res
  }
  count_after <- function(texts, pat) {
    for (x in texts) {
      m <- str_match(mark_extremes(x), rx(pat))
      if (!is.na(m[1])) { v <- count_range(m[2], m[3]); return(list(v = c(v, extremes_of(m[1], NA, v[["min"]], v[["max"]], scale_unit = FALSE)), txt = unmark(m[1]), raw = m[1])) }
    }
    NULL
  }

  # ---- habit
  habit <- unit_text(u, "habit")
  habit_txt <- join_units(habit)
  # host / habitat nouns are not this plant's growth form ("on the branches of rainforest trees")
  habit_gf <- if (!is.na(habit_txt)) str_remove_all(str_to_lower(habit_txt), "\\b(?:on|in|among|amongst|under|over|of|from|associated with|beneath|between|around)\\s+(?:the\\s+)?(?:[a-z-]+\\s+){0,3}?(?:trees?(?: boles?| trunks?)?|boles|shrubs|grasses|hosts?|vegetation|plants|mangroves|forests?|rocks)\\b") else NA
  # "Shrub to 2 m high, or rarely tree-like to c. 5 m": a qualified tree-like habit is a tree alternative (user's choice)
  if (!is.na(habit_gf)) habit_gf <- str_replace_all(habit_gf, paste0(qual_re, "\\s+(?:an?\\s+)?tree-like\\b"), "\\1 tree")
  # "tree fern" is one growth form (fern palmoid), not a tree and a fern
  if (!is.na(habit_gf)) habit_gf <- str_replace_all(habit_gf, "\\btree[- ]fern", "treefern")
  gf <- scan_terms(habit_gf, growth_form_dict, fillers = gf_fillers)
  if (nrow(gf)) {
    # specific climber types replace the generic one
    if (any(gf$value %in% c("climber_herbaceous", "climber_woody"))) gf <- gf %>% filter(value != "climber")
    # "tufted" graminoids are tussocks, in text order
    tuft <- str_locate(str_to_lower(habit_gf), "\\btuft(?:ed|s)\\b")[1]
    if (!is.na(tuft) && any(gf$value == "graminoid") && !any(gf$value == "tussock"))
      gf <- bind_rows(gf, tibble(pos = tuft, value = "tussock", term = "tufted", qual = gf$qual[gf$value == "graminoid"][1])) %>% arrange(pos)
  }
  # a tussock with no other growth form named is a graminoid (user's choice: Lomandra "Tussock c. 10-20 cm diam.");
  # Lomandra "Plant tufted" with no growth-form noun -> graminoid
  if (nrow(gf) && all(gf$value == "tussock")) gf <- bind_rows(gf, tibble(pos = max(gf$pos) + 1L, end = max(gf$pos) + 1L, value = "graminoid", term = "[tussock]", qual = gf$qual[1], region = NA_character_))
  # (in text order: "Large tufts or small trees" -> graminoid tree)
  tpos <- if (!is.na(habit_gf)) str_locate(habit_gf, "\\b(?:tuft(?:ed|s)?|caespitose|cespitose)\\b")[1] else NA
  if (word(taxon, 1) == "Lomandra" && !is.na(tpos) && !any(gf$value %in% c("graminoid", "tussock")))
    gf <- bind_rows(gf, tibble(pos = as.integer(tpos), end = as.integer(tpos), value = "graminoid", term = "[Lomandra, tufted]", qual = NA_character_, region = NA_character_)) %>% arrange(pos)
  # ferns: the growth form is definitional by family when the text does not name it
  # (a fern with a trunk is a tree fern: fern palmoid)
  if (fern_mode && !nrow(gf) && !is.na(desc)) gf <- tibble(pos = 1L, end = 1L, value = if (str_detect(desc, rx("\\btrunks?\\b[^.;]{0,40}?\\b[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s?m\\b[^.;]{0,20}?(?:tall|high)"))) "fern palmoid" else "fern",
                                                            term = "[fern family]", qual = NA_character_, region = NA_character_)
  # Arecaceae profiles that open "Trunk to 25 m tall" are palms; Centrolepidaceae are graminoid herbs (user's choices)
  # (and cycads: Cycadaceae / Zamiaceae)
  if (fam %in% c("Arecaceae", "Cycadaceae", "Zamiaceae") && !nrow(gf)) gf <- tibble(pos = 1L, end = 1L, value = "palmoid", term = paste0("[", fam, "]"), qual = NA_character_, region = NA_character_)
  # graminoid families (Centrolepidaceae rule extended to sedges, grasses, rushes, restiads): graminoid, plus herb when no form is named
  if (fam %in% c("Centrolepidaceae", "Cyperaceae", "Poaceae", "Juncaceae", "Restionaceae", "Anarthriaceae", "Ecdeiocoleaceae")) {
    if (!nrow(gf)) gf <- tibble(pos = 1L, end = 1L, value = "herb", term = paste0("[", fam, "]"), qual = NA_character_, region = NA_character_)
    if (!any(gf$value == "graminoid")) gf <- bind_rows(gf, tibble(pos = max(gf$pos) + 1L, end = max(gf$pos) + 1L, value = "graminoid", term = paste0("[", fam, "]"), qual = NA_character_, region = NA_character_))
  }
  # climbing ferns are herbaceous climbers
  if (fern_mode) gf <- gf %>% mutate(value = str_replace(value, "^climber(?:_woody)?$", "climber_herbaceous"))
  if (fam %in% lyco_families) gf <- gf %>% mutate(value = str_replace(value, "\\bfern\\b", "lycophyte"))
  # a climbing habit on a shrub / herb is a growth-form alternative too: "Shrub to 3 m high, rarely climbing"
  # -> shrub (usually) + climber_woody (rarely); "Spreading shrub, sometimes scandent" -> climber_woody (sometimes)
  if (nrow(gf) && !any(str_detect(gf$value, "climber"))) {
    ch <- scan_terms(habit_gf, c("climbing|scandent|twining|twiner" = "climb"), generic_neg = TRUE)
    if (nrow(ch)) {
      cv <- if (any(str_detect(gf$value, "shrub|tree|mallee"))) "climber_woody" else if (any(gf$value == "herb")) "climber_herbaceous" else "climber"
      gf <- bind_rows(gf, ch %>% mutate(value = cv)) %>% arrange(pos)
    }
  }
  add(rec_cat("plant_growth_form", gf, habit_txt))
  addc("life_history", habit_txt, life_history_dict)
  # FoA herb descriptions often open with the stems ("Stems prostrate, to 2 m long")
  stem_lead <- unit_text(u, "stem", subj_re = "^(?:the )?(?:main |flowering |aerial |vegetative )?(?:stems?|branches|culms?)$")
  # "trees with whorled, spreading branches": the branches' posture is not a tree's habit (it is for "Shrub with prostrate branches")
  is_tree <- nrow(gf) && all(gf$value %in% c("tree", "mallee", "palmoid"))
  addc("stem_growth_habit", collapse_text(c(habit_txt, stem_lead), " | "), stem_habit_dict,
       prep_fun = function(x) if (is_tree) str_remove_all(x, "\\b(?:with|and) (?:[a-z-]+,? (?:and |or )?){1,3}(?:branches|branchlets)\\b") else x)
  sl <- m_first(stem_lead, "long|in length", excl("stems?", "branches", "branchlets?", "culms?"))
  if (is.null(sl) && length(habit)) {
    sm <- str_match(mark_extremes(join_units(habit)), rx(paste0("\\b(?:stems?|branches)\\b[^,;|]{0,30}?(", meas, ")(?:\\s+or more)?\\s*long")))
    if (!is.na(sm[1])) sl <- list(v = meas_of(sm[2]), txt = unmark(sm[1]))
  }
  if (!is.null(sl)) addn("stem_length", sl$v, sl$txt)
  addc("stem_branching_form", habit_txt, branching_dict)
  addc("plant_succulence", habit_txt, c("succulent" = "succulent"))
  addc("plant_growth_substrate", habit_txt, substrate_dict)
  addc("parasitic", habit_txt, parasitic_dict)

  # heights / widths (habit units)
  h_other <- "articles?|pseudostems?|trunks?|stems?|branches|branchlets|pneumatophores?|leaves|scapes?|inflorescences?|flowering stems?|culms?|roots?|spines?|flowers?|rhizomes?|fronds?|lignotubers?|tubers?"
  ht <- m_first(habit, "high|tall|in height", h_other)
  # "Shrub to 40 cm", "Tree to 15 m, glabrous", "Solitary palm to 20 m": a bare size right after the growth-form noun is the height
  if (is.null(ht) && length(habit)) {
    bh <- str_match(mark_extremes(habit[1]), rx(paste0("^[^,;|]*?\\b(?:herbs?|shrubs?|subshrubs?|undershrubs?|shrublets?|trees?|treelets?|mallees?|perennials?|annuals?|biennials?|ephemerals?|palms?|plants?|geophytes?|climbers?|vines?|twiners?)\\b([^,;|0-9]{0,20}?)(", meas, ")(?=\\s*(?:[,;|.]|$|(?:or|and|with|often|rarely|usually)\\b))")))
    # ("Prostrate herb with stems to 1 m" is a stem length)
    if (!is.na(bh[1]) && !str_detect(bh[2], rx(paste0("\\b(?:", h_other, ")\\b")))) ht <- list(v = meas_of(bh[3]), txt = unmark(bh[3]))
  }
  is_climber <- nrow(gf) && any(str_detect(gf$value, "climber"))
  if (!is.null(ht)) {
    # a "climbing palm to 45 m tall" (rattan) is a climber too
    addn(if (is_climber && all(str_detect(gf$value, "climber|palmoid"))) "plant_height_climbing_plant" else "plant_height", ht$v, ht$txt, scale = 1 / 1000)
  } else if (is_climber) {
    cl <- m_first(habit, "long|in length|", h_other)
    if (!is.null(cl)) addn("plant_height_climbing_plant", cl$v, cl$txt, scale = 1 / 1000)
  }
  wd <- m_first(habit, "wide|across|diam|diameter|in diameter|broad", h_other)
  # "Tufts to c. 2.5 cm wide at base" is the basal width, not the plant's width
  if (!is.null(wd) && str_detect(join_units(habit), rx(paste0(fixed_re(wd$txt), "\\.?\\s*(?:at (?:the )?base|at base of)")))) wd <- NULL
  if (!is.null(wd)) addn("plant_width", wd$v, wd$txt, scale = 1 / 1000)

  # ---- stems, bark, underground organs
  stem <- unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig|shoot|cane|axes|culm")
  stem_txt <- join_units(stem)
  addc("stem_hairs", join_units(unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig|shoot|cane")), hairs_dict %>% { .[. != "glandular_pubescent"] } %>% c("glandular-(?:pubescent|hairy|pilose|puberulous)|glandular hairs" = "hairy"),
       prep_fun = function(x) str_remove_all(x, "\\b(?:when young|young parts?|at first|initially)\\b"))
  addc("stem_shape", join_units(unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig")), stem_shape_dict)
  if (is.null(ht)) {
    culm <- m_first(unit_text(u, "stem", subj_re = "culm|stem"), "tall|high", excl("culms?", "stems?"))
    if (!is.null(culm)) addn("plant_height", culm$v, paste("[culm/stem]", culm$txt), scale = 1 / 1000)
    # palms: "Trunk to 25 m tall, 25-40 cm diam."
    if (is.null(culm)) {
      trk <- m_first(unit_text(u, "stem", subj_re = "^(?:the )?trunks?$"), "tall|high", excl("trunks?"))
      # (tree ferns: trunk height on a plant_organ_measured = trunk row, as in LucidFerns_2026)
      if (!is.null(trk)) addn("plant_height", trk$v, paste("[trunk]", trk$txt), if (fern_mode) "plant_organ_measured" else NA_character_, if (fern_mode) "trunk" else NA_character_, scale = 1 / 1000)
    }
  }
  # trunk / main stem diameter ("trunk to 2 m diam.", "stems to 3 cm diam.") from the opening sentences
  sdm <- m_first(unit_text(u %>% filter(si <= 2), "stem", subj_re = "^(?:the )?(?:trunks?|stems?|main stems?|culms?)$"), "diam|diameter|in diameter|across|wide", excl("trunks?", "stems?", "culms?"))
  if (is.null(sdm)) {
    sm2 <- str_match(mark_extremes(join_units(habit) %||% ""), rx(paste0("\\b(?:with (?:a )?)?(?:trunks?|stems?)\\b[^,;|]{0,20}?(", meas, ")\\s*(?:diam|diameter|in diameter)")))
    if (!is.na(sm2[1])) sdm <- list(v = meas_of(sm2[2]), txt = unmark(sm2[1]))
  }
  if (!is.null(sdm)) addn("stem_diameter", sdm$v, sdm$txt)
  bark_txt <- join_units(c(unit_text(u, "bark", parts = "any")))
  addc("bark_texture", bark_txt, bark_texture_dict)
  addc("bark_colour", bark_txt, bark_colour_dict, prep_fun = function(x) {
    x <- str_remove_all(x, "\\b[a-z]+ when (?:wet|fresh|freshly exposed|new)\\b")
    str_replace_all(x, paste0("\\b(?:", cw_mod, ")\\s+(?=(?:", cw, ")\\b)"), "")
  })
  under_txt <- join_units(unit_text(u, "underground", parts = "any"))
  for (org in setdiff(c("bulb", "corm", "tuber", "pseudobulb", "rhizome", "lignotuber", "caudex"), if (fern_mode) c("rhizome", "caudex"))) {
    ou <- unit_text(u, "underground", subj_re = paste0("^(?:the )?", org, "s?$"))
    if (!length(ou)) next
    od <- dims(join_units(ou), excl(paste0(org, "s?")))
    if (!is.null(od$L)) addn("storage_organ_length", od$L$v, od$L$txt, "entity_measured", org)
    if (!is.null(od$W)) addn("storage_organ_diameter", od$W$v, od$W$txt, "entity_measured", org)
  }
  storage_txt <- collapse_text(c(habit_txt, under_txt, stem_txt), " | ")
  addc("storage_organ", storage_txt, if (fern_mode) storage_dict[!str_detect(storage_dict, "^(?:rhizome|caudex)$")] else storage_dict, storage_neg)
  def_txt <- collapse_text(c(habit_txt, stem_txt, join_units(unit_text(u, "stipule", parts = "any"))), " | ")
  dh <- scan_terms(def_txt, defence_dict, defence_neg)
  lam_all <- unit_text(u, c("leaf", "lamina", "leaflet"), parts = "any")
  ldh <- scan_terms(join_units(lam_all), leaf_defence_dict)
  if (nrow(ldh) || any(dh$value != "absent")) dh <- dh %>% filter(value != "absent")
  def_desc <- collapse_text(c(if (nrow(dh)) def_txt, if (nrow(ldh)) join_units(lam_all)), " | ")
  add(rec_cat("plant_physical_defence_structures", dh, def_desc))
  add(rec_cat("plant_physical_defence_structures", ldh, def_desc))
  # (adventitious / aerial roots are a climbing mechanism only for climbers: Centrolepidaceae stems "producing axillary adventitious roots")
  is_climbing <- (nrow(gf) && any(str_detect(gf$value, "climber"))) || str_detect(coalesce(habit_txt, ""), rx("climb|scandent|twin"))
  addc("plant_climbing_mechanism", collapse_text(c(habit_txt, stem_txt, join_units(unit_text(u, "leaf", parts = "any"))), " | "),
       if (is_climbing) climbing_dict else climbing_dict[climbing_dict != "adventitious_roots"])

  # ---- leaves
  leaf_gen <- unit_text(u, "leaf")
  lam <- unit_text(u, "lamina")
  lam_or_leaf <- if (length(lam)) lam else leaf_gen
  leaf_main_txt <- join_units(c(leaf_gen, lam))
  leaflet <- unit_text(u, "leaflet")
  is_phyllode <- any(str_detect(coalesce(u$subj[u$sec == "leaf"], ""), "phyllode"))
  is_cladode <- any(str_detect(coalesce(u$subj[u$sec == "leaf"], ""), "cladode|phylloclade"))
  if (is_phyllode) add(rec_cat("plant_photosynthetic_organ", tibble(pos = 1L, value = "phyllode", term = "phyllodes", qual = NA), join_units(leaf_gen)))
  if (is_cladode) add(rec_cat("plant_photosynthetic_organ", tibble(pos = 1L, value = "cladode", term = "cladodes", qual = NA), join_units(leaf_gen)))
  all_txt <- str_to_lower(desc %||% "")
  # ("flowering stems usually leafless" is not a leafless plant)
  # (only habit / stem / leaf clauses: "Inflorescence scape to 25 cm long, usually leafless")
  all_txt <- str_to_lower(join_units(unit_text(u, c("habit", "stem", "leaf", "lamina", "other"), parts = "any")) %||% "")
  all_txt <- str_remove_all(all_txt, "\\b(?:flowering |fertile |upper )?(?:stems?|scapes?|branches|shoots?|peduncles?|culms?|axes) (?:[a-z]+ ){0,2}leafless\\b")
  if (str_detect(all_txt, "\\bleafless\\b|\\bleaves (?:absent|0)\\b|\\bplants? without leaves\\b")) {
    lt <- grab_sent(desc, "leafless|leaves absent")
    lh <- scan_terms(lt, c("leafless|leaves absent|leaves 0|without leaves" = "leafless"), generic_neg = FALSE)
    add(rec_cat("leaf_length_type", lh, lt))
    add(rec_cat("leaf_type", lh, lt))
  } else if (str_detect(all_txt, "\\bleaves (?:reduced to|scale-like)|\\bscale-like leaves\\b|\\bleaves (?:are )?(?:minute )?scales\\b")) {
    lt <- grab_sent(desc, "leaves (?:reduced to|scale-like)|scale-like leaves|leaves (?:are )?(?:minute )?scales")
    add(rec_cat("leaf_length_type", tibble(pos = 1L, value = "scale_leaves", term = "scale", qual = NA), lt))
    add(rec_cat("leaf_type", tibble(pos = 1L, value = "scale", term = "scale", qual = NA), lt))
  } else if (str_detect(str_remove_all(str_to_lower(leaf_main_txt %||% ""), "(?:needle-like|acicular|needle-shaped) (?:[a-z-]+ ){0,3}(?:spines|prickles|bristles|hairs)"), "needle-like|acicular|needle-shaped")) {
    add(rec_cat("leaf_type", tibble(pos = 1L, value = "needle", term = "needle-like", qual = NA), leaf_main_txt))
  }
  gen_txt <- join_units(leaf_gen)
  # Casuarinaceae: leaves reduced to whorls of scale-like "teeth"
  if (casuarina_mode && str_detect(desc %||% "", rx("\\bteeth\\b"))) {
    add(rec_cat("leaf_length_type", tibble(pos = 1L, value = "scale_leaves", term = "teeth (scale leaves)", qual = NA), grab_sent(desc, "teeth")))
    add(rec_cat("leaf_type", tibble(pos = 1L, value = "scale", term = "teeth (scale leaves)", qual = NA), grab_sent(desc, "teeth")))
  }
  # counts of basal or cauline leaves alone go on entity_measured rows (basal_leaf / cauline_leaf), never summed
  for (lk in c("", "basal ", "cauline ", "rosette ")) {
    lcn <- count_after(leaf_gen, paste0("^", lk, "leaves (?:usually |mostly |c\\. |up to )?", cnt, "(?=,|$| per| in| \\()(?!\\s*(?:mm|cm|m)\\b)"))
    if (!is.null(lcn)) addn("leaf_count", lcn$v, lcn$txt, if (lk == "") NA_character_ else "entity_measured",
                            if (lk == "") NA_character_ else paste0(str_replace(str_trim(lk), "rosette", "basal"), "_leaf"))
  }
  addc("leaf_phyllotaxis", gen_txt, phyllotaxis_dict, prep_fun = function(x) str_remove_all(x, "\\b(?:appearing|appear|seemingly|falsely) [a-z]+"))
  addc("leaf_arrangement", gen_txt, arrangement_dict)
  # rosette is both a stem growth habit and a leaf arrangement (user's rule): copy it to whichever trait lacks it
  rr <- bind_rows(recs)
  if (nrow(rr)) {
    has_ros <- function(tr) any(rr$trait == tr & str_detect(coalesce(rr$value, ""), "\\brosette\\b"))
    ros_desc <- function(tr) collapse_text(rr$desc[rr$trait == tr & str_detect(coalesce(rr$value, ""), "\\brosette\\b")], "; ")
    if (has_ros("stem_growth_habit") && !has_ros("leaf_arrangement"))
      add(rec_cat("leaf_arrangement", tibble(pos = 1L, value = "rosette", term = ros_desc("stem_growth_habit"), qual = NA_character_), ros_desc("stem_growth_habit")))
    if (has_ros("leaf_arrangement") && !has_ros("stem_growth_habit"))
      add(rec_cat("stem_growth_habit", tibble(pos = 1L, value = "rosette", term = ros_desc("leaf_arrangement"), qual = NA_character_), ros_desc("leaf_arrangement")))
  }
  venation_rm <- function(x) str_remove_all(x, "[a-z-]*(?:pinnate|palmate)(?:ly)? (?:veined|nerved|venation|veins)|(?:pinnately|palmately) (?:veined|nerved)|(?:pinnate|palmate) venation|venation (?:is )?(?:pinnate|palmate)|leaflets? [^,;|]*")
  div_txt <- if (fern_mode) join_units(c(leaf_gen, lam)) else gen_txt
  cmp <- scan_terms(div_txt, compound_dict, prep_fun = function(x) str_remove_all(x, "[a-z-]*(?:pinnate|palmate)(?:ly)? (?:veined|nerved|venation|veins)|(?:pinnately|palmately) (?:veined|nerved)|(?:pinnate|palmate) venation|venation (?:is )?(?:pinnate|palmate)"))
  # fern fronds that are only lobed ("pinnatifid", "deeply pinnatisect") are simple
  if (fern_mode && !any(cmp$value == "compound")) {
    sl <- scan_terms(div_txt, c("bipinnatifid|pinnatifid|pinnatisect|pinnatipartite|pinnately lobed|palmately lobed|lobed" = "simple"), generic_neg = FALSE)
    cmp <- bind_rows(cmp, sl) %>% arrange(pos)
  }
  if (!nrow(cmp) && length(leaflet)) cmp <- tibble(pos = 1L, value = "compound", term = "leaflets", qual = NA)
  add(rec_cat("leaf_compoundness", cmp, div_txt %||% join_units(leaflet)))
  expand_degrees <- function(x) {
    x <- str_replace_all(x, "\\b([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)", function(z) {
      m <- str_match(z, "([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)")
      paste(paste0(seq(as.integer(m[2]), as.integer(m[3])), "-", m[4]), collapse = " ") })
    str_replace_all(x, "\\b([1-4])-?\\s*or\\s*([1-4])-(pinnate)", "\\1-\\3 \\2-\\3")
  }
  addc("leaf_lamina_division", div_txt, division_dict, prep_fun = function(x) expand_degrees(str_remove_all(x, "[a-z-]*(?:pinnate|palmate)(?:ly)? (?:veined|nerved|venation|veins)|(?:pinnately|palmately) (?:veined|nerved)|(?:pinnate|palmate) venation|venation (?:is )?(?:pinnate|palmate)")))
  addc("leaf_lobation", join_units(lam_or_leaf), lobation_dict, lobation_neg,
       prep_fun = function(x) str_remove_all(x, "\\blobes? [^,;|]*|\\b(?:[0-9]-)?lobed (?:at|towards) (?:the )?(?:apex|tip)"))
  compound_leaf <- nrow(cmp) && any(cmp$value == "compound")

  # leaf shape / base / apex / margin from the lamina (or general leaf) unit; parts handled separately
  # (a fern lamina is the whole leaf blade: its shape stays the leaf's)
  if (fern_mode) compound_leaf <- FALSE
  if (compound_leaf && length(lam)) { leaflet <- c(leaflet, lam); lam <- character(0); lam_or_leaf <- leaf_gen }
  leaf_shape_unit <- if (compound_leaf) leaflet else c(lam, leaf_gen)
  shape_ctx <- if (compound_leaf) c("leaf_division", "leaflets") else c(NA_character_, NA_character_)
  shape_txt <- join_units(leaf_shape_unit)
  if (!is.na(shape_txt)) shape_txt <- str_replace_all(shape_txt, "\\(\\s*((?:to|or) [a-z-]+)\\s*\\)", "\\1")
  shape_clean <- function(x) {
    x <- shape_prep(x)
    x <- str_remove_all(x, "\\b[a-z-]+ (?:at|towards|near) (?:the )?(?:base|apex|tip|summit)\\b|\\b(?:basally|apically) [a-z-]+")
    x <- str_remove_all(x, "\\b(?:(?:[a-z-]+ ){1,2}(?:to|or|and) ){0,3}(?:[a-z]+ly )?[a-z-]+ (?:at|towards|near) (?:the )?(?:base|apex|tip)\\b")
    x <- str_remove_all(x, "\\b(?:[a-z]+ly )?[a-z-]+ (?:to [a-z-]+ )?(?:base|apex|tip)\\b")
    x <- str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*|\\bin outline\\b")
    str_remove_all(x, "\\b(?:teeth|lobes?|glands?|veins?|midrib|segments)\\b[^,;|]*")
  }
  addc("leaf_shape", shape_txt, leaf_shape_dict, prep_fun = shape_clean, ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
  # base: "base cuneate", "cuneate at base", "basally attenuate", bare "attenuate" / "cuneate" tokens
  base_parts <- u %>% filter(sec %in% (if (compound_leaf) "leaflet" else c("leaf", "lamina")), part %in% c("base", "bases")) %>% pull(text)
  base_inline <- unlist(str_extract_all(str_to_lower(shape_txt %||% ""), "\\b(?:(?:[a-z-]+ ){1,2}(?:to|or|and) ){0,3}(?:[a-z]+ )?[a-z-]+ (?:at|towards) (?:the )?base\\b|\\bbasally [a-z-]+(?: to [a-z-]+)?|(?<=, |^)(?:long-)?(?:attenuate|cuneate)(?=,|$| \\|)|(?<=, |^)(?:[a-z]+ly )?(?:[a-z-]+ (?:to|or) )?(?!at\\b|the\\b|to\\b|its\\b|from\\b|towards\\b|near\\b|with\\b|on\\b)[a-z-]+ base\\b"))
  base_txt <- collapse_text(c(base_parts, base_inline), " | ")
  addc("leaf_base_shape", base_txt, base_dict, prep_fun = function(x) str_remove_all(x, "\\bbases?\\b|\\bbasally\\b|\\bat\\b"), ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
  apex_parts <- u %>% filter(sec %in% (if (compound_leaf) "leaflet" else c("leaf", "lamina")), part %in% c("apex", "apices", "tip", "tips")) %>% pull(text)
  apex_inline <- unlist(str_extract_all(str_to_lower(shape_txt %||% ""), "\\b(?:(?:[a-z-]+ ){1,2}(?:to|or|and) ){0,3}(?:[a-z]+ )?[a-z-]+ (?:at|towards) (?:the )?(?:apex|tip|summit)\\b|\\bapically [a-z-]+(?: to [a-z-]+)?|\\b(?:[a-z-]+ (?:to|or|and) )?[a-z-]+ apically\\b|(?<=, |^)(?:[a-z]+ly )?(?:[a-z-]+ (?:to|or) )?(?!at\\b|the\\b|to\\b|its\\b|towards\\b|near\\b|with\\b|an?\\b)[a-z-]+ apex\\b"))
  # bare apex tokens (", acuminate,", ", acute to obtuse,", ", obtusely and shortly acuminate,")
  apex_bare <- character(0)
  for (x in leaf_shape_unit) for (tk in split_tokens(str_to_lower(x))) {
    tk <- str_remove(tk, "^(?:the )?(?:adult |mature )?(?:leaves|leaf|lamina|blade|phyllodes?|leaflets?)\\s+")
    tk <- str_replace_all(tk, "\\b(?:short|long)-(?=\\s|$)", "")
    w <- str_extract_all(str_remove_all(tk, "\\b(?:shortly|long|obtusely|abruptly|narrowly|broadly|slightly|very|sharply|gradually|often|usually|sometimes|rarely|or|to|and|more|less|mostly|finely|minutely|long-)\\b"), "[a-z-]+")[[1]]
    if (length(w) && all(w %in% c("acuminate", "long-acuminate", "acute", "obtuse", "rounded", "apiculate", "mucronate", "mucronulate", "cuspidate", "caudate", "emarginate", "retuse", "subacute", "spine-tipped", "pungent", "subacuminate", "apiculate")) && any(w %in% c("acuminate", "long-acuminate", "acute", "obtuse", "rounded", "apiculate", "mucronate", "mucronulate", "cuspidate", "caudate", "subacute", "subacuminate")))
      apex_bare <- c(apex_bare, tk)
  }
  apex_txt <- collapse_text(c(apex_parts, apex_inline, apex_bare), " | ")
  addc("leaf_apex_shape", apex_txt, apex_dict, prep_fun = function(x) str_remove_all(x, "\\bap(?:ex|ices)\\b|\\btips?\\b|\\bapically\\b|\\bat\\b"), ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
  margin_parts <- u %>% filter(sec %in% (if (compound_leaf) "leaflet" else c("leaf", "lamina")), part %in% c("margin", "margins", "teeth")) %>% pull(text)
  mg_txt <- collapse_text(c(shape_txt, margin_parts), " | ")
  mh <- scan_terms(mg_txt, margin_dict, prep_fun = function(x) str_remove_all(x, "\\([^)]*\\b(?:teeth|lobes)\\b[^)]*\\)|\\blobes? [^,;|]*|\\bentire (?:plant|length)\\b|(?<!margins? )\\bentire\\b(?=(?:,? (?:to|or) |, [a-z]+ (?:to|or) )(?:[a-z]+ ){0,2}(?:[0-9]-)?lobed\\b)"))
  if (nrow(mh) && any(mh$value != "toothed")) mh <- mh %>% filter(value != "toothed")
  add(rec_cat("leaf_margin", mh, mg_txt, shape_ctx[1], shape_ctx[2]))
  mp_txt <- collapse_text(c(margin_parts, unlist(str_extract_all(str_to_lower(shape_txt %||% ""), "[^,;|]*\\bmargins?\\b[^,;|]*"))), " | ")
  addc("leaf_margin_posture", mp_txt, margin_posture_dict, prep_fun = function(x) str_remove_all(x, "\\bmargins?\\b"), ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
  addc("leaf_lamina_posture", shape_txt, lamina_posture_dict,
       prep_fun = function(x) str_remove_all(x, "[^,;|]*\\bmargins?\\b[^,;|]*|(?:with )?an? (?:[a-z-]+ ){1,2}(?:apex|tip|base)\\b|\\b(?:at|towards) (?:the )?(?:apex|tip|base)\\b"), ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
  surf_parts <- u %>% filter(sec %in% c("leaf", "lamina", "leaflet"), str_detect(coalesce(part, ""), "surface|undersurface|indumentum")) %>% pull(text)
  hair_txt <- collapse_text(c(if (compound_leaf) leaflet else lam_or_leaf, if (compound_leaf) NULL else leaf_gen, surf_parts), " | ")
  hair_prep <- function(x) {
    x <- str_remove_all(x, "\\b(?:young foliage|young growth|new growth)\\b[^;|]*")
    x <- str_remove_all(x, "\\b(?:when young|young leaves|at first|initially|when immature|in bud)\\b[^,;|]*|[^,;|]*\\b(?:when young|at first|initially)\\b")
    x <- str_remove_all(x, "\\b(?:except|apart from|but)\\b[^,;|]*")
    x <- str_remove_all(x, "\\b(?:on|along|confined to|at|towards|near)\\b (?:the )?(?:(?:[a-z-]+|and|or|,) ){0,6}?(?:midribs?|mid-veins?|veins|margins?|petioles?|nodes|base)\\b(?:,? (?:and |or )?(?:main |lateral |leaf )*(?:midribs?|veins|margins?|petioles?))*")
    x <- str_remove_all(x, "\\bwith (?:[a-z-]+ ){0,2}margins?\\b")
    str_remove_all(x, "(?:^|(?<=[,;|]))[^,;|]*\\b(?:midrib|mid-vein|veins|margins?|petioles?|ciliate)\\b[^,;|]*$")
  }
  hctx <- if (compound_leaf) c("leaf_division", "leaflets") else c(NA, NA)
  addc("leaf_hairs_adult_leaves", hair_txt, hairs_dict, prep_fun = hair_prep, ctx_type = hctx[1], ctx_value = hctx[2])
  addc("leaf_glaucousness", join_units(c(leaf_gen, lam, leaflet, surf_parts)), glaucous_dict, glaucous_neg)
  col_txt <- collapse_text(c(lam_or_leaf, if (length(lam)) leaf_gen else NULL, surf_parts, leaflet), " | ")
  leaf_col_prep <- function(x) {
    x <- str_remove_all(x, "\\b(?:with|without)\\b[^,;|]*?\\b(?:midrib|veins?|margins?|glands?|spots?|dots|blotch(?:es)?|markings?|tips?|bases?|hairs)\\b")
    x <- str_remove_all(x, "\\b(?:when young|when dry|on drying|drying [a-z-]+|in herbarium|when immature|young)\\b[^,;|]*")
    x <- str_remove_all(x, "\\b(?:tinged|flushed|suffused)\\b(?: with)?(?: (?:or|and|[a-z-]+)){0,3}|\\b[a-z]+-?(?: or [a-z]+-)?tinged\\b|\\b[a-z]+(?:- or [a-z]+)?-(?:tomentose|pubescent|hairy|villous|sericeous|setose)\\b")
    str_remove_all(x, "[^,;|]*\\b(?:midrib|veins?|glands?|hairs|indumentum|tomentum|scales|margins?|petioles?|spines|prickles|bristles|thorns)\\b[^,;|]*")
  }
  addc("leaf_surface_colour", col_txt, leaf_colour_dict, prep_fun = leaf_col_prep)
  addc("leaf_discolority", col_txt, discolor_dict)
  addc("leaf_surface_reflectivity", col_txt, reflect_dict, prep_fun = leaf_col_prep)
  addc("leaf_texture", collapse_text(c(lam_or_leaf, if (length(lam)) leaf_gen else NULL, leaflet), " | "), texture_dict,
       prep_fun = function(x) str_remove_all(x, "(?:[a-z-]+,? ){0,3}\\b(?:hairs?|bristles|setae|scales|papillae)\\b"))
  addc("plant_succulence", join_units(lam_or_leaf), c("succulent|fleshy" = "succulent_leaves"))
  addc("plant_succulence", stem_txt, c("succulent|fleshy" = "succulent_stems"))
  addc("leaf_attachment", gen_txt, attachment_dict)
  stip_txt <- collapse_text(c(join_units(unit_text(u, "stipule", parts = "any")), unlist(str_extract_all(gen_txt %||% "", rx("[^,;|]*\\b(?:exstipulate|estipulate|stipulate|stipules)\\b[^,;|]*")))), " | ")
  sp <- scan_terms(stip_txt, stipule_dict, generic_neg = FALSE)
  if (!nrow(sp) && length(unit_text(u, "stipule")) && !str_detect(str_to_lower(stip_txt), "absent|lacking|without|0\\b|not seen|\u2205")) sp <- tibble(pos = 1L, value = "present", term = "stipules", qual = NA)
  add(rec_cat("stipule_presence", sp, stip_txt))
  addc("leaf_phenology", collapse_text(c(habit_txt, gen_txt), " | "), phenology_dict)

  # juvenile leaves
  juv <- join_units(unit_text(u, "juvenile", parts = "any"))
  addc("leaf_hairs_juvenile_leaves", juv, hairs_dict)
  addc("leaf_glaucousness_juvenile_leaves", juv, glaucous_dict, glaucous_neg)

  # leaf sizes
  lf_other <- excl("leaves", "leaf", "blades?", "lamina", "phyllodes?", "cladodes?")
  ld <- d_first(lam_or_leaf, lf_other)
  if (!is.null(ld$L)) addn("leaf_length", ld$L$v, ld$L$txt)
  if (!is.null(ld$W)) addn("leaf_width", ld$W$v, ld$W$txt)
  if (length(lam) && is.null(ld$L)) {
    ld2 <- d_first(leaf_gen, lf_other)
    if (!is.null(ld2$L)) addn("leaf_length", ld2$L$v, ld2$L$txt)
    if (!is.null(ld2$W) && is.null(ld$W)) addn("leaf_width", ld2$W$v, ld2$W$txt)
  }
  pet <- m_first(unit_text(u, "petiole"), "long|in length", excl("petioles?"))
  if (is.null(pet)) {
    pm <- str_match(mark_extremes(join_units(c(leaf_gen, lam)) %||% ""), rx(paste0("\\bpetioles?\\b[^;:|]{0,30}?(", meas, ")\\s*long")))
    if (!is.na(pm[1])) pet <- list(v = meas_of(pm[2]), txt = unmark(pm[1]))
  }
  if (!is.null(pet)) addn("petiole_length", pet$v, pet$txt)
  if (length(leaflet) && !fern_mode) {
    lfd <- d_first(leaflet, excl("leaflets?", "pinnae", "pinnules?"))
    if (!is.null(lfd$L)) addn("leaflet_length", lfd$L$v, lfd$L$txt)
    if (!is.null(lfd$W)) addn("leaflet_width", lfd$W$v, lfd$W$txt)
  }
  if (fern_mode) {
    # one size per division, in the source's words ("primary pinnae", "secondary pinnae"); "longest pinnae ..." sizes
    # are labelled as such ("longest primary pinnae")
    for (sj in unique(na.omit(u$subj[u$sec == "leaflet"]))) {
      uu <- unit_text(u, "leaflet", subj_re = paste0("^", sj, "$"))
      lfd <- d_first(uu, excl("leaflets?", "pinnae", "pinna", "pinnules?"))
      dv <- str_replace(str_replace(sj, "^pinna$", "pinnae"), "^(?:the )?", "")
      for (k in c("L", "W")) {
        if (is.null(lfd[[k]])) next
        src <- uu[map_lgl(uu, ~ str_detect(.x, fixed(str_sub(lfd[[k]]$txt, 1, 12))))][1]
        dvk <- if (!is.na(src) && str_detect(src, rx("\\blongest\\b"))) paste("longest", dv) else dv
        addn(if (k == "L") "leaflet_length" else "leaflet_width", lfd[[k]]$v, lfd[[k]]$txt, "leaf_division", dvk)
      }
    }
    rh_txt <- join_units(unit_text(u, "underground", subj_re = "rhizome", parts = "any"))
    if (!is.na(rh_txt)) {
      addc("rhizome_form", rh_txt, rhizome_form_dict, rhizome_form_neg)
      rh <- bind_rows(tibble(pos = 0L, end = 0L, value = "rhizomatous", term = "rhizome", qual = NA_character_, region = NA_character_),
                      scan_terms(rh_txt, stem_habit_dict))
      add(rec_cat("stem_growth_habit", rh, rh_txt))
    }
    het_txt <- grab_sent(desc, "dimorphic|monomorphic|isophyllous|anisophyllous")
    addc("leaf_heterogeneity", het_txt, c("dimorphic|anisophyllous|heterophyllous" = "anisophyllous"),
         neg = c("not dimorphic|monomorphic|isophyllous|homophyllous" = "isophyllous"), generic_neg = FALSE)
    # no plant height given: frond length is recorded as plant height, organ = fronds (user's choice, as in Wenk_2025)
    if (is.null(ht) && !any(map_lgl(recs, ~ any(.x$trait %in% c("plant_height", "plant_height_climbing_plant"))))) {
      fr_l <- m_first(unit_text(u, "leaf", subj_re = "frond"), "long|in length", excl("fronds?", "leaves", "leaf"))
      if (!is.null(fr_l)) addn("plant_height", fr_l$v, paste("[fronds]", fr_l$txt), "plant_organ_measured", "fronds", scale = 1 / 1000)
    }
  }
  # leaflet counts: "with 5-9 leaflets", "leaflets 5-9", "leaflets 1.5-3-jugate", "2-5 pairs of leaflets", "pinnae unijugate"
  lc_txt <- c(leaf_gen, leaflet)
  jug <- c(unijugate = 1, bijugate = 2, trijugate = 3)
  for (x in lc_txt) {
    xl <- str_to_lower(str_remove_all(mark_extremes(x), "\u27e8[^\u27e9]*\u27e9"))
    m <- str_match(xl, "^(pinnae|leaflets?|pinnules?)\\b[^,;|]{0,15}?([0-9.]+)(?:\\s*-\\s*([0-9.]+))?-jugate")
    m2 <- str_match(xl, "^(pinnae|leaflets?|pinnules?) (unijugate|bijugate|trijugate)")
    m3 <- str_match(xl, "(?:with |of |in )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))? pairs(?: of)? (pinnae|leaflets?|pinnules?)|^((?:primary |secondary |tertiary |lateral |sterile |fertile )?(?:pinnae|leaflets?|pinnules?)) (?:in )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))? pairs")
    m4 <- str_match(xl, "(?:with |of |into )([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))? (?:[a-z-]+ ){0,2}(leaflets|pinnae|pinnules|leaflet)\\b|^(leaflets|pinnae|pinnules) ([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?=,|$| per| on)(?! ?(?:mm|cm|m)\\b)|\\b([0-9])-foliolate|\\b(tri|uni)foliolate")
    lvl <- NA_character_; v <- NULL; key <- "leaflet_count_pairs"
    if (!is.na(m[1])) { lvl <- m[2]; v <- count_range(m[3], m[4]) }
    else if (!is.na(m2[1])) { lvl <- m2[2]; v <- c(min = NA, max = jug[[m2[3]]]) }
    else if (!is.na(m3[1])) { lvl <- coalesce(m3[4], m3[5]); v <- if (!is.na(m3[2])) count_range(m3[2], m3[3]) else count_range(m3[6], m3[7]) }
    else if (!is.na(m4[1])) {
      key <- "leaflet_count"
      if (!is.na(m4[8])) { lvl <- "leaflets"; v <- c(min = NA, max = as.numeric(m4[8])) }
      else if (!is.na(m4[9])) { lvl <- "leaflets"; v <- c(min = NA, max = ifelse(m4[9] == "tri", 3, 1)) }
      else if (!is.na(m4[2])) { lvl <- m4[4]; v <- count_range(m4[2], m4[3]) }
      else { lvl <- m4[5]; v <- count_range(m4[6], m4[7]) }
    }
    if (is.null(v)) next
    lvl_src <- lvl
    lvl <- case_when(str_detect(lvl, "pinn(a|ae)$") ~ "pinnae", str_detect(lvl, "pinnule") ~ "pinnules", TRUE ~ "leaflets")
    if (fern_mode) lvl <- str_replace(str_replace(lvl_src, "pinna$", "pinnae"), "pinnule$", "pinnules")
    addn(key, v, x, ctx_type = if (lvl == "leaflets" && !fern_mode && !str_detect(str_to_lower(paste(lc_txt, collapse = " ")), "pinnae")) NA else "leaf_division",
         ctx_value = if (lvl == "leaflets" && !fern_mode && !str_detect(str_to_lower(paste(lc_txt, collapse = " ")), "pinnae")) NA else lvl)
  }

  # ---- inflorescence
  infl <- unit_text(u, "inflorescence")
  fl_gen <- unit_text(u, "flower", subj_re = "flower|floret|^(?:fe)?males$|ones$")
  infl_txt <- collapse_text(c(infl, fl_gen), " | ")
  addc("inflorescence_type", infl_txt, infl_type_dict,
       prep_fun = function(x) str_remove_all(x, "\\b(?:[0-9]+|one|two|three)?-?headed\\b|\\bfruiting heads?\\b|\\bspine-tipped\\b|\\bspines?\\b|\\b(?:flowers |dichasia )?(?:solitary|single)(?: or [a-z ]+?)? (?:on|at|along) (?:each |the )?(?:nodes?|rachis)\\b"))
  addc("inflorescence_shape", join_units(unit_text(u, "inflorescence", subj_re = "head|capitul|spike|glomerule|umbel")), infl_shape_dict)
  in_other <- paste0(excl("inflorescences?", "racemes?", "spikes?", "heads?", "flowers?"), "|peduncled|pedunculate")
  il <- m_first(unit_text(u, "inflorescence", subj_re = "inflorescence|raceme|spike|panicle|cyme|thyrse|corymb|synflorescence|conflorescence|spadix"), "long|in length", in_other)
  if (!is.null(il)) addn("inflorescence_length", il$v, il$txt)
  idm <- m_first(unit_text(u, "inflorescence", subj_re = "head|capitul|umbel|glomerule"), "diam|diameter|in diameter|across|wide", in_other)
  if (!is.null(idm)) addn("inflorescence_diameter", idm$v, idm$txt)
  fpi <- count_after(c(infl, fl_gen), paste0("\\b", cnt, "-flowered\\b|\\bof ", cnt, " (?:[a-z-]+ ){0,3}flowers\\b|\\bwith ", cnt, " (?:[a-z-]+ ){0,3}flowers\\b|^flowers ", cnt, "(?=,| per|$)(?! ?(?:mm|cm|m|-merous|x)\\b)|\\bflowers ", cnt, " per (?:head|inflorescence|umbel|spike|raceme|cyme|cluster|capitulum|axil|node)|\\bflowers? (?:[a-z-]+ ){0,3}?in (?:clusters|groups|fascicles|umbels|whorls|heads|threes) of (?:up to |c\\. )?", cnt, "\\b|^flowers in (?:clusters|groups|fascicles|umbels|whorls)\\b[^,]*, ", cnt, "(?=,|$)"))
  if (!is.null(fpi)) {
    mm <- str_match(fpi$txt, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?")
    addn("flowers_per_inflorescence", parse_count(fpi$raw), fpi$txt)
  }
  bpi <- count_after(c(infl, unit_text(u, "bud", parts = "any")), paste0("\\bbuds ", cnt, " per (?:umbel|inflorescence|head|cluster)"))
  if (!is.null(bpi)) addn("buds_per_inflorescence", bpi$v, bpi$txt)
  # (not the scape of a male / female inflorescence: Lomandra "Female inflorescence ...; scape concealed or to 1.5 cm long")
  scp <- m_first(unit_text(u %>% filter(is.na(entity)), "peduncle", subj_re = "scape"), "long|in length|tall|high", excl("scapes?"))
  if (!is.null(scp)) addn("plant_height_reproductive", scp$v, paste("[scape]", scp$txt), scale = 1 / 1000)
  ped <- m_first(c(unit_text(u, "peduncle", subj_re = "peduncle"), unit_text(u, "inflorescence")), "long|in length", excl("peduncles?", "scapes?"))
  if (!is.null(ped)) {
    pt <- str_match(mark_extremes(join_units(c(unit_text(u, "peduncle", subj_re = "peduncle"), unit_text(u, "inflorescence")))), rx(paste0("\\b(?:peduncles?)\\b[^;:|]{0,40}?(", meas, ")(?:\\s+or more)?\\s*long")))
    if (!is.na(pt[1])) addn("peduncle_length", meas_of(pt[2]), unmark(pt[1]))
    else if (length(unit_text(u, "peduncle", subj_re = "peduncle"))) addn("peduncle_length", ped$v, ped$txt)
  }

  # ---- flowers (sizes per entity: male / female flowers on their own rows)
  ents <- unique(u$entity[!is.na(u$entity)])
  for (en in c(NA, ents)) {
    uu <- if (is.na(en)) u %>% filter(is.na(entity)) else u %>% filter(entity == en)
    ct <- if (is.na(en)) c(NA, NA) else c("entity_measured", paste(str_replace(en, "staminate", "male") %>% str_replace("pistillate", "female"), "flowers"))
    # ("Male flowers in disjunct glomerules ... forming panicles 6-25 cm long": the sizes after "in ... panicles" are the inflorescence's)
    fl_u0 <- unit_text(uu, "flower", subj_re = "flower|floret|^(?:fe)?males$|ones$")
    # "Flowers in thyrses or racemes, to 50 cm long": a bare size in cm / m right after the inflorescence phrase is the
    # inflorescence length (recorded as such when the inflorescence has no length of its own)
    infl_re <- "\\bin (?:[a-z0-9-]+ ){0,3}(?:thyrses?|racemes?|panicles?|spikes?|cymes?|umbels?|subumbels?|heads?|corymbs?|inflorescences?|dichasi(?:a|um)|monochasi(?:a|um))(?: or (?:[a-z0-9-]+ ){0,2}(?:thyrses?|racemes?|panicles?|spikes?|cymes?|umbels?|heads?|corymbs?|subumbels?|dichasi(?:a|um)|monochasi(?:a|um)))?,?\\s*((?:(?:to|up to|c\\.|mostly|about)\\s+)*[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:cm|m) long)"
    for (x in fl_u0) { im <- str_match(x, rx(infl_re))
      if (!is.na(im[1]) && !any(map_lgl(recs, ~ "inflorescence_length" %in% .x$trait))) { v <- meas_of(im[2]); if (!is.null(v)) addn("inflorescence_length", v, im[2], ct[1], ct[2]) } }
    fl_u0 <- str_remove(fl_u0, rx(paste0("(?<=thyrses|thyrse|racemes|raceme|panicles|panicle|spikes|spike|cymes|cyme|umbels|umbel|heads|head|corymbs|corymb|inflorescences|inflorescence|subumbels|dichasia|dichasium|monochasia|monochasium),?\\s*(?:(?:to|up to|c\\.|mostly|about)\\s+)*[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:cm|m) long")))
    fl_u <- fl_u0 %>%
      # (only that phrase and any bare measurements right after it: "in a spike c. 3 mm diam., to 10 mm long")
      # (bare measurements after it are the inflorescence's only when the phrase itself is sized: "in a spike c. 3 mm diam., to 10 mm
      # long"; in "Flowers paired or in clusters of up to 5, 1-2 mm long" the size is the flower's)
      str_remove(rx("\\b(?:congested |arranged |borne |crowded )?in (?:[a-z0-9-]+ ){0,3}(?:spikes?|panicles?|glomerules?|clusters?|heads?|racemes?|cymes?|umbels?|fascicles?|inflorescences?)\\b(?:[^,;|]*\\b(?:mm|cm|m)\\b[^,;|]*(?:,\\s*(?:to |c\\. |up to |about )?[0-9][^,;|]*)*|[^,;|]*)"))
    fd <- d_first(fl_u, excl("flowers?", "florets?"))
    if (!is.null(fd$L)) addn("flower_length", fd$L$v, fd$L$txt, ct[1], ct[2])
    fdm <- m_first(fl_u, "diam|diameter|in diameter|across|wide", excl("flowers?", "florets?"))
    if (!is.null(fdm)) addn("flower_diameter", fdm$v, fdm$txt, ct[1], ct[2])
    spk <- d_first(unit_text(uu, "flower", subj_re = "spikelet"), excl("spikelets?"))
    if (!is.null(spk$L)) addn("spikelet_length", spk$L$v, spk$L$txt, ct[1], ct[2])
    pdl <- m_first(unit_text(uu, "pedicel"), "long|in length", excl("pedicels?"))
    if (is.null(pdl)) {
      pm <- str_match(mark_extremes(join_units(unit_text(uu, c("flower", "inflorescence", "fruit", "bract"), parts = "any")) %||% ""), rx(paste0("\\bpedicels?\\b[^;:|]{0,30}?(", meas, ")(?:\\s+or more)?\\s*long")))
      if (!is.na(pm[1])) pdl <- list(v = meas_of(pm[2]), txt = unmark(pm[1]))
    }
    if (!is.null(pdl)) addn("pedicel_length", pdl$v, pdl$txt, ct[1], ct[2])
    br <- m_first(unit_text(uu, "bract", subj_re = "^(?:the )?(?:floral |flower |subtending )?bracts?$"), "long|in length", excl("bracts?"))
    if (!is.null(br)) addn("flower_bract_length", br$v, br$txt, ct[1], ct[2])
    # (the long hypanthium tube of Elodea, Hydrilla is not the sepals: recorded as hypanthium_length)
    sep <- d_first(unit_text(uu, "calyx", parts = "any", subj_re = "^(?!(?:the )?hypanth)"), excl("calyx", "sepals?", "lobes?", "segments?", "tube", "hypanthium"))$L
    if (FALSE) sep <- m_first(unit_text(uu, "calyx", parts = "any"), "long|in length", excl("calyx", "sepals?", "lobes?", "segments?", "tube", "hypanthium"))
    if (!is.null(sep)) addn("flower_sepal_length", sep$v, sep$txt, ct[1], ct[2])
    cor <- unit_text(uu, "corolla", subj_re = "^(?:the )?corollas?$")
    cl <- m_first(cor, "long|in length", excl("corolla"))
    # corolla length is recorded as flower length when the flower itself is not measured
    if (!is.null(cl) && is.null(fd$L)) addn("flower_length", cl$v, paste("[corolla]", cl$txt), ct[1], ct[2])
    cdm <- m_first(c(cor, unit_text(uu, "corolla", subj_re = "^(?:the )?corollas?$", parts = "limb") %>% { .[str_detect(., rx("^limb"))] }), "diam|diameter|in diameter|across|wide", excl("corolla", "limb"))
    if (!is.null(cdm)) addn("flower_diameter", cdm$v, paste("[corolla]", cdm$txt), ct[1], ct[2])
    tb <- m_first(unit_text(uu, c("corolla", "flower"), parts = "tube") %>% { .[str_detect(., rx("^(?:corolla )?tube"))] }, "long|in length", excl("tube", "throat", "corolla"))
    if (!is.null(tb)) addn("flower_tube_length", tb$v, tb$txt, ct[1], ct[2])
    clo <- m_first(unit_text(uu, c("corolla", "flower"), subj_re = "^(?:the )?(?:corollas?|flowers?)$", parts = c("lobes", "lobe", "limb")) %>% { .[str_detect(., rx("^(?:corolla )?(?:lobes?|limb)"))] }, "long|in length", excl("lobes?", "limb", "corolla"))
    if (!is.null(clo)) addn("corolla_lobe_length", clo$v, clo$txt, ct[1], ct[2])
    ptl <- d_first(unit_text(uu, "corolla", subj_re = "^(?:the )?(?:(?:outer|inner|larger)(?: [0-9]+| two| three)? )?(?:petals?|standard)$"), excl("petals?"))$L
    if (!is.null(ptl)) addn("flower_petal_length", ptl$v, ptl$txt, ct[1], ct[2])
    tpl <- d_first(unit_text(uu, "corolla", subj_re = "^(?:the )?(?:(?:outer|inner|inner and outer|outer and inner)(?: [0-9]+| two| three)? )?(?:tepals?|perianths?|sepals and petals|petals and sepals)$", parts = c("segments", "segment", "lobes", "lobe")) %>%
                     { .[!str_detect(., rx("^(?:perianth )?tube"))] }, excl("tepals?", "perianth", "segments?", "lobes?", "sepals", "petals"))$L
    if (!is.null(tpl)) addn("tepal_length", tpl$v, tpl$txt, ct[1], ct[2])
    lab <- m_first(unit_text(uu, "corolla", subj_re = "labellum"), "long|in length", excl())
    if (!is.null(lab)) addn("labellum_length", lab$v, lab$txt, ct[1], ct[2])
    andr <- unit_text(uu, "androecium", parts = "any")
    stc <- count_after(unit_text(uu, "androecium", subj_re = "^(?:the )?(?:fertile )?stamens?$") %>% { .[!str_detect(., rx("^(?:fertile )?stamens? (?:usually |c\\. |mostly |often )?[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:mm|cm|m)\\b"))] },
                       "^(?:fertile )?stamens? (?:usually |c\\. |mostly |often )?([0-9]+)(?:(?:,? [a-z, -]+?)?(?:-| or | to )([0-9]+))?(?![0-9.])")
    if (is.null(stc)) stc <- count_after(fl_u, "\\b([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))? (?:fertile )?stamens\\b")
    if (!is.null(stc)) addn("flower_fertile_stamens_count", stc$v, stc$txt, ct[1], ct[2])
    anth <- m_first(unit_text(uu, "androecium", subj_re = "anther"), "long|in length", excl("anthers?"))
    if (is.null(anth)) {
      am <- str_match(mark_extremes(join_units(andr) %||% ""), rx(paste0("\\banthers?\\b[^;:|]{0,30}?(", meas, ")\\s*long")))
      if (!is.na(am[1])) anth <- list(v = meas_of(am[2]), txt = unmark(am[1]))
    }
    if (!is.null(anth)) addn("flower_anther_length", anth$v, anth$txt, ct[1], ct[2])
    fil <- m_first(unit_text(uu, "androecium", subj_re = "filament"), "long|in length", excl("filaments?"))
    if (is.null(fil)) {
      fm <- str_match(mark_extremes(join_units(andr) %||% ""), rx(paste0("\\bfilaments?\\b[^;:|]{0,30}?(", meas, ")\\s*long")))
      if (!is.na(fm[1])) fil <- list(v = meas_of(fm[2]), txt = unmark(fm[1]))
    }
    if (!is.null(fil)) addn("flower_filament_length", fil$v, fil$txt, ct[1], ct[2])
    gyn <- unit_text(uu, "gynoecium", parts = "any")
    sty <- m_first(unit_text(uu, "gynoecium", subj_re = "style"), "long|in length", excl("styles?"))
    if (is.null(sty)) {
      sm <- str_match(mark_extremes(join_units(gyn) %||% ""), rx(paste0("\\bstyles?\\b[^;:|]{0,30}?(", meas, ")\\s*long")))
      if (!is.na(sm[1])) sty <- list(v = meas_of(sm[2]), txt = unmark(sm[1]))
    }
    if (!is.null(sty)) addn("flower_style_length", sty$v, sty$txt, ct[1], ct[2])
  }
  # bud sizes (Myrtaceae "Mature buds clavate, 0.5-0.7 cm long, 0.3-0.5 cm wide")
  bd <- d_first(unit_text(u, "bud"), excl("buds?"))
  if (!is.null(bd$L)) addn("bud_length", bd$L$v, bd$L$txt)
  if (!is.null(bd$W)) addn("bud_width", bd$W$v, bd$W$txt)

  # flower categoricals
  fl_all <- unit_text(u, "flower", subj_re = "flower|floret|^(?:fe)?males$|ones$")
  cor_all <- unit_text(u, "corolla", parts = c(NA, "tube", "limb", "lobes", "lobe", "lips", "lip", "upper lip", "lower lip", "wings", "wing", "keel", "segments", "throat", "outer surface", "inner surface"))
  colour_units <- c(fl_all, cor_all)
  fc_txt <- join_units(colour_units)
  fc <- scan_terms(fc_txt, flower_colour_dict, prep_fun = colour_prep, generic_neg = TRUE)
  if (!nrow(fc)) {
    fc_txt <- join_units(unit_text(u, "inflorescence", subj_re = "head|capitul|spike|raceme|glomerule"))
    fc <- scan_terms(fc_txt, flower_colour_dict, prep_fun = colour_prep)
  }
  add(rec_cat("flower_colour", fc, fc_txt))
  fl_cat_txt <- collapse_text(c(fl_all, unit_text(u, "corolla", parts = "any")), " | ")
  addc("flower_perianth_symmetry", fl_cat_txt, symmetry_dict, prep_fun = function(x) str_remove_all(x, "\\bsubactinomorphic\\b|\\b(?:slightly|variably|weakly) zygomorphic\\b"))
  addc("flower_shape", join_units(unit_text(u, "corolla", subj_re = "corolla|perianth", parts = c(NA, "tube", "limb"))), flower_shape_dict)
  # "in a terminal, erect spike" is the inflorescence's posture, not the flowers'
  addc("flower_orientation", join_units(fl_all), orientation_dict,
       prep_fun = function(x) str_remove_all(x, "\\b(?:pendulous|pendent|erect|upright|nodding|drooping|deflexed) (?:[a-z-]+ )?(?:spikes?|racemes?|panicles?|inflorescences?|heads?|umbels?|cymes?|peduncles?|scapes?)\\b"))
  fsx <- scan_terms(collapse_text(c(fl_all, habit_txt), " | "), flower_sex_dict)
  if (any(ents %in% c("male", "female", "staminate", "pistillate", "functionally male", "functionally female")) && !any(fsx$value == "unisexual"))
    fsx <- bind_rows(fsx, tibble(pos = 99999L, value = "unisexual", term = "male / female flowers described", qual = NA))
  add(rec_cat("flower_structural_sex_type", fsx, collapse_text(c(fl_all, if (length(ents)) "[male and female flowers described separately]"), " | ")))
  addc("sex_type", desc, sex_type_dict)
  addc("flower_scent_production", join_units(unit_text(u, c("flower", "corolla", "inflorescence", "bud"), parts = "any")), scent_dict, scent_neg,
       prep_fun = function(x) str_remove_all(x, "\\baromatic\\b"))
  # ("Nectaries absent", "nectary 0": the negation follows the term)
  addc("flower_nectar_production", desc, nectar_dict, neg = c("(?:nectar(?:ies|y)?|nectariferous discs?) (?:absent|lacking|0|not seen)|without nectar\\w*|no nectar\\w*|nectarless" = "nectar_absent"), generic_neg = TRUE, prep_fun = function(x) str_remove_all(x, "extrafloral nectar\\w*|nectar glands? on (?:the )?(?:petiole|leaf|rachis|phyllode)\\w*"))
  # ovary position is also given inside flower clauses ("Female flowers with an inferior to half-inferior, unilocular ovary")
  ov_in_fl <- unit_text(u, "flower", parts = "any") %>% { .[str_detect(., rx("\\bovar(?:y|ies)\\b"))] }
  addc("flower_ovary_position", join_units(c(unit_text(u, "gynoecium", parts = "any"), ov_in_fl)), ovary_dict)
  # perianth merism: "5-merous" > petals count > corolla lobes count > tepals > sepals
  mer <- count_after(c(fl_all, cor_all, unit_text(u, "calyx", parts = "any")), "\\b([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?-merous\\b")
  if (is.null(mer)) mer <- count_after(unit_text(u, "corolla", subj_re = "petal|tepal|perianth|corolla", parts = c(NA, "lobes", "limb")),
                                       "^(?:petals?|tepals?|perianth (?:segments|lobes|parts)|corolla lobes|lobes) (?:usually |mostly )?([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?(?=,|$| )(?! ?(?:mm|cm|m)\\b)|^(?:corolla|limb|perianth)\\b[^,;|]{0,20}?\\b([0-9])-lobed\\b")
  if (is.null(mer)) mer <- count_after(unit_text(u, "calyx", parts = "any"), "^(?:calyx (?:segments|lobes)|sepals?|segments|lobes) (?:usually |mostly )?([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?(?=,|$| )(?! ?(?:mm|cm|m)\\b)")
  if (!is.null(mer)) {
    mm <- str_match(mer$txt, "([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?")
    addn("flower_perianth_merism", parse_count(mer$raw), mer$txt)
  }

  # ---- fruit
  fr <- unit_text(u, "fruit")
  fr_txt <- join_units(fr)
  fr_dim <- unit_text(u, "fruit", subj_re = "^(?!(?:the )?(?:pericarps?|endocarps?|valves|cocci|mericarps?)$).*$")
  fr_dim_txt <- join_units(fr_dim)
  fr_subj <- u$subj[u$sec == "fruit"]
  ft_txt <- collapse_text(c(fr, unlist(str_extract_all(desc %||% "", rx("\\bfruits? (?:a|an) [^,;.]+")))), " | ")
  ft <- scan_terms(ft_txt, c(fruit_type_dict, if (fam == "Fabaceae") c("pods?" = "legume")), generic_neg = FALSE,
                   prep_fun = function(x) str_remove_all(x, "\\bseeds?\\b[^,;|]*|\\bvalves?\\b"))
  add(rec_cat("fruit_type", ft, ft_txt))
  # "valves" says nothing about dehiscence when the fruit is stated to be indehiscent
  dh_f <- scan_terms(fr_txt, dehisc_dict, generic_neg = FALSE)
  if (nrow(dh_f) && any(dh_f$value == "indehiscent")) dh_f <- dh_f %>% filter(!(value == "dehiscent" & str_detect(term, "^valves?$")))
  add(rec_cat("fruit_dehiscence", dh_f, fr_txt))
  addc("fruit_fleshiness", fr_txt, fleshy_dict, prep_fun = function(x) str_remove_all(x, "\\b(?:seeds?|endocarp|stone|pyrene|aril)\\b[^,;|]*|[a-z ]*\\bhooks?\\b[^,;|]*"))
  addc("fruit_colour", fr_dim_txt, fruit_colour_dict, prep_fun = function(x) colour_prep(x, ageing = FALSE))
  addc("fruit_surface_hairs", fr_txt, hairs_dict %>% { .[. != "glandular_pubescent"] } %>% c("glandular-(?:pubescent|hairy|pilose|puberulous)|glandular hairs" = "hairy"))
  addc("fruit_shape", fr_txt, fruit_shape_dict, prep_fun = function(x) str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*"))
  frd <- dims(fr_txt %||% "", excl("fruits?", "capsules?", "pods?", "legumes?", "drupes?", "berr(?:y|ies)", "nuts?", "nutlets?", "achenes?", "cypselas?", "follicles?", "mericarps?", "samaras?", "valves", "syconi(?:a|um)", "cones?", "endocarps?", "pericarps?"))
  if (length(fr)) {
    fo <- excl("fruits?", "capsules?", "pods?", "legumes?", "drupes?", "nuts?", "nutlets?", "achenes?", "follicles?", "mericarps?", "samaras?", "valves", "cones?", "endocarps?", "pericarps?")
    frd <- d_first(fr_dim, fo)
    if (!is.null(frd$L)) addn("fruit_length", frd$L$v, frd$L$txt)
    if (!is.null(frd$W)) addn("fruit_width", frd$W$v, frd$W$txt)
    fth <- m_first(fr_dim, "thick|deep", fo)
    if (!is.null(fth)) addn("fruit_height", fth$v, fth$txt)
    # fruits measured only as their mericarps / cocci ("Cocci 5-10 mm long"): entity_measured rows
    if (is.null(frd$L)) {
      mc <- d_first(unit_text(u, "fruit", subj_re = "^(?:the )?(?:mericarps?|cocci|coccus|fruitlets?)$"), fo)
      if (!is.null(mc$L)) { addn("fruit_length", mc$L$v, mc$L$txt, "entity_measured", "mericarps"); frd$L <- mc$L }
      if (!is.null(mc$W)) addn("fruit_width", mc$W$v, mc$W$txt, "entity_measured", "mericarps")
    }
    # a utricle measured only as its pericarp ("pericarp hard and brittle, c. 2.5 mm long")
    if (is.null(frd$L)) {
      pc <- d_first(unit_text(u, "fruit", subj_re = "^(?:the )?pericarps?$"), fo)
      if (!is.null(pc$L)) addn("fruit_length", pc$L$v, paste("[pericarp]", pc$L$txt))
    }
  }
  # chenopod dispersal units: body (or valves, or tube) sizes on entity_measured rows, wing span on its own row,
  # spine length as a candidate trait (user's choices)
  dsp_subj <- unique(na.omit(u$subj[u$sec == "diaspore"]))
  for (sj in dsp_subj) {
    ent <- str_replace(str_replace(str_replace(sj, "perianths$", "perianth"), "bracteole$", "bracteoles"), "^(?:the )?", "")
    du <- u %>% filter(sec == "diaspore", subj == sj)
    dsp_other <- excl("bracteoles?", "perianth", "valves?", "tube", "lobes?")
    body <- c(unit_text(du, "diaspore"), unit_text(du, "diaspore", parts = c("valves", "valve")) %>% { .[str_detect(., rx("^valves?"))] },
              unit_text(du, "diaspore", parts = "tube") %>% { .[str_detect(., rx("^tube"))] })
    dd <- list(L = NULL, W = NULL)
    for (x in body) {
      d1 <- dims(x, dsp_other)
      if (is.null(dd$L) && !is.null(d1$L)) {
        dd$L <- d1$L
        # "4-13 mm long and wide"
        if (is.null(d1$W) && str_detect(x, rx("long and (?:wide|broad)"))) d1$W <- d1$L
      }
      if (is.null(dd$W) && !is.null(d1$W)) dd$W <- d1$W
    }
    if (!is.null(dd$L)) addn("fruit_length", dd$L$v, dd$L$txt, "entity_measured", ent)
    if (!is.null(dd$W)) addn("fruit_width", dd$W$v, dd$W$txt, "entity_measured", ent)
    wg <- m_first(unit_text(du, "diaspore", parts = c("wing", "wings")) %>% { .[str_detect(., rx("^wings?"))] }, "diam|diameter|in diameter|across|wide|broad", excl("wings?"))
    if (!is.null(wg)) addn("fruit_width", wg$v, wg$txt, "entity_measured", paste(ent, "wing"))
    spn <- m_first(unit_text(du, "diaspore", parts = c("spines", "spine")) %>% { .[str_detect(., rx("^spines?"))] }, "long|in length", excl("spines?"))
    if (!is.null(spn)) addn("fruit_spine_length", spn$v, spn$txt)
  }
  spf <- count_after(c(fr, unit_text(u, "seed")) %>% str_remove_all(rx("\\bwhen [0-9]+-seeded\\b")) %>% str_replace_all("([0-9])-\\s+or\\s+([0-9])", "\\1 or \\2"),
                     paste0("\\b", cnt, "[- ]seeded\\b|\\b", cnt, " seeds? (?:per|in each) (?:fruit|capsule|pod|legume|berry|drupe|follicle)\\b|^seeds? ", cnt, "(?=,|$| per (?:fruit|capsule|pod))(?! ?(?:mm|cm|m|x)\\b)"))
  if (!is.null(spf)) {
    mm <- str_match(spf$txt, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?")
    addn("seeds_per_fruit", parse_count(spf$raw), spf$txt)
  }

  # ---- seeds
  sd <- unit_text(u, "seed", subj_re = "seed")
  sd_txt <- join_units(sd)
  sdd <- d_first(sd, excl("seeds?"))
  # "1.4-1.5 mm across longest axis", "longest axis 2.3-2.5 mm"
  if (is.null(sdd$L)) {
    la <- str_match(mark_extremes(join_units(sd) %||% ""), rx(paste0("(", meas, ")\\s*(?:across|along|on) (?:the )?long(?:est|er) axis|long(?:est|er) axis (?:c\\. )?(", meas, ")")))
    if (!is.na(la[1])) sdd$L <- list(v = meas_of(coalesce(la[2], la[7])), txt = unmark(la[1]))
  }
  if (!is.null(sdd$L)) addn("seed_length", sdd$L$v, sdd$L$txt)
  if (!is.null(sdd$W)) addn("seed_width", sdd$W$v, sdd$W$txt)
  addc("seed_shape", sd_txt, seed_shape_dict, prep_fun = function(x) str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*|\\b(?:arils?|wings?|hilum|embryo)\\b[^,;|]*"))
  # (colours of the aril etc. are not the seed's: "with a prominent fleshy white aril")
  addc("seed_colour", sd_txt, seed_colour_dict, prep_fun = function(x) colour_prep(str_remove_all(x, "\\b(?:with )?(?:an? )?(?:[a-z-]+ ){0,3}(?:arils?|arillode|strophioles?|wings?|hilum|elaiosomes?|caruncles?)\\b[^,;|]*")))
  addc("seed_surface_texture", sd_txt, seed_texture_dict, prep_fun = function(x) str_remove_all(x, "\\b(?:arils?|wings?|hilum)\\b[^,;|]*"))
  addc("seed_surface_reflectivity", sd_txt, reflect_dict)
  addc("seed_surface_hairs", sd_txt, seed_hairs_dict)
  app_txt <- collapse_text(c(fr, unit_text(u, "seed", parts = "any")), " | ")
  # chenopod dispersal units: spines, wings, bladder-like appendages (inflated_parts)
  dsp_txt <- join_units(unit_text(u, "diaspore", parts = "any"))
  addc("dispersal_appendage", dsp_txt, c("spines?|spinose|spiny|spine-like" = "spines", "wings?|winged" = "wings",
                                         "bladder(?:-like)?|inflated|vesicular|spongy appendages?" = "inflated_parts", "hooks?|hooked" = "hooks"),
       neg = c("(?:spines|wings?|appendages) (?:absent|0)|wingless|without (?:spines|wings?|appendages)|not winged" = ""))
  addc("dispersal_appendage", app_txt, appendage_dict, appendage_neg,
       prep_fun = function(x) str_remove_all(x, "(?:seed-bearing |curved woody |woody )?hooks? (?:subtending|prominent|present|lacking)[^,;|]*|seed-bearing hooks|retinacul\\w*|\\bwing (?:petals?)\\b"))

  # ---- generic organ x character matrix (vocabularies above); male / female flower parts on entity_measured rows
  u_g <- u %>% mutate(okey = gen_okey(sec, part, subj), pcl = gen_pclass(part)) %>% filter(!is.na(okey), !is.na(pcl))
  # in ferns the indusium covers the sorus (not the Goodeniaceae pollen cup)
  if (fern_mode) u_g <- u_g %>% mutate(okey = if_else(okey == "indusium", "sorus_indusium", okey))
  for (en in c(NA, unique(u_g$entity[!is.na(u_g$entity)]))) {
    ug <- if (is.na(en)) u_g %>% filter(is.na(entity)) else u_g %>% filter(entity == en)
    ct <- if (is.na(en)) c(NA, NA) else c("entity_measured", paste(str_replace(en, "staminate", "male") %>% str_replace("pistillate", "female"), "flowers"))
    if (!nrow(ug)) next
    grp <- ug %>% group_by(okey, pcl, unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>%
      group_by(okey, pcl) %>% summarise(t = paste(t, collapse = " | "), .groups = "drop")
    for (gi in seq_len(nrow(grp))) {
      ok <- grp$okey[gi]; pc <- grp$pcl[gi]; tx <- grp$t[gi]
      chars <- c(gen_chars[[if (pc == "") "organ" else pc]] %||% character(0), if (pc == "") gen_extra[[ok]] %||% character(0))
      for (ch in chars) {
        key <- paste0(ok, "_", pc, "_", ch)
        if (key %in% gen_skip) next
        if (!str_detect(tx, gen_any[[ch]])) next
        tr <- paste0(ok, if (pc != "") paste0("_", pc), "_", ch) %>% str_replace("_apex_apex_shape$", "_apex_shape")
        pf <- switch(ch,
          colour = function(x) colour_prep(x, ageing = FALSE),
          shape = function(x) str_remove_all(x, "\\b(?:[a-z-]+ )?(?:at|towards|near) (?:the )?(?:base|apex|tip|summit)\\b|\\bin (?:cross[- ])?section\\b[^,;|]*|\\b(?:teeth|lobes?|glands?|veins?|hairs?|scales?|apex|apices|tips?|bases?)\\b[^,;|]*"),
          hairs = function(x) str_remove_all(x, "\\b(?:hairs?|indumentum) (?:of|on) (?:the )?(?:[a-z-]+ )?(?:veins?|midrib|margins?|base)\\b"),
          NULL)
        so <- gen_strip_other(ok)
        if (ch %in% c("shape", "colour", "texture", "fusion", "persistence", "orientation")) {
          so0 <- so; so <- function(x) str_remove_all(so0(x), "\\b(?:leaving|with|having|bearing|subtended by|surrounded by)\\b[^,;|]*")
        }
        pf2 <- if (is.null(pf)) so else function(x) pf(so(x))
        addc(tr, tx, gen_dicts[[ch]], if (ch == "hairs") gen_hairs_neg else NULL, prep_fun = pf2, ctx_type = ct[1], ctx_value = ct[2])
      }
    }
    # counts
    for (qi in seq_len(nrow(gen_counts))) {
      ks <- str_split(gen_counts$okeys[qi], "\\|")[[1]]
      tt <- ug %>% filter(okey %in% ks) %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      if (!length(tt)) next
      nn <- gen_counts$noun[qi]
      v <- count_after(tt, paste0("^(?:the )?(?:[a-z]+ ){0,2}?(?:", nn, ")\\s+(?:usually |c\\. |mostly |often |about |± )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?![0-9.]|\\s*(?:mm|cm|dm|m)\\b|\\s*(?:x|×)|\\s*-\\s*(?:[a-z]|or\\b)|\\s*(?:-|or|to)\\s*[0-9])(?:\\s*(?:per|in each) (?:locule|cell|carpel))?"))
      if (is.null(v)) v <- count_after(tt, paste0("(?<![0-9.,×x-]\\s?)\\b([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?\\s+(?:[a-z-]+ )?(?:", nn, ")\\b"))
      if (is.null(v)) next
      tr <- gen_counts$trait[qi]
      if (tr == "ovule_count" && str_detect(v$txt, rx("per (?:locule|cell|carpel)|in each"))) tr <- "ovules_per_locule"
      addn(tr, v$v, v$txt, ct[1], ct[2])
    }
    # style branches / stigma lobes: "Style 3-branched", "2- or 3-branched", "3-6-branched", "stigma 3-lobed"
    tt <- ug %>% filter(okey %in% c("style", "stigma")) %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
    if (length(tt)) {
      v <- count_after(tt, "\\b([0-9]+)-?(?:\\s*(?:-|or|to)\\s*([0-9]+))?-(?:branched|fid|lobed|partite|cleft)\\b")
      if (!is.null(v)) addn("style_branch_count", v$v, v$txt, ct[1], ct[2])
    }
    # locules: "ovary 3-locular", "2-celled", "unilocular", "locules 2"
    for (lk in list(c("ovary|carpel", "ovary_locule_count"), c("fruit", "fruit_locule_count"), c("anther", "anther_locule_count"))) {
      tt <- ug %>% filter(okey %in% str_split(lk[1], "\\|")[[1]]) %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      if (!length(tt)) next
      tj <- str_replace_all(paste(tt, collapse = " | "), rx("\\bunilocular\\b"), "1-locular") %>% str_replace_all(rx("\\bbilocular\\b"), "2-locular") %>%
        str_replace_all(rx("\\btrilocular\\b"), "3-locular")
      v <- count_after(tj, "\\b([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?-?\\s?(?:locular|loculed|celled|thecous)\\b")
      if (is.null(v)) v <- count_after(tj, "\\b(?:locules|cells|loculi)\\s+(?:usually |c\\. )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?![0-9.]|\\s*(?:mm|cm|m)\\b)")
      if (!is.null(v)) addn(lk[2], v$v, v$txt, ct[1], ct[2])
    }
    # sizes not covered by existing traits
    for (zi in seq_len(nrow(gen_sizes))) {
      tt <- ug %>% filter(okey == gen_sizes$okey[zi], pcl == "") %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      if (!length(tt)) next
      dd <- d_first(tt, do.call(excl, as.list(str_split(gen_sizes$own[zi], "\\|")[[1]])))
      if (!is.na(gen_sizes$len[zi]) && !is.null(dd$L)) addn(gen_sizes$len[zi], dd$L$v, dd$L$txt, ct[1], ct[2])
      if (!is.na(gen_sizes$wid[zi]) && !is.null(dd$W)) addn(gen_sizes$wid[zi], dd$W$v, dd$W$txt, ct[1], ct[2])
    }
  }

  # ---- cues: flowering / fruiting / germination triggered by rain, fire, floods ... (user's request: always documented)
  # verbatim clauses in *_cues_description; mapped only when stated (hedged clauses stay text-only)
  cue_src <- collapse_text(c(prep(r[["Phenology"]]), prep(r[["Ecology"]]), prep(r[["Notes"]]), prep(r[["Habitat"]])), " ")
  if (!is.na(cue_src)) {
    cue_re <- "\\b(?:rain\\w*|wet season|the wet|monsoon\\w*|wet conditions|soil moisture|moisture|wetting|fires?|burn\\w*|smoke|flood\\w*|inundat\\w*|water levels?|drying|dries out|frost|cold|heat|temperature)\\b"
    cue_hedge <- "\\b(?:probably|possibly|perhaps|may|might|appears? to|seems? to|thought to|likely|presumably|suggest\\w*|[Mm]ost species|[Ss]ome species|[Mm]any species)\\b|\\[genus description\\]"
    cue_kw <- list(flowering = "\\b(?:flower\\w*|anthesis|in bloom)\\b", fruiting = "\\b(?:fruit\\w*|seed(?:s|ing)? (?:set|mature|ripen)\\w*|capsules?)\\b",
                   germination = "\\b(?:germinat\\w*|seedlings?|recruit\\w*|regenerat\\w* from seed)\\b")
    cue_dict <- c("(?:summer|wet[- ]season|monsoon(?:al)?) rain\\w*|rain\\w* (?:in|during) (?:the )?(?:summer|wet season)|wet season|the wet\\b|monsoon\\w*" = "rain_summer",
                  "winter rain\\w*|rain\\w* (?:in|during) winter" = "rain_winter", "autumn rain\\w*|rain\\w* (?:in|during) autumn" = "rain_autumn",
                  "spring rain\\w*|rain\\w* (?:in|during) spring" = "rain_spring",
                  "rain\\w* (?:at any time|regardless of season|throughout the year)|any time (?:of (?:the )?year )?after rain\\w*" = "rain_all_year",
                  "(?<!winter )(?<!summer )(?<!autumn )(?<!spring )rain\\w*|wet conditions|soil moisture|moisture|wetting" = "rain", "fires?|burn\\w*" = "fire", "smoke" = "smoke",
                  "flood\\w*|inundat\\w*|water levels?" = "floods", "heat" = "heat")
    cl <- unlist(map(sentences(cue_src), ~ str_split(.x, ";\\s*")[[1]]))
    cl <- cl[str_detect(cl, rx(cue_re))]
    for (k in names(cue_kw)) {
      ck <- cl[str_detect(cl, rx(cue_kw[[k]]))]
      # the fire / rainfall must be the trigger: "flowers after fire", "germinates following rain", "in response to rainfall"
      ck <- ck[str_detect(ck, rx(paste0("\\b(?:after|following|in association with|associated with|in response to|response to|triggered by|stimulated by|induced by|dependent on|depending on|depends on|with|when|once|until|requires?|prompted by|cued by|post-?)\\b[^.;]{0,40}", cue_re, "|", cue_re, "[- ](?:stimulated|induced|triggered|dependent|related)")))]
      # capsules exploding "in response to wetting or drying" and fruits "washed ashore during the monsoon" are dehiscence /
      # dispersal, not cues
      ck <- ck[!str_detect(ck, rx("\\b(?:explod\\w*|dehisc\\w*|flung|washed|flotsam|dispers\\w*|carried)\\b"))]
      if (!length(ck)) next
      tr <- paste0(k, "_cues")
      dsc <- collapse_text(ck, " | ")
      # (case-sensitive: the month "May" is not a hedge)
      mapped <- ck[!str_detect(ck, cue_hedge)]
      d_use <- if (k == "flowering") cue_dict[cue_dict != "smoke" & cue_dict != "heat"] else cue_dict
      # ("mainly after the Wet": the commonness word reaches across the trigger words)
      # "after winter or summer rainfall" -> "winter rain or summer rainfall"
      mtxt <- join_units(mapped)
      if (!is.na(mtxt)) mtxt <- str_remove_all(str_replace_all(str_to_lower(mtxt), "\\b(winter|summer|autumn|spring) (or|and) (winter|summer|autumn|spring) (rain\\w*)", "\\1 rain \\2 \\3 \\4"),
                                             "\\b(?:fire|flood|rainfall|rain)[- ]?(?:breaks?|scar\\w*|history|frequency|regime|plain)\\b|\\bfrom (?:north|south|east|west) to (?:north|south|east|west)\\b")
      # ("mainly after the Wet": the commonness word reaches across the trigger words)
      h <- scan_terms(mtxt, d_use, generic_neg = TRUE,
                      fillers = c(filler_words, "after", "following", "in", "response", "to", "favourable", "good", "heavy", "late", "early", "by", "of", "on", "dependent", "depending", "triggered"))
      # a region after "X or Y rainfall in Qld" covers both
      if (nrow(h) > 1) for (i in rev(seq_len(nrow(h) - 1))) {
        gap <- str_sub(mtxt, h$end[i] + 1, h$pos[i + 1] - 1)
        if (is.na(h$region[i]) && !is.na(h$region[i + 1]) && str_detect(gap, "^\\s*(?:or|and)\\s*$")) h$region[i] <- h$region[i + 1]
      }
      # field observations of seedlings / germination after fire are post-fire recruitment (not heat / smoke trials)
      if (k == "germination" && nrow(h) && any(h$value == "fire") &&
          !str_detect(join_units(mapped) %||% "", rx("\\b(?:trials?|treatments?|treated|laborator\\w*|experiment\\w*|petri)\\b")))
        add(rec_cat("post_fire_recruitment", h %>% filter(value == "fire") %>% mutate(value = "post_fire_recruitment") %>% slice(1), dsc) %>% mutate(desc = dsc))
      # flowering after fire is also post_fire_flowering, mapped only when the strength of the response is stated
      if (k == "flowering" && nrow(h) && any(h$value == "fire")) {
        pf <- case_when(
          str_detect(mtxt, rx("\\b(?:not|never) (?:dependent on|requir\\w+|need\\w*) (?:a )?fire|irrespective of fire|regardless of fire|equally (?:well )?in burnt and unburnt")) ~ "fire_independent_flowering",
          str_detect(mtxt, rx("\\b(?:only|exclusively|predominantly|largely) (?:after|following|in response to) (?:a )?fire|rarely (?:flowers?|flowering) (?:in the )?(?:absence of|without) fire|(?:absent|rare) in unburnt")) ~ "fire_dependent_flowering",
          str_detect(mtxt, rx("\\b(?:best|most prolifically|more prolifically|profusely|prolifically|abundantly|more abundantly|heavily|enhanced|stimulated|promoted|mass(?:ed)?|mainly|mostly|particularly|especially) (?:flowering )?(?:after|following|in response to|by) (?:a )?(?:recent )?fire|fire[- ](?:stimulated|enhanced|promoted)|(?:greater|increased) (?:profusion|numbers|flowering) after fire")) ~ "fire_enhanced_flowering",
          TRUE ~ "")
        add(tibble(trait = "post_fire_flowering", value = pf, desc = dsc, min = NA, max = NA, ctx_type = NA, ctx_value = NA, qualifier = NA_character_, kind = "cat"))
      }
      if (nrow(h)) add(rec_cat(tr, h, dsc) %>% mutate(desc = dsc)) else add(tibble(trait = tr, value = "", desc = dsc, min = NA, max = NA, ctx_type = NA, ctx_value = NA, qualifier = NA_character_, kind = "cat"))
    }
  }

  # ---- phenology
  ph_txt <- prep(r[["Phenology"]])
  ph <- if (!is.na(ph_txt)) phenology_parse(ph_txt) else list(fl = NA, fl_s = NA, fr = NA, fr_s = NA, fl_x = NA, fr_x = NA, q_extra = tibble())
  for (k in c("fl", "fr")) {
    tr <- c(fl = "flowering_time", fr = "fruiting_time")[[k]]; xk <- ph[[paste0(k, "_x")]]
    qx <- if (nrow(ph$q_extra)) ph$q_extra %>% filter(k == !!k) else tibble()
    if (!is.na(ph[[k]])) add(tibble(trait = tr, value = ph[[k]], desc = ph[[paste0(k, "_s")]], min = NA, max = NA, ctx_type = NA, ctx_value = NA,
                                    qualifier = if (!is.null(ph$main_q) && !is.na(ph$main_q[[k]])) ph$main_q[[k]] else if (!is.na(xk) || nrow(qx)) "usually" else NA_character_))
    if (nrow(qx)) qx <- qx %>% group_by(q) %>% summarise(m = list(sort(unique(unlist(m)))), s = first(s), .groups = "drop")
    for (j in seq_len(nrow(qx))) add(tibble(trait = tr, value = yn(sort(unique(unlist(qx$m[j])))), desc = qx$s[j], min = NA, max = NA,
                                            ctx_type = NA, ctx_value = NA, qualifier = qx$q[j]))
    if (!is.na(xk)) add(tibble(trait = tr, value = xk, desc = paste(ph[[paste0(k, "_s")]], "[parenthetical extremes]"), min = NA, max = NA,
                               ctx_type = NA, ctx_value = NA, qualifier = "rarely"))
  }

  recs <- bind_rows(recs)
  # ---- verbatim text columns for the main row
  eco <- prep(r[["Ecology"]]); notes <- prep(r[["Notes"]]); hab <- prep(r[["Habitat"]])
  eco_all <- collapse_text(c(eco, notes), " ")
  main <- tibble(
    taxon_name = taxon, family = fam, taxon_rank = r[["rank"]], foa_url = r[["url"]],
    common_name = na_if_empty(r[["Common Name"]]), biostatus = na_if_empty(r[["Biostatus"]]),
    profile_author = na_if_empty(r[["Author"]]), description_treatment_used = trt$used,
    habit_description = habit_txt, bark_description = bark_txt, stem_description = stem_txt, underground_organ_description = under_txt,
    leaf_description = collapse_text(c(join_units(unit_text(u, c("leaf", "lamina", "petiole", "leaflet", "stipule"), parts = "any"))), " | "),
    inflorescence_description = join_units(unit_text(u, c("inflorescence", "spadix_part", "peduncle", "bract", "pedicel"), parts = "any")),
    flower_description = join_units(unit_text(u, c("flower", "bud", "calyx", "corolla", "androecium", "gynoecium"), parts = "any")),
    fruit_description = join_units(unit_text(u, c("diaspore", "fruit"), parts = "any")), seed_description = join_units(unit_text(u, "seed", parts = "any")),
    phenology_text = ph_txt,
    pollination_description = grab_sent(eco_all, "pollinat|self-poll|visited by|visitors|cleistogam"),
    dispersal_description = grab_sent(eco_all, "dispers|flung|explod|\\bants?\\b|birds? (?:eat|feed)|eaten by|carried by|by water|currents?\\b|spread by"),
    fire_response_description = grab_sent(eco_all, "\\bfires?\\b|\\bburn|resprout|regenerat|coppic|epicormic|lignotuber"),
    vegetative_reproduction_description = grab_sent(eco_all, "vegetative|clonal|suckers?|sucker(?:ing|s)|layering|rooting (?:at|from) (?:the )?nodes|roots? (?:at|from) (?:the )?nodes|take root|stolon|rhizom|fragments|bulbils?|colon(?:y|ies)|patches"),
    germination_description = grab_sent(eco_all, "germinat|seedlings?|dormancy|seed ?bank|soil seed"),
    ecology_description = eco, habitat_description = hab, distribution_description = prep(r[["Distribution"]]),
    seedling_description = prep(r[["Seedlings"]]), notes = notes
  )
  list(main = main, recs = recs, units = u)
}
grab_sent <- function(x, pattern) {
  if (is.na(x)) return(NA_character_)
  s <- sentences(x)
  collapse_text(s[str_detect(s, rx(pattern))], " ")
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

res <- map(seq_len(nrow(d)), function(i) {
  tryCatch(extract_one(d[i, ]), error = function(e) stop(paste(d$scientific_name[i], conditionMessage(e))))
})
main <- map_dfr(res, "main")
recs <- map2_dfr(res, d$scientific_name, ~ if (!is.null(.x$recs) && nrow(.x$recs)) mutate(.x$recs, taxon_name = .y) else NULL)
# A group description can merge two treatments ("... (Hay 2011). Now includes Lemnaceae: Aquatic plants floating ...";
# Aristolochia "Now includes Pararistolochia: ..."): each part describes only its own members, so the parts are read
# separately and a value is universal only when every part states it.
d_hi_parts <- d_higher %>%
  # (also sunk genera appended as "Selliera (Carolin 1992: 281): ..." in Goodenia)
  mutate(Description = str_split(Description, "\\s*(?=\\bNow includes [A-Z][a-z]+(?: \\([^)]*\\))?:|(?<=[.]\\s)[A-Z][a-z]+ \\([A-Z][^()]*\\b(?:1[89]|20)[0-9]{2}[a-z]?(?::\\s*[0-9-]+)?\\):)")) %>%
  unnest(Description) %>%
  mutate(Description = str_remove(Description, "^(?:Now includes [A-Z][a-z]+(?: \\([^)]*\\))?|[A-Z][a-z]+ \\([A-Z][^()]*\\b(?:1[89]|20)[0-9]{2}[a-z]?(?::\\s*[0-9-]+)?\\)):\\s*")) %>%
  filter(str_detect(Description, "[a-z]")) %>%
  group_by(scientific_name) %>% mutate(part = row_number(), n_parts = n()) %>% ungroup()
hi_recs <- function(group_filter) {
  res <- map(seq_len(nrow(d_hi_parts)), ~ extract_one(d_hi_parts[.x, ], group_filter = group_filter))
  map2_dfr(res, seq_len(nrow(d_hi_parts)), ~ if (!is.null(.x$recs) && nrow(.x$recs))
    mutate(.x$recs, group_name = d_hi_parts$scientific_name[.y], group_rank = d_hi_parts$rank[.y],
           part = d_hi_parts$part[.y], n_parts = d_hi_parts$n_parts[.y]) else NULL)
}
recs_hi <- hi_recs(TRUE)
exclusive_sets <- list(
  stem_growth_habit = list(c("erect", "prostrate", "decumbent", "sprawling", "spreading", "climbing", "creeping", "pendulous", "floating", "submerged")),
  inflorescence_type = list(c("terminal", "axillary"), c("solitary", "raceme", "spike", "cyme", "corymb", "panicle", "umbel", "head")))
# the same descriptions read in full: a trait given several values anywhere in the group description ("terrestrial,
# lithophytic or aquatic", "palmate, costapalmate, paripinnate ... or entire") varies within the group, even though the
# segment-level filter above leaves a single list item standing
# inflorescence position (terminal / axillary) and type (raceme, umbel ...) are separate facets of one trait, as are
# stem posture and other habit words: variation is counted within each facet
facet_of <- function(trait, v) map2_chr(trait, v, function(t, x) {
  if (!t %in% names(exclusive_sets)) return("all")
  k <- which(map_lgl(exclusive_sets[[t]], ~ x %in% .x))
  if (length(k)) as.character(k[1]) else x
})
hi_full_vals <- hi_recs(FALSE) %>%
  filter(!is.na(value), value != "", !trait %in% c("flowering_time", "fruiting_time")) %>%
  separate_rows(value, sep = " ")
hi_variable <- hi_full_vals %>% mutate(facet = facet_of(trait, value)) %>%
  group_by(group_name, trait, facet) %>%
  summarise(n_vals = n_distinct(value), any_alts = any(coalesce(alts, 1L) > 1 & !trait %in% names(exclusive_sets)),
            any_qual = any(!is.na(qualifier) | !is.na(region)), .groups = "drop") %>%
  filter(n_vals > 1 | any_alts | any_qual) %>% distinct(group_name, trait, facet)
# group statements that read as universal but are not (vetted by reading the group description)
copydown_exclude <- tribble(
  ~group_name, ~trait,
  # "monoecious spadices with female zone lowermost" describes the unisexual-flowered genera only; Pothos, Gymnostachys,
  # Rhaphidophora etc. have bisexual flowers
  "Araceae", "sex_type",
  # "leaves of short shoots modified into spines", "Leaves ... usually rudimentary and caducous or absent": not every cactus
  # is spiny (Rhipsalis, Epiphyllum) and most have no leaves to describe
  "Cactaceae", "plant_physical_defence_structures",
  "Cactaceae", "leaf_arrangement", "Cactaceae", "leaf_compoundness", "Cactaceae", "leaf_margin", "Cactaceae", "leaf_phyllotaxis",
  # "bisexual, monoecious or (not in Australia) dioecious": plants bisexual or monoecious
  "Atherospermataceae", "sex_type",
  # Tecticornia's description stitches together the former genera merged into it; only one of them ("... perennials, dioecious") is dioecious
  "Tecticornia", "sex_type",
  # Casuarinaceae "teeth" are the scale leaves themselves, not leaf-margin teeth
  "Casuarinaceae", "leaf_margin", "Allocasuarina", "leaf_margin", "Casuarina", "leaf_margin", "Gymnostoma", "leaf_margin"
)

# ---------------------------------------------------------------- overrides
# split calendars ("in northern populations ... in southern populations") and other phrasing the parser can't read
ph_over_file <- "data/ABRS_2026/raw/phenology_overrides.csv"
if (file.exists(ph_over_file)) {
  pho <- read_csv(ph_over_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(taxon_name %in% main$taxon_name)
  recs <- recs %>% filter(!(taxon_name %in% pho$taxon_name & trait %in% c("flowering_time", "fruiting_time")))
  recs <- bind_rows(recs, pho %>% pivot_longer(c(flowering_time, fruiting_time), names_to = "trait", values_to = "value") %>%
                      filter(!is.na(value)) %>%
                      transmute(taxon_name, trait, value, desc = description, min = NA_real_, max = NA_real_,
                                ctx_type = NA_character_, ctx_value = NA_character_, qualifier = commonness_qualifier, region = population_region))
}
# ecology traits vetted by reading each Ecology / Notes text (pollination, dispersal, clonality, fire, lifespan ...)
eco_file <- "data/ABRS_2026/raw/ecology_vetted.csv"
if (file.exists(eco_file)) {
  ev <- read_csv(eco_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(taxon_name %in% main$taxon_name)
  recs <- bind_rows(recs, ev %>% transmute(taxon_name, trait = trait_name, value, desc = evidence, min = NA_real_, max = NA_real_,
                                           ctx_type = NA_character_, ctx_value = NA_character_, qualifier = commonness_qualifier))
}

# ---------------------------------------------------------------- species -> infraspecific inheritance
# A subspecies / variety / form whose own description is silent on a trait inherits the parent species' own values
# (categorical and numeric, with their commonness / region / organ rows), with trait_scoring_method = inferred_from_species
# (user's choice). Values the species itself copied from its genus / family are not passed on; genus / family copy-down
# below then fills what is still missing.
infra <- main %>% filter(taxon_rank %in% c("subspecies", "variety", "form")) %>% transmute(taxon_name, species = word(taxon_name, 1, 2)) %>%
  filter(species %in% main$taxon_name)
if (!"scoring" %in% names(recs)) recs$scoring <- NA_character_
own_traits <- recs %>% distinct(taxon_name, trait)
sp_recs <- recs %>% filter(taxon_name %in% infra$species, is.na(scoring))
inherited <- infra %>% inner_join(sp_recs %>% rename(species = taxon_name), by = "species", relationship = "many-to-many") %>%
  anti_join(own_traits, by = c("taxon_name", "trait")) %>%
  mutate(desc = paste("[species description]", desc), scoring = "inferred_from_species") %>% select(-species)
recs <- bind_rows(recs, inherited)

# ---------------------------------------------------------------- species -> infraspecific inheritance
# A subspecies / variety / form whose own description is silent on a trait inherits the parent species' own values
# (categorical and numeric, with their commonness / region / organ rows), with trait_scoring_method = inferred_from_species
# (user's choice). Values the species itself copied from its genus / family are not passed on; genus / family copy-down
# below then fills what is still missing.
infra <- main %>% filter(taxon_rank %in% c("subspecies", "variety", "form")) %>% transmute(taxon_name, species = word(taxon_name, 1, 2)) %>%
  filter(species %in% main$taxon_name)
if (!"scoring" %in% names(recs)) recs$scoring <- NA_character_
own_traits <- recs %>% distinct(taxon_name, trait)
sp_recs <- recs %>% filter(taxon_name %in% infra$species, is.na(scoring))
inherited <- infra %>% inner_join(sp_recs %>% rename(species = taxon_name), by = "species", relationship = "many-to-many") %>%
  anti_join(own_traits, by = c("taxon_name", "trait")) %>%
  mutate(desc = paste("[species description]", desc), scoring = "inferred_from_species") %>% select(-species)
recs <- bind_rows(recs, inherited)

# ---------------------------------------------------------------- genus / family copy-down
# A categorical trait stated in the genus (else family) description as a single, unqualified value ("Leaves decussate",
# "Fruit a loculicidal capsule", "Ovary superior") holds throughout the group, so it is copied to member taxa whose own
# description is silent on that trait, on rows with trait_scoring_method = inferred_from_genus / inferred_from_family.
# Alternatives ("herbs or shrubs", "terminal or axillary"), qualified ("usually"), regional and contextual values are not copied.
# A group statement counts only when it names one term ("flat, terete or triquetrous" does not, even though only
# "terete" has a level), and only when no member taxon describing the trait itself contradicts it.
# (cues and recruitment come from Ecology statements, often about one member species: never copied down; nor the climbing
# mechanism, which genus descriptions give as one of several growth forms: Scaevola "herbs, scramblers, shrubs or small trees")
universal <- recs_hi %>%
  filter(!is.na(value), value != "", is.na(qualifier), is.na(region), is.na(ctx_type), !str_detect(value, " "),
         coalesce(alts, 1L) == 1, !trait %in% c("flowering_time", "fruiting_time", "flowering_cues", "fruiting_cues", "germination_cues", "post_fire_recruitment",
                                       "plant_climbing_mechanism", "post_fire_flowering")) %>%
  group_by(group_name, trait, value) %>% filter(n_distinct(part) == first(n_parts)) %>% ungroup() %>%
  mutate(facet = facet_of(trait, value)) %>% anti_join(hi_variable, by = c("group_name", "trait", "facet")) %>%
  semi_join(hi_full_vals, by = c("group_name", "trait", "value")) %>%
  anti_join(copydown_exclude, by = c("group_name", "trait")) %>%
  select(group_name, group_rank, trait, value, desc) %>% distinct(group_name, trait, .keep_all = TRUE)
own_tv <- recs %>% filter(!is.na(value), value != "", !trait %in% c("flowering_time", "fruiting_time")) %>%
  group_by(taxon_name, trait) %>% summarise(vals = paste(value, collapse = " "), .groups = "drop") %>%
  left_join(main %>% transmute(taxon_name, genus = word(taxon_name, 1), family), by = "taxon_name")
# Values that can co-occur do not conflict (a plant can be tufted and rhizomatous): for these traits a member taxon
# contradicts the group value only with another value from the same mutually exclusive set (erect vs prostrate).
compatible_traits <- c("storage_organ", "plant_growth_substrate", "plant_physical_defence_structures", "plant_climbing_mechanism",
                       "leaf_arrangement", "stem_branching_form", "plant_succulence")
conflicts <- function(trait, value, vals) {
  if (trait %in% compatible_traits) return(FALSE)
  # generic and specific climbers are the same growth form at different resolution
  if (trait == "plant_growth_form" && str_detect(value, "^climber")) return(!any(str_detect(vals, "^climber")))
  if (trait %in% names(exclusive_sets)) {
    set <- keep(exclusive_sets[[trait]], ~ value %in% .x)
    if (!length(set)) return(FALSE)
    return(any(vals %in% set[[1]]) && !value %in% vals)
  }
  !value %in% vals
}
universal <- universal %>% rowwise() %>%
  mutate(members_reporting = sum(own_tv$trait == trait & (if (group_rank == "genus") own_tv$genus == group_name else own_tv$family == group_name)),
         members_agree = members_reporting - sum(map_lgl(str_split(own_tv$vals[own_tv$trait == trait & (if (group_rank == "genus") own_tv$genus == group_name else own_tv$family == group_name)], " "),
                                                        function(v) conflicts(trait, value, v)))) %>% ungroup()
rejected <- universal %>% filter(members_agree < members_reporting)
universal <- universal %>% filter(members_agree == members_reporting)
taxa <- main %>% transmute(taxon_name, genus = word(taxon_name, 1), family)
copied <- list()
for (lvl in c("genus", "family")) {
  have <- bind_rows(recs %>% select(taxon_name, trait), if (length(copied)) bind_rows(copied) %>% select(taxon_name, trait)) %>% distinct()
  key <- if (lvl == "genus") "genus" else "family"
  cp <- taxa %>% inner_join(universal %>% filter(group_rank == lvl), by = setNames("group_name", key), relationship = "many-to-many") %>%
    anti_join(have, by = c("taxon_name", "trait")) %>%
    transmute(taxon_name, trait, value, desc = paste0("[", lvl, " description] ", desc), min = NA_real_, max = NA_real_,
              ctx_type = NA_character_, ctx_value = NA_character_, qualifier = NA_character_, region = NA_character_,
              scoring = paste0("inferred_from_", lvl))
  copied[[lvl]] <- cp
}
copied <- bind_rows(copied)
recs <- bind_rows(recs, copied)
if (Sys.getenv("FOA_QA") != "") saveRDS(list(main = main, recs = recs, units = map2_dfr(res, d$scientific_name, ~ mutate(.x$units, taxon_name = .y)),
                                             src = d, universal = universal, rejected = rejected, copied = copied), Sys.getenv("FOA_QA"))

# ---------------------------------------------------------------- assemble rows
# one row per taxon x (commonness_qualifier, leaf_division, entity_measured, population_region, trait_scoring_method);
# the main row has no context
for (cc in c("region", "scoring", "extreme_min", "extreme_max", "kind")) if (!cc %in% names(recs)) recs[[cc]] <- NA
# phenology, overrides, vetted ecology and copy-down rows are categorical
recs <- recs %>% mutate(kind = coalesce(kind, "cat"))
recs <- recs %>%
  mutate(commonness_qualifier = qualifier,
         leaf_division = ifelse(ctx_type %in% "leaf_division", ctx_value, NA),
         entity_measured = ifelse(ctx_type %in% "entity_measured", ctx_value, NA),
         population_region = coalesce(region, ifelse(ctx_type %in% "population_region", ctx_value, NA)),
         plant_organ_measured = ifelse(ctx_type %in% "plant_organ_measured", ctx_value, NA),
         trait_scoring_method = scoring) %>%
  # categorical values found twice for the same row are merged (stem spines + spiny leaf teeth); for numbers the first statement wins
  group_by(taxon_name, trait, commonness_qualifier, leaf_division, entity_measured, population_region, plant_organ_measured, trait_scoring_method) %>%
  summarise(is_num = first(kind) == "num",
            desc = if (first(kind) == "num") first(desc) else collapse_text(desc, "; "),
            value = if (first(kind) == "num") NA_character_ else collapse_unique(value[!is.na(value) & value != ""]),
            min = first(min), max = first(max), extreme_min = first(extreme_min), extreme_max = first(extreme_max), .groups = "drop")
keys <- c("taxon_name", "commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "trait_scoring_method")
cat_w <- recs %>% filter(!is_num) %>% select(all_of(keys), trait, value, desc) %>%
  pivot_wider(names_from = trait, values_from = c(value, desc), names_glue = "{trait}{ifelse(.value == 'desc', '_description', '')}")
num_w <- recs %>% filter(is_num) %>% mutate(across(c(min, max, extreme_min, extreme_max), ~ signif(.x, 6))) %>%
  select(all_of(keys), trait, description = desc, min, max, extreme_min, extreme_max) %>%
  pivot_wider(names_from = trait, values_from = c(description, min, max, extreme_min, extreme_max), names_glue = "{trait}_{.value}")
rows <- full_join(cat_w, num_w, by = keys)

# column order: verbatim description then mapped value for each categorical trait; for each numeric trait the verbatim
# measurement, _min / _max (typical range) and _extreme_min / _extreme_max (parenthetical extremes)
trait_order <- union(c(
  "plant_growth_form", "life_history", "stem_growth_habit", "rhizome_form", "stem_branching_form", "plant_growth_substrate", "parasitic",
  "plant_succulence", "plant_climbing_mechanism", "plant_physical_defence_structures", "storage_organ", "leaf_phenology",
  "plant_height", "plant_height_climbing_plant", "plant_height_reproductive", "plant_width", "stem_length", "stem_diameter",
  "storage_organ_length", "storage_organ_diameter", "bark_texture", "bark_colour", "stem_hairs", "stem_shape",
  "plant_photosynthetic_organ", "leaf_length_type", "leaf_type", "leaf_phyllotaxis", "leaf_arrangement", "leaf_compoundness",
  "leaf_lamina_division", "leaf_heterogeneity", "leaf_attachment", "stipule_presence", "leaf_lobation", "leaf_shape", "leaf_base_shape", "leaf_apex_shape",
  "leaf_margin", "leaf_margin_posture", "leaf_lamina_posture", "leaf_hairs_adult_leaves", "leaf_hairs_juvenile_leaves",
  "leaf_glaucousness", "leaf_glaucousness_juvenile_leaves", "leaf_surface_colour", "leaf_discolority", "leaf_surface_reflectivity",
  "leaf_texture", "leaf_count", "leaf_length", "leaf_width", "petiole_length", "leaflet_count", "leaflet_count_pairs", "leaflet_length", "leaflet_width",
  "inflorescence_type", "inflorescence_shape", "inflorescence_length", "inflorescence_diameter", "peduncle_length",
  "flowers_per_inflorescence", "buds_per_inflorescence", "bud_length", "bud_width", "flower_structural_sex_type", "sex_type",
  "flower_colour", "flower_perianth_symmetry", "flower_shape", "flower_orientation", "flower_scent_production",
  "flower_nectar_production", "flower_perianth_merism", "flower_ovary_position", "pedicel_length", "flower_bract_length",
  "flower_length", "flower_diameter", "spikelet_length", "flower_sepal_length", "flower_tube_length",
  "corolla_lobe_length", "flower_petal_length", "tepal_length", "labellum_length", "flower_fertile_stamens_count",
  "flower_filament_length", "flower_anther_length", "flower_style_length", "fruit_type", "fruit_dehiscence", "fruit_fleshiness",
  "fruit_colour", "fruit_surface_hairs", "fruit_shape", "fruit_length", "fruit_width", "fruit_height", "seeds_per_fruit",
  "seed_shape", "seed_colour", "seed_surface_texture", "seed_surface_reflectivity", "seed_surface_hairs", "dispersal_appendage",
  "seed_length", "seed_width", "flowering_time", "fruiting_time", "flowering_cues", "fruiting_cues", "germination_cues", "post_fire_recruitment", "post_fire_flowering"), unique(recs$trait))
cat_traits <- intersect(trait_order, unique(recs$trait[!recs$is_num]))
num_traits <- intersect(trait_order, unique(recs$trait[recs$is_num]))
cat_cols <- as.vector(rbind(paste0(cat_traits, "_description"), cat_traits))
num_cols <- as.vector(rbind(paste0(num_traits, "_description"), paste0(num_traits, "_min"), paste0(num_traits, "_max"),
                            paste0(num_traits, "_extreme_min"), paste0(num_traits, "_extreme_max")))
num_cols <- num_cols[num_cols %in% names(rows)]
num_cols <- num_cols[!str_detect(num_cols, "_extreme_(min|max)$") | map_lgl(num_cols, ~ any(!is.na(rows[[.x]])))]
for (cc in cat_cols) if (!cc %in% names(rows)) rows[[cc]] <- NA
rows <- rows %>% select(all_of(keys), all_of(cat_cols), all_of(num_cols))

ctx <- c("commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "trait_scoring_method")
ids <- main %>% select(taxon_name, family, taxon_rank, foa_url)
is_main <- rowSums(!is.na(rows[, ctx])) == 0
out_df <- bind_rows(
  main %>% left_join(rows[is_main, ] %>% select(-all_of(ctx)), by = "taxon_name"),
  inner_join(ids, rows[!is_main, ], by = "taxon_name")
) %>%
  relocate(all_of(ctx), .after = foa_url) %>%
  relocate(common_name:notes, .after = last_col()) %>%
  relocate(common_name, biostatus, profile_author, .after = trait_scoring_method) %>%
  mutate(row_order = case_when(!is.na(trait_scoring_method) ~ 5, !is.na(commonness_qualifier) ~ 1, !is.na(population_region) ~ 2,
                               !is.na(leaf_division) ~ 3, !is.na(entity_measured) ~ 4, !is.na(plant_organ_measured) ~ 4, TRUE ~ 0),
         fam_order = match(family, sort(unique(family)))) %>%
  arrange(fam_order, taxon_name, row_order, trait_scoring_method, commonness_qualifier, population_region, leaf_division, entity_measured) %>%
  select(-row_order, -fam_order)

# numeric ranges inside text written as "a--b" (Excel reads a hyphenated number pair as a formula / date)
out_df <- out_df %>% mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--")))

# ---------------------------------------------------------------- candidate trait register
# raw/candidate_traits.csv: every extracted trait column that is not (yet) mapped in metadata.yml, with its coverage, so
# new traits.yml definitions can be prioritised; rewritten on every full build
write_candidates <- function(df, path) {
  meta_traits <- tryCatch({
    m <- yaml::read_yaml("data/ABRS_2026/metadata.yml")
    unique(str_remove(map_chr(m$traits, ~ as.character(.x$var_in)), "_(?:min|max)$"))
  }, error = function(e) character(0))
  ctx_or_id <- c("taxon_name", "family", "taxon_rank", "foa_url", ctx, "common_name", "biostatus", "profile_author", "description_treatment_used")
  base <- names(df)[!names(df) %in% ctx_or_id & !str_detect(names(df), "_description$|_extreme_(?:min|max)$")]
  num_b <- unique(str_remove(base[str_detect(base, "_(?:min|max)$")], "_(?:min|max)$"))
  cat_b <- setdiff(base[!str_detect(base, "_(?:min|max)$")], c(num_b, "habit", "bark", "stem", "underground_organ", "leaf", "inflorescence", "flower",
                                                                "fruit", "seed", "pollination", "dispersal", "fire_response", "vegetative_reproduction",
                                                                "germination", "ecology", "habitat", "distribution", "seedling", "notes"))
  cat_b <- cat_b[paste0(cat_b, "_description") %in% names(df)]
  top <- function(x, k = 8) { x <- unlist(str_split(x[!is.na(x) & x != ""], ";\\s*|\\s+(?=[a-z_]+$)")); if (!length(x)) return(NA_character_)
    t <- sort(table(str_squish(x)), decreasing = TRUE); paste0(names(t)[seq_len(min(k, length(t)))], " (", t[seq_len(min(k, length(t)))], ")", collapse = "; ") }
  rc <- map_dfr(cat_b, function(t) {
    v <- df[[t]]; d <- df[[paste0(t, "_description")]]; has <- !is.na(d) & d != ""
    tibble(trait = t, type = "categorical", n_taxa = n_distinct(df$taxon_name[has]), n_values = sum(has),
           n_copied = sum(has & !is.na(df$trait_scoring_method)),
           suggested_values = top(unlist(str_split(v[has & !is.na(v)], " "))), source_terms = top(str_remove(d[has], "^\\[[a-z ]+\\]\\s*")),
           range = NA_character_)
  })
  rn <- map_dfr(num_b, function(t) {
    lo <- suppressWarnings(as.numeric(df[[paste0(t, "_min")]])); hi <- suppressWarnings(as.numeric(df[[paste0(t, "_max")]])); has <- !is.na(lo) | !is.na(hi)
    tibble(trait = t, type = "numeric", n_taxa = n_distinct(df$taxon_name[has]), n_values = sum(has), n_copied = sum(has & !is.na(df$trait_scoring_method)),
           suggested_values = NA_character_, source_terms = NA_character_,
           range = if (any(has)) paste(signif(min(c(lo, hi), na.rm = TRUE), 3), "-", signif(max(c(lo, hi), na.rm = TRUE), 3)) else NA_character_)
  })
  reg <- bind_rows(rc, rn) %>% filter(n_values > 0) %>%
    mutate(in_metadata = trait %in% meta_traits, organ = str_extract(trait, "^[a-z]+")) %>%
    filter(!in_metadata) %>% select(trait, type, organ, n_taxa, n_values, n_copied, range, suggested_values, source_terms) %>%
    arrange(desc(n_taxa), trait)
  write_csv(reg, path, na = "")
  invisible(reg)
}

if (Sys.getenv("FOA_FAMILIES") == "") {
  write_csv(out_df, out, na = "")
  write_candidates(out_df, "data/ABRS_2026/raw/candidate_traits.csv")
} else {
  write_csv(out_df, Sys.getenv("FOA_OUT", file.path(tempdir(), "foa_chunk.csv")), na = "")
}
