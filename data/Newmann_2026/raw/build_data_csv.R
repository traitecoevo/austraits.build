# Builds data/Newmann_2026/data.csv from the scraped WA Orchids fact sheets
# (wa_orchids_fact_sheets.csv, austraits.build-data.scraping.scripts/data_extra).
# Not run by the build -- kept for provenance / re-extraction.
# Categorical traits are written as a pair of columns: `<trait>_description`
# (verbatim source phrase) and `<trait>` (best-effort mapping to config/traits.yml levels),
# so the mapping can be revised later without re-reading the source text.

library(dplyr)
library(stringr)
library(purrr)
library(tibble)

src <- "~/GitHub/austraits.build-data.scraping.scripts/data_extra/wa_orchids_fact_sheets.csv"
out <- "data/Newmann_2026/data.csv"

d <-readr::read_csv(src, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
d[is.na(d)] <- ""
d <- d %>% mutate(across(everything(), str_squish))

# source typos: sentence break inside the C. bicalliata subsp. cleistogama flower phrase,
# and a reversed flower-size range for Diuris concinna ("20–15 mm across")
d <- d %>% mutate(Description = str_replace(Description, "up to two small\\. rarely opening", "up to two small, rarely opening"),
                  Description = ifelse(taxon_name == "Diuris concinna", str_replace(Description, "20\u201315 mm across", "15\u201320 mm across"), Description))

na_if_empty <- function(x) ifelse(is.na(x) | x == "", NA_character_, x)
collapse_unique <- function(x, sep = " ") {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

sentences <- function(x) {
  s <- str_split(x, "(?<=[.;])\\s+(?=[A-Z])")[[1]]
  s[s != ""]
}
grab_sentences <- function(x, pattern) {
  s <- sentences(x)
  collapse_unique(s[str_detect(s, regex(pattern, ignore_case = TRUE))], " ")
}

num_words <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8,
               nine = 9, ten = 10, eleven = 11, twelve = 12, fifteen = 15, twenty = 20, a = 1,
               single = 1, solitary = 1)
to_num <- function(x) {
  x <- str_to_lower(x)
  ifelse(x %in% names(num_words), num_words[x], suppressWarnings(as.numeric(x)))
}

dash <- "\\s*[\u2013\u2014-]\\s*"
num <- "[0-9]+(?:\\.[0-9]+)?"

# ---------------------------------------------------------------- flowering time
months <- month.abb
fm_to_yn <- function(x) {
  paste(ifelse(str_detect(x, months), "y", "n"), collapse = "")
}
parse_flowering <- function(fm) {
  if (fm == "") return(tibble(population_region = NA_character_, flowering_time = NA_character_))
  if (str_detect(fm, "\\(northern populations\\)")) {
    parts <- str_match(fm, "^(.*)\\(northern populations\\)(.*)\\(southern populations\\)")
    return(tibble(population_region = c("northern populations", "southern populations"),
                  flowering_time = c(fm_to_yn(parts[2]), fm_to_yn(parts[3]))))
  }
  tibble(population_region = NA_character_, flowering_time = fm_to_yn(fm))
}

# ---------------------------------------------------------------- description parsing
first_desc_sentence <- function(x) sentences(x)[1]

parse_height <- function(x) {
  m <- str_match(x, paste0("(", num, ")(?:", dash, "(", num, "))? mm (?:\\([^)]*\\) )?(?:high|tall)"))
  m2 <- str_match(x, paste0("(?:occasionally |rarely )?up to (", num, ") mm(?: high)?\\)"))
  tibble(plant_height_reproductive_min = ifelse(is.na(m[, 3]), NA, m[, 2]),
         plant_height_reproductive_max = coalesce(m2[, 2], m[, 3], m[, 2]),
         plant_height_reproductive_description = na_if_empty(str_match(x, paste0("(", num, "(?:", dash, num, ")? mm (?:\\([^)]*\\) )?(?:high|tall)(?: \\([^)]*\\))?)"))[, 2]))
}

# leaf phrase: from "with" up to the leaf noun
leaf_phrase <- function(x) {
  str_match(x, regex("\\bwith (?:an? |one |two |three |four |five |six |seven |eight |nine |ten |[0-9]+ |between |from |numerous |persistent )?([^.;]{0,160}?)\\b(leaf|leaves)\\b", ignore_case = TRUE))[, 1]
}

parse_leaf_dims <- function(x) {
  # first "long by ... wide" measurement that follows the word leaf/leaves
  m <- str_match(x, paste0("(?:leaf|leaves)[^.;]{0,120}?(", num, ")(?:", dash, "(", num, "))?\\s?mm long by (", num, ")(?:", dash, "(", num, "))?\\s?mm (?:wide|across)"))
  tibble(leaf_length_min = ifelse(is.na(m[, 3]), NA, m[, 2]),
         leaf_length_max = coalesce(m[, 3], m[, 2]),
         leaf_width_min = ifelse(is.na(m[, 5]), NA, m[, 4]),
         leaf_width_max = coalesce(m[, 5], m[, 4]))
}

parse_leaf_count <- function(x) {
  w <- "(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|fifteen|twenty|[0-9]+)"
  m <- str_match(str_to_lower(x), paste0("with (?:between )?", w, "(?: to | and | or |", dash, ")", w, " (?![^.;]{0,80}?cauline leaves)[^.;]{0,80}?leaves"))
  m1 <- str_match(str_to_lower(x), "with (a|one|single) [^.;0-9]{0,80}?\\bleaf\\b")
  tibble(leaf_count_min = ifelse(!is.na(m[, 2]), to_num(m[, 2]), ifelse(!is.na(m1[, 2]), 1, NA)),
         leaf_count_max = ifelse(!is.na(m[, 3]), to_num(m[, 3]), ifelse(!is.na(m1[, 2]), 1, NA)))
}

parse_cauline <- function(x) {
  w <- "(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|fifteen|twenty|[0-9]+)"
  m <- str_match(str_to_lower(x), paste0(w, "(?:(?: to |", dash, ")", w, ")? cauline leaves"))
  tibble(cauline_leaf_count_min = to_num(m[, 2]), cauline_leaf_count_max = coalesce(to_num(m[, 3]), to_num(m[, 2])))
}

parse_rosette <- function(x) {
  m <- str_match(x, paste0("rosette[^.;]{0,40}?(", num, ")(?:", dash, "(", num, "))? mm across"))
  tibble(plant_width_min = ifelse(is.na(m[, 3]), NA, m[, 2]),
         plant_width_max = coalesce(m[, 3], m[, 2]))
}

# flower phrase: from the count ("up to eight", "a single", "one (rarely two)") to "flower(s)"
flower_phrase <- function(x) {
  str_match(x, regex("(?:\\band |\\bflowering, |\\b(?:with|of|comprise|producing) (?=up to ))((?:up to (?:a )?[a-z0-9]+|a single|a solitary|one|two|single|numerous)(?: \\([^)]*\\))?)\\b,?\\s*([^.;]*?)\\bflowers?\\b", ignore_case = TRUE))
}

parse_flower_count <- function(count_txt) {
  n <- str_extract_all(str_to_lower(count_txt), "[0-9]+|\\b(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|fifteen|twenty|single|solitary)\\b")
  map_dbl(n, ~ if (length(.x) == 0) NA_real_ else suppressWarnings(max(to_num(.x), na.rm = TRUE))) %>% ifelse(is.infinite(.), NA_real_, .)
}

parse_storage_dims <- function(x) {
  m <- str_match(x, paste0("pseudobulbs?[^.;]{0,40}?(", num, ")(?:", dash, "(", num, "))? mm long by (", num, ")(?:", dash, "(", num, "))? mm (?:wide|across)"))
  tibble(storage_organ_length_min = ifelse(is.na(m[, 3]), NA, m[, 2]),
         storage_organ_length_max = coalesce(m[, 3], m[, 2]),
         storage_organ_diameter_min = ifelse(is.na(m[, 5]), NA, m[, 4]),
         storage_organ_diameter_max = coalesce(m[, 5], m[, 4]))
}

parse_flower_size <- function(x) {
  m <- str_match(x, paste0("\\bflowers?\\b[^.;]{0,140}?(", num, ")(?:", dash, "(", num, "))? mm (?:across|wide)"))
  tibble(flower_diameter_min = ifelse(is.na(m[, 3]), NA, m[, 2]),
         flower_diameter_max = coalesce(m[, 3], m[, 2]))
}

# ---------------------------------------------------------------- colour mapping
colour_map <- c(
  white = "white_cream", cream = "white_cream", creamy = "white_cream", ivory = "white_cream",
  yellow = "yellow_orange", yellowish = "yellow_orange", lemon = "yellow_orange", golden = "yellow_orange",
  gold = "yellow_orange", orange = "yellow_orange", apricot = "yellow_orange", straw = "yellow_orange",
  red = "red_brown", reddish = "red_brown", brown = "red_brown", brownish = "red_brown", maroon = "red_brown",
  bronze = "red_brown", crimson = "red_brown", rufous = "red_brown", fawn = "red_brown", cinnamon = "red_brown",
  rust = "red_brown", blood = "red_brown", russet = "red_brown", copper = "red_brown", tan = "red_brown",
  scarlet = "red_brown", burgundy = "red_brown", chestnut = "red_brown",
  pink = "pink", pinkish = "pink", rose = "pink", salmon = "pink", magenta = "pink",
  blue = "blue_purple", bluish = "blue_purple", purple = "blue_purple", purplish = "blue_purple",
  mauve = "blue_purple", lilac = "blue_purple", violet = "blue_purple", lavender = "blue_purple",
  indigo = "blue_purple",
  plum = "blue_purple",
  green = "green", greenish = "green", black = "black", blackish = "black", grey = "grey", greyish = "grey"
)

map_colours <- function(phrase) {
  if (is.na(phrase) || phrase == "") return(NA_character_)
  p <- str_to_lower(phrase)
  # drop rare variants and markings: "(rarely white)", "more rarely white", "brown marked", "red striped"
  p <- str_remove_all(p, "\\([^)]*rarely[^)]*\\)|,? more rarely [a-z -]+|,? rarely [a-z -]+")
  p <- str_remove_all(p, "\\b[a-z]+(?:-[a-z]+)? ?[-]?(?:marked|striped|blotched|spotted|tinged|suffused|veined|streaked|tipped|flushed)\\b")
  p <- str_remove_all(p, "\\bwith [a-z ]+ (?:stripes|blotches|markings|spots)\\b")
  p <- str_replace_all(p, "-coloured", " coloured")
  # for hyphenated compounds (greenish-yellow, creamy-white, blue-mauve) keep only the head colour
  p <- str_replace_all(p, "\\b[a-z]+-(?=[a-z]+\\b)", "")
  words <- str_extract_all(p, "[a-z]+")[[1]]
  vals <- unname(colour_map[words[words %in% names(colour_map)]])
  collapse_unique(vals)
}

# ---------------------------------------------------------------- leaf mapping
map_leaf_hairs <- function(p) {
  if (is.na(p)) return(NA_character_)
  p <- str_to_lower(p)
  p <- str_remove_all(p, "smooth (or wavy )?margined|smooth margin")
  case_when(str_detect(p, "almost hairless|glabrous|smooth") ~ "glabrous",
            str_detect(p, "hairy|hairs|pubescent") ~ "hairy",
            TRUE ~ NA_character_)
}
leaf_shape_map <- c("heart-shaped" = "cordate", "heart shaped" = "cordate", "tubular" = "terete",
                    "terete" = "terete", "rounded" = "orbicular", "orbicular" = "orbicular",
                    "oval" = "elliptical", "ovate" = "ovate", "lanceolate" = "lanceolate",
                    "linear" = "linear", "narrow" = "linear", "strap-like" = "strap-shaped",
                    "strap-shaped" = "strap-shaped", "grass-like" = "linear", "kidney-shaped" = "reniform",
                    "oblong" = "oblong", "elliptic" = "elliptical", "thread-like" = "filiform")
map_leaf_shape <- function(p) {
  if (is.na(p)) return(NA_character_)
  p <- str_to_lower(p)
  hits <- names(leaf_shape_map)[str_detect(p, paste0("\\b", names(leaf_shape_map), "\\b"))]
  # "narrow" only used as a shape when no more specific shape word is present
  if (length(hits) > 1) hits <- setdiff(hits, "narrow")
  collapse_unique(unname(leaf_shape_map[hits]))
}
leaf_colour_map <- c("dull green" = "green", "green" = "green", "bluish-green" = "blue_green",
                     "blue-green" = "blue_green", "grey-green" = "grey_green", "greyish-green" = "grey_green",
                     "olive-green" = "green_olive", "dark green" = "dark_green", "pale green" = "pale_green",
                     "yellowish-green" = "green_yellow", "yellowish green" = "green_yellow",
                     "silvery-green" = "green_silvery", "light-green" = "pale_green", "maroon" = "red")
map_leaf_colour <- function(p) {
  if (is.na(p)) return(NA_character_)
  p <- str_to_lower(p)
  pats <- names(leaf_colour_map)[order(-nchar(names(leaf_colour_map)))]
  hits <- c()
  for (k in pats) if (str_detect(p, fixed(k))) { hits <- c(hits, leaf_colour_map[[k]]); p <- str_remove_all(p, fixed(k)) }
  collapse_unique(hits)
}

# ---------------------------------------------------------------- fire
fire_pattern <- "fire|burnt|unburnt"
map_fire <- function(s) {
  if (is.na(s)) return(NA_character_)
  s <- str_to_lower(s)
  case_when(
    str_detect(s, "flowering equally well in burnt and unburnt") ~ "fire_independent_flowering",
    str_detect(s, "does not require (a )?(summer )?fire to flower but") ~ "fire_enhanced_flowering",
    str_detect(s, "does not (appear to )?require (a )?(summer )?fire") ~ "fire_independent_flowering",
    str_detect(s, "in some areas .*only .*while in others it flowers every year") ~ "fire_dependent_flowering fire_enhanced_flowering",
    str_detect(s, "flower(s|ing)? only in the season following|only in the season following") ~ "fire_dependent_flowering",
    str_detect(s, "rare or absent in unburnt|rare in unburnt|rarely flowers in unburnt|predominantly (flowers )?in the season following|appearing in much lower numbers in unburnt|requires a summer fire") ~ "fire_dependent_flowering",
    str_detect(s, "found on just two occasions, both times in the season following") ~ "fire_dependent_flowering",
    str_detect(s, "flower(s|ing)? best|greater profusion|most prolifically|greater numbers|particularly common in the season following|especially common in the season following") ~ "fire_enhanced_flowering",
    TRUE ~ NA_character_
  )
}

# ---------------------------------------------------------------- habitat
soil_words <- "sandy-clay|sandy clay|clay-loam|clay loam|sandy|sand|clay|loamy|loam|lateritic|laterite|granitic|gravelly|gravel|peaty|peat|limestone|calcareous|alluvial|skeletal|stony|rocky|ironstone|quartzite|basalt|sandstone|black|white/grey|white|grey|brown|red|yellow|organic|humus|shallow|moist|damp|boggy|saline|swampy|loose|deep|leaf litter"
parse_habitat <- function(dh) {
  if (dh == "") return(tibble(distribution_description = NA, habitat_description = NA, soil_terms = NA, habitat_terms = NA))
  s <- sentences(dh)
  first <- s[1]
  dist <- str_trim(str_match(first, "^(.*?),? (?:in inland areas it )?(?:growing|grows|found growing)\\b")[, 2])
  if (is.na(dist)) dist <- first
  # habitat = all "growing in/on ..." clauses + other habitat sentences, minus fire and extra-WA distribution text
  grow <- str_match_all(dh, "(?:growing|grows) (?:in|on|as|among|amongst|along|around|near|under|beside|at) ([^.]*?)(?=, flowering|,? and flowers best|\\.|$)")[[1]][, 2]
  grow <- grow[!is.na(grow)]
  other <- s[-1]
  other <- other[!str_detect(other, regex("also found in|also occurs in|fire|burnt", ignore_case = TRUE))]
  hab_desc <- collapse_unique(c(grow, other), "; ")
  # soil terms: words in front of "soil(s)/sand"
  soil_phr <- str_extract_all(str_to_lower(paste(grow, collapse = "; ")), "(?:[a-z/-]+(?:,| and| or)? ){0,6}(?:soils?|sand|loam|clay)\\b")[[1]]
  soil_t <- unlist(str_extract_all(soil_phr, paste0("\\b(", soil_words, ")\\b")))
  soil_t <- soil_t[!soil_t %in% c("shallow", "moist", "damp", "deep", "loose")]
  soil_t <- str_replace_all(soil_t, c("^sandy clay$" = "sandy-clay", "^sand$" = "sandy", "^laterite$" = "lateritic",
                                      "^peat$" = "peaty", "^loam$" = "loamy", "^gravel$" = "gravelly",
                                      "^clay loam$" = "clay-loam"))
  # habitat terms: what follows "soil(s) in/on/around/along ..." (or the whole clause when no soil is named)
  hab_parts <- map(grow, function(g) {
    rest <- str_match(g, "(?:soils?|sand|loam|clay|peat|limestone|laterite)\\b,? (?:in|on|around|along|at|near|of|over|amongst|among|beside|under|surrounding|fringing) (.*)$")[, 2]
    if (is.na(rest)) rest <- g
    rest <- str_remove(rest, "^(the )?(edges|margins|fringes|bases|base) of ")
    str_split(rest, ",\\s*| and (?!adjacent)| or |; ")[[1]]
  })
  hab_t <- str_squish(str_remove(unlist(hab_parts), "^(in|on|and|or|the|along|around) "))
  hab_t <- hab_t[hab_t != ""]
  tibble(distribution_description = na_if_empty(dist), habitat_description = hab_desc,
         soil_terms = collapse_unique(soil_t, "; "), habitat_terms = collapse_unique(hab_t, "; "))
}

# ---------------------------------------------------------------- per-taxon extraction
extract_one <- function(r) {
  desc <- r[["Description"]]
  dh <- r[["Distribution and Habitat"]]
  df <- r[["Distinguishing Features"]]
  notes <- r[["Notes"]]
  all_txt <- paste(desc, dh, df, notes)
  s1 <- first_desc_sentence(desc)

  lp <- leaf_phrase(s1)
  fp <- flower_phrase(desc)
  flower_count_txt <- fp[, 2]
  colour_phr <- str_squish(str_remove(fp[, 3], "^,"))
  # strip scent / size / non-colour adjectives from the front, but keep it verbatim otherwise
  colour_phr_clean <- str_squish(str_remove_all(colour_phr, regex("\\b(sweetly|strongly|highly|high|pungently|sometimes|faintly)?[- ]?(fragrant|scented|perfumed|lemon-scented|cinnamon-scented|acridly-scented|unpleasantly-scented)\\b,?|\\b(large|small|well-spaced|widely spaced|insignificant|attractive|self-pollinating|short-lived|rarely opening|semi-closed|tubular|inverted|nodding|fleshy|semi translucent|translucent|spirally arranged|inward facing|glossy|upward-facing|predominantly|variably coloured|fan-shaped|bell-shaped|smallish|relatively|colourful|hairy|prominently cupped|slightly cupped|often widely-spaced|narrow|greasy-textured|\\(upside down\\))(?![a-z-]),?|\\bglossy-", ignore_case = TRUE)))
  colour_phr_clean <- na_if_empty(str_remove(colour_phr_clean, "^,\\s*|,$"))

  # case-sensitive so that common names ("Scented Sun Orchid", "Fragrant China Orchid") are skipped
  scent <- str_extract_all(all_txt, "(?<![A-Za-z-])(?:(?:sometimes |strongly|sweetly|highly|high|pungently sweet|pungently|faintly|acridly|unpleasantly|unpleasant|lemon|cinnamon|citrus|sweet|floral)[- ]?)?(?:fragrant|scented|perfumed|perfume|odour)\\b(?: faint, like burning metal)?")[[1]]
  scent_desc <- collapse_unique(str_squish(scent), "; ")

  poll_desc <- grab_sentences(all_txt, "pollinat|cleistog|rarely opening|freely opening|nectar|hinged labellum|insect-like|mantis-like|temperature sensitive|open for one day|short-lived|spur")
  poll_explicit <- case_when(
    str_detect(all_txt, regex("(?<!rather than )self[- ]pollinating", ignore_case = TRUE)) &
      !str_detect(all_txt, regex("insect pollinated rather than self", ignore_case = TRUE)) ~ "self",
    str_detect(all_txt, regex("insect pollinated", ignore_case = TRUE)) ~ "insect",
    TRUE ~ NA_character_)
  breeding <- case_when(
    str_detect(all_txt, regex("cleistogam|rarely opening", ignore_case = TRUE)) & poll_explicit %in% "self" ~ "autogamy cleistogamy",
    poll_explicit %in% "self" ~ "autogamy",
    TRUE ~ NA_character_)
  nectar <- case_when(str_detect(all_txt, regex("nectar producing|nectary|nectar-producing", ignore_case = TRUE)) ~ "nectar_produced",
                      TRUE ~ NA_character_)
  flower_life_desc <- grab_sentences(all_txt, "open for one day|rarely open for more than|short-lived")
  flower_life <- case_when(str_detect(all_txt, "open for one day only") ~ "1", TRUE ~ NA_character_)

  fire_desc <- grab_sentences(paste(desc, dh, df, notes), fire_pattern)
  fire_desc <- if (is.na(fire_desc)) NA_character_ else fire_desc
  # firebreak/graded-track sentences are about disturbance habitat, not fire response
  if (!is.na(fire_desc) && !str_detect(fire_desc, regex("summer fire|following (a )?fire|in burnt|require fire|absence of fire|recently burnt|unburnt", ignore_case = TRUE))) fire_desc <- NA_character_

  substrate_desc <- grab_sentences(all_txt, "epiphyt|lithophyt|on trees|tree stumps|fallen logs|rock ledges|crevices|host")
  substrate <- collapse_unique(c(
    if (str_detect(all_txt, regex("epiphyt|unknown host|tree hosts", ignore_case = TRUE))) "epiphyte",
    if (str_detect(all_txt, regex("lithophyt", ignore_case = TRUE))) "lithophyte",
    if (str_detect(dh, regex("soil|sand|clay|loam|peat|gravel", ignore_case = TRUE)) | str_detect(all_txt, regex("geophyt|below the soil surface", ignore_case = TRUE))) "terrestrial"))

  gf_desc <- grab_sentences(all_txt, "geophyt|lithophytic herbs|epiphytic or lithophytic")
  gf <- collapse_unique(c(if (str_detect(all_txt, regex("geophyt", ignore_case = TRUE))) "geophyte",
                          if (str_detect(all_txt, regex("epiphytic or lithophytic herbs|lithophytic herbs", ignore_case = TRUE))) "herb"))

  photo_desc <- grab_sentences(all_txt, "leafless|non-green|lack chlorophyll|saprophyt|mycorrhizal fungi|entire life cycle|reduced to (inconspicuous|a minute) bract")
  photo <- case_when(
    str_detect(all_txt, regex("some even being leafless saprophytes", ignore_case = TRUE)) ~ NA_character_,
    str_detect(all_txt, regex("non-green|lack chlorophyll|leafless saprophyt|entire life cycle, including flowering, below the soil", ignore_case = TRUE)) ~ "non-photosynthetic_plant",
    TRUE ~ NA_character_)
  leafless <- case_when(
    str_detect(all_txt, regex("leaves that are reduced to inconspicuous bracts", ignore_case = TRUE)) ~ "scale_leaves",
    str_detect(all_txt, regex("leafless \\(when flowering\\)", ignore_case = TRUE)) ~ NA_character_,
    str_detect(all_txt, regex("\\bleafless\\b(?! geophytes)", ignore_case = TRUE)) & !str_detect(r[["taxon_name"]], "^(Eulophia|Dipodium)$") ~ "leafless",
    TRUE ~ NA_character_)

  storage_desc <- grab_sentences(all_txt, "tuber|pseudobulb|rhizom|storage organ")
  storage <- collapse_unique(c(
    if (str_detect(all_txt, regex("stem tuber", ignore_case = TRUE))) "stem_tuber",
    if (str_detect(all_txt, regex("\\btubers?\\b", ignore_case = TRUE)) & !str_detect(all_txt, regex("stem tuber", ignore_case = TRUE))) "tuber",
    if (str_detect(all_txt, regex("pseudobulb", ignore_case = TRUE))) "pseudobulb",
    if (str_detect(all_txt, regex("rhizome[^.]*succulent", ignore_case = TRUE))) "rhizome_fleshy"
    else if (str_detect(all_txt, regex("rhizom", ignore_case = TRUE)) & !str_detect(all_txt, regex("pseudobulb", ignore_case = TRUE))) "rhizome"))

  veg_pattern <- "colon(y|ies)[- ]forming|colony forming|clonal|vegetative reproduction|daughter tuber"
  veg_desc <- grab_sentences(all_txt, veg_pattern)
  veg <- if (is.na(veg_desc)) NA_character_ else "vegetative"
  clonal <- if (is.na(veg_desc)) NA_character_ else if (str_detect(veg_desc, "stem tuber")) "stem_tuber" else "clonal"
  # "grows in clumps" / "clumping habit" is not evidence of vegetative reproduction; kept as text only
  clump_desc <- grab_sentences(all_txt, "\\bclump(?!s? of spinifex)(?<!spinifex clump)")

  phen_desc <- grab_sentences(all_txt, "evergreen|deciduous|withered at the time of flowering|shrivelled (at|when)|appears following flowering|produced after|brown and shrivelled")
  phen <- collapse_unique(c(if (str_detect(all_txt, regex("\\bevergreen\\b", ignore_case = TRUE))) "evergreen",
                            if (str_detect(all_txt, regex("\\bdeciduous\\b", ignore_case = TRUE))) "deciduous",
                            if (str_detect(all_txt, regex("(withered|shrivelled)[^.]*(at the time of flowering|when plants are in flower)", ignore_case = TRUE))) "withered_at_flowering"))

  lp_desc <- na_if_empty(str_squish(str_remove(lp, "^with ")))
  leaf_arr <- collapse_unique(c(if (str_detect(s1, regex("rosette", ignore_case = TRUE))) "rosette",
                                if (str_detect(s1, regex("ground[- ]hugging|basal leaf|basal leaves", ignore_case = TRUE)) & !str_detect(s1, regex("rosette", ignore_case = TRUE))) "clustered_basal"))

  hab <- parse_habitat(dh)

  bind_cols(
    tibble(
      taxon_name = r[["taxon_name"]],
      taxon_rank = r[["taxon_rank"]],
      common_name = na_if_empty(r[["Common Name"]]),
      fact_sheet_url = r[["url"]],
      wa_conservation_code = na_if_empty(r[["WA Conservation Code (Threatened Status)"]]),
      flowering_months_description = na_if_empty(r[["Flowering Months"]])
    ),
    parse_height(s1),
    tibble(leaf_description = lp_desc,
           leaf_shape = map_leaf_shape(lp_desc),
           leaf_hairs_adult_leaves = map_leaf_hairs(lp_desc),
           leaf_surface_colour = map_leaf_colour(lp_desc),
           leaf_arrangement = leaf_arr),
    parse_leaf_count(s1),
    parse_cauline(s1),
    parse_rosette(s1),
    parse_leaf_dims(s1),
    tibble(leaf_length_type = leafless,
           flower_count_description = na_if_empty(flower_count_txt),
           flower_count_maximum = parse_flower_count(flower_count_txt),
           flower_colour_description = colour_phr_clean,
           flower_colour = map_colours(colour_phr_clean)),
    parse_flower_size(desc),
    tibble(flower_scent_description = scent_desc,
           flower_scent_production = if (is.na(scent_desc)) NA_character_ else if (str_detect(scent_desc, regex("lemon|cinnamon|sweet|fragrant|scented|perfum|odour", ignore_case = TRUE))) "scent_produced" else NA_character_,
           pollination_description = poll_desc,
           pollination_syndrome = poll_explicit,
           breeding_system = breeding,
           flower_nectar_production = nectar,
           flower_lifespan_description = flower_life_desc,
           flower_lifespan = flower_life,
           post_fire_flowering_description = fire_desc,
           post_fire_flowering = map_fire(fire_desc),
           plant_growth_substrate_description = substrate_desc,
           plant_growth_substrate = substrate,
           plant_growth_form_description = gf_desc,
           plant_growth_form = gf,
           plant_photosynthetic_organ_description = photo_desc,
           plant_photosynthetic_organ = photo,
           storage_organ_description = storage_desc,
           storage_organ = storage,
           vegetative_reproduction_description = veg_desc,
           vegetative_reproduction_ability = veg,
           clonal_spread_mechanism = clonal,
           clumping_description = clump_desc,
           leaf_phenology_description = phen_desc,
           leaf_phenology = phen),
    parse_storage_dims(desc),
    hab
  )
}

wide <- map_dfr(seq_len(nrow(d)), ~ extract_one(d[.x, ]))

# habitat terms: drop frequency qualifiers ("often", "more rarely in", ...) and harmonise spelling,
# keeping the fact sheets' own vocabulary; then apply hand-curated fixes for irregular sentences
clean_terms <- function(x) {
  if (is.na(x)) return(NA_character_)
  t <- str_split(x, "; ")[[1]]
  t <- str_remove(t, "^(?:(?:often|also|sometimes|more rarely|occasionally|usually|particularly|predominantly|rarely|in|on|around|a|an|the)\\s+)+")
  t <- str_replace_all(t, c("^Mallee" = "mallee", "mallee-woodlands" = "mallee woodlands", "seasonally-" = "seasonally ",
                            "\\brun off\\b|\\brunoff\\b" = "run-off", "^seasonally wet flat$" = "seasonally wet flats"))
  collapse_unique(t, "; ")
}
overrides <- readr::read_csv("data/Newmann_2026/raw/habitat_overrides.csv", show_col_types = FALSE, col_types = "ccc")
wide <- wide %>%
  mutate(habitat_terms = map_chr(habitat_terms, clean_terms)) %>%
  rows_update(overrides, by = "taxon_name", unmatched = "error")

# Praecoxanthus aphyllus: "with a fragrant, creamy-white to pale yellow, fan-shaped flower" (no count word the regex anchors on)
wide <- wide %>%
  mutate(flower_count_description = ifelse(taxon_name == "Praecoxanthus aphyllus", "a", flower_count_description),
         flower_count_maximum = ifelse(taxon_name == "Praecoxanthus aphyllus", 1, flower_count_maximum),
         flower_colour_description = ifelse(taxon_name == "Praecoxanthus aphyllus", "creamy-white to pale yellow", flower_colour_description),
         flower_colour = ifelse(taxon_name == "Praecoxanthus aphyllus", "white_cream yellow_orange", flower_colour))

# Eriochilus valens: fire-dependent in the high-rainfall lower south-west, but "at least some plants
# flower every year" between the Stirling Range and Munglinup (sentence has no fire keyword)
wide <- wide %>%
  mutate(post_fire_flowering = ifelse(taxon_name == "Eriochilus valens", "fire_dependent_flowering fire_enhanced_flowering", post_fire_flowering),
         post_fire_flowering_description = ifelse(taxon_name == "Eriochilus valens",
           grab_sentences(d[["Distribution and Habitat"]][d$taxon_name == "Eriochilus valens"], "fire|every year"), post_fire_flowering_description))

# flowering time: one row per taxon, plus northern/southern rows where the sheet splits the calendar
fl <- map_dfr(seq_len(nrow(d)), ~ parse_flowering(d[["Flowering Months"]][.x]) %>% mutate(taxon_name = d$taxon_name[.x]))
fl_main <- fl %>% filter(is.na(population_region)) %>% select(taxon_name, flowering_time)
fl_pop <- fl %>% filter(!is.na(population_region))

# Contexts in traits.build apply to a whole row, so values that carry a context get their own rows:
#   population_region     -- northern/southern flowering calendars
#   entity_measured       -- rosette (plant_width), basal_leaf / cauline_leaf (leaf_count)
#   trait_scoring_method  -- inferred_from_genus (values copied down from a genus fact sheet)
id_cols <- c("taxon_name", "taxon_rank", "common_name", "fact_sheet_url")

# genus fact sheets: only statements that apply to every species without hedging ("most", "some",
# "often", ranges such as "evergreen to deciduous") are copied down, and only where the species sheet is silent
genus_inherit <- tribble(
  ~genus,         ~trait,                       ~desc_col,
  "Didymoplexis", "plant_photosynthetic_organ", "plant_photosynthetic_organ_description",
  "Didymoplexis", "leaf_length_type",           "plant_photosynthetic_organ_description",
  "Dipodium",     "plant_photosynthetic_organ", "plant_photosynthetic_organ_description", # "The Kimberley species are all leafless saprophytic" (all WA spp. are Kimberley)
  "Dipodium",     "plant_growth_form",          "plant_growth_form_description",          # "... wet season growing geophytes"
  "Gastrodia",    "plant_photosynthetic_organ", "plant_photosynthetic_organ_description",
  "Gastrodia",    "leaf_length_type",           "plant_photosynthetic_organ_description",
  "Rhizanthella", "plant_photosynthetic_organ", "plant_photosynthetic_organ_description",
  "Rhizanthella", "storage_organ",              "storage_organ_description",
  "Bulbophyllum", "storage_organ",              "storage_organ_description",
  "Bulbophyllum", "plant_growth_substrate",     "plant_growth_substrate_description",
  "Bulbophyllum", "plant_growth_form",          "plant_growth_form_description",
  "Cymbidium",    "storage_organ",              "storage_organ_description",
  "Cymbidium",    "leaf_phenology",             "leaf_phenology_description",
  "Cryptostylis", "leaf_phenology",             "leaf_phenology_description",
  "Dendrobium",   "plant_growth_substrate",     "plant_growth_substrate_description",
  "Empusa",       "plant_growth_form",          "plant_growth_form_description",
  "Empusa",       "leaf_phenology",             "leaf_phenology_description",
  "Eulophia",     "plant_growth_form",          "plant_growth_form_description",          # "all Eulophia species are geophytic"
  "Eulophia",     "storage_organ",              "storage_organ_description",
  "Habenaria",    "storage_organ",              "storage_organ_description",
  "Pecteilis",    "storage_organ",              "storage_organ_description",
  "Pecteilis",    "flower_nectar_production",   "pollination_description",
  "Cyrtostylis",  "flower_nectar_production",   "pollination_description",
  "Nervilia",     "storage_organ",              "storage_organ_description",
  "Nervilia",     "vegetative_reproduction_ability", "vegetative_reproduction_description",
  "Nervilia",     "clonal_spread_mechanism",    "vegetative_reproduction_description",
  "Zeuxine",      "storage_organ",              "storage_organ_description",
  "Eriochilus",   "flower_scent_production",    "flower_scent_description",
  "Microtis",     "flower_scent_production",    "flower_scent_description",
  "Prasophyllum", "flower_scent_production",    "flower_scent_description"
)
genus_vals <- wide %>% filter(taxon_rank == "genus")
genus_rows <- pmap_dfr(genus_inherit, function(genus, trait, desc_col) {
  g <- genus_vals %>% filter(taxon_name == genus)
  stopifnot(nrow(g) == 1, !is.na(g[[trait]]))
  wide %>%
    filter(taxon_rank != "genus", word(taxon_name, 1) == genus, is.na(.data[[trait]])) %>%
    select(all_of(id_cols)) %>%
    mutate(!!trait := g[[trait]],
           !!paste0(trait, "_description") := paste0("[genus fact sheet] ", g[[desc_col]]))
}) %>%
  group_by(across(all_of(id_cols))) %>%
  summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop") %>%
  mutate(trait_scoring_method = "inferred_from_genus")

# rosette width -> plant_width with entity_measured = rosette
rosette_rows <- wide %>% filter(!is.na(plant_width_max)) %>%
  select(all_of(id_cols), plant_width_min, plant_width_max) %>%
  mutate(entity_measured = "rosette")

# storage organ dimensions -> own rows, entity_measured = the organ measured (all are pseudobulbs here)
storage_rows <- wide %>% filter(!is.na(storage_organ_length_max) | !is.na(storage_organ_diameter_max)) %>%
  select(all_of(id_cols), starts_with("storage_organ_length_"), starts_with("storage_organ_diameter_")) %>%
  mutate(entity_measured = "pseudobulb")

# leaf counts: where cauline leaves are counted separately, both counts get their own labelled rows
cauline <- !is.na(wide$cauline_leaf_count_max)
leaf_rows <- bind_rows(
  wide[cauline & !is.na(wide$leaf_count_max), ] %>% select(all_of(id_cols), leaf_count_min, leaf_count_max) %>% mutate(entity_measured = "basal_leaf"),
  wide[cauline, ] %>% select(all_of(id_cols), leaf_count_min = cauline_leaf_count_min, leaf_count_max = cauline_leaf_count_max) %>% mutate(entity_measured = "cauline_leaf")
)

main <- wide %>%
  mutate(leaf_count_min = ifelse(cauline, NA, leaf_count_min),
         leaf_count_max = ifelse(cauline, NA, leaf_count_max)) %>%
  select(-plant_width_min, -plant_width_max, -cauline_leaf_count_min, -cauline_leaf_count_max,
         -starts_with("storage_organ_length_"), -starts_with("storage_organ_diameter_")) %>%
  left_join(fl_main, by = "taxon_name")

out_df <- bind_rows(
  main,
  fl_pop %>% left_join(wide %>% select(all_of(id_cols), flowering_months_description), by = "taxon_name"),
  rosette_rows, storage_rows, leaf_rows, genus_rows
) %>%
  relocate(population_region, entity_measured, trait_scoring_method, .after = taxon_rank) %>%
  relocate(flowering_time, .after = flowering_months_description) %>%
  relocate(plant_width_min, plant_width_max, .after = leaf_count_max) %>%
  mutate(row_order = case_when(!is.na(population_region) ~ 1, !is.na(entity_measured) ~ 2, !is.na(trait_scoring_method) ~ 3, TRUE ~ 0)) %>%
  arrange(taxon_name, row_order, population_region, entity_measured) %>%
  select(-row_order)

# numeric ranges inside text written as "a--b" (a hyphen or en dash between numbers is read as a formula/date by Excel)
out_df <- out_df %>% mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-\u2013\u2014]\\s*(?=[0-9])", "--")))

readr::write_csv(out_df, out, na = "")
