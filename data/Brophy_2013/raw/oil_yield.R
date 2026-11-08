# Brophy_2013: the book's "Oil yield" field -> raw/ecology_vetted.csv rows of leaf_essential_oil_per_dry_mass (% w/w, as written; the
# metadata's unit_in is %, converted to the trait's mg/g by traits.build), which the parser
# (data/ABRS_AtoM_2026/raw/build_data_csv.R, FOA_SOURCE=brophy) reads.
#   - only dry-weight yields: "The oil yield (dry weight, w/w) was 2.5-2.8%." The fresh-weight yields (most species) have no AusTraits
#     trait (fresh mass) and are left in the text.
#   - "0.1-1.2%" -> 0.1-1.2; "<0.2%" -> max:0.2
#   - a species with subspecies / varieties gives one paragraph per taxon ("subsp. corrugata: The oil yield (dry weight, w/w) was 2.2%.
#     subsp. fulgens: ... (fresh weight ...)"): each taxon is read from its own paragraph
#   - statements mixing chemotypes, collections or fresh and dry weight are transcribed by hand below (their dry-weight figures only,
#     the range over the chemotypes / collections); the two yields the book calls unreliable ("obtained on a very small sample and is
#     probably not reliable": M. fabri, M. violacea) are left out
# Run from the repository root: Rscript data/Brophy_2013/raw/oil_yield.R

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr); library(readr); library(tibble); library(tidyr) })

prof <- read_csv("~/GitHub/austraits.build-data.scraping.scripts/data_extra/brophy2013_profiles.csv", show_col_types = FALSE, col_types = cols(.default = "c"))

# one paragraph per taxon
paras <- prof %>% select(taxon_name, oil_yield) %>% filter(!is.na(oil_yield)) %>%
  mutate(p = map(oil_yield, function(x) {
    if (!str_detect(x, "^(?:subsp|var)\\. [a-z]")) return(tibble(infra = NA_character_, txt = x))
    m <- str_match_all(x, "((?:subsp|var)\\. [a-z][a-z-]+): (.*?)(?=\\s*(?:subsp|var)\\. [a-z][a-z-]+:|$)")[[1]]
    tibble(infra = m[, 2], txt = m[, 3]) })) %>%
  select(-oil_yield) %>% unnest(p) %>% mutate(taxon = ifelse(is.na(infra), taxon_name, paste(taxon_name, infra)))

pct <- "(<\\s?)?([0-9.]+)(?:\\s?–\\s?([0-9.]+))?\\s?%"
simple_re <- paste0("^The oil yield \\((?:dry weight, w/w|w/w, dry weight)\\) was ", pct, "\\.?$")
simple <- paras %>% filter(str_detect(txt, simple_re)) %>%
  mutate(m = str_match(txt, simple_re), lt = m[, 2], lo = m[, 3], hi = m[, 4],
         value = case_when(!is.na(lt) ~ paste0("max:", lo), !is.na(hi) ~ paste0(lo, "-", hi), TRUE ~ lo)) %>%
  transmute(taxon_name = taxon, value, evidence = txt)

# (by hand: dry-weight figures in mixed statements, in %)
hand <- tribble(
  ~taxon_name, ~value,
  "Melaleuca citrolens", "2-6",                    # 3-6% for form (b), 2-4% for the others (dry weight)
  "Melaleuca incana subsp. incana", "0.5",           # 0.5% (dry weight) for the ANBG sample; the other yield is fresh weight
  "Melaleuca leucadendra", "0.1-2.5",                  # chemotypes I and II 1.0-2.5%, chemotype III 0.1-0.5% (dry weight)
  "Melaleuca linariifolia", "3.0-4.5",                 # 3.0-4.5% (dry weight) for chemotype III; chemotypes I and II fresh weight
  "Melaleuca trichostachya", "1.6-2.3",                # 1.6-2.3% (dry weight) for JRC 9623; BVG 2526 fresh weight
  "Melaleuca viridiflora", "0.4-4")                    # chemotype I variants one and three 1.3-2.1% and 0.4-0.9%, chemotype II 4% (dry weight)
hand <- hand %>% left_join(paras %>% select(taxon_name = taxon, evidence = txt), by = "taxon_name")
stopifnot(!any(is.na(hand$evidence)))

out <- bind_rows(simple %>% filter(!taxon_name %in% hand$taxon_name), hand) %>%
  transmute(taxon_name, trait_name = "leaf_essential_oil_per_dry_mass", value, commonness_qualifier = NA_character_,
            trait_scoring_method = NA_character_, evidence, scoring_certainty = NA_character_) %>%
  arrange(taxon_name)
write_csv(out, "data/Brophy_2013/raw/ecology_vetted.csv", na = "")
cat(nrow(out), "rows,", n_distinct(out$taxon_name), "taxa\n")
# dry-weight statements not read (to check: all should be the unreliable ones)
left <- paras %>% filter(str_detect(txt, "dry weight"), !taxon %in% out$taxon_name)
print(left %>% select(taxon, txt), width = 200)
