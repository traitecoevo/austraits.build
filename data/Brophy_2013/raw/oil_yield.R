# Brophy_2013: the book's "Oil yield" field -> raw/ecology_vetted.csv rows of leaf_essential_oil_per_dry_mass and
# leaf_essential_oil_per_fresh_mass (% w/w, as written; the metadata's unit_in is %, converted to the traits' mg/g by traits.build), which
# the parser (data/ABRS_AtoM_2026/raw/build_data_csv.R, FOA_SOURCE=brophy) reads.
#   - the basis decides the trait: "The oil yield (dry weight, w/w) was 2.5-2.8%." -> per dry mass; "(fresh weight, w/w)" -> per fresh
#     mass (user 2026-11-09: the fresh-mass trait was added to traits.yml for these)
#   - "0.1-1.2%" -> 0.1-1.2; "<0.2%" -> max:0.2; "nil" -> 0
#   - a species with subspecies / varieties gives one paragraph per taxon ("subsp. corrugata: The oil yield (dry weight, w/w) was 2.2%.
#     subsp. fulgens: ... (fresh weight ...)"): each taxon is read from its own paragraph
#   - statements mixing chemotypes, collections or fresh and dry weight are transcribed by hand below (the range over the chemotypes /
#     collections, for each basis; a "<" figure among others gives a maximum only); "only a trace amount" (M. brachyandra) is not a value;
#     the two yields the book calls unreliable ("obtained on a very small sample and is probably not reliable": M. fabri, M. violacea)
#     are left out
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

traits <- c(dry = "leaf_essential_oil_per_dry_mass", fresh = "leaf_essential_oil_per_fresh_mass")
pct <- "(<\\s?)?([0-9.]+)(?:\\s?–\\s?([0-9.]+))?\\s?%"
simple <- map_dfr(names(traits), function(b) {
  re <- paste0("^The oil yield \\((?:", b, " weight, w/w|w/w, ", b, " weight)\\) was ", pct, "\\.?$")
  paras %>% filter(str_detect(txt, re)) %>%
    mutate(m = str_match(txt, re), lt = m[, 2], lo = m[, 3], hi = m[, 4],
           value = case_when(!is.na(lt) ~ paste0("max:", lo), !is.na(hi) ~ paste0(lo, "-", hi), TRUE ~ lo)) %>%
    transmute(taxon_name = taxon, trait_name = traits[[b]], value, evidence = txt)
})

# (by hand: mixed statements, in %)
hand <- tribble(
  ~taxon_name, ~basis, ~value,
  "Melaleuca citrolens", "dry", "2-6",                          # 3-6% for form (b), 2-4% for the others
  "Melaleuca incana subsp. incana", "dry", "0.5",               # 0.5% (dry weight) for the ANBG sample
  "Melaleuca incana subsp. incana", "fresh", "0.1",             # 0.1% (fresh weight)
  "Melaleuca leucadendra", "dry", "0.1-2.5",                    # chemotypes I and II 1.0-2.5%, chemotype III 0.1-0.5%
  "Melaleuca linariifolia", "dry", "3.0-4.5",                   # chemotype III 3.0-4.5% (dry weight)
  "Melaleuca linariifolia", "fresh", "1-2",                     # chemotypes I and II 1-2% (fresh weight)
  "Melaleuca trichostachya", "dry", "1.6-2.3",                  # JRC 9623 1.6-2.3% (dry weight)
  "Melaleuca trichostachya", "fresh", "1.0-1.5",                # BVG 2526 1.0-1.5% (fresh weight)
  "Melaleuca viridiflora", "dry", "0.4-4",                      # chemotype I variants one and three 1.3-2.1% and 0.4-0.9%, chemotype II 4%
  "Melaleuca viridiflora", "fresh", "0.4-0.7",                  # chemotype I variant two 0.4-0.7% (fresh weight)
  "Melaleuca alsophila", "fresh", "0.1-1.2",                    # three collections: 0.1%, 1.0-1.2%, 0.1-0.3%
  "Melaleuca alternifolia", "fresh", "3-6",                     # 3-6%, the terpinolene chemotype 4-5%
  "Melaleuca atroviridis", "fresh", "0.3-3.3",                  # two samples: 0.3-1.0%, 2.5-3.3%
  "Melaleuca bracteata", "fresh", "0.1-1.5",                    # aromatic chemotypes 0.1-1.5%, terpenoid samples 0.1-0.2%
  "Melaleuca cajuputi subsp. platyphylla", "fresh", "0.1-1.2",  # chemotype I 0.2-0.6%, chemotype II 0.1-1.2%
  "Melaleuca carrii", "fresh", "0.7-0.8",                       # both collections 0.7-0.8%
  "Melaleuca cordata", "fresh", "0.4-0.7",                      # two chemical forms: 0.4%, 0.7%
  "Melaleuca dealbata", "fresh", "0.1-0.2",                     # 0.1-0.2%, the Mt Molloy samples likewise
  "Melaleuca decora", "fresh", "0.1-0.2",                       # both cases 0.1-0.2%
  "Melaleuca exuvia", "fresh", "1.3-2.3",                       # chemotypes I-III: 1.3-2.1%, 1.3%, 2.0-2.3%
  "Melaleuca groveana", "fresh", "max:0.8",                     # trace-0.2% and 0.5-0.8% (two collections)
  "Melaleuca halophila", "fresh", "1.0-1.7",                    # chemotype I 1.0-1.3%, chemotype II 1.7%
  "Melaleuca hamata", "fresh", "0.6-2.0",                       # chemotype I 0.6-1.0%, chemotype II 0.7-2.0%
  "Melaleuca lanceolata", "fresh", "max:0.4",                   # <0.1%, one sample 0.4%
  "Melaleuca leiopyxis", "fresh", "0.3-0.8",                    # chemotypes I-III: 0.3%, 0.8%, 0.4%
  "Melaleuca linearis var. linearis", "fresh", "0.1-1.0",       # "variable ..., being 0.1-1.0%"
  "Melaleuca pentagona var. latifolia", "fresh", "0.3-1.1",     # two collections: 0.6-1.1%, 0.3-0.6%
  "Melaleuca phratra", "fresh", "0",                            # nil
  "Melaleuca polandii", "fresh", "0.01",                        # approximately 0.01%
  "Melaleuca pungens", "fresh", "max:0.03",                     # very low, <0.03%
  "Melaleuca quinquenervia", "fresh", "max:2.5",                # <0.4% north of 25°S, 1.0-2.5% south of 25°S
  "Melaleuca sericea", "fresh", "0.2-1.5",                      # two collections: 0.2%, 1.5%
  "Melaleuca spicigera", "fresh", "0.1-0.7",                    # chemotype I 0.1%, chemotype II 0.5-0.7%
  "Melaleuca squamophloia", "fresh", "0.4-3.1",                 # 0.4-3.1%, the majority >1.7%
  "Melaleuca stenostachya", "fresh", "1.0-1.8",                 # 1.3-1.8%, sample JD 2036 1.0-1.2%
  "Melaleuca teretifolia", "fresh", "0.2-1.5",                  # chemotype I 0.2%, chemotype II 1.5%
  "Melaleuca tuberculata var. macrophylla", "fresh", "0.2",     # 0.2% in both samples
  "Melaleuca tuberculata var. tuberculata", "fresh", "0.4",     # in all cases 0.4%
  "Melaleuca uncinata", "fresh", "0.3-1.3",                     # chemotype I 0.3-0.5%, chemotype II 1.3%
  "Melaleuca viminalis subsp. viminalis", "fresh", "0.1-0.6",   # 0.1-0.6%, independent of latitude
  "Melaleuca viminea subsp. viminea", "fresh", "max:1.4",       # chemotype I <0.1%, chemotype II 1.4%
  "Melaleuca vinnula", "fresh", "1.2-2.8")                      # chemotype I 1.2%, chemotype II 1.9-2.8%
hand <- hand %>% mutate(trait_name = traits[basis]) %>% select(-basis) %>%
  left_join(paras %>% select(taxon_name = taxon, evidence = txt), by = "taxon_name")
stopifnot(!any(is.na(hand$evidence)))

out <- bind_rows(simple %>% filter(!taxon_name %in% hand$taxon_name), hand) %>%
  transmute(taxon_name, trait_name, value, commonness_qualifier = NA_character_,
            trait_scoring_method = NA_character_, evidence, scoring_certainty = NA_character_) %>%
  arrange(taxon_name, trait_name)
write_csv(out, "data/Brophy_2013/raw/ecology_vetted.csv", na = "")
cat(nrow(out), "rows,", n_distinct(out$taxon_name), "taxa\n"); print(count(out, trait_name))
# yield statements not read (to check: the unreliable ones, the trace amount, and those with no yield)
left <- paras %>% filter(!taxon %in% out$taxon_name)
print(left %>% select(taxon, txt), width = 200)
