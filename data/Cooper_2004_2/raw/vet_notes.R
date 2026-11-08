# Cooper_2004_2: the Notes of Cooper & Cooper (2004) name the animals that eat each taxon's fruit ("fruit eaten by Cassowaries, lorikeets and
# Spectacled Flying-foxes"), seeds or arils. This script turns them into raw/ecology_vetted.csv rows, which the parser
# (data/ABRS_AtoM_2026/raw/build_data_csv.R, FOA_SOURCE=cooper) reads:
#   - fruit, flesh or aril eaten by X (alone or with the seeds: "seeds and aril eaten by") -> `dispersers` (user 2026-11-08, as in Cooper_2013)
#   - seeds alone eaten by X -> candidate `seed_predators` (seed predation, not dispersal)
# Animals are mapped to the dispersers levels in text order: Cassowaries -> cassowaries; flying-foxes -> flying_foxes; bats; rats, melomys,
# mice -> rodents; rat-kangaroos, pademelons, wallabies, tree-kangaroos, possums, cuscus, bandicoots, pigs -> mammals; fish; any bird ->
# birds. The evidence column keeps the source clause, so the animal names (the species) stay with the value.
# Run from the repository root: Rscript data/Cooper_2004_2/raw/vet_notes.R

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr); library(readr); library(tibble) })

prof <- read_csv("~/GitHub/austraits.build-data.scraping.scripts/data_extra/cooper2004_profiles.csv", show_col_types = FALSE, col_types = cols(.default = "c"))

animal_dict <- c(
  "cassowar" = "cassowaries",
  "flying[- ]?fox" = "flying_foxes",
  "\\bbats?\\b|blossom-bats?" = "bats",
  "(?<!-)\\brats?\\b(?!-)|melomys|\\bmice\\b|\\brodents?\\b" = "rodents",
  "rat-?kangaroos?|pademelons?|wallab|kangaroos?|possums?|ringtails?|cuscus|bandicoots?|\\bpigs?\\b|\\bmammals?\\b|cattle|deer" = "mammals",
  "\\bfish\\b" = "fish",
  "\\bants?\\b" = "ants")
bird_re <- paste0("\\bbirds?\\b|parrots?|pigeons?|doves?\\b|bower-?birds?|honey-?eaters?|figbirds?|starlings?|catbirds?|rosellas?|cockatoos?|",
         "rifle-?birds?|cuckoo-?shrikes?|lorikeets?|silver-?eyes?|mistletoebirds?|currawongs?|orioles?|koels?|trillers?|butcherbirds?|",
         "manucodes?|brush-?turkeys?|turkeys?|shrike-?tits?|\\bemus?\\b|friarbirds?|bulbuls?|mynas?|crows?|ravens?|cuckoos?|scrubfowl|pittas?|",
         "robins?|whipbirds?|kingfishers?|wompoo|bowerbirds?|fruit-?doves?|thrush\\w*|magpies?|jays?|drongos?|monarchs?|fantails?")
animal_dict <- c(animal_dict, setNames("birds", bird_re))

map_animals <- function(x) {
  parts <- str_squish(str_split(x, ",|\\band\\b|&|\\bor\\b")[[1]])
  parts <- parts[parts != ""]
  vals <- map_chr(parts, function(p) {
    hit <- names(animal_dict)[map_lgl(names(animal_dict), ~ str_detect(p, regex(.x, ignore_case = TRUE)))]
    if (length(hit)) animal_dict[[hit[1]]] else NA_character_
  })
  # (a shared head noun: "Regent and Satin Bowerbirds", "Lewin's and Macleay's Honeyeaters" -> the next part's group)
  for (k in rev(seq_along(vals))) if (is.na(vals[k]) && k < length(vals) && str_detect(parts[k], "^[A-Z][A-Za-z'-]*(?: [A-Z][A-Za-z'-]*)?$")) vals[k] <- vals[k + 1]
  list(values = unique(na.omit(vals)), unmapped = parts[is.na(vals)])
}

rows <- list(); unmapped <- character(0)
for (i in seq_len(nrow(prof))) {
  nt <- coalesce(prof$notes[i], "")
  if (nt == "") next
  # clauses: "fruit eaten by A and B; seeds eaten by C."
  cl <- str_squish(str_split(nt, "(?<=[a-z)])[.;]\\s+|[.;]$")[[1]])
  for (c1 in cl[cl != ""]) {
    m <- str_match(c1, regex("^(?:the )?((?:fruit(?: flesh)?|flesh|arils?|seeds?)(?:\\s+(?:and|or)\\s+(?:arils?|flesh|funicles?|seeds?))?)\\s+(?:are |is |being )?eaten by (.+)$", ignore_case = TRUE))
    if (is.na(m[1])) next
    organ <- str_to_lower(m[2])
    trait <- if (str_detect(organ, "^seeds?$")) "seed_predators" else "dispersers"
    a <- map_animals(m[3])
    if (length(a$unmapped)) unmapped <- c(unmapped, paste0(prof$taxon_name[i], ": ", a$unmapped))
    if (!length(a$values)) next
    rows[[length(rows) + 1]] <- tibble(taxon_name = str_replace(str_squish(prof$taxon_name[i]), " forma ", " f. "), trait_name = trait, value = paste(a$values, collapse = " "),
                                       commonness_qualifier = NA_character_, trait_scoring_method = NA_character_, evidence = paste0(c1, "."),
                                       scoring_certainty = NA_character_)
  }
}
out <- bind_rows(rows) %>%
  # (two clauses for one trait: "fruit eaten by ...; aril eaten by ..." -> one row, values in text order)
  group_by(taxon_name, trait_name) %>%
  summarise(value = paste(unique(unlist(str_split(value, " "))), collapse = " "), commonness_qualifier = NA_character_,
            trait_scoring_method = NA_character_, evidence = paste(evidence, collapse = " "), scoring_certainty = NA_character_, .groups = "drop")
write_csv(out, "data/Cooper_2004_2/raw/ecology_vetted.csv", na = "")
cat(nrow(out), "rows,", n_distinct(out$taxon_name), "taxa\n")
print(count(out, trait_name))
if (length(unmapped)) { cat("animals not mapped (kept in the evidence only):\n"); print(sort(table(str_remove(unmapped, "^[^:]*: ")), decreasing = TRUE)) }
