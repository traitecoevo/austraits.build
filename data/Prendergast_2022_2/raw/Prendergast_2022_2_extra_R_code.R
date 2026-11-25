library(tidyverse)

raw_path <- "data/Prendergast_2022_2/raw/raw_format_as_single_csv.csv"

raw <- read_csv(raw_path, col_names = FALSE, skip = 1, col_types = cols(.default = "c"))

n_flower_cols <- ncol(raw) - 5
names(raw) <- c("date", "habitat", "site", "bee_taxon", "total_bee_n",
                 paste0("flower_", seq_len(n_flower_cols)))

raw <- raw %>%
  mutate(across(everything(), ~ na_if(str_squish(replace_na(., "")), "")))

# flag the block-header rows (the only rows where habitat/site are populated
# in the original sheet) BEFORE filling down, then assign a block id
raw <- raw %>%
  mutate(is_header = !is.na(habitat),
         block_id = cumsum(replace_na(is_header, FALSE)))

# fix obvious source typos so fill-down/grouping isn't split by them
raw <- raw %>%
  mutate(habitat = recode(habitat, "Bushalnd remnant" = "Bushland remnant"))

raw <- raw %>%
  fill(habitat, site, .direction = "down")

# fix site-name typos and a mislabelled habitat (Bibra Lake, Nov 2017, was
# entered under "Bushland remnant" in the source spreadsheet; it is a
# residential garden site everywhere else and in raw/GPS coords (1).xlsx)
raw <- raw %>%
  mutate(site = recode(site, "Shenton" = "Shenton Park", "Osbourne Park" = "Osborne Park"),
         habitat = if_else(site == "Bibra Lake", "Residential garden", habitat))

# per-block lookup: flower column position -> flower species name,
# taken from that block's header row (varies in length block to block)
flower_lookup <- raw %>%
  filter(is_header) %>%
  select(block_id, starts_with("flower_")) %>%
  pivot_longer(-block_id, names_to = "flower_col", values_to = "flower_species") %>%
  filter(!is.na(flower_species))

bee_rows <- raw %>%
  filter(!is_header, !is.na(bee_taxon)) %>%
  mutate(bee_taxon = str_squish(bee_taxon))

long <- bee_rows %>%
  pivot_longer(starts_with("flower_"), names_to = "flower_col", values_to = "count") %>%
  filter(!is.na(count)) %>%
  inner_join(flower_lookup, by = c("block_id", "flower_col")) %>%
  mutate(count = as.integer(count)) %>%
  select(date, habitat, site, bee_taxon, total_bee_n = total_bee_n, flower_species, count) %>%
  mutate(total_bee_n = as.integer(total_bee_n))

# a handful of block header rows list the same flower species name in more than
# one column (e.g. two separate patches of the same species tallied under
# separate count columns within a single survey) -- sum those into one row per
# date/site/bee_taxon/flower_species so the real repeat is merged here, in the
# data, rather than left as a same-key duplicate for the metadata layer to
# paper over
long <- long %>%
  group_by(date, habitat, site, bee_taxon, total_bee_n, flower_species) %>%
  summarise(count = sum(count), .groups = "drop")

# basis_of_record: bushland remnants were surveyed in situ; residential garden
# plants are deliberately planted/tended, i.e. captive_cultivated
long <- long %>%
  mutate(basis_of_record = if_else(habitat == "Bushland remnant", "field", "captive_cultivated"))

# cultivar vs wild: named cultivars (quote-tagged, or an unquoted marketing name
# trailing a native binomial), food/crop plants, and genera that are purely
# exotic garden ornamentals (never Australian native/naturalised) are "cultivar";
# Australian native/naturalised flora (including naturalised weeds, which are
# not deliberately planted) is "wild"
has_quote_tag <- function(x) {
  grepl("'[^']+'", x) | grepl("‘[^’]+’", x) | grepl('"[^"]+"', x)
}

cultivar_overrides <- c(
  "Grevillea Peaches and Cream", "Grevillea pink surprise", "Grevillea Pink Surprise",
  "Grevillea Yellow var.", "Grevillia moonlight",
  "Melaleuca braceata (Eastern States) Revolution var", "Melaleuca braceata (Revolution)",
  "Sida fallax Butterkin", "Royal Pelargonium hortium", "Pandorea jasminoides Charisma"
)

food_regex <- paste(c(
  "tomat", "onion", "\\bcepa\\b", "broccoli", "\\bkale\\b", "canola",
  "\\brocket\\b", "eruca", "broad bean", "vicia faba", "pisum sativum", "\\bpea\\b",
  "rhubarb", "parsley", "strawberry", "\\bfragaria\\b", "\\blemon\\b", "\\blime\\b",
  "tangerine", "pomegranate", "\\bolive\\b", "olea europaea", "\\bbasil\\b", "ocimum",
  "rosemary", "rosenary", "rosmarinus", "fennel", "foeniculum", "coriand",
  "vitis vinifera", "\\bapple\\b", "malus pumila", "macadamia", "\\bchili\\b",
  "\\bsunflower\\b", "solanum lycopersicum", "allium", "\\balium\\b", "brassica"
), collapse = "|")

weed_terms <- c(
  "Arctotheca calendula", "Carpobrotus edulis", "Carpobrutus edulis",
  "Euphorbia terracina", "Galenia secunda", "Lotus subbiflorus", "Medicago polymorpha",
  "Medigaco polymorpha", "Oxalis pes-caprae", "Raphanus raphanistrum", "Solanum nigrum",
  "Taraxicum officinale", "Taraxacum officinale", "^Tamarix", "Verbesina encelioides",
  "Linaria vulgaris", "^Toadflax", "Allium triquetrum", "^Capeweed", "^Artotheca",
  "^Taraxicum"
)

exotic_genus <- c(
  "Abelia", "Agapanthus", "Agaphanthus", "Apapanthus", "Antigonon", "Antirrhinum",
  "Argyranthemum", "Aptenia", "Bauhinia", "Buddleja", "Buddlegia", "Buddlejia",
  "Caesalpinia", "Calibrachoa", "Centaurea", "Centranthus", "Coleonema", "Crassula",
  "Cuphea", "Cyphea", "Cymbalaria", "Dahlia", "Duranta", "Echeveria", "Erigeron",
  "Euryops", "Gaura", "Gazania", "Geranium", "Pelargonium", "Jacaranda", "Kalanchoe",
  "Lagerstroemia", "Lantana", "Lavandula", "Lavendula", "Leucospermum", "Limonium",
  "Liriope", "Lirope", "Lobelia", "Lobularia", "Lonicera", "^Honeysuckle", "Magnolia",
  "Matthiola", "Metrosideros", "Nasturtium", "Nasturtian", "Nemesia", "Nereium",
  "Oenothera", "Papaver", "Plumeria", "^Frangipani", "Polemonium", "Polygala",
  "Portulaca", "Rhaphiolepis", "Rhododendron", "^Rosa$", "^Rosa ", "^Rose$",
  "Sambus", "Sambucus", "Schinus", "^Sedum", "Tagetes", "^Marigold", "Tanacetum",
  "Thymus", "Trachelospermum", "Triadica", "Viburnum", "Vinca", "Wahlenbergia capensis",
  "Wisteria", "Tropaeolum", "^Ivy geranium", "^geraniums", "^Zonal perlargonijm",
  "^Regal pelargonium", "^Royal Pelargonium", "^Canna", "Ornamental Pear", "^Echeveria",
  "Borago officinalis", "^Coral vine", "^Crepe myrtle", "Euphorbia cyathophora",
  "Euphorbia millii", "Exhium candicans", "Echium candicans", "Jacobaea maritima",
  "Jacobi maritima", "Laverta maritima", "^Salvia", "Sida fallax", "Tipuanu tipu",
  "Tipuana tipu", "^Valerian", "Citrus latifolia", "Senecio cineraria", "^Sida$"
)

native_genus <- c(
  "Acacia", "Agonis", "Alyogyne", "Anigozanthos", "Anigozanthus", "Arnocrinum", "Arnocrium",
  "Astartea", "Astroloma", "^Bankisa", "Banksia", "Baeckea", "Baekia", "Brachychiton", "Calothammus",
  "Calothamnus", "Callistemon", "Calytrix", "Carpobrotus glaucescens", "Ceratopetalum", "Chamelaucium",
  "Chrysocephalum", "Compholobium", "Gompholobi", "Conostylis", "Corynotheca",
  "Corymbia", "Dampiera", "Dasypogon", "Daveisia", "Daviesia", "Dianella", "Eremaea",
  "Eucalyptus", "Euchilopsis", "Eutaxia", "^Geralton Wax", "Gevillea", "Grevillea", "Grevillia", "Hakea",
  "Hardenbergia", "Hemiandra", "Hibbertia", "Hibiscus tiliaceus", "Hypocalymma",
  "Isopogon", "^J\\. furcellata", "^J\\. sericea", "Jacksonia", "^Jarrah", "Kunzea",
  "Lechenaultia", "Leptospermum", "Leucophyta", "Leucopogon", "^ucopogon",
  "Lophostemon", "Marianthus", "^Marri", "Melaleuca", "Nutsyia", "Nuytsia",
  "Pandorea", "Patersonia", "Petrophile", "Pimelea", "Pithocarpa", "Poaceae",
  "^Riceflower", "Ozothamnus", "Regalia", "Regelia", "Rhagodia", "Ricinocarpos",
  "Ricinocarpus", "Scaevola", "Scholtzia", "Senecio$", "Syzigium",
  "Syzygium", "Tetragonia", "^Botany Bay spinach", "Thryptomene", "Thysannotus",
  "Thysanotus", "Tricoryne", "Tricoyne", "Tricyoryne", "Vitex trifolia",
  "Vitrix trifolia", "Westeringia", "Westringia", "Xanthorrea", "Xanthorrhoea",
  "^Seaside daisy", "^Margurite daisy"
)

classify_cultivar <- function(x) {
  if (x %in% cultivar_overrides) return("cultivar")
  if (has_quote_tag(x)) return("cultivar")
  if (grepl(food_regex, x, ignore.case = TRUE)) return("cultivar")
  if (any(sapply(weed_terms, function(p) grepl(p, x, ignore.case = TRUE)))) return("wild")
  if (any(sapply(exotic_genus, function(p) grepl(p, x, ignore.case = TRUE)))) return("cultivar")
  if (any(sapply(native_genus, function(p) grepl(p, x, ignore.case = TRUE)))) return("wild")
  NA_character_
}

long <- long %>%
  mutate(cultivar_status = map_chr(flower_species, classify_cultivar))

cat("Total rows:", nrow(long), "\n")
cat("Blocks (date x habitat x site):", n_distinct(raw$block_id[raw$is_header]), "\n")
cat("Any NA count left:", sum(is.na(long$count)), "\n")
cat("Distinct habitats:", paste(unique(long$habitat), collapse = ", "), "\n")

# sanity check: total_bee_n should equal sum of per-flower counts for each bee row
check <- long %>%
  group_by(date, habitat, site, bee_taxon, total_bee_n) %>%
  summarise(summed = sum(count), .groups = "drop") %>%
  filter(summed != total_bee_n)
cat("Mismatched total_bee_n vs summed counts:", nrow(check), "\n")
if (nrow(check) > 0) print(check)

write_csv(long, "data/Prendergast_2022_2/data.csv")
