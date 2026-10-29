# Builds the data.csv files of the ABRS_*_2026 datasets (see `own_dataset` below) from the Flora of Australia (ABRS)
# online profiles scraped to
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
# data.csv grew past GitHub's file-size limit as one dataset, so the extraction is written to several datasets by
# family: Asteraceae, Fabaceae, Myrtaceae and Proteaceae each have their own, and the other families are split
# alphabetically (A-M, N-Z). This script and its override / vetted CSVs live in ABRS_AtoM_2026/raw and build all of them.
raw_dir <- "data/ABRS_AtoM_2026/raw"
own_dataset <- c(Asteraceae = "ABRS_Asteraceae_2026", Fabaceae = "ABRS_Fabaceae_2026", Myrtaceae = "ABRS_Myrtaceae_2026",
                 Proteaceae = "ABRS_Proteaceae_2026")
# datasets written in long format (see "long format" below); ABRS_NtoZ_2026 since the second Orchidaceae batch (user's request,
# 2026-10-22); ABRS_Proteaceae_2026 from the start (user, 2026-10-29)
long_ids <- c("ABRS_Fabaceae_2026", "ABRS_NtoZ_2026", "ABRS_Proteaceae_2026")
dataset_of <- function(fam) unname(ifelse(fam %in% names(own_dataset), own_dataset[fam],
                                          ifelse(substr(fam, 1, 1) <= "M", "ABRS_AtoM_2026", "ABRS_NtoZ_2026")))
n_cores <- as.integer(Sys.getenv("FOA_CORES", "10"))
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
                   "Hydrocharitaceae", "Hydroleaceae", "Hydrophyllaceae", "Hymenophyllaceae", "Hypericaceae", "Hypoxidaceae", "Icacinaceae",
                   "Iridaceae", "Isoetaceae", "Juglandaceae", "Juncaceae", "Juncaginaceae", "Lamiaceae",
                   "Lauraceae", "Lecythidaceae", "Lentibulariaceae", "Liliaceae", "Limeaceae", "Linaceae", "Linderniaceae", "Lindsaeaceae",
                   "Loganiaceae", "Lomariopsidaceae", "Loranthaceae", "Luzuriagaceae", "Lycopodiaceae", "Lygodiaceae", "Lythraceae",
                   "Macarthuriaceae", "Malpighiaceae", "Malvaceae", "Marantaceae", "Marattiaceae", "Marsileaceae", "Martyniaceae",
                   "Maundiaceae", "Melastomataceae", "Meliaceae", "Melianthaceae", "Menispermaceae", "Menyanthaceae", "Molluginaceae",
                   "Monimiaceae", "Moraceae", "Moringaceae", "Muntingiaceae", "Musaceae", "Myodocarpaceae", "Myristicaceae", "Myrtaceae",
                   # N-Z (ABRS_NtoZ_2026)
                   "Nelumbonaceae", "Nepenthaceae", "Nitrariaceae", "Nothofagaceae", "Nyctaginaceae", "Nymphaeaceae", "Ochnaceae", "Olacaceae",
                   "Oleaceae", "Oleandraceae", "Onagraceae", "Ophioglossaceae", "Opiliaceae",
                   # (batch of 2026-10-27: Orobanchaceae - Plumbaginaceae, 505 taxa)
                   "Orobanchaceae", "Osmundaceae", "Oxalidaceae", "Pandanaceae", "Papaveraceae", "Paracryphiaceae", "Passifloraceae",
                   "Pedaliaceae", "Pennantiaceae", "Petermanniaceae", "Philydraceae", "Phyllanthaceae", "Phytolaccaceae", "Picrodendraceae",
                   "Pinaceae", "Piperaceae", "Pittosporaceae", "Plantaginaceae", "Plumbaginaceae",
                   # (batch of 2026-10-28: Podocarpaceae - Restionaceae, without Proteaceae (own dataset), 546 taxa)
                   "Podocarpaceae", "Podostemaceae", "Polygalaceae", "Polygonaceae", "Polypodiaceae", "Pontederiaceae", "Posidoniaceae",
                   "Potamogetonaceae", "Primulaceae", "Psilotaceae", "Pteridaceae", "Putranjivaceae", "Ranunculaceae", "Resedaceae",
                   "Restionaceae",
                   # (batch of 2026-10-28: Rhamnaceae - Rutaceae, 912 taxa)
                   "Rhamnaceae", "Rhizophoraceae", "Ripogonaceae", "Rubiaceae", "Ruppiaceae", "Rutaceae",
                   # (batch of 2026-10-28: Salicaceae - Solanaceae, 675 taxa)
                   "Salicaceae", "Salviniaceae", "Santalaceae", "Sapindaceae", "Sapotaceae", "Sarraceniaceae", "Schizaeaceae",
                   "Scrophulariaceae", "Selaginellaceae", "Simaroubaceae", "Smilacaceae", "Solanaceae",
                   # (batch of 2026-10-28: Sparganiaceae - Zygophyllaceae, c. 600 described taxa; Stylidiaceae, Surianaceae and
                   # most Symplocaceae profiles are names only)
                   "Sparganiaceae", "Sphenocleaceae", "Stemonaceae", "Stylidiaceae", "Surianaceae", "Symplocaceae", "Taccaceae",
                   "Tamaricaceae", "Tecophilaeaceae", "Tectariaceae", "Theaceae", "Thelypteridaceae", "Thymelaeaceae", "Trimeniaceae",
                   "Triuridaceae", "Tropaeolaceae", "Typhaceae", "Ulmaceae", "Urticaceae", "Verbenaceae", "Viburnaceae", "Violaceae",
                   "Vitaceae", "Winteraceae", "Woodsiaceae", "Xanthorrhoeaceae", "Zamiaceae", "Zingiberaceae", "Zosteraceae",
                   "Zygophyllaceae",
                   # Fabaceae (ABRS_Fabaceae_2026, long format), checked in batches of 500 taxa: see partial_n below
                   "Fabaceae")
# very large families checked in batches of taxa (in name order): only the first N taxa of these families are written
partial_n <- c(Fabaceae = 1500)
# very large families are extracted in parts, genus by genus: the genera checked so far of families not yet complete
# (env FOA_GENERA="A,B" adds genera to a FOA_FAMILIES chunk run)
genera_done <- c(
  # Orchidaceae (1,660 taxa, in parts)
  "Acianthus", "Acriopsis", "Adenochilus", "Anoectochilus", "Aphyllorchis", "Apostasia", "Appendicula", "Arthrochilus", "Arundina",
  "Blepharoglossum", "Bogoria", "Brachypeza", "Bromheadia", "Bryobium", "Bulbophyllum", "Burnettia", "Cadetia",
  "Caladenia", "Calanthe", "Caleana", "Calochilus", "Cheirostylis", "Chiloglottis", "Chiloschista", "Coelogyne", "Cooktownia",
  # (batch of 2026-10-27: Corunastylis - Pecteilis, 507 taxa)
  "Corunastylis", "Corybas", "Corymborkis", "Cryptostylis", "Cyanicula", "Cymbidium", "Cyrtostylis", "Danhatchia", "Demorchis",
  "Dendrobium", "Didymoplexis", "Dienia", "Diplocaulobium", "Dipodium", "Disa", "Diuris", "Dockrillia", "Drakaea", "Drymoanthus",
  "Elythranthera", "Epiblema", "Epidendrum", "Epipogium", "Ericksonella", "Eriochilus", "Erythrorchis", "Eucosia", "Eulophia",
  "Flickingeria", "Gastrodia", "Genoplesium", "Geodorum", "Glossodia", "Grastidium", "Habenaria", "Hetaeria", "Leporella",
  "Leptoceras", "Liparis", "Luisia", "Lyperanthus", "Micropera", "Microtatorchis", "Microtis", "Mobilabium", "Nervilia", "Oberonia",
  "Octarrhena", "Oeceoclades", "Orthoceras", "Pachystoma", "Pecteilis",
  # (batch of 2026-10-27, second session: Peristeranthus - Pterostylis, 446 taxa)
  "Peristeranthus", "Peristylus", "Phaius", "Phalaenopsis", "Pheladenia", "Phoringopsis", "Phreatia", "Pinalia", "Plectorrhiza",
  "Plexaure", "Pomatocalpa", "Praecoxanthus", "Prasophyllum", "Pseudovanilla", "Pterostylis",
  # (batch of 2026-10-27, third: Pyrorchis - Zeuxine and the nothogenera x Cyanthera, x Phelodia, 196 taxa; Orchidaceae complete)
  "Pyrorchis", "Rhinerrhiza", "Rhipidorchis", "Rhizanthella", "Rhomboda", "Rimacola", "Robiquetia", "Saccolabiopsis", "Salacistis",
  "Sarcanthopsis", "Sarcochilus", "Schoenorchis", "Serapias", "Spathoglottis", "Spiculaea", "Spiranthes", "Stigmatodactylus", "Taeniophyllum",
  "Tainia", "Thelasis", "Thelymitra", "Thrixspermum", "Thynninorchis", "Townsonia", "Trachoma", "Trichoglottis", "Tropidia", "Vanda",
  "Vanilla", "Vrydagzynea", "Zeuxine", "x Cyanthera", "x Phelodia",
  # Poaceae (1,066 taxa, in parts; batch of 2026-10-28: Acrachne - Eragrostis, 60 genera)
  "Acrachne", "Agrostis", "Aira", "Alexfloydia", "Alopecurus", "Amelichloa", "Amphibromus", "Amphipogon", "Anisopogon", "Anthosachne",
  "Anthoxanthum", "Apera", "Aristida", "Arrhenatherum", "Arundo", "Astrebla", "Australopyrum", "Austrochloris", "Austroderia",
  "Austrostipa", "Avellinia", "Avena", "Bambusa", "Brachypodium", "Briza", "Bromus", "Calamagrostis", "Capillipedium", "Catapodium",
  "Cenchrus", "Chionachne", "Chionochloa", "Chloris", "Cortaderia", "Corynephorus", "Crypsis", "Cymbopogon", "Cynodon", "Cynosurus",
  "Dactylis", "Dactyloctenium", "Danthonia", "Deschampsia", "Deyeuxia", "Dichelachne", "Digitaria", "Dimeria", "Dinebra", "Diplachne",
  "Distichlis", "Dryopoa", "Echinopogon", "Ehrharta", "Eleusine", "Elytrigia", "Elytrophorus", "Enneapogon", "Enteropogon",
  "Eragrostiella", "Eragrostis",
  # (batch of 2026-10-28, second: Eriachne - Zoysia and x Cynochloris, 93 genera; Poaceae complete)
  "Eriachne", "Eulalia", "Eustachys", "Festuca", "Gastridium", "Gaudinia", "Germainia", "Glyceria", "Hackelochloa", "Hainardia",
  "Heteropogon", "Hierochloe", "Holcus", "Hookerochloa", "Hordeum", "Hygrochloa", "Ichnanthus", "Ischaemum", "Jarava", "Koeleria",
  "Lachnagrostis", "Lagurus", "Lamarckia", "Leersia", "Leptaspis", "Leptochloa", "Lepturus", "Leymus", "Lolium", "Melica", "Micraira",
  "Microchloa", "Microlaena", "Mnesithea", "Molineriella", "Monachather", "Mullerochloa", "Nardus", "Nassella", "Neololeba", "Notochloe",
  "Oplismenus", "Oryza", "Ottochloa", "Oxychloris", "Parapholis", "Pentameris", "Pentapogon", "Perotis", "Phalaris", "Pheidochloa",
  "Phleum", "Phragmites", "Phyllostachys", "Piptatherum", "Piptochaetium", "Plinthanthesis", "Poa", "Polypogon", "Potamophila",
  "Pseudopogonatherum", "Psilurus", "Puccinellia", "Rostraria", "Rottboellia", "Rytidosperma", "Saxipoa", "Schismus", "Schizachyrium",
  "Sclerochloa", "Scrotochloa", "Secale", "Sehima", "Spartina", "Sphenopus", "Spinifex", "Sporobolus", "Sylvipoa", "Taeniatherum",
  "Tetrarrhena", "Thinopyrum", "Tragus", "Tribolium", "Triodia", "Tripogon", "Triraphis", "Trisetum", "Triticum", "Vulpia", "Yakirra",
  "Zea", "Zoysia", "x Cynochloris",
  # Proteaceae (ABRS_Proteaceae_2026, long format; user 2026-10-29), batch 1: the genera before Grevillea (412 taxa)
  "Acidonia", "Adenanthos", "Agastachys", "Alloxylon", "Athertonia", "Austromuellera", "Banksia", "Bellendena", "Buckinghamia",
  "Cardwellia", "Carnarvonia", "Catalepidia", "Cenarrhenes", "Conospermum", "Darlingia", "Eidothea", "Floydia", "Franklandia", "Gevuina",
  # Proteaceae batches 2 and 3 (Grevillea; Hakea - Xylomelum): Proteaceae complete
  "Grevillea", "Hakea", "Helicia", "Hicksbeachia", "Hollandaea", "Isopogon", "Lambertia", "Lomatia", "Macadamia", "Megahertzia", "Musgravea", "Neorites", "Opisthiolepis", "Orites", "Persoonia", "Petrophile", "Placospermum", "Sphalmium", "Stenocarpus", "Stirlingia", "Strangea", "Symphionema", "Synaphea", "Telopea", "Triunia", "Xylomelum")
fams <- if (Sys.getenv("FOA_FAMILIES") != "") str_split(Sys.getenv("FOA_FAMILIES"), ",")[[1]] else families_done
gens <- if (Sys.getenv("FOA_FAMILIES") != "") str_split(Sys.getenv("FOA_GENERA"), ",")[[1]] %>% .[. != ""] else genera_done

d_all <- read_csv(src, show_col_types = FALSE, col_types = cols(.default = "c"), guess_max = 1e5)
# genus key: the first word of the name, or the first two for nothogenera ("x Cyanthera glossodioides" -> "x Cyanthera"), so that
# "x" does not pull together hybrids of different families (x Abacopterella, x Cynochloris)
gkey <- function(x) ifelse(word(x, 1) == "x", word(x, 1, 2), word(x, 1))
# ~230 scraped profiles lack rank and family: rank from the name, family from the genus (other profiles, else taxon_list.csv)
gen_fam <- d_all %>% filter(!is.na(family)) %>% transmute(genus = gkey(scientific_name), family) %>% distinct(genus, .keep_all = TRUE)
tl_fam <- read_csv("config/taxon_list.csv", show_col_types = FALSE, col_types = cols(.default = "c")) %>% distinct(genus, family) %>% filter(!is.na(genus)) %>% distinct(genus, .keep_all = TRUE)
d_all <- d_all %>%
  mutate(rank = case_when(!is.na(rank) ~ rank, str_detect(scientific_name, " subsp\\. ") ~ "subspecies", str_detect(scientific_name, " var\\. ") ~ "variety",
                          str_detect(scientific_name, " f\\. ") ~ "form", str_detect(scientific_name, "^[A-Z][a-z-]+ (?:[a-z-]+|sp\\. .+)$") ~ "species", TRUE ~ NA_character_),
         genus = gkey(scientific_name)) %>%
  left_join(gen_fam %>% rename(fam_g = family), by = "genus") %>% left_join(tl_fam %>% rename(fam_t = family), by = "genus") %>%
  mutate(family = coalesce(family, fam_g, fam_t)) %>% select(-genus, -fam_g, -fam_t)
# env FOA_DATASETS="ABRS_NtoZ_2026" (full runs only): extract and write only the families / genera of these datasets, for a
# faster build cycle (the other datasets' data.csv files are left as they are)
if (Sys.getenv("FOA_DATASETS") != "" && Sys.getenv("FOA_FAMILIES") == "") {
  only_ids <- str_split(Sys.getenv("FOA_DATASETS"), ",")[[1]]
  fams <- fams[dataset_of(fams) %in% only_ids]
  gens <- gens[dataset_of(coalesce(gen_fam$family[match(gens, gen_fam$genus)], "")) %in% only_ids]
}
# genus and family profiles: used only for copy-down of categorical traits that hold throughout the group
gens_fam <- unique(d_all$family[gkey(d_all$scientific_name) %in% gens])
d_higher <- d_all %>% filter(family %in% fams | (rank == "family" & family %in% gens_fam) | (rank == "genus" & scientific_name %in% gens),
                             rank %in% c("genus", "family"), !is.na(Description), !str_detect(Description, "^\\s*Pending\\b"))
d <- d_all %>%
  filter(family %in% fams | gkey(scientific_name) %in% gens, rank %in% c("species", "subspecies", "variety", "form")) %>%
  # (informal infrageneric groups filed as species: Grevillea "Acacioides Group", "Banksii Subgroup")
  # (not "Iris Louisiana Hybrid Group", a naturalised hybrid swarm)
  filter(!str_detect(scientific_name, "^[A-Z][a-z]+(?:-[A-Z][a-z]+)? (?:Group|Subgroup)$")) %>%
  mutate(Description = ifelse(str_detect(coalesce(Description, ""), "^\\s*Pending\\b"), NA, Description)) %>%
  filter(!is.na(Description) | !is.na(Phenology)) %>%
  arrange(family, scientific_name)
# orchid genera whose description opens by calling them terrestrial orchids (not epiphytic / lithophytic): rock words in their
# species' Habitat text are not read as lithophytic (see the orchid substrate block)
# (a "sens. lat." genus text opens with "Pterostylis sens. lat., as recognised by the Australian Plant Census [...], includes the following
# descriptions from Jones (2021): Bunochilus D.L.Jones & M.A.Clem.: Terrestrial, tuberous orchids ...": its first segregate's opening is read)
orch_terr_genera <- d_higher %>% filter(family == "Orchidaceae", rank == "genus") %>%
  mutate(s1 = str_extract(str_remove(Description, "^.*?includes the following descriptions from [^:]*:\\s*[A-Z][a-z]+(?: [^:]{1,40})?:\\s*"), "^[^.]*")) %>%
  filter(str_detect(s1, regex("terrestrial", ignore_case = TRUE)), !str_detect(s1, regex("epiphyt|lithophyt", ignore_case = TRUE))) %>%
  pull(scientific_name)
# (in FOA_FAMILIES chunk runs the whole family is read, for QA of the next batch)
if (Sys.getenv("FOA_FAMILIES") == "") d <- d %>% group_by(family) %>% filter(!(family %in% names(partial_n)) | row_number() <= partial_n[family]) %>% ungroup()

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
  # shrubs "0.4-1.8 m high, 0.35-1.5 mm wide" (m intended)
  "Anticoryne ovalifolia", "0.35–1.5 mm wide", "0.35–1.5 m wide",
  "Ericomyrtus serpyllifolia", "0.2–1.5 (–2) mm wide", "0.2–1.5 (–2) m wide",
  # Cardamine corymbosa: fruits "10–30 mm long, 0.5–1 (–2) m wide" (mm intended)
  "Cardamine corymbosa", "0.5–1 (–2) m wide", "0.5–1 (–2) mm wide",
  # Oenothera genus profile: the sunk genus Gaura's description is appended ("Gaura L.; from the Greek gauros (majestic).
  # Annual or perennial herbs ... Fruit indehiscent, woody and nut-like"): marked as a separate part, so its characters are not
  # read as true of all Oenothera
  "Oenothera", "Gaura L.; from the Greek gauros (majestic). ", "Now includes Gaura: ",
  # Acacia camptoclada: "±a symmetrically oblong-oblanceolate" (the ± sign run into "asymmetrically")
  "Acacia camptoclada", "±a symmetrically", "± asymmetrically",
  # Acacia mutabilis: text lost in the scrape between the inflorescence and the pods ("racemes, with axes rhynchophylla) slightly or not at
  # all constricted between seeds ..."): the rest is the pod description
  "Acacia mutabilis", "with axes rhynchophylla) slightly", "with axes [text missing in source]. Pods slightly",
  # Acacia insolita subsp. insolita: "Shrub 0.3–0.6 cm high" (m intended; the species is 0.3–1.2 m high)
  "Acacia insolita subsp. insolita", "Shrub 0.3–0.6 cm high", "Shrub 0.3–0.6 m high",
  # Acacia humifusa: seeds "4.3–6.5 cm long" (mm intended)
  "Acacia humifusa", "4.3–6.5 cm long", "4.3–6.5 mm long",
  # Acacia translucens: "Peduncles (5–) 13–22 cm long, 0.5–0.7 mm diam." on a shrub with 4–5 mm heads (mm intended)
  "Acacia translucens", "Peduncles (5–) 13–22 cm long", "Peduncles (5–) 13–22 mm long",
  # Acacia volubilis: phyllodes "rudimentary (> 1 mm long) to 9 mm long": the bracketed minimum was read as the length
  "Acacia volubilis", "rudimentary (> 1 mm long) to 9 mm long", "rudimentary (> 1 mm) to 9 mm long",
  # Erythrina x sykesii: "wings and keel similar, 1.5–2.8 mm long" beside a 4.5–6.4 cm standard (cm intended)
  "Erythrina x sykesii", "wings and keel similar, 1.5–2.8 mm long", "wings and keel similar, 1.5–2.8 cm long",
  # Caladenia carnea: reversed range "Flowers 18–16 × 20–30 mm"
  "Caladenia carnea", "Flowers 18–16 × 20–30 mm", "Flowers 16–18 × 20–30 mm",
  # Caladenia viridescens: "Labellum 17–22 × 0–14 mm" (a zero width; the intended lower bound is unknown, so the width is dropped)
  "Caladenia viridescens", "Labellum 17–22 × 0–14 mm", "Labellum 17–22 mm long",
  # Caladenia osmera: leaf "60–120 × 5–1 0mm" (5–10 mm)
  "Caladenia osmera", "60–120 × 5–1 0mm", "60–120 × 5–10 mm",
  # Corunastylis nublingii: dorsal sepal "4 × 2 m" (mm intended; the labellum is "4 × 2 mm")
  "Corunastylis nublingii", "4 × 2 m; margins with short hairs", "4 × 2 mm; margins with short hairs",
  # Diuris palustris: labellum "7–1 0mm long" (7–10 mm)
  "Diuris palustris", "Labellum 7–1 0mm long", "Labellum 7–10 mm long",
  # Elythranthera emarginata: "Leaf 40–800 × 5–7 mm" on a plant with 120–250 mm flower stems (40–80 mm assumed; flagged)
  "Elythranthera emarginata", "Leaf 40–800 × 5–7 mm", "Leaf 40–80 × 5–7 mm",
  # Eriochilus latifolius: a "broadly ovate" leaf "40–750 × 5–30 mm" (40–75 mm assumed; flagged)
  "Eriochilus latifolius", "broadly ovate, 40–750 × 5–30 mm", "broadly ovate, 40–75 × 5–30 mm",
  # Pterostylis torquata: "Flower stem 100–20 0mm tall" (100–200 mm)
  "Pterostylis torquata", "Flower stem 100–20 0mm tall", "Flower stem 100–200 mm tall",
  # Thelymitra petrophila: reversed range "Leaf straight, leathery, 100–40 × 4–10 mm" (40–100 assumed)
  "Thelymitra petrophila", "leathery, 100–40 × 4–10 mm", "leathery, 40–100 × 4–10 mm",
  # Plantago albicans: "peduncle 5–l 0 cm long" (5–10 cm); Veronica derwentiana subsp. subglauca: reversed "75–45 mm long" (45–75)
  "Plantago albicans", "peduncle 5–l 0 cm long", "peduncle 5–10 cm long",
  "Veronica derwentiana subsp. subglauca", "acuminate, 75–45 mm long", "acuminate, 45–75 mm long",
  # Veronica ciliolata: "rigid (to loose) cushion 2–6 mm high, with many erect branches and a woody base" (cm assumed; flagged)
  "Veronica ciliolata", "cushion 2–6 mm high", "cushion 2–6 cm high",
  # Austrostipa heights with the unit wrong (user, 2026-10-28, from the 2022 taxon descriptions): A. anaiwaniorum "tussock grass 6–10 m
  # high" (600–1000 mm tall), A. turbinata "2.5–4.8 m high" (250–480 mm tall), A. mundula "Plants to 0.6 cm high" (0.6 m)
  "Austrostipa anaiwaniorum", "tussock grass 6–10 m high", "tussock grass 600–1000 mm high",
  "Austrostipa turbinata", "2.5–4.8 m high", "250–480 mm high",
  "Austrostipa mundula", "Plants to 0.6 cm high", "Plants to 0.6 m high",
  # A. heteranthera "shoots intra- or extra-vaginal; 3–7 mm high" (dm intended: its Diagnostic Features give flowering culms "≤ 600 mm high")
  "Austrostipa heteranthera", "extra-vaginal; 3–7 mm high", "extra-vaginal; 300–700 mm high",
  # Acacia sp. Indiana Station: a tree to 15 m high "with d.b.h. to c. 2.5 m" (2.5 dm most likely, user)
  "Acacia sp. Indiana Station (P.K.Latz 4430)", "d.b.h. to c. 2.5 m", "d.b.h. to c. 2.5 dm",
  # Euphrasia amphisysepala: anther awns "0.1–02 mm long" (0.1–0.2)
  "Euphrasia amphisysepala", "0.1–02 mm long", "0.1–0.2 mm long",
  # Tacca: "'floral bracts' c. 40-100, filiform, mostly 10-15 cm long" (the quotes hid the bracts, read as flower length)
  "Tacca leontopetaloides", "'floral bracts'", "floral bracts",
  "Tacca maculata", "'floral bracts'", "floral bracts",
  # Conospermum: the height in flower is plant_height_reproductive (vetted rows), the vegetative height plant_height
  "Conospermum crassinervium", "to 1.5 m tall in flower, c. 50 cm tall when vegetative", "c. 50 cm tall",
  "Conospermum huegelii", "to 1 m tall when in flower, c. 40 cm tall when vegetative", "c. 40 cm tall",
  "Conospermum scaposum", "to 75 cm tall when in flower, c. 15 cm tall when vegetative", "c. 15 cm tall",
  "Conospermum wycherleyi", "to 1.1 m tall (when flowering), to 50 cm when vegetative", "to 50 cm tall",
  # Banksia elderiana: "Pistil straight, 24–28 cm long" with a perianth 27–29 mm long (mm intended)
  "Banksia elderiana", "Pistil straight, 24–28 cm long", "Pistil straight, 24–28 mm long",
  # Grevillea wilsonii: leaves "2-6 cm long, 40-60 cm wide" (mm intended)
  "Grevillea wilsonii", "40-60 cm wide", "40-60 mm wide"
)
for (i in seq_len(nrow(fix_text))) {
  fx <- function(z) z %>% mutate(Description = ifelse(scientific_name == fix_text$taxon[i],
                                                     str_replace(Description, fixed(fix_text$find[i]), fix_text$replace[i]), Description))
  d <- fx(d); d_higher <- fx(d_higher)
}

# ---------------------------------------------------------------- helpers
fixed_re <- function(x) str_replace_all(x, "([.()\\[\\]{}+*?^$|\\\\])", "\\\\\\1")
# spread of a plant or colony ("forming dense mats", "spreading to 2 m diam.") rather than the thickness of an organ
spread_re <- "\\b(?:forming|forms|spreading|spreads|mats?|tufts?|patches|clumps?|tussocks?|colonies|thickets?|cushions?|clones?)\\b"
# leafless only while flowering (lower case text): Praecoxanthus "flowering plants are leafless", "leafless at flowering"
leafless_flowering_re <- "\\bflowering plants? (?:are |is |remain |usually |often |generally )*leafless\\b|\\bleafless (?:at|when|while|during) (?:the time of )?(?:flowering|anthesis)\\b"
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

prep <- function(x, keep_q = FALSE) {
  if (is.na(x)) return(NA_character_)
  x <- str_replace_all(x, "[\u2013\u2014\u2212\u2011\u2010]", "-")
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_replace_all(x, "\\bFlowersand\\b", "Flowers and")
  x <- str_replace_all(x, "\\bInilorescence", "Inflorescence")
  # PDF glitches: soft hyphens inside numbers ("1\u00ad.5"), a letter l for the digit 1 ("l.4-1.5 mm")
  x <- str_remove_all(x, "\u00ad")
  # middle-dot decimals ("3–4 × 2–3·5 mm")
  x <- str_replace_all(x, "(?<=[0-9])\u00b7(?=[0-9])", ".")
  x <- str_replace_all(x, "(?<![A-Za-z])l\\.(?=[0-9])", "1.")
  x <- str_replace_all(x, "(?<=[0-9])\\s*-\\s*-\\s*(?=[0-9])", "-")
  # ("Flowers usually August–-October": a doubled dash between month names)
  x <- str_replace_all(x, "(?<=[A-Za-z.])\\s*-\\s*-\\s*(?=[A-Z])", "-")
  # thousands separators in measurements ("Flower stem 500-1,000 mm tall", Thelymitra "Flower stem 200–1,000 × 4–9 mm")
  x <- str_replace_all(x, "(?<=\\b[1-9][0-9]?),(?=[0-9]{3}(?![0-9])\\s*(?:(?:mm|cm|m)\\b|\u00d7))", "")
  # square brackets hold other regions' values or references ("[3-5 in India]", "[See Barker (1986)]")
  x <- str_replace_all(x, "\\s*\\[[^\\]]*\\]", "")
  # missing space after a full stop ("forming a lignotuber.Bark usually smooth")
  x <- str_replace_all(x, "(?<=[a-z)])\\.(?=[A-Z][a-z])", ". ")
  x <- str_replace_all(x, "\u00b1\\s*", "")
  # "1.5-2. 5 cm" -> "1.5-2.5 cm"
  x <- str_replace_all(x, "(?<=[0-9])\\. (?=[0-9]+\\s?(?:mm|cm|m)\\b)", ".")
  # uncertain terms ("?Erect, ?tuberous herb", "?November"): the term is dropped; "rarely? white" keeps the qualifier
  # (phenology keeps them: a queried month is scored on a scoring_certainty = possibly row, user 2026-10-29)
  if (!keep_q) {
    x <- str_replace_all(x, "(?<![A-Za-z0-9])\\?[A-Za-z][A-Za-z-]*", "")
    x <- str_replace_all(x, "\\?", "")
  }
  # words run together before a month or in phenology phrases ("FlowersJan.-Mar.", "fruitsSept.", "andfruits", "allmonths")
  x <- str_replace_all(x, "(?<=[a-z])(?=(?:Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)[a-z]*(?=[.,;\\s-]|$))", " ")
  x <- str_replace_all(x, "(?i)\\b(fruits|flowers|and)(?=(?:all|fruits|flowers|sporadically)\\b)", "\\1 ")
  x <- str_replace_all(x, "\\ballmonths\\b", "all months")
  # missing space after a comma ("erect stem,with short lactifers")
  x <- str_replace_all(x, ",(?=[a-z])", ", ")
  # states that do not occur in Australia ("acaulescent (not in Australia)", "spiny (not in Australia)") are dropped
  # "convex or (not in Australia) truncate or conical", "simple (not in Australia) or imparipinnate", "acaulescent (not in Australia),"
  # ("opposite or rarely (not in Australia) whorled": a commonness word may sit before the bracket)
  x <- str_remove_all(x, "\\s*\\bor (?:(?:very rarely|rarely|sometimes|occasionally|often|usually) )?\\(\\s*not in Australia\\s*\\) [a-z-]+(?: (?:or|and) [a-z-]+)*")
  x <- str_remove_all(x, "[^,;:.()]*\\(\\s*not in Australia\\s*\\)\\s+or\\s+")
  x <- str_remove_all(x, "(?:,\\s*|\\bor\\s+)?[^,;:.()]*\\(\\s*not in Australia\\s*\\)")
  x <- str_replace_all(x, ",\\s*(?=[,;])", "")
  str_squish(x)
}
# hedged statements are not scored ("probably white with purple markings", "apparently up to 1 m high")
dehedge <- function(x) {
  if (is.na(x)) return(x)
  x <- str_remove_all(x, regex("\\b(?:probably|possibly|perhaps) (?:dependent|depending) on [a-z ]+", ignore_case = TRUE))
  # (a hedge after its noun covers the noun too: Hakea ochroptera "lignotuber apparently absent" is not "lignotuber". The whole hedged
  # phrase goes, back to its clause start or an "or" / "and" / "but" (Euphrasia bowdeniae "with yellow markings apparently lacking" would
  # otherwise leave "with yellow"; "oil glands obscure or apparently absent" keeps "oil glands obscure", Acronychia "exocarp fleshy but mesocarp apparently lacking" keeps "exocarp fleshy"), but only when it ends its clause:
  # "blade apparently lacking a ligule", "rachilla apparently absent or prolonged" keep the usual cut after the hedge)
  x <- str_replace_all(x, regex("(^|[,;(]\\s*|\\.\\s+|\\b(?:or|and|but|whereas|while)\\s+)(?:(?!\\b(?:or|and|but|whereas|while)\\b)[^,;.()])*?\\b(?:apparently|probably|possibly|presumably|perhaps) (?:absent|present|lacking|wanting)\\b(?=\\s*(?:[,;.)]|$))", ignore_case = TRUE),
                       function(m) str_extract(m, regex("^(?:[,;(]\\s*|\\.\\s+)", ignore_case = TRUE)) %>% coalesce(""))
  x <- str_replace_all(str_replace_all(str_replace_all(x, "[,;]\\s*(?=[,;.])", ""), "\\.\\s+\\.", "."), "\\s+(?=[,;])", "") %>% str_remove("^[,;.]\\s*")
  # ("Annual or possibly ephemeral herb": the hedge covers "ephemeral", not the growth-form noun)
  str_squish(str_remove_all(x, regex("\\b(?:probably|possibly|perhaps|apparently|presumably)\\b(?:[^,;.)]*?(?=\\s+(?:herbs?|shrubs?|subshrubs?|trees?|climbers?|vines?|palms?|grass(?:es)?|sedges?)\\b)|[^,;.)]*)", ignore_case = TRUE)))
}

sentences <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  x <- str_replace_all(x, "\\b([A-Z])\\.(?=\\s)", "\\1\u00a7")
  x <- str_replace_all(x, "\\b(subsp|var|sp|spp|ssp|cv|Mt|St|ca|c|approx|al|cf|vs|pers|comm|ed|eds|e\\.g|i\\.e|fig|figs|Fl|Herb|Austral|Bot|J|Proc|Soc|Mus|incl)\\.(?=\\s)", "\\1\u00a7")
  # (Fabaceae: "heads 8-9 mm diam. (when dry), 30-42-flowered": a bracket with a lower-case remark does not open a sentence)
  # (and a quoted sentence opens one: 'Flowers May-August. "Very fire tolerant ..."')
  s <- str_split(x, if (legume_mode) "(?<=[.])\\s+(?=[\"\u201c]?[A-Z]|\\((?![a-z]))" else "(?<=[.])\\s+(?=[A-Z(])")[[1]]
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
mk_hi <- paste0("(?:\\s*\u27e8(?:hi|q:[a-z ]+):", num, "(?:~[^\u27e9]*)?\u27e9)?")
# ("Plants to c. 0.5 m high": "to" and "c." together, so the value stays a maximum)
meas <- paste0("(?:(?:up to|to|about|approximately|c\\.|ca\\.|less than|more than|under|mostly|usually|commonly|generally|often|frequently|reaching)\\s+)?",
               "(?:(?:c\\.|ca\\.|about|approximately)\\s+)?",
               mk_lo, "(", num, ")", lo_unit, "(?:", dash, "(", num, "))?", mk_hi, "\\s?(mm|cm|dm|m)\\b", mk_hi)
mark_extremes <- function(x) {
  if (is.na(x)) return(x)
  # (an upper extreme in its own unit, Desmocladus "to 40 cm (–1 m) across": the marker carries the value in the main unit and the
  # source words, <hi:100~1 m>)
  x <- str_replace_all(x, paste0("(", num, ")\\s?(mm|cm|dm|m)\\b(\\s*\\(\\s*-\\s*)(", num, ")\\s*(mm|cm|dm|m)\\s*\\)"), function(z) map_chr(z, function(zz) {
    m <- str_match(zz, paste0("(", num, ")\\s?(mm|cm|dm|m)\\b(\\s*\\(\\s*-\\s*)(", num, ")\\s*(mm|cm|dm|m)\\s*\\)"))
    if (m[3] == m[6]) return(zz)
    v <- as.numeric(m[5]) * unname(unit_mult[m[6]]) / unname(unit_mult[m[3]])
    paste0(m[2], " ", m[3], "\u27e8hi:", format(signif(v, 6), scientific = FALSE, drop0trailing = TRUE, trim = TRUE), "~", m[5], " ", m[6], "\u27e9") }))
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
  x <- str_replace_all(x, "\u27e8hi:[0-9.]+~([^\u27e9]+)\u27e9", " (-\\1)")
  x <- str_replace_all(x, "\u27e8hi:([0-9.]+)\u27e9", " (-\\1)")
  str_squish(str_replace_all(x, "\u27e8q:([a-z ]+):([0-9.]+)\u27e9", " (\\1 \\2)"))
}
# extremes in a matched measurement, in mm (unit `u`), placed below / above the typical range
extremes_of <- function(txt, u, typ_min, typ_max, scale_unit = TRUE) {
  f <- function(v) if (scale_unit) to_mm(v, u) else as.numeric(v)
  lo <- str_match(txt, "\u27e8lo:([0-9.]+)\u27e9")[2]; hi <- str_match(txt, "\u27e8hi:([0-9.]+)(?:~[^\u27e9]*)?\u27e9")[2]
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
                  "partially", "distinctly", "obscurely", "conspicuously", "dull", "glossy", "shiny", "much", "well", "variously", "irregularly",
                  "wholly", "entirely", "completely", "totally", "uniformly")
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
  # a rare commonness word carries over a list continuing it: "occasionally brownish, pinkish or reddish" (all occasional);
  # colours only (in "rarely glaucous, smooth or warty", "sometimes 2-lobed, linear" the later terms are other facets)
  is_colour_dict <- any(unname(dict) %in% c("white_cream", "yellow_orange", "red_brown", "pink", "blue_purple", "green", "white", "yellow", "red", "purple", "brown", "orange", "grey"))
  if (is_colour_dict && nrow(h) > 1) for (i in 2:nrow(h)) {
    if (is.na(h$qual[i]) && !is.na(h$qual[i - 1]) && h$qual[i - 1] %in% rare_quals &&
        str_detect(str_sub(tq, h$end[i - 1] + 1, h$pos[i] - 1), "^\\s*(?:,\\s*(?:or\\s+|and\\s+)?|or\\s+|and\\s+|to\\s+)$")) h$qual[i] <- h$qual[i - 1]
  }
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
  "lamina", "^(?:the )?(?:leaf |phyllode |adult |mature |sterile )?(?:blades?|laminae?|laminas)\\b",
  "leaflet", "^(?:the )?(?:lateral |terminal |basal |upper |lower |distal |proximal |primary |secondary |tertiary |ultimate |sterile |fertile |middle |median |central )?(?:leaflets?|pinnules?|pinnae|pinna|foliolules?)\\b",
  "leaf", "^(?:the )?(?:adult |mature |cauline |basal |stem |upper |lower |vegetative |rosette |radical |sterile |fertile |all |emergent |floating |submerged |aerial |floral |lateral )?(?:leaves|leaf|phyllodes?|cladodes?|phylloclades?|fronds?)\\b",
  "stipule", "^(?:the )?(?:stipules?|ochreae?|ochreas|stipular)\\b",
  "stem", "^(?:the )?(?:young |older |ultimate |upper |lower |main |flowering |fertile |sterile |lateral |primary |secondary |aerial |vegetative |mature )?(?:branchlets?|branches|branch|stems?|twigs?|culms?|shoots?|trunks?|canes?|internodes|axes)\\b",
  "underground", "^(?:the )?(?:rhizomes?|rootstocks?|stolons?|roots?|tubers?|bulbs?|corms?|lignotubers?|caudex|caudices|pseudobulbs?|taproots?)\\b",
  "peduncle", "^(?:the )?(?:common )?(?:peduncles?|scapes?)\\b",
  # Araceae spadix zones: not flowers or anthers ("male zone c. 8 cm long", "appendix ... to c. 15 cm long")
  "spadix_part", "^(?:the )?(?:appendix|appendices|(?:female|male|sterile|staminate|pistillate|neuter|fertile) zones?|(?:sterile |naked )?interstices?|neuter organs|staminodes zone)\\b",
  "inflorescence", "^(?:the )?(?:male |female |staminate |pistillate |terminal |axillary |lateral |fruiting )?(?:inflorescences?|racemes?|spikes?|panicles?|heads?|capitul(?:a|um)|umbels?|cymes?|corymbs?|thyrses?|fascicles?|synflorescences?|conflorescences?|spadix|spadices|glomerules?|cymules?|flower[- ]heads?|rachis|rachises)\\b(?! bracts?\\b| bracteoles?\\b)",
  # Chenopodiaceae dispersal units: the fruiting bracteoles (Atriplex) or fruiting perianth (Maireana, Sclerolaena) enclose
  # the fruit and are what the flora measures (user's choice: fruit_* sizes on entity_measured rows)
  "diaspore", "^(?:the )?(?:fruiting|fruit-bearing) (?:bracteoles?|perianths?|perianth-tubes?|articles?|spikes?)\\b",
  # ("inflorescence bracts usually scale-like, 1-3 mm long", Zieria: bracts, not the inflorescence)
  "bract", "^(?:the )?(?:floral |flower |inflorescence |inner |outer |subtending |involucral |lower |upper |basal |lowest |uppermost )?(?:bracts?|bracteoles?|prophylls?|spathes?|involucres?|phyllaries)\\b",
  "pedicel", "^(?:the )?(?:pedicels?|pedicles?)\\b",
  "bud", "^(?:the )?(?:mature |flower )?buds?\\b",
  "flower", "^(?:the )?(?:males|females|staminate ones|pistillate ones)\\b|^(?:the )?(?:male |female |staminate |pistillate |bisexual |hermaphrodite |functionally (?:male|female) |sterile |fertile |ray |disc |marginal |central |outer |inner |lateral |terminal |chasmogamous |cleistogamous |mature |open )?(?:flowers?|florets?|spikelets?)\\b",
  "corona", "^(?:the )?(?:coronas?|paracorolla)\\b",
  "corolla", "^(?:the )?(?:(?:inner and outer|outer and inner|outer|inner|lateral|upper|lower|larger|smaller)(?: [0-9]+| two| three)? )?(?:sepals and petals|petals and sepals|corollas?|petals?|perianth|perianths|tepals?|standard|keel|labellum|dorsal sepal|lateral sepals|operculum|opercula)\\b",
  "calyx", "^(?:the )?(?:calyx|calyces|sepals?|hypanthium|hypanthia|outer perianth whorl)\\b",
  # ("Female flowers: rudimentary stamens ligulate, to 1 mm long", Zanthoxylum: stamens, not the flower)
  "androecium", "^(?:the )?(?:(?:abaxial|adaxial|longer|shorter|outer|inner|upper|lower|fertile|staminal|rudimentary|vestigial|sterile|reduced) )?(?:pair of )?(?:stamens?|filaments?|anthers?|staminodes?|androecium|pollen|staminal (?:column|tube))\\b",
  "gynoecium", "^(?:the )?(?:(?:rudimentary|vestigial|sterile|reduced) )?(?:ovary|ovaries|styles?|stigmas?|carpels?|pistils?|gynoecium|ovules?|placentae?|disc|column|indusi(?:a|um))\\b",
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
orchid_mode <- FALSE
legume_mode <- FALSE
grass_mode <- FALSE
graminoid_rule_families <- c("Poaceae", "Cyperaceae", "Juncaceae", "Restionaceae")
bamboo_genera <- c("Bambusa", "Dendrocalamus", "Mullerochloa", "Neololeba", "Phyllostachys", "Pseudosasa", "Arundinaria", "Thyrsostachys", "Gigantochloa")
grass_qual <- "(?:lower|upper|first|second|fertile|sterile|lowest|basal|terminal|lateral|central|median)"
classify_token <- function(tok) {
  # Fabaceae: new shoots / young foliage tips are their own organ (their hairs and colours are not the stem's); the funicle
  # (often fleshy and aril-like in Acacia) is a seed part, not the seed; "interstices" are the spaces between branchlet ribs
  # (not Araceae spadix interstices)
  if (legume_mode) {
    if (str_detect(tok, rx("^(?:the )?(?:new|young|juvenile) (?:shoots?|growth|foliage(?:[- ]tips?)?|shoot[- ]tips?|tips)\\b"))) return(list(kind = "organ", sec = "stem", part = NA_character_, subj = "new growth"))
    if (str_detect(tok, rx("^(?:the )?(?:funicles?|funicle-arils?)\\b"))) return(list(kind = "organ", sec = "seed", part = NA_character_, subj = "funicle"))
    if (str_detect(tok, rx("^(?:the )?(?:areoles?|pleurograms?)\\b"))) return(list(kind = "part", sec = NA_character_, part = "pleurogram", subj = NA_character_))
    if (str_detect(tok, rx("^(?:the )?interstices\\b"))) return(list(kind = "part", sec = NA_character_, part = "interstices", subj = NA_character_))
  }
  # Orchidaceae (Jones 2021 treatment): the "flower stem" is the inflorescence axis (its length is read separately: plant
  # height when flowering for terrestrial orchids, inflorescence length for epiphytes); the dorsal and lateral sepals are
  # measured separately (entity_measured rows); the callus, column, labellum stalk and sepal clubs (osmophores) are their
  # own organs, so their characters and sizes are not given to the labellum or sepals
  if (orchid_mode) {
    if (str_detect(tok, rx("^(?:the )?(?:flower(?:ing)?|floral) stems?\\b"))) return(list(kind = "organ", sec = "inflorescence", part = NA_character_, subj = "flower stem"))
    m <- str_match(tok, rx("^(?:the )?(dorsal sepals?|lateral sepals?)\\b"))
    if (!is.na(m[1])) return(list(kind = "organ", sec = "calyx", part = NA_character_, subj = str_to_lower(m[2])))
    # (Caladenia "Lamina calli to 2 mm long", "Basal calli 4": the labellum calli, not the leaf lamina; the "basal glands" of the
    # column (Caladenia) or of the labellum callus (Acianthus) are their own organ, so their colour and gloss are not the column's)
    m <- str_match(tok, rx("^(?:the )?(basal|column) glands?\\b"))
    if (!is.na(m[1])) return(list(kind = "organ", sec = "orchid_part", part = NA_character_, subj = paste(str_to_lower(m[2]), "glands")))
    m <- str_match(tok, rx("^(?:the )?(?:(?:lamina|basal|marginal|apical|central|labellum) )?(call(?:us|i)|columns?|labellum (?:stalk|claw|hinge)|(?:sepal(?:ine)? )?clubs?|osmophores?)\\b"))
    if (!is.na(m[1])) return(list(kind = "organ", sec = "orchid_part", part = NA_character_, subj = str_to_lower(m[2])))
  }
  # Casuarinaceae: the woody "cone" is the infructescence (sizes on entity_measured = cone rows, as for chenopod dispersal units);
  # the samara is the fruit
  if (casuarina_mode && str_detect(tok, rx("^(?:the )?(?:mature )?cones?(?: body| bodies)?\\b"))) return(list(kind = "organ", sec = "diaspore", part = NA_character_, subj = "cone"))
  # cycads and conifers: seed / pollen cones are their own organ, not the seed (Macrozamia communis "Seed cones ovoid, 20-45 cm
  # long, 10-20 cm diam."; the seeds follow: "Seeds flattened-ovoid, 30-45 mm long"); their sizes are the seed_cone_* and
  # pollen_cone_* candidate traits
  m <- str_match(tok, rx("^(?:the )?(?:mature |young |immature )?((?:male and female|female and male|seed|pollen|ovulate|male|female|microsporangiate|megasporangiate) (?:cones?|strobil(?:i|us)))\\b"))
  if (!is.na(m[1]) && !casuarina_mode) return(list(kind = "organ", sec = "cone", part = NA_character_, subj = str_to_lower(m[2])))
  # Proteaceae: the pollen presenter (the style end) is its own organ, not the pollen or stamens ("pollen presenter 1 mm long,
  # smooth")
  if (str_detect(tok, rx("^(?:the )?pollen[- ]presenters?\\b"))) return(list(kind = "organ", sec = "gynoecium", part = NA_character_, subj = "pollen presenter"))
  # Chenopodiaceae (Tecticornia, Salicornia): the stem "articles" (in other families articles are fruit segments)
  if (chenopod_mode && str_detect(tok, rx("^(?:the )?(?:vegetative |flowering |fertile |sterile )?articles?\\b"))) return(list(kind = "organ", sec = "stem", part = NA_character_, subj = "articles"))
  if (fern_mode && str_detect(tok, rx("^(?:the )?stipes?\\b"))) return(list(kind = "organ", sec = "petiole", part = NA_character_, subj = "stipe"))
  # Poaceae: the spikelet parts (glumes, lemma, palea, callus, awn and its column, lodicules, rachilla, coma, bristles) and the leaf's
  # sheath, ligule and auricles are their own organs (candidate traits, user 2026-10-28); a qualified glume, lemma or awn ("lower glume",
  # "fertile lemma", "lateral awns") is measured on entity_measured rows (units_of)
  if (grass_mode) {
    m <- str_match(tok, rx("^(?:the )?((?:panicle |rachis |mid-culm |vegetative culm |culm )?(?:internodes?|axes|axis))\\b"))
    if (!is.na(m[1])) return(list(kind = "organ", sec = "stem", part = NA_character_, subj = str_to_lower(m[2])))
    m <- str_match(tok, rx(paste0("^(?:the )?((?:", grass_qual, " )?(?:glumes?|lemmas?|paleas?|paleae|awns?)|call(?:us|i)|columns?|bristles?|lodicules?|",
                                  "rachillas?(?: extensions?)?|coma|(?:basal |leaf |upper |lower )?sheaths?|ligules?|auricles?)\\b")))
    if (!is.na(m[1])) {
      # ("lemma lobes 0-0.2 mm long", "glume apex acute": a part of the organ, not the organ's own size)
      pm <- str_match(str_sub(tok, nchar(m[1]) + 1), rx("^\\s+(lobes?|teeth|apex|apices|tips?|margins?|keels?|bases?|surfaces?|veins?|nerves|hairs|indumentum)\\b"))
      if (!is.na(pm[1])) return(list(kind = "part", sec = "grass_part", part = str_to_lower(pm[2]), subj = str_to_lower(m[2])))
      return(list(kind = "organ", sec = "grass_part", part = NA_character_, subj = str_to_lower(m[2])))
    }
  }
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
        # Fabaceae "Leaves bipinnate ...; petiole ...; rachis 1-12 cm long, with ... jugary glands": a rachis inside a leaf
        # clause is the leaf rachis, not an inflorescence
        if (legume_mode && cc$kind == "organ" && cc$sec == "inflorescence" && str_detect(coalesce(cc$subj, ""), "rachis") &&
            !is.null(cur) && cur$sec %in% c("leaf", "lamina", "petiole", "leaflet")) cc <- list(kind = "part", sec = "leaf", part = "rachis", subj = "leaves")
        # Poaceae: "Panicle dense ...; branches 1-3 together, 1-4 mm long": branches after an inflorescence are its branches, not stems
        # (also "lowest internodes 15-30 mm long", "axes and branches flat" after a panicle; vegetative culm internodes stay with the culm)
        if (grass_mode && cc$kind == "organ" && cc$sec == "stem" && str_detect(coalesce(cc$subj, ""), "branch|internode|^ax|panicle|rachis") &&
            !str_detect(coalesce(cc$subj, ""), "culm") && !is.null(cur) && cur$sec == "inflorescence")
          cc <- list(kind = "part", sec = "inflorescence", part = if (str_detect(cc$subj, "branch")) "branches" else if (str_detect(cc$subj, "internode")) "internodes" else "axis", subj = cur$subj)
        # (Micraira "Culms much-branched; nodes numerous; internodes c. 1 mm long": the culm's internodes)
        if (grass_mode && cc$kind == "organ" && cc$sec == "stem" && str_detect(coalesce(cc$subj, ""), "^(?:[a-z-]+ )?internodes?$") &&
            !is.null(cur) && cur$sec == "stem" && str_detect(coalesce(cur$subj, ""), "culm"))
          cc <- list(kind = "part", sec = "stem", part = "internodes", subj = cur$subj)
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
          # (Fabaceae: the words after a gland or the pulvinus describe it, "gland 1, prominent, basal", "pulvinus distinct, 4-5 mm long, orange")
          persist <- !is.na(cc$sec) || isTRUE(cur$sec == "diaspore") || (legume_mode && ti == 1 && str_detect(part, "^(?:glands?|pulvin(?:us|i)|pleurogram)$")) || !str_detect(part, "^(?:base|bases|apex|apices|tips?|margins?|teeth|midribs?|mid-?veins?|veins?|venation|lateral veins|side-veins|reticulation|glands?|oil glands|gland|nerves|ribs?|pulvinus|spines?|thorns|prickles|axis|axes|texture|sheathing bases?)$")
          # (Fabaceae: "lower margin", "upper surface" ... are token-level too)
          if (legume_mode && str_detect(part, "(?:margins?|surfaces?)$")) persist <- FALSE
          if (persist) cur <- list(kind = "organ", sec = sec, part = part, subj = subj)
        } else {
          if (is.null(cur)) cur <- list(kind = "organ", sec = if (si == habit_si) "habit" else "other", part = NA_character_, subj = NA_character_)
          sec <- cur$sec; part <- cur$part; subj <- cur$subj
        }
        # (Poaceae: "Spikelets bisexual" says nothing about the glumes / lemma that follow; male / female spikelet parts keep their entity)
        ent <- if (grass_mode && identical(sec, "grass_part") && str_detect(coalesce(subj, ""), rx(paste0("^", grass_qual, " ")))) str_remove(subj, "s$") else
          if (grass_mode && identical(sec, "grass_part") && str_detect(coalesce(entity, ""), "^(?:bisexual|hermaphrodite)")) NA_character_ else entity
        rows[[length(rows) + 1]] <- tibble(si = si, ci = ci, ti = ti, sec = sec, part = part, subj = subj, entity = ent, text = tk)
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
  "woody (?:(?:perennial|evergreen|deciduous|slender|robust|large) )?(?:climbers?|vines?|twiners?|scramblers?)|lianas?|lianes?" = "climber_woody",
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
  "plumose|feathery" = "plumose", "curved|arcuate|horseshoe-shaped" = "curved", "straight" = "straight", "filamentous" = "filiform", "lobed" = "lobed", "sagittate" = "sagittate", "hastate" = "hastate")
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
  "appressed|adpressed" = "appressed", "imbricate" = "imbricate", "twisted|contorted" = "twisted")
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
               "elaiosomes?", "claws?", "spurs?", "glands?", "keels?", "sheaths?", "spines?", "awns?", "horns?",
               "endocarps?", "pyrenes?", "stones?")
gen_own <- list(leaf = "leaves|leaf|laminae?|blades?", juvenile_leaf = "leaves|leaf|laminae?|blades?", leaflet = "", petiole = "petioles?", stipule = "stipules?", inflorescence = "inflorescences?",
                peduncle = "peduncles?", pedicel = "pedicels?", bud = "", bract = "bracts?", bracteole = "bracteoles?", involucral_bract = "bracts?",
                sepal = "sepals?|calyx", calyx = "calyx|calyces|sepals?", hypanthium = "hypanthium|calyx", perianth = "perianths?|tepals?",
                petal = "petals?|corollas?", corolla = "corollas?|petals?", anther = "anthers?|stamens?", filament = "filaments?|stamens?",
                staminode = "staminodes?", stamen = "stamens?|filaments?|anthers?", ovary = "ovary|ovaries|carpels?", style = "styles?",
                stigma = "stigmas?|styles?", carpel = "carpels?|ovary", disc = "disc", fruit = "fruits?|capsules?", seed = "seeds?",
                aril = "arils?|seeds?", funicle = "funicles?|arils?|seeds?", new_shoot = "shoots?|stems?|branchlets?|tips?",
                stem = "stems?|branches|branchlets?", branchlet = "branchlets?|branches|stems?",
                dorsal_sepal = "sepals?", lateral_sepal = "sepals?", callus = "callus|calli", column = "columns?",
                labellum_stalk = "labellum|stalks?|claws?|hinges?", sepal_club = "clubs?|osmophores?", column_gland = "glands?", basal_gland = "glands?",
                glume = "glumes?", lemma = "lemmas?", palea = "paleas?|paleae", awn = "awns?", awn_column = "columns?", bristle = "bristles?",
                lodicule = "lodicules?", rachilla = "rachillas?", coma = "coma", leaf_sheath = "sheaths?", ligule = "ligules?", auricle = "auricles?")
# (Poaceae: the spikelet parts are "other organs" for each other, "shorter than both glumes")
grass_other <- c("glumes?", "lemmas?", "paleas?", "paleae", "ligules?", "lodicules?", "rachillas?", "call(?:us|i)", "coma", "florets?",
                 "spikelets?", "caryops[ie]s", "grains?", "auricles?", "bristles?")
gen_strip_other <- function(ok) {
  own <- str_split(gen_own[[ok]] %||% "", "\\|")[[1]]
  oth <- setdiff(c(gen_other, if (grass_mode) grass_other), own)
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
    sec == "stem" & s == "new growth" ~ "new_shoot", sec == "stem" & str_detect(s, "branchlet|twig") ~ "branchlet", sec == "stem" ~ "stem",
    sec %in% c("leaf", "lamina") ~ "leaf", sec == "juvenile" ~ "juvenile_leaf", sec == "leaflet" ~ "leaflet", sec == "petiole" ~ "petiole", sec == "stipule" ~ "stipule",
    sec == "inflorescence" ~ "inflorescence", sec == "peduncle" ~ "peduncle", sec == "pedicel" ~ "pedicel", sec == "bud" ~ "bud",
    sec == "bract" & str_detect(s, "bracteole|prophyll") ~ "bracteole", sec == "bract" & str_detect(s, "involuc|phyllar") ~ "involucral_bract",
    sec == "bract" & str_detect(s, "spathe") ~ "spathe", sec == "bract" ~ "bract",
    sec == "calyx" & str_detect(s, "^dorsal sepal") ~ "dorsal_sepal", sec == "calyx" & str_detect(s, "^lateral sepal") ~ "lateral_sepal",
    sec == "calyx" & str_detect(s, "sepal") ~ "sepal", sec == "calyx" & str_detect(s, "hypanth") ~ "hypanthium", sec == "calyx" ~ "calyx",
    sec == "orchid_part" & s == "column glands" ~ "column_gland", sec == "orchid_part" & s == "basal glands" ~ "basal_gland",
    sec == "orchid_part" & str_detect(s, "^call") ~ "callus", sec == "orchid_part" & str_detect(s, "^column") ~ "column",
    sec == "orchid_part" & str_detect(s, "^labellum") ~ "labellum_stalk", sec == "orchid_part" ~ "sepal_club",
    sec == "corolla" & str_detect(s, "tepal|perianth|sepals and petals|petals and sepals") ~ "perianth",
    sec == "corolla" & str_detect(s, "labellum") ~ "labellum", sec == "corolla" & str_detect(s, "petal|standard|keel") ~ "petal",
    sec == "corolla" & str_detect(s, "corolla") ~ "corolla", sec == "corona" ~ "corona",
    sec == "androecium" & str_detect(s, "anther") ~ "anther", sec == "androecium" & str_detect(s, "filament") ~ "filament",
    sec == "androecium" & str_detect(s, "staminode") ~ "staminode", sec == "androecium" & str_detect(s, "stamen|staminal") ~ "stamen",
    sec == "gynoecium" & str_detect(s, "presenter") ~ "pollen_presenter", sec == "gynoecium" & str_detect(s, "ovar") ~ "ovary", sec == "gynoecium" & str_detect(s, "style") ~ "style",
    sec == "gynoecium" & str_detect(s, "stigma") ~ "stigma", sec == "gynoecium" & str_detect(s, "carpel|pistil") ~ "carpel",
    sec == "gynoecium" & str_detect(s, "disc") ~ "disc", sec == "gynoecium" & str_detect(s, "indusi") ~ "indusium",
    sec == "fruit" & str_detect(s, "infructesc") ~ "infructescence", sec == "fruit" ~ "fruit", sec == "seed" & str_detect(s, "aril") ~ "aril",
    sec == "seed" & s == "funicle" ~ "funicle", sec == "seed" & str_detect(s, "seed") ~ "seed",
    sec == "grass_part" & str_detect(s, "glume") ~ "glume", sec == "grass_part" & str_detect(s, "lemma") ~ "lemma",
    sec == "grass_part" & str_detect(s, "palea") ~ "palea", sec == "grass_part" & str_detect(s, "^call") ~ "callus",
    sec == "grass_part" & str_detect(s, "column") ~ "awn_column", sec == "grass_part" & str_detect(s, "awn") ~ "awn",
    sec == "grass_part" & str_detect(s, "bristle") ~ "bristle", sec == "grass_part" & str_detect(s, "lodicule") ~ "lodicule",
    sec == "grass_part" & str_detect(s, "rachilla") ~ "rachilla", sec == "grass_part" & s == "coma" ~ "coma",
    sec == "grass_part" & str_detect(s, "sheath") ~ "leaf_sheath", sec == "grass_part" & str_detect(s, "ligule") ~ "ligule",
    sec == "grass_part" & str_detect(s, "auricle") ~ "auricle",
    TRUE ~ NA_character_)
}
gen_pclass <- function(part) {
  p <- str_to_lower(coalesce(part, ""))
  # (Poaceae: the culm's "internodes" and the inflorescence "axis" are the organ itself: "internodes smooth, glabrous")
  case_when(p == "" ~ "", grass_mode & p %in% c("internodes", "axis") ~ "", str_detect(p, "^(?:lower|abaxial) surfaces?$|^under-?surface$") ~ "abaxial",
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
gen_skip <- c("juvenile_leaf__hairs", "juvenile_leaf__texture", "leaf__hairs", "leaf__shape", "leaf__colour", "leaf__texture", "leaf__surface", "leaf_apex_apex_shape", "leaflet__shape",
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
  "pollen_presenter", "pollen_presenter_length", "pollen_presenter_width", "presenters?",
  "bracteole", "bracteole_length", "bracteole_width", "bracteoles?",
  "involucral_bract", "involucral_bract_length", "involucral_bract_width", "bracts?",
  "spathe", "spathe_length", "spathe_width", "spathes?",
  "bract", NA, "flower_bract_width", "bracts?",
  "sepal", NA, "flower_sepal_width", "sepals?|calyx|lobes?|segments?",
  "petal", NA, "flower_petal_width", "petals?",
  "perianth", NA, "tepal_width", "tepals?|perianth|lobes?|segments?",
  "labellum", NA, "labellum_width", "labellum",
  "callus", "callus_length", "callus_width", "callus|calli",
  "column", "column_length", "column_width", "columns?",
  "labellum_stalk", "labellum_stalk_length", NA, "labellum|stalks?|claws?|hinges?",
  "sepal_club", "sepal_club_length", NA, "clubs?|osmophores?",
  "column_gland", "column_gland_length", NA, "glands?",
  "basal_gland", "basal_gland_length", NA, "glands?",
  "hypanthium", "hypanthium_length", "hypanthium_width", "hypanthium|tube|calyx",
  "staminode", "staminode_length", NA, "staminodes?",
  "ovary", "ovary_length", "ovary_width", "ovary",
  "stigma", "stigma_length", NA, "stigmas?",
  "aril", "aril_length", NA, "arils?",
  "corona", "corona_length", NA, "coronas?",
  "disc", NA, "disc_diameter", "disc",
  "indusium", "indusium_length", "indusium_width", "indusi(?:a|um)",
  "infructescence", "infructescence_length", "infructescence_width", "infructescences?",
  "juvenile_leaf", "juvenile_leaf_length", "juvenile_leaf_width", "leaves|leaf|laminae?|blades?",
  # (Poaceae)
  "glume", "glume_length", "glume_width", "glumes?", "lemma", "lemma_length", "lemma_width", "lemmas?",
  "palea", "palea_length", "palea_width", "paleas?|paleae", "awn", "awn_length", NA, "awns?", "awn_column", "awn_column_length", NA, "columns?",
  "bristle", "bristle_length", NA, "bristles?", "ligule", "ligule_length", NA, "ligules?", "leaf_sheath", "leaf_sheath_length", NA, "sheaths?",
  "lodicule", "lodicule_length", NA, "lodicules?", "rachilla", "rachilla_length", NA, "rachillas?", "coma", "coma_length", NA, "coma|hairs?",
  "auricle", "auricle_length", NA, "auricles?")

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
hedge <- "(?:,\\s*)?(?:\\b(?:and|but|although)\\s+)?\\b(?:possibly|probably|likely|almost certainly|it may also|may also|could|might|may occur|may continue|may start|may commence|thought to be|is thought|appears to be|appear to be|seems to be|appears? to|seems? to|presumably|apparently|perhaps|seemingly)\\b.*$"
kw_both <- "\\b(?:flower(?:s|ing)?,? (?:and |, )?(?:(?:semi-mature|mature|ripe|immature|young) )?(?:fruit(?:s|ing)?|seeds?)|flowers, fruit and seed|flowers and fruit|flowering and fruiting)\\b"
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
    mq <- str_match(s2, regex("(?:for |during )?(?:sporadically )?(?:most (?:months )?of (?:the )?year|most months|much of (?:the )?year),?\\s*(?:with (?:an? )?)?(primarily|mainly|mostly|particularly|especially|peak(?:s|ing)?)\\s+(?:(?:in|from|during)\\s+)?", ignore_case = TRUE))
    if (!is.na(mq[1])) {
      pre <- str_sub(s2, 1, str_locate(s2, fixed(mq[1]))[1, 1] - 1)
      kk <- if (str_detect(pre, regex(kw_both, ignore_case = TRUE))) c("fl", "fr") else if (str_detect(pre, regex(kw_fruit, ignore_case = TRUE))) "fr" else "fl"
      main_q[kk] <- str_replace(str_to_lower(mq[2]), "^peak(?:s|ing)$", "peak"); s2 <- str_replace(s2, fixed(mq[1]), "")
    }
    s2 <- str_remove_all(s2, regex(",?\\s*(?:for |during )?(?:sporadically )?(?:most (?:months )?of (?:the )?year|most months|much of (?:the )?year),?", ignore_case = TRUE))
    s2 <- str_remove_all(s2, regex("\\b(?:probably|possibly|perhaps) (?:dependent|depending) on [a-z ]+", ignore_case = TRUE))
    s2 <- str_remove_all(s2, regex(hedge, ignore_case = TRUE))
    # a queried month ("?Aug.-Nov.", "Aug.-?Nov.") ends the stated part; the rest is read on a scoring_certainty = possibly row
    s2 <- str_remove(s2, regex(paste0("\\?\\s*(?=", mon_pat, ").*$|(?<=\\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]{0,6}\\.?)\\s*\\?.*$"), ignore_case = TRUE))
    lo_re <- paste0("\\(\\s*(", ext_pat, ")\\s*-\\s*\\)"); hi_re <- paste0("\\(\\s*-\\s*(", ext_pat, ")\\s*\\)")
    # "Flowers and fruits at any time but mainly September-March": X on a "mainly" row, the whole year on an
    # "occasionally" row (user's choice)
    # (also "all year round, but mainly in spring", "all year, but predominantly July-December")
    # (and "throughout the year with peak in November-December", "year round, prolifically from c. November-March")
    am <- str_match(s2, regex("\\b(?:(?:at )?any time(?: of (?:the )?year)?|all year(?: round| around)?|all months|year[- ]round|throughout the year),?\\s*(?:but\\s+|with (?:an? )?)?(mainly|mostly|chiefly|usually|especially|particularly|primarily|predominantly|peak(?:s|ing)?|prolifically|most frequently|most commonly)\\s+(?:(?:in|from|during)\\s+)?(?:c\\.\\s*)?", ignore_case = TRUE))
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
    # "Flowers mainly Aug.-Nov., sporadically in other months": the remaining months on a row with that word
    om <- str_match(s2, regex(",?\\s*(?:but\\s+|and\\s+|with\\s+)?(occasional(?:ly)?|sporadic(?:ally)?|rarely)\\s+(?:flowering\\s+)?(?:in|at)\\s+other\\s+(?:months|times)", ignore_case = TRUE))
    if (!is.na(om[1])) {
      s2 <- str_replace(s2, fixed(om[1]), "")
      oq <- str_to_lower(om[2]) %>% str_replace("^occasional$", "occasionally") %>% str_replace("^sporadic$", "sporadically")
      main_m <- pheno_hits(str_squish(str_remove_all(str_remove_all(s2, lo_re), hi_re)))
      for (k in c("fl", "fr")) if (length(main_m[[k]])) q_extra[[length(q_extra) + 1]] <- tibble(k = k, q = oq, m = list(setdiff(1:12, main_m[[k]])), s = s)
    }
    # a lone parenthetical month ("Flowers (Apr.) Aug.-Nov.", "Sept.-Nov. (Jan.)") is a rare month, like "(Apr.-)"
    lone_re <- paste0("\\(\\s*(", ext_pat, "(?:\\s*(?:,|and|&)\\s*", ext_pat, ")*)\\s*\\)")
    lone <- str_match_all(s2, regex(lone_re, ignore_case = TRUE))[[1]]
    if (nrow(lone)) {
      lead <- str_extract(s2, regex("^[^0-9]*?\\b(?:flower(?:s|ing)?|fruit(?:s|ing)?)\\b(?: and (?:fruits?|flowers?))?", ignore_case = TRUE))
      lx <- pheno_hits(paste(coalesce(lead, "Flowers"), paste(lone[, 2], collapse = ", ")))
      fl_x <- c(fl_x, lx$fl); fr_x <- c(fr_x, lx$fr)
      s2 <- str_squish(str_remove_all(s2, regex(lone_re, ignore_case = TRUE)))
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
other_organs <- "clubs?|osmophores?|shoots?|petioles?|petiolate|petiolules?|pedicels?|pedicles?|papillae|peduncles?|stalks?|stipes?|stipules?|bracts?|bracteoles?|hairs?|scales?|glands?|spines?|thorns?|prickles?|teeth|lobes?|tube|throat|limb|lips?|calyx|corolla|sepals?|petals?|stamens?|filaments?|anthers?|styles?|stigmas?|ovary|seeds?|arils?|wings?|beak|radicle|rachis|axis|axes|internodes?|sheaths?|ligules?|veins?|midribs?|leaflets?|pinnae|pinnules?|leaves|leaf|blades?|lamina|branchlets?|branches|stems?|trunks?|roots?|pneumatophores?|heads?|spikes?|racemes?|inflorescences?|flowers?|fruits?|capsules?|pods?|buds?|cotyledons?|tubers?|rhizomes?|bulbs?|corms?|segments?|apex|tips?|base|margins?|disc|hypanthium|operculum|valves|spikelets?|awns?|glumes?|lemmas?|paleas?|tepals?|scapes?|culms?|cones?|bulbils?|tendrils?|cladodes?|phyllodes?"
excl <- function(...) paste(setdiff(str_split(other_organs, "\\|")[[1]], c(...)), collapse = "|")

# Orchidaceae subspecies diagnoses ("Differs from C. attingens subsp. attingens by its smaller flowers (20-30 mm across),
# obliquely decurved lateral sepals, smaller tepals (dorsal sepal 30-40 mm long, ...) and smaller column (9-11 x 3-4 mm)"):
# rewritten as organ-led sentences ("Flowers 20-30 mm across. Lateral sepals obliquely decurved. Dorsal sepal 30-40 mm long, ...
# Column 9-11 x 3-4 mm."), so each size and term goes to its organ. Comparative words (relative to the other taxon) are dropped,
# and items that name no organ ("its coastal distribution", "an earlier flowering period") are not kept.
orchid_diag_organ <- "\\b(?:dorsal sepals?|lateral sepals?|sepaline clubs|sepal clubs|clubs|petals?|sepals?|tepals|(?:flower )?segments|labellum|labella|columns?|(?:basal |lamina )?calli|callus|flower stems?|flowers?|leaf|leaves|(?:labellum )?marginal teeth|basal glands|midlobe|habit|plants?)\\b"
split_top <- function(x, sep = ";") {
  depth <- 0; out <- character(0); cur <- ""
  for (ch in str_split(x, "")[[1]]) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- max(0, depth - 1)
    if (ch == sep && depth == 0) { out <- c(out, cur); cur <- "" } else cur <- paste0(cur, ch)
  }
  str_squish(c(out, cur)) %>% .[. != ""]
}
orchid_diagnosis <- function(desc) {
  m <- str_match(desc, paste0("^((?:[A-Z][a-z]+ [^.]*? and )?(?:[Dd]iffers?|[Dd]istinguished) (?:[^.]|\\.(?=\\s*[a-z(,])|(?<=[0-9])\\.(?=[0-9]))*?",
                              "\\b(?:by|in having|in) (?:its |the |a |an |having )?)((?:[^.]|\\.(?=\\s*[a-z0-9(,])|(?<=[0-9])\\.(?=[0-9]))*)\\.(?:\\s+|$)"))
  if (is.na(m[1])) return(desc)
  rest <- str_sub(desc, nchar(m[1]) + 1)
  cmp <- "\\b(?:smaller|larger|bigger|narrower|broader|wider|shorter|longer|taller|thicker|thinner|paler|darker|brighter|more|less|slightly|relatively|much|somewhat)\\b,?"
  toks <- unlist(map(split_top(m[3], ";"), ~ split_tokens(str_replace_all(str_remove(.x, "^and\\s+"), "\\) and ", "), "))))
  toks <- str_remove(toks, "^and\\s+")
  out <- character(0); pend <- ""
  for (tk in toks) {
    tk <- str_squish(paste(pend, tk)); pend <- ""
    loc <- str_locate(str_to_lower(tk), orchid_diag_organ)
    if (is.na(loc[1])) { pend <- paste0(tk, ","); next }
    pre <- str_sub(tk, 1, loc[1] - 1); org <- str_sub(tk, loc[1], loc[2]); post <- str_sub(tk, loc[2] + 1)
    pre <- pre %>% str_remove("^(?:its|the|a|an|having)\\s+") %>% str_remove_all(rx(cmp)) %>% str_squish() %>% str_remove("^,\\s*|,$") %>% str_squish()
    pm <- str_match(post, "^\\s*\\(([^()]*)\\)(.*)$")
    if (!is.na(pm[1])) {
      inner <- str_squish(pm[2])
      if (str_detect(inner, "^(?:c\\. )?[0-9.]+(?:\\s*[-–]\\s*[0-9.]+)?\\s*mm$")) inner <- paste(inner, "long")
      cl <- if (str_detect(str_to_lower(inner), paste0("^", orchid_diag_organ))) paste0(inner, pm[3])
            else paste0(org, if (pre != "") paste0(" ", pre, ","), " ", inner, pm[3])
    } else cl <- paste0(org, if (pre != "") paste0(" ", pre, if (str_detect(post, "^\\s*$")) "" else ","), post)
    cl <- str_squish(str_remove(cl, ",\\s*$"))
    out <- c(out, paste0(str_to_upper(str_sub(cl, 1, 1)), str_sub(cl, 2)))
  }
  str_squish(paste(c(if (length(out)) paste0(paste(out, collapse = ". "), "."), rest), collapse = " "))
}
# Caladenia-type sepal clubs (osmophores) described inside the sepal or petal clause ("sepals and petals with yellowish brown
# clubs 5-15 mm long", "tapering toward tip, terminated by clubs 6-12 mm long") become their own clause, so the club colour and
# length are not read as the flower colour or tepal length
# (a club clause in brackets, "(sepals sometimes developed into thin, imperfect, blackish clubs 8-12 mm long)", is taken out of them first)
orchid_clubs <- function(desc) str_replace_all(str_replace_all(desc, "\\s*\\(([^()]*\\bclubs\\b[^()]*)\\)", "; \\1"), rx("(?:,\\s*|\\s+)(?:with|terminated by|terminating in|ending in|tipped with|tipped by|developed into) ((?:[a-z-]+,? (?:and |or |to )?){0,10}?)(?:sepaline |sepal )?(?:clubs|osmophores)\\b"), "; Clubs \\1")

extract_one <- function(r, group_filter = TRUE) {
  taxon <- r[["scientific_name"]]; fam <- r[["family"]]
  # (all family modes are set first: sentences() and classify_token() read them, also in the genus / family filter below; set
  # later, the filter saw the previous taxon's mode, so genus copy-downs depended on how the taxa were split between workers)
  fern_mode <<- fam %in% fern_families
  chenopod_mode <<- fam %in% c("Chenopodiaceae", "Casuarinaceae")
  casuarina_mode <<- fam == "Casuarinaceae"
  orchid_mode <<- fam == "Orchidaceae"
  legume_mode <<- fam == "Fabaceae"
  grass_mode <<- fam == "Poaceae"
  trt <- pick_treatment(r[["Description"]])
  desc <- dehedge(prep(trt$text))
  # gametophyte sentences (Lycopodiaceae, ferns) describe the gametophyte, not the plant
  if (!is.na(desc)) desc <- str_remove_all(desc, "(?:^|(?<=\\.\\s))Gametophytes?\\b[^.]*\\.\\s*")
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
    # ("Habit varied, including: (i) ... (ii) ...": an enumeration of different habits in the group)
    ss <- ss[!str_detect(ss, rx("\\b(?:varied|various|variable)\\b[^.]*\\bincluding\\b|\\(i\\)"))]
    ss <- map_chr(ss, function(x) {
      # ranges ("herbaceous to coriaceous", "globose to obloid-ovoid") are variation across the group: the range words are
      # removed, the rest of the segment kept ("Fruit a globose to obloid-ovoid, syncarpous fleshy berry")
      adv <- "(?:(?:slightly|weakly|strongly|very|more|less|shortly|narrowly|broadly|widely|deeply|shallowly|densely|sparsely|somewhat|minutely|finely|irregularly)\\s+)?"
      x <- str_remove_all(x, paste0(adv, "\\b(?!(?:up|reduced|similar|adnate|attached|fused|close|due|next|equal|subequal|inserted|appressed|confined|restricted|tapering|tapered|narrowed|narrowing|extending|leading|according|relative|opposite|decurrent|connate|joined)\\b)[a-z-]+(?: to ", adv, "(?!the\\b|an?\\b|[0-9])[a-z-]+)+"))
      seg <- str_split(x, "(?<=[,;:])\\s+")[[1]]
      # segments naming a particular genus ("glandular-pubescent (Hydrocleys)") describe only that genus
      # ... and alternatives ("smooth or variously winged") are variation across the group
      drop <- str_detect(seg, rx(qual_re)) | str_detect(seg, "\\([A-Z][a-z]+") | str_detect(seg, "\\bor\\b")
      # (a clause in which any segment names a genus describes alternatives across genera: "Leaves reduced to scales
      # (Cassytha), or fully developed, simple, exstipulate": the whole clause is dropped)
      # (only the alternatives: segments before an "X or Y (Genus)" segment, and segments after it that continue with "or";
      # "Flowers zygomorphic or rarely actinomorphic (Brunonia), bisexual" keeps "bisexual")
      cl_id <- cumsum(c(1, head(str_detect(seg, ";$|:$"), -1)))
      for (gk in which(str_detect(seg, "\\([A-Z][a-z]+(?:,| and| or|\\))"))) {
        same <- which(cl_id == cl_id[gk])
        if (str_detect(seg[gk], "\\bor\\b")) drop[same[same <= gk]] <- TRUE
        nx <- same[same > gk]
        if (length(nx) && str_detect(seg[nx[1]], "^or\\b")) drop[nx] <- TRUE
      }
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
  # (hair terms frequent in Acacia descriptions)
  if (legume_mode) hairs_dict <- c(hairs_dict, "tomentulose|hirsutellous|villosulous|tomentellous|puberulent|crisped-pubescent|appressed-hairy" = "hairy")
  # Fabaceae: "clothed with sparse, usually brown, appressed hairs" is one hair phrase, so its colour words are not read as the
  # organ's colour (the commas between the hair adjectives are dropped)
  # ("1-1.5 mm in diam." reads as "1-1.5 mm diam.")
  if (legume_mode && !is.na(desc)) desc <- str_replace_all(desc, "\\bin diam\\.", "diam.")
  # (written fractions, "sepals 1/2-2/3-united", "calyx 3/4 length of corolla", are not counts)
  if (legume_mode && !is.na(desc)) desc <- str_replace_all(desc, "\\b([1-9])/([1-9])\\b", function(z) c("1/2" = "half", "1/3" = "one-third", "2/3" = "two-thirds",
    "1/4" = "one-quarter", "3/4" = "three-quarters", "1/5" = "one-fifth", "2/5" = "two-fifths", "3/5" = "three-fifths", "4/5" = "four-fifths",
    "1/6" = "one-sixth", "5/6" = "five-sixths", "1/8" = "one-eighth")[z] %>% coalesce(str_replace(z, "/", " in ")))
  if (legume_mode && !is.na(desc)) desc <- str_replace_all(desc, "\\b(?:with|by|of) (?:[a-z-]+,? (?:or |and |to )?){1,6}?(?:hairs|hairlets|trichomes)\\b",
                                                         function(z) str_replace_all(z, ",\\s*", " "))
  # (Hovea "Aril of seed 3.5–4 mm long": "seed" would claim the number; Chamaecrista petiolar "gland with slender stipe 0.2–0.7 mm
  # long; head 0.2–0.4 mm diam.": the gland's head, not a flower head, so the leaf rachis that follows stays with the leaf)
  if (legume_mode && !is.na(desc)) desc <- desc %>% str_replace_all("\\b([Aa]rils?) of (?:the )?seeds?\\b", "\\1") %>%
    str_replace_all("(\\bglands?\\b(?:[^.;:]|(?<=[0-9])\\.(?=[0-9])){0,60}); head\\b", "\\1, gland-head")
  # (Orchidaceae, Flora of Victoria style "Labellum curved forward ..., lamina ovate, obscurely 3-lobed": the labellum lamina, not a leaf)
  if (orchid_mode && !is.na(desc)) desc <- orchid_clubs(orchid_diagnosis(desc)) %>%
    str_replace_all("(\\b[Ll]abellum\\b[^.]*?[,;]\\s*)lamina\\b", "\\1labellum lamina") %>%
    # ("oblong-elliptical when flattened", "6-9 mm wide (when flattened)": how the part was measured, not a flattened shape)
    str_remove_all("\\s*\\(when flattened\\)|,?\\s*\\bwhen flattened\\b")
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
  # (Fabaceae: crown / habit adjectives between a commonness word and the form, "sometimes an obconic tree")
  gf <- scan_terms(habit_gf, growth_form_dict, fillers = if (legume_mode) c(gf_fillers, "obconic", "obconical", "rounded", "graceful", "dense", "spreading", "bushy",
                                                                                         "spindly", "straggly", "single-stemmed", "multistemmed", "multi-stemmed", "erect", "slender", "spinescent") else gf_fillers)
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
  # orchids are herbs (user's rule; climbing orchids are named as climbers in their descriptions)
  if (fam == "Orchidaceae" && !nrow(gf) && r[["rank"]] %in% c("species", "subspecies", "variety", "form")) gf <- tibble(pos = 1L, end = 1L, value = "herb", term = "[Orchidaceae]", qual = NA_character_, region = NA_character_)
  # graminoid families (Centrolepidaceae rule extended to sedges, grasses, rushes, restiads): graminoid, plus herb when no form is named
  if (fam %in% c("Centrolepidaceae", "Cyperaceae", "Poaceae", "Juncaceae", "Restionaceae", "Anarthriaceae", "Ecdeiocoleaceae")) {
    if (!nrow(gf)) gf <- tibble(pos = 1L, end = 1L, value = "herb", term = paste0("[", fam, "]"), qual = NA_character_, region = NA_character_)
    if (!any(gf$value == "graminoid")) gf <- bind_rows(gf, tibble(pos = max(gf$pos) + 1L, end = max(gf$pos) + 1L, value = "graminoid", term = paste0("[", fam, "]"), qual = NA_character_, region = NA_character_))
  }
  # Poaceae, Cyperaceae, Juncaceae (user, 2026-10-28): graminoid, "tussock graminoid" where "tussock", "tufted" or "caespitose" (= tufted)
  # appears, "hummock graminoid" where "hummock" appears; not herb
  if (fam %in% graminoid_rule_families && nrow(gf)) {
    gf <- gf %>% filter(value != "herb")
    if (!is.na(habit_gf) && str_detect(habit_gf, "\\bhummock") && !any(gf$value == "hummock"))
      gf <- bind_rows(tibble(pos = 0L, end = 0L, value = "hummock", term = "hummock", qual = NA_character_, region = NA_character_), gf)
    if (!is.na(habit_gf) && str_detect(habit_gf, "\\btussock|\\btuft(?:ed|s)?\\b|\\bc(?:a)?espitose\\b") && !any(gf$value == "tussock"))
      gf <- bind_rows(tibble(pos = 0L, end = 0L, value = "tussock", term = "tussock", qual = NA_character_, region = NA_character_), gf)
    gf <- gf %>% arrange(pos)
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
  # (orchids: "dense clumps with thin, spreading roots", "forming spreading clonal colonies": the roots or the colonies (of many
  # plants) spread, not the stems; a plant's own clump can be dense or spreading)
  addc("stem_growth_habit", collapse_text(c(habit_txt, stem_lead), " | "), stem_habit_dict,
       prep_fun = function(x) {
         if (is_tree) x <- str_remove_all(x, "\\b(?:with|and) (?:[a-z-]+,? (?:and |or )?){1,3}(?:branches|branchlets)\\b")
         if (orchid_mode) x <- x %>%
           str_remove_all("\\b(?:(?:thin|thick|coarse|fleshy|spreading|wiry|long|short|slender|fibrous|aerial|numerous|branching|branched|stout|straggly|tuberous|basal|flattened|tough|creeping|extensive)[,]? (?:and |or )?){0,4}roots?\\b") %>%
           str_remove_all("\\b(?:(?:spreading|clonal|dense|densely crowded|sparse|large|small|extensive|loose|vegetative|crowded)[,]? (?:and |or )?){0,3}(?:colonies|patches|groups|populations)\\b")
         x })
  # Poaceae: culm internodes ("mid-culm internodes relatively long, 8-11 cm long", Micraira "Culms ..., internodes c. 1 mm long") are
  # candidate internode_length, not the culm's length (inflorescence internodes, "lowest internodes" of a panicle, are not)
  if (grass_mode) {
    # ("Vegetative culm internodes all short (never elongated as in T. claytonii), 0.3-1.0 cm long"; not hairs on the internodes,
    # "internodes pubescent to pilose with hairs to c. 1 mm long")
    int_re <- paste0("\\b(?:[a-z-]+ )?internodes?\\b(?:(?!\\bhairs?\\b)[^;|0-9]){0,70}?(", meas, ")\\s*long")
    im <- str_match(mark_extremes(join_units(c(stem_lead, unit_text(u, "stem", parts = "any", subj_re = "culm")))), rx(int_re))
    if (!is.na(im[1])) addn("internode_length", meas_of(im[2]), unmark(im[1]))
    stem_lead <- map_chr(stem_lead, ~ unmark(str_remove_all(mark_extremes(.x), rx(int_re))))
    # (Triodia "aerial roots absent or few and short, to 5 cm long")
    stem_lead <- map_chr(stem_lead, ~ unmark(str_remove_all(mark_extremes(.x), rx(paste0("\\b(?:aerial |prop |adventitious |stilt )?roots?\\b[^;|0-9]{0,70}?", meas, "\\s*long")))))
  }
  # (Poaceae: the culm's own length, not branches)
  if (grass_mode) stem_lead <- unit_text(u, "stem", subj_re = "^(?:the )?(?:main |flowering |aerial |vegetative )?(?:stems?|culms?)$") %>%
    map_chr(~ unmark(str_remove_all(mark_extremes(.x), rx(paste0("\\b(?:[a-z-]+ )?internodes?\\b(?:(?!\\bhairs?\\b)[^;|0-9]){0,70}?", meas, "\\s*long|\\b(?:aerial |prop |adventitious |stilt )?roots?\\b[^;|0-9]{0,70}?", meas, "\\s*long")))))
  sl <- m_first(stem_lead, "long|in length", excl("stems?", "branches", "branchlets?", "culms?"))
  if (is.null(sl) && length(habit)) {
    sm <- str_match(mark_extremes(join_units(habit)), rx(paste0("\\b(?:stems?|branches)\\b[^,;|]{0,30}?(", meas, ")(?:\\s+or more)?\\s*long")))
    if (!is.na(sm[1])) sl <- list(v = meas_of(sm[2]), txt = unmark(sm[1]))
  }
  if (!is.null(sl)) addn("stem_length", sl$v, sl$txt)
  # (orchid stems given as length x diameter: Cadetia "Stems slender, 40-70 x 1-1.5 mm, widening from base to apex")
  orch_sd <- if (orchid_mode && is.null(sl)) d_first(stem_lead, excl("stems?")) else list(L = NULL, W = NULL)
  if (!is.null(orch_sd$L)) addn("stem_length", orch_sd$L$v, orch_sd$L$txt)
  addc("stem_branching_form", habit_txt, branching_dict)
  if (legume_mode) {
    # crown shape (plant_canopy_form): "Obconic shrub", "Rounded shrub", "flat-topped tree", "obconic with spreading, open crowns"
    addc("plant_canopy_form", habit_txt, c("obconic(?:al)?|turbinate|top-shaped" = "obconical", "round(?:ed)?(?:-topped)?|sub-rounded" = "rounded",
                                           "flat-topped|flat-crowned|umbrella-like|umbrella-shaped|umbrageous" = "flat-topped",
                                           "(?:low-)?domed|dome-shaped|hemispherical" = "hemispherical", "pyramidal" = "pyramidal", "globose|globular" = "globose",
                                           "columnar|fastigiate" = "columnar", "broom-like" = "broom-like", "(?:pseudo-?)?conifer(?:-like)?|pine-like" = "pine-like",
                                           "diffuse" = "diffuse", "(?:dense|compact)(?=,? (?:[a-z-]+ )?crowns?\\b)|crowns? (?:dense|compact)" = "dense-crowned",
                                           "open(?=,? (?:[a-z-]+ )?crowns?\\b)|crowns? open" = "open-crowned", "mounded|mound-like|mound-forming" = "mounded"),
         prep_fun = function(x) str_remove_all(x, "\\b(?:with|and) (?:[a-z-]+ ){0,3}(?:branchlets?|branches|phyllodes?|leaves|foliage|stems?)\\b|\\brounded (?:at|towards) (?:the )?[a-z]+"))
    # clonal spread stated in the habit ("may spread by root suckers", "root-suckering shrub", "forming clonal thickets", "spreading by runners")
    # ("sometimes clonal due to root suckering" is one statement: the mechanism takes the qualifier; "often spreading by runners")
    veg_txt <- if (is.na(habit_txt)) habit_txt else str_replace_all(habit_txt, rx("\\bclonal (?:due to|by|through|from|via) (?:a |its |the )?((?:root[- ])?suckering|(?:root[- ])?suckers?)(?: habit)?\\b"), "\\1") %>%
      str_replace_all(rx("\\b(spreading|spreads?|reproducing|multiplying) by\\b"), "\\1")
    veg <- scan_terms(veg_txt, c("root[- ]suckering|root[- ]suckers?|suckering from (?:the )?roots|suckers? (?:from|on) (?:the )?roots" = "root_buds",
                                   "suckering|suckers?" = "clonal", "clonal(?:ly)?" = "clonal", "rhizomatous|rhizomes?" = "rhizome",
                                   "(?:subterranean|underground) runners?" = "rhizome", "stoloniferous|stolons?|runners?" = "stolon"))
    if (nrow(veg)) {
      add(rec_cat("clonal_spread_mechanism", veg, habit_txt))
      add(rec_cat("vegetative_reproduction_ability", veg %>% mutate(value = "vegetative"), habit_txt))
    }
  }
  addc("plant_succulence", habit_txt, c("succulent" = "succulent"))
  sub_h <- addc("plant_growth_substrate", habit_txt, substrate_dict)
  # orchids: species descriptions rarely name the substrate, but the Habitat text does ("growing on the upper branches of
  # trees", "forms strands on trees, boulders and cliff faces", "in freely draining sands"). Substrates are read in text order
  # from "on ..." phrases (trees, branches, trunks ... -> epiphyte; rocks, boulders, cliff faces ... -> lithophyte; outcrops and
  # ledges only when no soil is named) and "in ... soil / sand / loam" phrases (-> terrestrial), with a commonness word before
  # "on" ("occasionally on rocks") and an island / state ("On Lord Howe Island it grows on ...") as qualifiers.
  if (orchid_mode && (is.null(sub_h) || !nrow(sub_h)) && !is.na(prep(r[["Habitat"]]))) {
    orch_terr <- word(taxon, 1) %in% orch_terr_genera
    hab_s <- sentences(prep(r[["Habitat"]]))
    soil_re <- "\\b(?:soils?|sands?|loams?|clays?|humus|leaf litter|litter|gravel|peat|mud|silt|alluvium)\\b"
    tree_re <- "\\b(?:trees?|trunks?|branch(?:es|lets?)?|twigs?|limbs?|boles?|bark|shrubs?|mangroves?|palms?|hosts?|saplings?|stumps?|logs?|tree ?ferns?|paperbarks?|she-?oaks?|casuarinas?|eucalypts?|pines?|figs?|canopy|epiphytes?)\\b"
    # ("on rock ledges / plates" is read like "ledges": lithophytic only when no soil is named; "on (rock) outcrops" is a habitat
    # (Dipodium stenocheilum "growing mainly among grass in open forest and woodland and on rock outcrops"), not a substrate)
    rock_re <- "\\b(?:rocks?(?! (?:outcrops?|ledges?|plates?|sheets?|shelves|platforms?|pavements?|crevices?))|boulders?|cliffs?|cliff[- ]faces?|rock[- ]faces?|rock walls?|stones?|scree)\\b"
    rock2_re <- "\\b(?:(?:rock )?ledges?|rock shelves|rock platforms?|rock (?:plates?|sheets?|pavements?|crevices?))\\b"
    hh <- list(); hsrc <- character(0)
    for (hsi in seq_along(hab_s)) {
      sl <- str_to_lower(hab_s[hsi]); found <- FALSE
      # ("Occasional plants grow on boulders", "Occasional plants grow as terrestrials in soil": a commonness word)
      sl <- str_replace_all(sl, "\\b(?:an? )?occasional (plants?|specimens?|individuals?|colonies)\\b", "occasionally \\1")
      # (a region only when the sentence opens with it: "On Lord Howe Island it grows on stunted trees and rocks")
      reg <- str_match(sl, paste0("^(?:on|in) (", places, ")\\b"))[2]
      has_soil <- str_detect(sl, soil_re)
      for (k in c("epiphyt\\w*", "lithophyt\\w*")) {
        loc <- str_locate_all(sl, k)[[1]]
        for (i in seq_len(nrow(loc))) { hh[[length(hh) + 1]] <- tibble(pos = hsi * 1e4 + loc[i, 1], value = if (str_detect(k, "^epi")) "epiphyte" else "lithophyte",
                                                                        term = str_sub(sl, loc[i, 1], loc[i, 2]), qual = NA_character_, region = reg); found <- TRUE }
      }
      on_loc <- str_locate_all(sl, "\\bon\\b")[[1]]
      for (i in seq_len(nrow(on_loc))) {
        rest <- str_sub(sl, on_loc[i, 2] + 1)
        ph <- str_split(rest, paste0("[;.]|\\b(?:in|on|also|among|amongst|along|near|beside|at|under|beneath|close to|where|which|that|within|between|from|below|above|around|with)\\b|", qual_re))[[1]][1]
        # (an island / state named right after "on" is a place, not a substrate)
        if (str_detect(ph, paste0("^\\s*(?:the )?(?:", places, ")\\b"))) next
        pre <- str_sub(sl, 1, on_loc[i, 1] - 1)
        # (the last commonness word in the same comma segment: "usually found growing on ...", "less commonly among lichen ... on rock")
        seg <- str_remove(pre, "^.*[,;:]")
        q <- tail(str_extract_all(seg, qual_re)[[1]], 1)
        q <- if (length(q)) q else NA_character_
        ws <- bind_rows(str_locate_all(ph, tree_re)[[1]] %>% as_tibble() %>% mutate(value = "epiphyte"),
                        str_locate_all(ph, rock_re)[[1]] %>% as_tibble() %>% mutate(value = "lithophyte"),
                        if (!has_soil) str_locate_all(ph, rock2_re)[[1]] %>% as_tibble() %>% mutate(value = "lithophyte"))
        # (terrestrial genera, e.g. Caladenia "on granite outcrops", Caleana "in a thin layer of moss and lichen on rock surfaces",
        # Calanthe "perched on boulders, rotting logs and stumps": plants of a genus described as terrestrial orchids root in soil,
        # moss or litter on the rock, so rock words are terrestrial (flagged in the term); rotting logs, stumps and tree roots are
        # not hosts. Explicit "lithophytic" / "epiphytic" wording and living trees still count.)
        if (orch_terr && nrow(ws)) {
          ws <- ws %>% filter(!(value == "epiphyte" & (str_detect(str_sub(ph, start, end), "^(?:stumps?|logs?)$") |
                                                         str_detect(str_sub(ph, end + 1), "^\\s*roots?\\b")))) %>%
            mutate(conv = value == "lithophyte", value = ifelse(conv, "terrestrial", value))
        }
        # (Corunastylis nudiscapa "on north-facing open dry eucalypt woodland": a vegetation type, not a host; Erythrorchis "aerial stems
        # climbing on living and dead hosts": a climber rooted in the ground, not an epiphyte)
        if (nrow(ws)) ws <- ws %>% filter(!(value == "epiphyte" & str_detect(str_sub(ph, end + 1), "^\\s*(?:woodlands?|forests?|scrub|heath\\w*|shrubland|communit\\w*|savannah?|thickets?|grassland|country|plantations?)\\b")))
        if (str_detect(pre, "\\b(?:climbing|scrambling|clambering|twining)\\s*$")) ws <- ws %>% filter(value != "epiphyte")
        if (!nrow(ws)) next
        ws <- ws %>% arrange(start)
        if (!"conv" %in% names(ws)) ws$conv <- FALSE
        hh[[length(hh) + 1]] <- ws %>% transmute(pos = hsi * 1e4 + on_loc[i, 2] + start, value,
                                                 term = paste0(str_sub(ph, start, end), ifelse(conv, " (terrestrial orchid genus)", "")),
                                                 qual = ifelse(conv, NA_character_, q), region = reg)
        found <- TRUE
      }
      if (has_soil && str_detect(sl, paste0("\\b(?:in|on|into)\\s+(?:[a-z,-]+\\s+){0,5}?", str_remove(soil_re, "^\\\\b")))) {
        sp <- str_locate(sl, soil_re)
        # (the last commonness word in the comma segment before the soil word: Liparis reflexa "usually growing on rocks ..., rarely on tree
        # trunks, even less commonly in sandy soil")
        sq <- tail(str_extract_all(str_remove(str_sub(sl, 1, sp[1, 1] - 1), "^.*[,;:]"), qual_re)[[1]], 1)
        hh[[length(hh) + 1]] <- tibble(pos = hsi * 1e4 + sp[1, 1], value = "terrestrial", term = str_sub(sl, sp[1, 1], sp[1, 2]),
                                       qual = if (length(sq)) sq else NA_character_, region = reg)
        found <- TRUE
      }
      if (found) hsrc <- c(hsrc, hab_s[hsi])
    }
    if (length(hh)) {
      # (each substrate once, at its first mention: "on trees ..., occasionally on tree ferns" is not a second, rarer epiphyte)
      hh <- bind_rows(hh) %>% arrange(pos) %>% distinct(value, region, .keep_all = TRUE) %>% mutate(end = pos)
      # (a single substrate "usually" found there is simply that substrate)
      # (nor is soil named only with a commonness word: "growing in open forest, sometimes in sandy soil" is still terrestrial)
      if (n_distinct(hh$value) == 1 && all(is.na(hh$region)) && (!any(hh$qual %in% rare_quals) || all(hh$value == "terrestrial"))) hh$qual <- NA_character_
      hdesc <- paste("[habitat]", paste(hsrc, collapse = " "))
      add(rec_cat("plant_growth_substrate", hh, hdesc) %>% mutate(desc = paste0("[habitat] ", desc, ": ", paste(hsrc, collapse = " "))))
    }
  }
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
  # "Matted perennial, ... with erect stems to 15 cm high", "often with ascending stems to c. 15 cm high": the height of the stems is the
  # plant's height (stem lengths, "stems to 1 m long", are not)
  if (is.null(ht) && length(habit)) {
    sh <- str_match(mark_extremes(habit[1]), rx(paste0("\\b(?:with )?(?:[a-z]+ ){0,2}stems?\\b[^,;|0-9]{0,15}?(", meas, ")\\s*(?:high|tall)\\b")))
    if (!is.na(sh[1])) ht <- list(v = meas_of(sh[2]), txt = paste("[stems]", unmark(sh[1])))
  }
  # Poaceae: "Annual with culms 10-40 cm high", "Perennial grass with ... culms arising from a short rhizome; shoots ..., 0.4-0.6 m high"
  # (also across clauses: A. turbinata "... from a short rhizome; shoots intra- or extra-vaginal; 250-480 mm high")
  if (is.null(ht) && grass_mode && length(habit)) {
    sh <- str_match(mark_extremes(join_units(habit)), rx(paste0("\\b(?:culms?|shoots?)\\b[^0-9]{0,80}?(", meas, ")\\s*(?:high|tall)\\b")))
    if (!is.na(sh[1])) ht <- list(v = meas_of(sh[2]), txt = paste("[culms]", unmark(sh[1])))
  }
  # Fabaceae: "Spindly shrub to c. 2 m high, maturing to spindly tree 4-5 m high", "Compact shrub to 3 m high, or tree to 6 m",
  # "shrub 2-4 m high, becoming trees to 6 m high in favourable sites": an unqualified tree alternative raises the maximum
  if (legume_mode && !is.null(ht)) {
    th <- str_match(mark_extremes(str_to_lower(join_units(habit) %||% "")),
                    paste0("(?:\\bor|\\bmaturing (?:in)?to|\\bbecoming)\\s+(?:an?\\s+)?(?:[a-z-]+\\s+){0,2}?trees?\\b[^,;|0-9]{0,10}?(", meas, ")\\s*(?:high|tall)?(?=\\s*(?:[,;|.]|$|in\\b))"))
    # (a qualified tree, "or rarely a tree to 8 m", has its own commonness row above)
    tv <- if (!is.na(th[1]) && !str_detect(th[1], qual_re)) meas_of(th[2]) else NULL
    if (!is.null(tv) && !is.na(tv[["max"]]) && tv[["max"]] > ht$v[["max"]]) {
      ht$v[["max"]] <- tv[["max"]]
      if ("extreme_max" %in% names(tv) && !is.na(tv[["extreme_max"]])) ht$v[["extreme_max"]] <- tv[["extreme_max"]]
      ht$txt <- paste0(ht$txt, " ... ", unmark(th[1]))
    }
  }
  is_climber <- nrow(gf) && any(str_detect(gf$value, "climber"))
  if (!is.null(ht)) {
    # a "climbing palm to 45 m tall" (rattan) is a climber too
    # (orchids: "Flowering plant 10-35 cm tall" is the height when flowering; so is "Plants 200-400 mm tall" of the leek orchids and
    # other deciduous terrestrial orchids, measured to the top of the flower spike (skill rule: orchid heights to the top of the
    # flowering stem); only the height of persistent stems or canes, "Canes 1-3 m tall", is the plant's own height)
    addn(if (is_climber && all(str_detect(gf$value, "climber|palmoid"))) "plant_height_climbing_plant"
         else if (orchid_mode && (str_detect(coalesce(habit_txt, ""), rx("^flowering plants?\\b")) || !str_detect(ht$txt, "^\\[(?:culm/stem|stems)\\]"))) "plant_height_reproductive"
         else "plant_height", ht$v, ht$txt, scale = 1 / 1000)
  } else if (is_climber) {
    cl <- m_first(habit, "long|in length|", h_other)
    if (!is.null(cl)) addn("plant_height_climbing_plant", cl$v, cl$txt, scale = 1 / 1000)
  }
  # Fabaceae: "Tree 6-30 (-45) m high; sometimes a shrub 1.5-3 m high": a height given for a qualified growth form goes on that
  # form's commonness row, and the first height then on a "usually" row
  if (legume_mode && !is.null(ht) && nrow(gf) && any(gf$qual %in% rare_quals)) {
    hx <- mark_extremes(str_to_lower(join_units(habit)))
    for (gi in which(gf$qual %in% rare_quals)) {
      m <- str_match(hx, paste0("\\b", gf$qual[gi], "\\s+(?:an?\\s+)?(?:[a-z-]+\\s+){0,2}?", fixed_re(str_to_lower(gf$term[gi])), "\\b[^,;|]{0,20}?(", meas, ")\\s*(?:high|tall)?"))
      if (is.na(m[1]) || unmark(m[2]) == str_to_lower(ht$txt) || str_detect(str_to_lower(ht$txt), fixed(unmark(m[2])))) next
      add(rec_num("plant_height", meas_of(m[2]), unmark(m[1]), scale = 1 / 1000) %>% mutate(qualifier = gf$qual[gi]))
      for (k in seq_along(recs)) if (all(recs[[k]]$trait == "plant_height") && all(is.na(recs[[k]]$qualifier))) recs[[k]]$qualifier <- "usually"
    }
  }
  # Fabaceae: "Shrub 1-2 m high, sometimes 3-4 m", "Tree 3-6 m high, occasionally 10-12 m": a qualified second height with no
  # growth-form noun goes on that commonness row
  if (legume_mode && !is.null(ht)) {
    hx <- mark_extremes(str_to_lower(join_units(habit) %||% ""))
    qh <- str_match(hx, paste0("(?:,|;|\\bor)\\s*", qual_re, "\\s+(?:to |up to |reaching )?(", meas, ")\\s*(?:high|tall)?(?=\\s*(?:[,;|.]|$))"))
    if (!is.na(qh[1]) && qh[2] %in% rare_quals && !any(map_lgl(recs, ~ all(.x$trait == "plant_height") && any(!is.na(.x$qualifier) & .x$qualifier == qh[2])))) {
      add(rec_num("plant_height", meas_of(qh[3]), unmark(str_remove(qh[1], "^[,;]\\s*")), scale = 1 / 1000) %>% mutate(qualifier = qh[2]))
      for (k in seq_along(recs)) if (all(recs[[k]]$trait == "plant_height") && all(is.na(recs[[k]]$qualifier))) recs[[k]]$qualifier <- "usually"
    }
  }
  # Fabaceae: "Tree to 25 m high; d.b.h. to 36 cm": trunk diameter at breast height
  if (legume_mode) {
    dbh <- str_match(mark_extremes(desc %||% ""), rx(paste0("\\bd\\.?\\s?b\\.?\\s?h\\.?\\s*(?:of\\s+)?(", meas, ")")))
    # ("Tree to 40 m high, to 0.9 m d.b.h.", "mature trunks to 12 cm d.b.h.")
    if (is.na(dbh[1])) { dbh <- str_match(mark_extremes(desc %||% ""), rx(paste0("(", meas, ")\\s*d\\.?\\s?b\\.?\\s?h\\b"))); if (!is.na(dbh[1])) dbh[2] <- dbh[2] }
    if (!is.na(dbh[1])) addn("plant_diameter_breast_height", meas_of(dbh[2]), unmark(dbh[1]))
  }
  wd <- m_first(habit, "wide|across|diam|diameter|in diameter|broad", h_other)
  # "Tufts to c. 2.5 cm wide at base" is the basal width, not the plant's width
  if (!is.null(wd) && str_detect(join_units(habit), rx(paste0(fixed_re(wd$txt), "\\.?\\s*(?:at (?:the )?base|at base of)")))) wd <- NULL
  # ("coloured sessile glands orange, up to c. 0.05 mm wide": a gland / hair size in the habit sentence)
  # (the matched text is not always found verbatim, e.g. Acacia castanostegia "0.6 (–1.5 ) m wide": the check is then skipped)
  wd_pos <- if (!is.null(wd)) str_locate(join_units(habit), fixed(wd$txt))[1, 1] else NA
  if (!is.na(wd_pos) && str_detect(str_sub(join_units(habit), 1, wd_pos - 1), rx("\\b(?:glands?|hairs?|trichomes?|papillae|scales?|spines?|prickles?)\\b[^|;]{0,40}$"))) wd <- NULL
  # (a width that continues the stems' own measurements is their thickness, whatever its size: Oxalis glabra "erect slender stems 5-30 cm
  # high, 0.4-0.8 mm wide" -> stem_diameter; but Rinzia "Shrub with main stems prostrate, 0.2-0.5 m high, 0.2-0.6 m wide", where the
  # measurements follow the plant's description, and Proboscidea "stout stem and taproot, procumbent, spreading to 2 m diam." are the plant's)
  stem_w <- NULL
  if (!is.na(wd_pos) && !str_detect(wd$txt, rx(spread_re)) &&
      str_detect(str_sub(join_units(habit), 1, wd_pos - 1), rx("\\b(?:stems?|trunks?|canes?|culms?|branch(?:es|lets)?)\\s+(?:(?:to|up to|c\\.)\\s+)?[0-9][^,;|]*?\\b(?:high|tall|long)\\b,\\s*$"))) {
    stem_w <- wd; wd <- NULL
  }
  # (spread wording that names the stems: Banksia "Shrub with underground stems forming open tufts to 60 cm diam", Chordifex "forming
  # patches of culms to 1 m across")
  if (is.null(wd) && is.null(stem_w)) {
    sp <- str_match(mark_extremes(join_units(habit) %||% ""), rx(paste0(spread_re, "[^;|]{0,80}?(", meas, ")\\s*(?:across|diam\\.?|diameter|in diam\\w*|wide)")))
    # (not a measurement attached to a trunk or stem: Austrocylindropuntia "spreading shrub 1-3 m high, often with woody trunk to 10 cm
    # diam"; "patches of culms to 1 m across" is the patch)
    if (!is.na(sp[1]) && !str_detect(sp[1], rx("\\bat (?:the )?base\\b")) &&
        !str_detect(unmark(sp[1]), rx("(?<!\\bof )(?<!\\bof [a-z]{3,12} )\\b(?:trunks?|stems?|canes?|culms?|branch(?:es|lets)?)\\s+(?:(?:to|up to|c\\.)\\s+)?[0-9][^,;|]*$")))
      wd <- list(v = meas_of(sp[2]), txt = unmark(sp[1]))
  }
  # (not an underground organ's size: Loxocarya "forming small dense clumps from short slender rhizomes to 20 cm long and 1.8-2 mm diam.",
  # Nymphaea "with vertical tuber 1.5-3.5 cm long, 1-2 cm wide", Wurmbea "from an ovoid corm to 3 cm diam.": the organ is in the
  # measurement's own comma segment, or in the one before it ending "... long"; but Macarthuria "with a thick tap root, multi-stemmed,
  # 0.5-0.75 m tall, to c. 1 m across" is the plant's width)
  if (!is.null(wd)) {
    hj <- join_units(habit) %||% ""; wpos <- str_locate(hj, fixed(wd$txt))[1, 1]
    if (!is.na(wpos)) {
      segs <- str_split(paste0(str_extract(str_sub(hj, 1, wpos - 1), "[^;|]*$"), wd$txt), ",")[[1]]
      org_re <- rx("\\b(?:rhizomes?|stolons?|roots?|tubers?|bulbs?|corms?|cataphylls?)\\b")
      last <- segs[length(segs)]; prev <- if (length(segs) > 1) segs[length(segs) - 1] else ""
      # (within the measurement's own segment, the organ must come after any spread word: "clumps from ... rhizomes to 20 cm long ...")
      # (Loxocarya striata "forming large clumps with creeping rhizomes to 40 cm long, 3-4 mm diam.": the organ follows the spread word)
      org_after <- function(z) { o <- str_locate_all(z, org_re)[[1]]; sp <- str_locate_all(z, rx(spread_re))[[1]]
        nrow(o) > 0 && (!nrow(sp) || max(o[, 1]) > max(sp[, 1])) }
      if (org_after(last) || (org_after(prev) && str_detect(prev, rx("\\b(?:long|thick)\\s*$")))) wd <- NULL
    }
  }
  if (!is.null(wd)) addn("plant_width", wd$v, wd$txt, scale = 1 / 1000)

  # ---- stems, bark, underground organs
  stem <- unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig|shoot|cane|axes|culm")
  stem_txt <- join_units(stem)
  addc("stem_hairs", join_units(unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig|shoot|cane")), hairs_dict %>% { .[. != "glandular_pubescent"] } %>% c("glandular-(?:pubescent|hairy|pilose|puberulous)|glandular hairs" = "hairy"),
       prep_fun = function(x) str_remove_all(x, "\\b(?:when young|young parts?|at first|initially)\\b"))
  addc("stem_shape", join_units(unit_text(u, "stem", subj_re = "branchlet|branch|stem|twig")), stem_shape_dict)
  if (is.null(ht)) {
    # (orchids: "Canes 1-3 m tall", Arundina)
    # (Poaceae: Austrostipa turbinata "shoots intra- or extra-vaginal, 250-480 mm high")
    culm <- m_first(unit_text(u, "stem", subj_re = if (orchid_mode) "culm|stem|cane" else if (grass_mode) "culm|stem|shoot" else "culm|stem"), "tall|high",
                    excl("culms?", "stems?", if (orchid_mode) "canes?", if (grass_mode) "shoots?"))
    if (!is.null(culm)) addn("plant_height", culm$v, paste("[culm/stem]", culm$txt), scale = 1 / 1000)
    # palms: "Trunk to 25 m tall, 25-40 cm diam."
    if (is.null(culm)) {
      trk <- m_first(unit_text(u, "stem", subj_re = "^(?:the )?trunks?$"), "tall|high", excl("trunks?"))
      # (tree ferns: trunk height on a plant_organ_measured = trunk row, as in LucidFerns_2026)
      if (!is.null(trk)) addn("plant_height", trk$v, paste("[trunk]", trk$txt), if (fern_mode) "plant_organ_measured" else NA_character_, if (fern_mode) "trunk" else NA_character_, scale = 1 / 1000)
      # (grass-trees: "Trunk usually 1-2 m long", "Trunk none or up to 30 cm long": the trunk's height below the leaf crown, on a
      # plant_organ_measured = trunk row as for tree ferns)
      if (is.null(trk) && fam == "Xanthorrhoeaceae") {
        trk <- m_first(unit_text(u, "stem", subj_re = "^(?:the )?trunks?$"), "long", excl("trunks?"))
        if (!is.null(trk)) addn("plant_height", trk$v, paste("[trunk]", trk$txt), "plant_organ_measured", "trunk", scale = 1 / 1000)
      }
    }
  }
  # Poaceae, Cyperaceae, Juncaceae: culm / plant heights are flowering heights, written to both plant_height and plant_height_reproductive
  # (user, 2026-10-28); bamboo culms are persistent vegetative stems (plant_height only)
  if (fam %in% graminoid_rule_families && !word(taxon, 1) %in% bamboo_genera)
    for (k in seq_along(recs)) if (all(recs[[k]]$trait == "plant_height")) add(recs[[k]] %>% mutate(trait = "plant_height_reproductive"))
  # trunk / main stem diameter ("trunk to 2 m diam.", "stems to 3 cm diam.") from the opening sentences
  sdm <- m_first(unit_text(u %>% filter(si <= 2), "stem", subj_re = "^(?:the )?(?:trunks?|stems?|main stems?|culms?)$"), "diam|diameter|in diameter|across|wide", excl("trunks?", "stems?", "culms?"))
  if (is.null(sdm)) {
    sm2 <- str_match(mark_extremes(join_units(habit) %||% ""), rx(paste0("\\b(?:with (?:a )?)?(?:trunks?|stems?)\\b[^,;|]{0,20}?(", meas, ")\\s*(?:diam|diameter|in diameter)")))
    if (!is.na(sm2[1])) sdm <- list(v = meas_of(sm2[2]), txt = unmark(sm2[1]))
  }
  if (is.null(sdm) && !is.null(orch_sd$W)) sdm <- orch_sd$W
  # (Lomandra mucronata "stems forming dense mats up to 30 cm diam": the plant's spread, see plant_width)
  if (!is.null(sdm) && str_detect(sdm$txt, rx(spread_re))) sdm <- NULL
  if (is.null(sdm) && exists("stem_w") && !is.null(stem_w)) sdm <- list(v = stem_w$v, txt = paste("[stems]", stem_w$txt))
  if (!is.null(sdm)) addn("stem_diameter", sdm$v, sdm$txt)
  bark_txt <- join_units(c(unit_text(u, "bark", parts = "any")))
  addc("bark_texture", bark_txt, if (legume_mode) c(bark_texture_dict, "rimose|cracked|corrugated" = "furrowed", "minni[- ]?ritchi" = "flaky") else bark_texture_dict)
  addc("bark_colour", bark_txt, bark_colour_dict, prep_fun = function(x) {
    x <- str_remove_all(x, "\\b[a-z]+ when (?:wet|fresh|freshly exposed|new)\\b")
    str_replace_all(x, paste0("\\b(?:", cw_mod, ")\\s+(?=(?:", cw, ")\\b)"), "")
  })
  under_txt <- join_units(unit_text(u, "underground", parts = "any"))
  for (org in setdiff(c("bulb", "corm", "tuber", "pseudobulb", "rhizome", "lignotuber", "caudex"), if (fern_mode) c("rhizome", "caudex"))) {
    ou <- unit_text(u, "underground", subj_re = paste0("^(?:the )?", org, "s?$"))
    if (!length(ou)) next
    # (spread wording in the organ's clause is the plant's spread: Baloskion "Rhizome shortly horizontal, but plants forming patches to
    # 80 cm across")
    od <- dims(str_remove_all(join_units(ou), rx(paste0("(?:\\b(?:but |and )?plants? )?", spread_re, "[^,;|]*"))), excl(paste0(org, "s?")))
    if (!is.null(od$L)) addn("storage_organ_length", od$L$v, od$L$txt, "entity_measured", org)
    if (!is.null(od$W)) addn("storage_organ_diameter", od$W$v, od$W$txt, "entity_measured", org)
  }
  storage_txt <- collapse_text(c(habit_txt, under_txt, stem_txt), " | ")
  # (a plant can have both: "Pseudobulbs widely spaced on creeping rhizome" is scored pseudobulb + rhizome; user's rule 2026-10-27)
  # (absent organs are not scored: Rhipidorchis "Pseudobulbs absent", Cryptostylis "(no tubers)", "without pseudobulbs")
  addc("storage_organ", storage_txt, if (fern_mode) storage_dict[!str_detect(storage_dict, "^(?:rhizome|caudex)$")] else storage_dict, storage_neg,
       prep_fun = function(x) str_remove_all(x, rx(paste0("\\b(?:pseudobulbs?|tubers?|bulbs?|corms?|rhizomes?|stolons?)\\s+(?:absent|lacking|none|0)\\b",
                                                         "|\\b(?:without|lacking|lacks|no)\\s+(?:a\\s+|any\\s+)?(?:distinct\\s+|obvious\\s+)?(?:pseudobulbs?|tubers?|bulbs?|corms?|rhizomes?|stolons?)\\b"))))
  def_txt <- collapse_text(c(habit_txt, stem_txt, join_units(unit_text(u, "stipule", parts = "any"))), " | ")
  dh <- scan_terms(def_txt, defence_dict, defence_neg)
  lam_all <- unit_text(u, c("leaf", "lamina", "leaflet"), parts = "any")
  # (orchids: "apex sharply pointed" is an acute leaf tip, not a pungent one)
  ldh <- scan_terms(join_units(lam_all), if (orchid_mode) setNames(leaf_defence_dict, str_remove(names(leaf_defence_dict), "\\|sharply pointed")) else leaf_defence_dict)
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
  # Fabaceae: phyllodes "reduced to continuous, thin ... scales", or absent ("Branchlets aphyllous"): the green stems photosynthesise
  phy_txt <- str_to_lower(join_units(unit_text(u, "leaf", subj_re = "phyllode", parts = "any")) %||% "")
  phy_scale <- legume_mode && str_detect(phy_txt, "^(?:mature |adult )?phyllodes? (?:[a-z]+ )?(?:reduced to|represented by)\\b[^|]*\\bscales?\\b|\\bphyllodes? (?:are )?scale-like\\b")
  # ("Branchlets numerous, short, spinescent, commonly aphyllous" beside a described phyllode: only the spiny branchlets lack phyllodes)
  phy_absent <- legume_mode && (str_detect(phy_txt, "^(?:mature |adult )?phyllodes? (?:absent|0|lacking|wanting)\\b") ||
                                  (!is_phyllode && str_detect(str_to_lower(join_units(unit_text(u, c("habit", "stem"), parts = "any")) %||% ""), "\\baphyllous\\b")))
  # (Daviesia anceps "Branchlets modified to cladodes ... Phyllodes all reduced to small, exstipulate scales": the cladodes photosynthesise)
  stem_clad <- legume_mode && str_detect(str_to_lower(join_units(unit_text(u, c("habit", "stem"), parts = "any")) %||% ""), "\\b(?:cladodes?|phylloclad\\w*)\\b")
  if (is_phyllode && !phy_scale && !phy_absent) add(rec_cat("plant_photosynthetic_organ", tibble(pos = 1L, value = "phyllode", term = "phyllodes", qual = NA), join_units(leaf_gen)))
  if (phy_scale || phy_absent) {
    pst <- grab_sent(desc, if (stem_clad) "phyllode|aphyllous|leafless|cladode" else "phyllode|aphyllous|leafless")
    add(rec_cat("plant_photosynthetic_organ", tibble(pos = 1L, value = if (stem_clad) "cladode" else "stem",
                                                     term = if (stem_clad) "cladodes" else if (phy_scale) "phyllodes reduced to scales" else "aphyllous", qual = NA), pst))
    add(rec_cat("leaf_length_type", tibble(pos = 1L, value = if (phy_scale) "scale_leaves" else "leafless", term = if (phy_scale) "phyllodes reduced to scales" else "aphyllous", qual = NA), pst))
    add(rec_cat("leaf_type", tibble(pos = 1L, value = if (phy_scale) "scale" else "leafless", term = if (phy_scale) "phyllodes reduced to scales" else "aphyllous", qual = NA), pst))
  }
  if (is_cladode) add(rec_cat("plant_photosynthetic_organ", tibble(pos = 1L, value = "cladode", term = "cladodes", qual = NA), join_units(leaf_gen)))
  all_txt <- str_to_lower(desc %||% "")
  # ("flowering stems usually leafless" is not a leafless plant)
  # (only habit / stem / leaf clauses: "Inflorescence scape to 25 cm long, usually leafless")
  all_txt <- str_to_lower(join_units(unit_text(u, c("habit", "stem", "leaf", "lamina", "other"), parts = "any")) %||% "")
  all_txt <- str_remove_all(all_txt, "\\b(?:flowering |fertile |upper )?(?:stems?|scapes?|branches|shoots?|peduncles?|culms?|axes) (?:[a-z]+ ){0,2}leafless\\b")
  # (orchids: Micropera "lower half usually leafless", Grastidium "apical part often leafless", Dendrobium "a leafless apical region
  # where flowers are borne": part of the stem is leafless, not the plant)
  if (orchid_mode) all_txt <- str_remove_all(all_txt, "\\b(?:lower|upper|apical|basal|distal|proximal|terminal) (?:half|part|portion|section|region|third|two-thirds|nodes?)(?: of (?:the )?[a-z]+)? (?:[a-z]+ ){0,2}leafless\\b|\\bleafless (?:apical |basal |terminal |distal |upper |lower )?(?:region|portion|part|section|internodes?|nodes?)\\b")
  # (Praecoxanthus "Non-flowering plants consist of single leaf; flowering plants are leafless": leafless only while flowering, a
  # leaf_phenology value (leafless_while_flowering, user's choice 2026-10-27), not a leafless plant)
  all_txt <- str_remove_all(all_txt, leafless_flowering_re)
  # (part of the plant only: phyllanthoid Sauropus / Synostemon / Breynia "Stem leaves scale-like" with normal branchlet leaves, Limonium
  # "leafless above rosette", Callitriche "lower leaves reduced to ... scales")
  all_txt <- str_remove_all(all_txt, "\\bstem leaves (?:[a-z]+ ){0,2}scale-like\\b|\\bleafless above (?:the )?(?:basal )?rosettes?\\b|\\b(?:lower|basal|lowermost|upper|uppermost|stem) leaves (?:[a-z]+ ){0,2}reduced to\\b")
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
  # (Fabaceae: "basal gland", "basal pinnae", "basal pair of pinnae" are not basal leaves)
  addc("leaf_arrangement", gen_txt, arrangement_dict, prep_fun = if (legume_mode) function(x) str_remove_all(x, "\\bbasal\\b(?! leaves| rosettes?)(?: [a-z-]+)?") else NULL)
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
  # (Fabaceae: "simple or stellate hairs" are hairs, not simple leaves)
  cmp <- scan_terms(div_txt, compound_dict, prep_fun = function(x) str_remove_all(x, paste0("[a-z-]*(?:pinnate|palmate)(?:ly)? (?:veined|nerved|venation|veins)|(?:pinnately|palmately) (?:veined|nerved)|(?:pinnate|palmate) venation|venation (?:is )?(?:pinnate|palmate)",
                                                                                     if (legume_mode) "|\\bsimple\\b(?: (?:or|and|to) [a-z-]+)? (?:hairs|trichomes)|\\b[a-z-]+ (?:and|or) simple (?:hairs|trichomes)" else "")))
  # fern fronds that are only lobed ("pinnatifid", "deeply pinnatisect") are simple
  if (fern_mode && !any(cmp$value == "compound")) {
    sl <- scan_terms(div_txt, c("bipinnatifid|pinnatifid|pinnatisect|pinnatipartite|pinnately lobed|palmately lobed|lobed" = "simple"), generic_neg = FALSE)
    cmp <- bind_rows(cmp, sl) %>% arrange(pos)
  }
  if (!nrow(cmp) && length(leaflet)) cmp <- tibble(pos = 1L, value = "compound", term = "leaflets", qual = NA)
  add(rec_cat("leaf_compoundness", cmp, div_txt %||% join_units(leaflet)))
  expand_degrees <- function(x) {
    # (vectorised: Cheilanthes "2-3-pinnate at base, 1-2-pinnate above" has two matches)
    x <- str_replace_all(x, "\\b([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)", function(z) map_chr(z, function(zz) {
      m <- str_match(zz, "([1-4])\\s*(?:-|or|to)\\s*([1-4])-(pinnate|pinnatifid|pinnatisect)")
      paste(paste0(seq(as.integer(m[2]), as.integer(m[3])), "-", m[4]), collapse = " ") }))
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
  # (Fabaceae bipinnate leaves: the leaflets described are the pinnules)
  if (legume_mode && compound_leaf && any(str_detect(coalesce(u$subj[u$sec == "leaflet"], ""), "pinnule"))) shape_ctx <- c("leaf_division", "pinnules")
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
  # Fabaceae phyllodes: apex phrases inside other tokens ("tapered to a curved-acuminate to caudate", "with obtuse and mucronate
  # apex", "obliquely mucronate to excentrically rostellate", "with ± pungent mucro"); tokens about the base or margins are skipped
  apex_leg <- character(0)
  if (legume_mode && !compound_leaf) for (x in leaf_shape_unit) for (tk in split_tokens(str_to_lower(x))) {
    if (!str_detect(tk, "\\b(?:ap(?:ex|ices|ical)|tips?|mucro\\w*|cusp\\w*|points?|rostell\\w*|rostrate|acuminate|acute|obtuse|caudate|apiculate|subacute|subobtuse|truncate|emarginate|retuse|uncinate)\\b")) next
    if (str_detect(tk, "\\b(?:margins?|veins?|nerves?|midrib|glands?|base|basally|pulvinus)\\b") && !str_detect(tk, "\\bap(?:ex|ices)\\b")) next
    apex_leg <- c(apex_leg, str_remove_all(tk, "\\b(?:[a-z-]+ )?(?:at|towards|near) (?:the )?base\\b|\\bacutely\\b|\\bobtusely angled\\b"))
  }
  apex_txt <- collapse_text(c(apex_parts, apex_inline, apex_bare, apex_leg), " | ")
  apex_d <- if (legume_mode) c("subacute" = "acute", "subobtuse" = "obtuse", "rostellate|rostrate|rostriform|cusp" = "apiculate",
                               "uncinate|hooked" = "", apex_dict) else apex_dict
  addc("leaf_apex_shape", apex_txt, apex_d, prep_fun = function(x) str_remove_all(x, "\\bap(?:ex|ices)\\b|\\btips?\\b|\\bapically\\b|\\bat\\b"), ctx_type = shape_ctx[1], ctx_value = shape_ctx[2])
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
    # (Fabaceae: a segment naming the margin, veins or glands goes whole: "with margin often dark red in upper third of pinnule")
    if (legume_mode) x <- str_remove_all(x, "[^,;|]*\\b(?:midrib|veins?|glands?|margins?|hairs|indumentum)\\b[^,;|]*")
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
  # (Fabaceae: "acute with a deciduous callus point" is not deciduous foliage)
  # (Orchidaceae: "Leaf often withered at flowering", "Leaf withered at flowering time")
  # (and "flowering plants leafless", "leafless at flowering": leafless_while_flowering, from whichever sentence says so)
  lwf_txt <- unlist(str_extract_all(str_to_lower(desc %||% ""), paste0("[^.;|]*(?:", leafless_flowering_re, ")[^.;|]*")))
  addc("leaf_phenology", collapse_text(c(habit_txt, gen_txt, lwf_txt), " | "),
       c(if (orchid_mode) c(phenology_dict, "(?:withered|withering|shrivelled|dried) (?:at|by|before) (?:the time of )?(?:flowering|anthesis)(?: time)?" = "withered_at_flowering") else phenology_dict,
         setNames("leafless_while_flowering", leafless_flowering_re)),
       prep_fun = if (legume_mode) function(x) str_remove_all(x, "\\b(?:semi-)?deciduous (?!phyllodes|leaves|foliage|shrubs?|trees?|habit)[a-z-]+") else NULL)

  # juvenile leaves
  juv <- join_units(unit_text(u, "juvenile", parts = "any"))
  addc("leaf_hairs_juvenile_leaves", juv, hairs_dict)
  addc("leaf_glaucousness_juvenile_leaves", juv, glaucous_dict, glaucous_neg)

  # leaf sizes
  lf_other <- excl("leaves", "leaf", "blades?", "lamina", "phyllodes?", "cladodes?")
  # (Acacia "uncinate with mucro 0.2-0.5 mm long, terete, commonly 8-16 mm long", "pungent, cusp slender and c. 1 mm long, 5-9 mm long")
  if (legume_mode) lf_other <- paste0(lf_other, "|mucros?|cusps?|points?|spinules?|apiculum")
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
  # Fabaceae bipinnate leaves: pinna and pinnule sizes on leaf_division rows (as for ferns)
  leg_div <- legume_mode && any(str_detect(coalesce(u$subj[u$sec == "leaflet"], ""), "pinnule|pinnae|pinna\\b"))
  if (leg_div) for (sj in unique(na.omit(u$subj[u$sec == "leaflet"]))) {
    uu <- unit_text(u, "leaflet", subj_re = paste0("^", sj, "$"))
    lfd <- d_first(uu, excl("leaflets?", "pinnae", "pinna", "pinnules?"))
    dv <- str_replace(str_replace(sj, "^pinna$", "pinnae"), "^(?:the )?", "") %>% str_replace("pinnule$", "pinnules") %>% str_replace("leaflet$", "leaflets")
    if (!is.null(lfd$L)) addn("leaflet_length", lfd$L$v, lfd$L$txt, "leaf_division", dv)
    if (!is.null(lfd$W)) addn("leaflet_width", lfd$W$v, lfd$W$txt, "leaf_division", dv)
  }
  if (length(leaflet) && !fern_mode && !leg_div) {
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
      # ("Fronds erect, 4-25 cm tall": Ophioglossum, Botrychium)
      fr_l <- m_first(unit_text(u, "leaf", subj_re = "frond"), "long|in length|tall|high", excl("fronds?", "leaves", "leaf"))
      if (!is.null(fr_l)) addn("plant_height", fr_l$v, paste("[fronds]", fr_l$txt), "plant_organ_measured", "fronds", scale = 1 / 1000)
    }
  }
  # leaflet counts: "with 5-9 leaflets", "leaflets 5-9", "leaflets 1.5-3-jugate", "2-5 pairs of leaflets", "pinnae unijugate"
  lc_txt <- c(leaf_gen, leaflet)
  jug <- c(unijugate = 1, bijugate = 2, trijugate = 3)
  for (x in lc_txt) {
    xl <- str_to_lower(str_remove_all(mark_extremes(x), "\u27e8[^\u27e9]*\u27e9"))
    # (Acacia wollarensis "jugary glands ... at base of upper 1-4 pairs of pinnae and occasionally on lower 1 or 2 pairs": gland positions)
    if (legume_mode) xl <- str_remove_all(xl, "\\b(?:upper|lower|uppermost|lowermost|lowest|basal|terminal|distal|proximal|top|first|last|apical) [0-9]+(?:\\s*(?:-|or|to)\\s*[0-9]+)? pairs?(?: of (?:pinnae|leaflets?|pinnules?))?")
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

  # ---- Fabaceae leaves and phyllodes (Acacia and allies)
  if (legume_mode) {
    lf_any <- unit_text(u, c("leaf", "lamina", "petiole", "leaflet"), parts = "any")
    lf_tok <- unlist(map(lf_any, split_tokens))
    # leaf type from the cross-section of phyllodes / leaves / leaflets ("flat", "terete", "± pentagonal in section, rarely
    # flat", "quadrangular"): flat -> broadleaf, terete / angular / needle-like -> needle (traits.yml: broadleaf = flat lamina;
    # needle = needle or awl-shaped, generally 3-dimensional)
    if (!phy_scale && !phy_absent) {
      lt_src <- join_units(unit_text(u, c("leaf", "lamina", "leaflet")))
      addc("leaf_type", lt_src, c("flat|flattened|flattish|plane|bifacial|dorsiventrally flattened" = "broadleaf",
                                  "terete|subterete|semi-?terete|sub-terete|(?:sub)?quadrangular|tetragonal|tetragonous|pentagonal|hexagonal|triquetrous|trigonous|(?:triangular|rhombic|quadrate) in (?:cross[- ])?section|acicular|needle-like|needle-shaped|filiform|setaceous|subulate" = "needle"),
           # (qualifying phrases go first, so "flat with a vein-like upper margin", "flat but appearing angular ... due to raised veins" keep "flat")
           prep_fun = function(x) str_remove_all(str_remove_all(x, "\\b(?:with|due to|by|near|at|towards|except)\\b[^,;|]*"), "[^,;|]*\\b(?:margins?|veins?|nerves?|midribs?|glands?|pulvinus|ap(?:ex|ices)|tips?|mucro\\w*|bases?|stem-projections|branchlets?)\\b[^,;|]*|\\bflat-topped\\b|\\bflattened (?:vertically|laterally)\\b"))
    }
    # venation (candidate trait leaf_venation; draft values pinnate, parallel, anastomosing, reticulate, striate, in text order)
    # (phyllodes and simple leaves only: the veins of leaflets / pinnules are described differently)
    vn_tok <- unlist(map(unit_text(u, c("leaf", "lamina"), parts = "any"), split_tokens))
    vn_tok <- vn_tok[str_detect(vn_tok, rx("vein|nerv|striat|reticul|anastom|anastam|penni|venation|venules|midrib|midvein|\\bper mm\\b"))]
    if (length(vn_tok) && !compound_leaf) {
      vn_txt <- paste(vn_tok, collapse = " | ")
      addc("leaf_venation", vn_txt, c("penni-?(?:veined|nerved)|pinnately (?:veined|nerved)|pinnate venation" = "pinnate", "(?:sub)?parallel" = "parallel",
                                      "anastomos\\w*|anastamos\\w*|anatomos\\w*" = "anastomosing", "reticulat\\w*|reticulum|net-like|(?:vein|nerve)-islands|reticulately" = "reticulate",
                                      "multi-?striate|striate" = "striate"),
           neg = c("(?:not|non-?|never) ?anastomos\\w*|nonanastomosing|without anastomoses|not reticulate|veinless|nerveless" = ""))
      # main (longitudinal) vein count: "with 3 or 4 distant raised main veins", "1-veined per face", "with 5 prominent yellowish veins"
      # (minor / secondary / lateral veins are not counted; "numerous" gives no number)
      vc <- count_after(vn_tok %>% str_remove_all(rx("\\b[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:\\([^)]*\\)\\s*)?(?:per mm|mm)\\b")),
                        paste0("\\bwith ", cnt, " (?!(?:[a-z-]+,? ){0,6}?(?:minor|secondary|lateral|less|fine|faint))(?:[a-z-]+,? (?:and |to )?){0,6}?(?:veins|nerves)\\b|\\b",
                               cnt, "-(?:veined|nerved)\\b(?: per face)?|^(?:main |longitudinal |major |primary )(?:veins|nerves) ", cnt, "(?![0-9.]|\\s*(?:mm|per))"))
      if (!is.null(vc)) addn("leaf_main_vein_count", parse_count(vc$raw), vc$txt)
      # longitudinal vein spacing ("longitudinal veins numerous, 3 or 4 (-5) per mm"): candidate leaf_parallel_veins_per_mm
      vd <- count_after(paste(vn_tok, collapse = ", "), paste0("\\b", cnt, " per mm\\b"))
      if (!is.null(vd)) addn("leaf_parallel_veins_per_mm", parse_count(vd$raw), vd$txt)
    }
    # foliar glands of Acacia and other legumes (phyllode margin, petiole, rachis, between leaflets) are extrafloral nectaries
    # (candidate traits: extrafloral_nectaries present / absent, extrafloral_nectary_position, _count, _distance_above_pulvinus)
    # (a gland clause carries on to the clause end: "gland 1, prominent, basal")
    part_units <- function(re) u %>% filter(sec %in% c("leaf", "lamina", "petiole", "leaflet"), str_detect(coalesce(part, ""), re)) %>%
      group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
    # (a gland clause misplaced after the stipules or branchlets, "gland 5-22 mm above pulvinus", is the phyllode's too)
    gl_other <- u %>% filter(!sec %in% c("leaf", "lamina", "petiole", "leaflet"), str_detect(coalesce(part, ""), "^glands?$")) %>%
      group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t) %>% { .[str_detect(., rx("pulvinus|phyllode"))] }
    gl_tok <- unique(c(part_units("^glands?$"), gl_other, lf_tok[str_detect(lf_tok, rx("\\bglands?\\b|\\beglandular\\b|\\bglanduliferous\\b|\\bgland-bearing\\b"))])) %>%
      str_remove_all(rx("\\bgland-tipped [a-z-]+|\\bglandular[a-z-]*\\b|\\b(?:hairs?|trichomes|hairlets) [^,;|]*\\bglands?\\b|\\bresin[- ]glands?\\b"))
    gl_tok <- gl_tok[str_detect(gl_tok, rx("\\bglands?\\b|\\beglandular\\b|\\bglanduliferous\\b|\\bgland-bearing\\b"))]
    if (length(gl_tok)) {
      gl_txt <- paste(gl_tok, collapse = " | ")
      gh <- scan_terms(gl_txt, c("glands?|glanduliferous|gland-bearing" = "present"),
                       neg = c("(?:(?:inter)?jugary |petiolar |leaf |phyllode |basal |apical )?glands? (?:absent|lacking|wanting|not seen|0(?!\\s*(?:-|\\.|[0-9]|mm|cm)))|eglandular|without (?:a |any )?glands?|lacking (?:a )?glands?|no glands?" = "absent"),
                       generic_neg = FALSE)
      # (some glands present: "interjugary glands absent" beside a jugary gland is not an absence)
      if (any(gh$value == "present")) gh <- gh %>% filter(value == "present")
      add(rec_cat("extrafloral_nectaries", gh, gl_txt))
      if (any(gh$value == "present")) {
        gl_pos <- str_remove_all(str_to_lower(gl_txt), "(?:(?:inter)?jugary |petiolar |leaf |phyllode |basal |apical )?glands? (?:absent|lacking|wanting|not seen)[^,;|]*|without (?:a |any )?(?:[a-z]+ )?glands?[^,;|]*")
        addc("extrafloral_nectary_position", gl_pos, c("interjugary" = "rachis_interjugary", "jugary|at (?:each|the) (?:pair|junction) of pinnae|between (?:the )?(?:[a-z]+ )?pairs? of pinnae" = "rachis_jugary",
                                                      "between (?:the )?(?:[a-z]+ )?(?:pairs? of )?leaflets?|between (?:the )?(?:lowest|lowermost|lower|basal|upper|each|all|first|proximal) (?:[a-z]+ )?(?:pairs?|leaflets?)|on (?:the )?rachis" = "rachis",
                                                      "on (?:the )?petiole|petiolar|near (?:the )?(?:lowest|lowermost|basal) pair of pinnae|on (?:the )?upper (?:surface|side) of (?:the )?petiole" = "petiole",
                                                      "(?:just |immediately )?above (?:the )?pulvinus|(?:at|near|towards) (?:the )?base|basal|proximal|from (?:the )?pulvinus|on (?:the )?pulvinus" = "base",
                                                      "(?:at|near|about|above|below) (?:the )?(?:middle|mid-point|midpoint)|median" = "middle",
                                                      "(?:at|near|towards) (?:the )?apex|apical|subapical|distal|near (?:the )?tip|below (?:the )?apex" = "apex"),
             generic_neg = FALSE)
        gd <- str_match(mark_extremes(gl_txt), rx(paste0("\\b(?:glands?|lowermost|lowest)\\b[^;|]{0,60}?(", meas, ")\\s*(?:above|from) (?:the )?(?:vestigial )?(?:pulvinus|base(?: of (?:the )?phyllode)?|phyllode (?:base|axil))")))
        if (!is.na(gd[1])) addn("extrafloral_nectary_distance_above_pulvinus", meas_of(gd[2]), unmark(gd[1]))
        gc <- count_after(gl_tok, paste0("^(?:the )?(?:[a-z]+ )?glands? ", cnt, "(?![0-9.]|\\s*(?:-|or|to)\\s*[0-9]|\\s*(?:mm|cm)\\b|\\s*\\(|\\s*\u27e8)|\\b", cnt, " (?:[a-z-]+ )?glands?\\b"))
        if (is.null(gc) && str_detect(gl_txt, rx("\\bglands? (?:single|solitary)|\\b(?:single|solitary|a single) gland\\b|^gland 1\\b")))
          gc <- list(v = c(min = NA, max = 1), txt = str_extract(gl_txt, rx("\\bglands? (?:single|solitary)|\\b(?:single|solitary|a single) gland\\b")))
        if (!is.null(gc)) addn("extrafloral_nectary_count", if (!is.null(gc$raw)) parse_count(gc$raw) else gc$v, gc$txt)
      }
    }
    # pulvinus length (candidate pulvinus_length): "pulvinus 4-8 (-10) mm long"
    pv <- m_first(part_units("^pulvin(?:us|i)$"), "long|in length", excl())
    # leaf rachis length (candidate leaf_rachis_length): "rachis 1-12 (-17) cm long"
    rch <- m_first(part_units("^rachis$"), "long|in length", excl("rachis"))
    if (!is.null(rch)) addn("leaf_rachis_length", rch$v, rch$txt)
    # phyllodes per whorl / node (candidate leaves_per_node): "Phyllodes 15-20 per whorl", "in clusters of (2) 3-5 per node"
    lpn <- count_after(unit_text(u, "leaf"), paste0("\\b", cnt, " per (?:whorl|node|cluster|fascicle)\\b|\\bwhorls? of ", cnt, "\\b"))
    if (!is.null(lpn)) addn("leaves_per_node", parse_count(lpn$raw), lpn$txt)
    # mucro / cusp length (candidate leaf_mucro_length): "tapered at apex to cusp 1.5-3.5 mm long", "with a straight point 0.1-0.3 mm long"
    mcr <- str_match(mark_extremes(paste(lf_tok, collapse = " | ")), rx(paste0("\\b(?:mucros?|cusps?|points?|spinules?|setae?|tips?)\\b[^,;|]{0,25}?(", meas, ")\\s*long")))
    if (!is.na(mcr[1])) addn("leaf_mucro_length", meas_of(mcr[2]), unmark(mcr[1]))
    if (!is.null(pv)) addn("pulvinus_length", pv$v, pv$txt)
    # length : width ratio of phyllodes ("l:w ratio = 11-30", "l:w = 8-15")
    lw <- str_match(paste(lf_tok, collapse = " | "), rx("\\bl\\s*:\\s*w(?:\\s*ratio)?\\s*(?:=|of|is|c\\.)?\\s*(?:c\\.\\s*)?([0-9.]+)(?:\\s*-\\s*([0-9.]+))?"))
    if (!is.na(lw[1])) addn("leaf_length_to_width_ratio", c(min = if (is.na(lw[3])) NA else as.numeric(lw[2]), max = as.numeric(coalesce(lw[3], lw[2]))), lw[1])
    # symmetry and curvature of the blade (candidate leaf_symmetry, leaf_curvature), from the shape tokens only
    sh_tok <- str_remove_all(lf_tok, "\\([^)]*\\)")
    sh_tok <- sh_tok[!str_detect(sh_tok, rx("\\b(?:veins?|nerves?|midribs?|glands?|pulvinus|margins?|hairs?|ap(?:ex|ices)|mucro\\w*|tips?|points?|bases?|basally|tapered|tapering|narrowed|acuminate|caudate|acute|obtuse)\\b"))]
    if (!compound_leaf && length(sh_tok)) {
      sh_txt <- paste(sh_tok, collapse = " | ")
      addc("leaf_symmetry", sh_txt, c("dimidiate(?:ly)?|inequilateral(?:ly)?|asymmetric(?:al|ally)?|oblique(?:ly)?|unequal-sided" = "asymmetric", "symmetric(?:al|ally)?|equilateral" = "symmetric"),
           prep_fun = function(x) str_remove_all(x, "\\bobliquely (?=[a-z-]*(?:mucro|apic|acut|obtus|rostel|point|cusp|truncat|retus|emargin|ascend|erect|spread|patent|inclin|reflex|deflex))|\\bobliquely (?:ascending|erect|spreading|inclined|patent)\\b"))
      addc("leaf_curvature", sh_txt, c("(?:sub)?straight|straightish" = "straight", "incurved" = "incurved", "recurved" = "recurved", "(?:sub)?falcate(?:ly)?" = "falcate",
                                       "sigmoid(?:al)?|s-shaped" = "sigmoid", "curved|arcuate|curving" = "curved", "uncinate|hooked" = "uncinate"))
    }
  }

  # ---- inflorescence
  infl <- unit_text(u, "inflorescence")
  fl_gen <- unit_text(u, "flower", subj_re = "flower|floret|^(?:fe)?males$|ones$")
  infl_txt <- collapse_text(c(infl, fl_gen), " | ")
  addc("inflorescence_type", infl_txt, infl_type_dict,
       prep_fun = function(x) str_remove_all(x, "\\b(?:[0-9]+|one|two|three)?-?headed\\b|\\bfruiting heads?\\b|\\bspine-tipped\\b|\\bspines?\\b|\\b(?:flowers |dichasia )?(?:solitary|single)(?: or [a-z ]+?)? (?:on|at|along) (?:each |the )?(?:nodes?|rachis)\\b"))
  addc("inflorescence_shape", join_units(unit_text(u, "inflorescence", subj_re = "head|capitul|spike|glomerule|umbel")),
       if (legume_mode) c(infl_shape_dict, "obloid|short-obloid|ellipsoid|ovoid" = "elongated") else infl_shape_dict)
  # (Pimelea "Inflorescence: receptacle 3.5-6 mm long at first", Acacia incognita heads "4-6 x 4-5 mm; receptacles usually 1-2 mm
  # long": the receptacle, not the inflorescence; but an Acacia spike's receptacle is its axis, Acacia sp. Castletower "spikes
  # interrupted; receptacle 8-11 cm long", and A. pudica "spikes (judging from receptacles) 20-25 mm long")
  rcp_excl <- !legume_mode || !str_detect(str_to_lower(join_units(unit_text(u, "inflorescence")) %||% ""), "\\bspikes?\\b")
  in_other <- paste0(excl("inflorescences?", "racemes?", "spikes?", "heads?", "flowers?"), "|peduncled|pedunculate", if (rcp_excl) "|receptacles?(?!\\s*\\))")
  il <- m_first(unit_text(u, "inflorescence", subj_re = "inflorescence|raceme|spike|panicle|cyme|thyrse|corymb|synflorescence|conflorescence|spadix"), "long|in length", in_other)
  # (Fabaceae: the length of the heads / spikes is the inflorescence length; a raceme or panicle bearing them goes on an
  # entity_measured = raceme axis / panicle row: "Inflorescences rudimentary racemes 1-3 mm long ...; spikes 25-50 mm long")
  if (legume_mode) {
    il <- m_first(unit_text(u, "inflorescence", subj_re = "spike|head|capitul"), "long|in length", in_other)
    # ("heads 4-6 x 4-5 mm (when dry)", "spikes 7-10 x 4-5 mm")
    hdd <- d_first(unit_text(u, "inflorescence", subj_re = "spike|head|capitul"), in_other)
    if (is.null(il) && !is.null(hdd$L) && str_detect(hdd$L$txt, "\\u00d7| x | by ")) il <- hdd$L
    ilr <- m_first(unit_text(u, "inflorescence", subj_re = "inflorescence|raceme|panicle|cyme|thyrse|corymb"), "long|in length", in_other)
    ilr_u <- str_to_lower(join_units(unit_text(u, "inflorescence", subj_re = "inflorescence|raceme|panicle|cyme|thyrse|corymb")) %||% "")
    if (!is.null(ilr)) addn("inflorescence_length", ilr$v, ilr$txt, "entity_measured", if (str_detect(ilr_u, "panicl") && !str_detect(ilr_u, "racem")) "panicle" else "raceme axis")
  }
  if (!is.null(il)) addn("inflorescence_length", il$v, il$txt)
  # orchids: "Flower stem 50-200 mm long / tall" (mm here; converted after the substrate is known: plant_height_reproductive for
  # terrestrial orchids, inflorescence_length for epiphytes and lithophytes)
  if (orchid_mode) {
    fs <- m_first(unit_text(u, "inflorescence", subj_re = "flower stem"), "long|tall|high|in length", excl("stems?", "flowers?", "inflorescences?", "racemes?"))
    # (Thelymitra "Flower stem 200-1000 × 4-9 mm": length × diameter, the unit given once at the end)
    if (is.null(fs)) {
      fsx <- str_match(join_units(unit_text(u, "inflorescence", subj_re = "flower stem")) %||% "",
                       rx(paste0("\\bflower stems?\\b[^;:|0-9]{0,15}?(", num, "(?:\\s*-\\s*", num, ")?)\\s*\u00d7\\s*", num, "(?:\\s*-\\s*", num, ")?\\s*(mm|cm|m)\\b")))
      if (!is.na(fsx[1])) fs <- list(v = meas_of(paste(fsx[2], fsx[3])), txt = fsx[1])
    }
    if (!is.null(fs)) addn("orchid_flower_stem_length", fs$v, fs$txt)
  }
  idm <- m_first(unit_text(u, "inflorescence", subj_re = "head|capitul|umbel|glomerule"), "diam|diameter|in diameter|across|wide", in_other)
  if (legume_mode && is.null(idm) && !is.null(hdd$W) && str_detect(hdd$W$txt, "\\u00d7| x | by ")) idm <- hdd$W
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
  # (orchids: a scape is the flower stem, so it follows the flower-stem rule below: Taeniophyllum, an epiphyte, "scape 4-6 mm long"
  # is not the plant's height)
  if (!is.null(scp)) { if (orchid_mode) addn("orchid_flower_stem_length", scp$v, paste("[scape]", scp$txt))
                       else addn("plant_height_reproductive", scp$v, paste("[scape]", scp$txt), scale = 1 / 1000) }
  ped <- m_first(c(unit_text(u, "peduncle", subj_re = "peduncle"), unit_text(u, "inflorescence")), "long|in length", excl("peduncles?", "scapes?"))
  if (!is.null(ped)) {
    pt <- str_match(mark_extremes(join_units(c(unit_text(u, "peduncle", subj_re = "peduncle"), unit_text(u, "inflorescence")))), rx(paste0("\\b(?:peduncles?)\\b[^;:|]{0,40}?(", meas, ")(?:\\s+or more)?\\s*long")))
    if (!is.na(pt[1])) addn("peduncle_length", meas_of(pt[2]), unmark(pt[1]))
    else if (length(unit_text(u, "peduncle", subj_re = "peduncle"))) addn("peduncle_length", ped$v, ped$txt)
  }
  if (legume_mode) {
    # Acacia: "Inflorescences simple, 1 or 2 per axil", "normally 1 or 2 in phyllode axils" (candidate inflorescences_per_axil);
    # "3-5-headed racemes" (candidate flower_heads_per_inflorescence); "raceme axes 6-40 mm long" (inflorescence_length of the raceme axis)
    ipa <- count_after(infl, paste0("\\b", cnt, " (?:per|in each) (?:phyllode |leaf |upper )?(?:axils?|nodes?)\\b|\\b", cnt, " in (?:the )?(?:upper |lower )?(?:phyllode |leaf |scale |)?axils?\\b|\\b",
                                    cnt, " in axils of\\b"))
    if (!is.null(ipa)) addn("inflorescences_per_axil", parse_count(ipa$raw), ipa$txt)
    hpi <- count_after(infl, paste0("\\b", cnt, "-headed\\b|\\bracemes? (?:with |of )?", cnt, " heads?\\b|\\bwith ", cnt, " heads\\b"))
    if (!is.null(hpi)) addn("flower_heads_per_inflorescence", parse_count(hpi$raw), hpi$txt)
    rax <- str_match(mark_extremes(join_units(infl) %||% ""), rx(paste0("\\b(?:raceme |racemes |rachis |inflorescence )?ax(?:is|es)\\b[^,;|]{0,15}?(", meas, ")(?:\\s+or more)?\\s*long")))
    # ("Axes of racemes 4-16 mm long" as its own sentence)
    if (is.na(rax[1])) rax <- str_match(mark_extremes(desc %||% ""), rx(paste0("\\b(?:raceme ax(?:is|es)|ax(?:is|es) of (?:the )?racemes?)\\b[^,;|.]{0,15}?(", meas, ")(?:\\s+or more)?\\s*long")))
    # (Albizia "Inflorescence ... paniculate; axis 8-24 cm long", Bauhinia "Racemes few-flowered ...; rachis 5-15 mm long": an axis /
    # rachis clause of the inflorescence sentence)
    infl_any <- join_units(unit_text(u, "inflorescence", parts = "any")) %||% ""
    if (is.na(rax[1])) rax <- str_match(mark_extremes(infl_any), rx(paste0("(?:^|\\|\\s*)(?:ax(?:is|es)|rach(?:is|ides))\\b[^,;|]{0,15}?(", meas, ")(?:\\s+or more)?\\s*long")))
    if (!is.na(rax[1])) addn("inflorescence_length", meas_of(rax[2]), unmark(str_remove(rax[1], "^\\|\\s*")), "entity_measured",
                             if (str_detect(str_to_lower(infl_any), "panicl") && !str_detect(str_to_lower(infl_any), "racem")) "panicle" else "raceme axis")
    # peduncle width (candidate peduncle_diameter): "peduncles ... 1-4 mm wide", "0.5-0.8 mm diam."
    pdd <- m_first(unit_text(u, "peduncle", subj_re = "peduncle"), "wide|diam|diameter|in diameter|thick", excl("peduncles?"))
    if (!is.null(pdd)) addn("peduncle_diameter", pdd$v, pdd$txt)
    # spike width ("spikes c. 15 mm long, c. 5 mm diam.")
    if (is.null(idm)) {
      # (not the axis: Acacia synantha "spikes 30-40 mm long ...; receptacles slender (c. 0.4 mm diam.)")
      sdi <- m_first(unit_text(u, "inflorescence", subj_re = "spike"), "diam|diameter|in diameter|across|wide", paste0(in_other, "|receptacles?|rachis"))
      if (!is.null(sdi)) addn("inflorescence_diameter", sdi$v, sdi$txt)
    }
    # heads arranged in racemes / panicles: flower counts and diameters of the heads are on entity_measured = flower heads rows
    # (only when there are heads or spikes: Chamaecrista "Racemes terminal, c. 10-flowered" counts the raceme's own flowers)
    if (str_detect(str_to_lower(join_units(infl) %||% ""), "racem|panicl|-headed") &&
        str_detect(str_to_lower(join_units(c(infl, fl_gen)) %||% ""), "\\b(?:heads?|spikes?|capitul\\w*|-headed)\\b")) for (k in seq_along(recs)) {
      if (all(recs[[k]]$trait %in% c("flowers_per_inflorescence", "inflorescence_diameter")) && all(is.na(recs[[k]]$ctx_type)) &&
          str_detect(str_to_lower(paste(recs[[k]]$desc, collapse = " ")), "flowered|diam|wide|across|flowers"))
        recs[[k]] <- recs[[k]] %>% mutate(ctx_type = "entity_measured", ctx_value = if (any(str_detect(coalesce(u$subj[u$sec == "inflorescence"], ""), "head"))) "flower heads" else "spikes")
    }
  }

  # ---- flowers (sizes per entity: male / female flowers on their own rows)
  ents <- unique(u$entity[!is.na(u$entity)])
  if (grass_mode) ents <- ents[!str_detect(ents, rx(paste0("^", grass_qual, " ")))]
  for (en in c(NA, ents)) {
    uu <- if (is.na(en)) u %>% filter(is.na(entity)) else u %>% filter(entity == en)
    ct <- if (is.na(en)) c(NA, NA) else c("entity_measured", paste(str_replace(en, "staminate", "male") %>% str_replace("pistillate", "female"), "flowers"))
    # ("Male flowers in disjunct glomerules ... forming panicles 6-25 cm long": the sizes after "in ... panicles" are the inflorescence's)
    fl_u0 <- unit_text(uu, "flower", subj_re = "flower|floret|^(?:fe)?males$|ones$")
    # "Flowers in thyrses or racemes, to 50 cm long": a bare size in cm / m right after the inflorescence phrase is the
    # inflorescence length (recorded as such when the inflorescence has no length of its own)
    infl_re <- "\\bin (?:[a-z0-9-]+ ){0,3}(?:thyrses?|racemes?|panicles?|spikes?|cymes?|umbels?|subumbels?|heads?|corymbs?|inflorescences?|dichasi(?:a|um)|monochasi(?:a|um)|catkins?|amenta)(?: or (?:[a-z0-9-]+ ){0,2}(?:thyrses?|racemes?|panicles?|spikes?|cymes?|umbels?|heads?|corymbs?|subumbels?|dichasi(?:a|um)|monochasi(?:a|um)))?,?\\s*((?:(?:to|up to|c\\.|mostly|about)\\s+)*[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:cm|m) long)"
    for (x in fl_u0) { im <- str_match(x, rx(infl_re))
      if (!is.na(im[1]) && !any(map_lgl(recs, ~ "inflorescence_length" %in% .x$trait))) { v <- meas_of(im[2]); if (!is.null(v)) addn("inflorescence_length", v, im[2], ct[1], ct[2]) } }
    fl_u0 <- str_remove(fl_u0, rx(paste0("(?<=thyrses|thyrse|racemes|raceme|panicles|panicle|spikes|spike|cymes|cyme|umbels|umbel|heads|head|corymbs|corymb|inflorescences|inflorescence|subumbels|dichasia|dichasium|monochasia|monochasium|catkins|catkin),?\\s*(?:(?:to|up to|c\\.|mostly|about)\\s+)*[0-9.]+(?:\\s*-\\s*[0-9.]+)?\\s*(?:cm|m) long")))
    fl_u <- fl_u0 %>%
      # (only that phrase and any bare measurements right after it: "in a spike c. 3 mm diam., to 10 mm long")
      # (bare measurements after it are the inflorescence's only when the phrase itself is sized: "in a spike c. 3 mm diam., to 10 mm
      # long"; in "Flowers paired or in clusters of up to 5, 1-2 mm long" the size is the flower's)
      str_remove(rx("\\b(?:congested |arranged |borne |crowded )?in (?:[a-z0-9-]+ ){0,3}(?:spikes?|panicles?|glomerules?|clusters?|heads?|racemes?|cymes?|umbels?|fascicles?|inflorescences?)\\b(?:[^,;|]*\\b(?:mm|cm|m)\\b[^,;|]*(?:,\\s*(?:to |c\\. |up to |about )?[0-9][^,;|]*)*|[^,;|]*)"))
    # (Fabaceae: "flowers in globular or obloid heads or cylindrical spikes, 5-22 mm long": a bare size after the head / spike phrase is the
    # inflorescence's, never the flower's)
    if (legume_mode) {
      hs <- str_match(fl_u0, rx(paste0("\\bin (?:[a-z0-9-]+ ){0,4}(?:heads?|spikes?|racemes?)\\b[^;|0-9]*?,\\s*(", meas, ")\\s*(?:long|in length)")))
      if (any(!is.na(hs[, 1]))) {
        k <- which(!is.na(hs[, 1]))[1]
        if (!any(map_lgl(recs, ~ "inflorescence_length" %in% .x$trait))) addn("inflorescence_length", meas_of(hs[k, 2]), unmark(hs[k, 1]), ct[1], ct[2])
        fl_u[k] <- str_remove(fl_u[k], rx(paste0(",?\\s*", meas, "\\s*(?:long|in length)")))
      }
    }
    fd <- d_first(fl_u, excl("flowers?", "florets?"))
    if (!is.null(fd$L)) addn("flower_length", fd$L$v, fd$L$txt, ct[1], ct[2])
    fdm <- m_first(fl_u, "diam|diameter|in diameter|across|wide", excl("flowers?", "florets?"))
    if (!is.null(fdm)) addn("flower_diameter", fdm$v, fdm$txt, ct[1], ct[2])
    spk <- d_first(unit_text(uu, "flower", subj_re = "spikelet"), excl("spikelets?"))
    if (!is.null(spk$L)) addn("spikelet_length", spk$L$v, spk$L$txt, ct[1], ct[2])
    # Poaceae: florets per spikelet ("florets (9-) 21-81", "Spikelets 2-flowered", "with 3-5 florets"; candidate floret_count)
    if (grass_mode) {
      ftx <- unit_text(uu, "flower", subj_re = "spikelet|floret")
      lo_m <- "(?:\u27e8lo:[^\u27e9]+\u27e9\\s*)?"
      v <- if (length(ftx)) count_after(ftx, paste0("\\bflorets?\\s+(?:usually |c\\. |mostly |often |about |± )?", lo_m, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?![0-9.]|\\s*(?:mm|cm|dm|m)\\b|\\s*(?:x|×))")) else NULL
      if (is.null(v) && length(ftx)) v <- count_after(ftx, "(?<![0-9.])([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?[- ]?(?:flowered|floreted)\\b")
      if (is.null(v) && length(ftx)) v <- count_after(ftx, paste0("(?<![0-9.,×x-]\\s?)\\b", lo_m, "([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?\\s+(?:[a-z-]+ )?florets?\\b"))
      if (!is.null(v)) addn("floret_count", v$v, v$txt, ct[1], ct[2])
    }
    pdl <- m_first(unit_text(uu, "pedicel"), "long|in length", excl("pedicels?"))
    if (is.null(pdl)) {
      pm <- str_match(mark_extremes(join_units(unit_text(uu, c("flower", "inflorescence", "fruit", "bract"), parts = "any")) %||% ""), rx(paste0("\\b(elongat[a-z]+ )?pedicels?\\b[^;:|]{0,30}?(", meas, ")(?:\\s+or more)?\\s*long")))
      # (Didymoplexis "Capsules erect, ... on elongated pedicels 30-40 cm long": the pedicel lengthens after flowering, so this is the
      # pedicel length when fruiting, on a phenological_stage row; user's choice 2026-10-27)
      if (orchid_mode && !is.na(pm[1]) && !is.na(pm[2])) { addn("pedicel_length", meas_of(pm[3]), unmark(pm[1]), "phenological_stage", "fruiting"); pm[1] <- NA }
      if (!is.na(pm[1])) pdl <- list(v = meas_of(pm[3]), txt = unmark(pm[1]))
    }
    if (!is.null(pdl)) addn("pedicel_length", pdl$v, pdl$txt, ct[1], ct[2])
    br <- m_first(unit_text(uu, "bract", subj_re = "^(?:the )?(?:floral |flower |subtending )?bracts?$"), "long|in length", excl("bracts?"))
    # (Fabaceae: the bract(s) at the base of the peduncle, candidate inflorescence_bract_length)
    if (legume_mode && is.na(en)) { bb <- m_first(unit_text(uu, "bract", subj_re = "^(?:the )?(?:basal|peduncular|lowest|lower) (?:peduncular )?bracts?$"), "long|in length", excl("bracts?"))
      if (!is.null(bb)) addn("inflorescence_bract_length", bb$v, bb$txt) }
    if (!is.null(br)) addn("flower_bract_length", br$v, br$txt, ct[1], ct[2])
    # (the long hypanthium tube of Elodea, Hydrilla is not the sepals: recorded as hypanthium_length)
    # (Fabaceae: "Flowers with sepals 0.7-1 mm long", "Flowers with calyx c. 0.8 mm long")
    if (legume_mode) { fsp <- str_match(mark_extremes(join_units(fl_u0) %||% ""), rx(paste0("\\bwith (?:the )?(?:calyx|sepals)\\b[^,;|]{0,15}?(", meas, ")\\s*long")))
      if (!is.na(fsp[1])) addn("flower_sepal_length", meas_of(fsp[2]), unmark(fsp[1]), ct[1], ct[2]) }
    sep <- d_first(unit_text(uu, "calyx", parts = "any", subj_re = "^(?!(?:the )?(?:hypanth|dorsal sepal|lateral sepal))"), excl("calyx", "sepals?", "lobes?", "segments?", "tube", "hypanthium"))$L
    if (FALSE) sep <- m_first(unit_text(uu, "calyx", parts = "any"), "long|in length", excl("calyx", "sepals?", "lobes?", "segments?", "tube", "hypanthium"))
    if (!is.null(sep)) addn("flower_sepal_length", sep$v, sep$txt, ct[1], ct[2])
    # orchids: dorsal and lateral sepals measured separately ("Dorsal sepal 9-14 x 4-6 mm"; "Lateral sepals 9-12 x 1.3 mm")
    if (orchid_mode && is.na(en)) for (sp in c("dorsal sepal", "lateral sepals")) {
      sd <- d_first(unit_text(uu, "calyx", subj_re = paste0("^(?:the )?", word(sp, 1), " sepals?$")), excl("sepals?", "calyx", "lobes?", "segments?", "clubs?", "tips?"))
      if (!is.null(sd$L)) addn("flower_sepal_length", sd$L$v, sd$L$txt, "entity_measured", sp)
      if (!is.null(sd$W)) addn("flower_sepal_width", sd$W$v, sd$W$txt, "entity_measured", sp)
    }
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
    # (Fabaceae: the standard, wings and keel of papilionaceous flowers are measured on their own entity_measured rows, below)
    ptl <- d_first(unit_text(uu, "corolla", subj_re = if (legume_mode) "^(?:the )?(?:(?:outer|inner|larger)(?: [0-9]+| two| three)? )?petals?$" else "^(?:the )?(?:(?:outer|inner|larger)(?: [0-9]+| two| three)? )?(?:petals?|standard)$"), excl("petals?"))$L
    if (!is.null(ptl)) addn("flower_petal_length", ptl$v, ptl$txt, ct[1], ct[2])
    tpl <- d_first(unit_text(uu, "corolla", subj_re = "^(?:the )?(?:(?:outer|inner|inner and outer|outer and inner)(?: [0-9]+| two| three)? )?(?:tepals?|perianths?|sepals and petals|petals and sepals)$", parts = c("segments", "segment", "lobes", "lobe")) %>%
                     { .[!str_detect(., rx("^(?:perianth )?tube"))] }, excl("tepals?", "perianth", "segments?", "lobes?", "sepals", "petals"))$L
    if (!is.null(tpl)) addn("tepal_length", tpl$v, tpl$txt, ct[1], ct[2])
    # Proteaceae: the perianth limb ("limb 3-3.5 mm long", candidate perianth_limb_length) and the pistil ("Pistil straight, 30-33 mm long",
    # candidate pistil_length, as for Fabaceae)
    plb <- m_first(unit_text(uu, "corolla", subj_re = "^(?:the )?perianths?$", parts = "limb") %>% { .[str_detect(., rx("^limb"))] }, "long|in length", excl("limb", "perianth"))
    # ("Perianth 15-22 mm long including limb of 3-4.5 mm")
    if (is.null(plb)) {
      lm <- str_match(mark_extremes(join_units(unit_text(uu, "corolla", subj_re = "^(?:the )?perianths?$", parts = "any")) %||% ""), rx(paste0("\\bincluding (?:the )?limb (?:of )?(", meas, ")(?:\\s*long)?")))
      if (!is.na(lm[1])) plb <- list(v = meas_of(lm[2]), txt = unmark(lm[1]))
    }
    if (!is.null(plb)) addn("perianth_limb_length", plb$v, plb$txt, ct[1], ct[2])
    if (!legume_mode) {
      pis <- m_first(unit_text(uu, "gynoecium", subj_re = "pistil"), "long|in length", excl("pistils?"))
      if (!is.null(pis)) addn("pistil_length", pis$v, pis$txt, ct[1], ct[2])
    }
    lab_u <- unit_text(uu, "corolla", subj_re = "labellum")
    # (orchids: the lamina when the stalk is measured separately; "Labellum 5.5-7 x 2.5-4 mm" gives length x width)
    if (orchid_mode && any(str_detect(lab_u, rx("^labellum lamina")))) lab_u <- lab_u[str_detect(lab_u, rx("^labellum lamina"))]
    lab <- if (orchid_mode) d_first(lab_u, excl("labellum", "lamina", "spurs?", "stalks?", "claws?"))$L else NULL
    if (is.null(lab)) lab <- m_first(lab_u, "long|in length", excl())
    if (!is.null(lab)) addn("labellum_length", lab$v, lab$txt, ct[1], ct[2])
    andr <- unit_text(uu, "androecium", parts = "any")
    # (Fabaceae: "stamens 2-3 (-3.5) mm long" is a stamen length: a parenthetical extreme may sit before the unit)
    stc <- count_after(unit_text(uu, "androecium", subj_re = "^(?:the )?(?:fertile )?stamens?$") %>% { .[!str_detect(., rx(paste0("^(?:fertile )?stamens? (?:usually |c\\. |mostly |often )?[0-9.]+(?:\\s*-\\s*[0-9.]+)?", if (legume_mode) "(?:\\s*\\([^)]*\\))?" else "", "\\s*(?:mm|cm|m)\\b")))] },
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
    if (legume_mode) {
      # stamen length ("stamens 2.5-3 mm long", flower_stamen_length); calyx tube and lobe lengths (candidates calyx_tube_length, calyx_lobe_length)
      stl <- m_first(unit_text(uu, "androecium", subj_re = "^(?:the )?(?:fertile )?stamens?$"), "long|in length", excl("stamens?"))
      if (!is.null(stl)) addn("flower_stamen_length", stl$v, stl$txt, ct[1], ct[2])
      cxt <- m_first(unit_text(uu, "calyx", parts = c("tube")) %>% { .[str_detect(., rx("^(?:calyx )?tube"))] }, "long|in length", excl("tube", "calyx"))
      if (is.null(cxt)) { m <- str_match(mark_extremes(join_units(unit_text(uu, "calyx", parts = "any")) %||% ""), rx(paste0("\\btube\\b[^,;|]{0,15}?(", meas, ")\\s*long")))
        if (!is.na(m[1])) cxt <- list(v = meas_of(m[2]), txt = unmark(m[1])) }
      if (!is.null(cxt)) addn("calyx_tube_length", cxt$v, cxt$txt, ct[1], ct[2])
      cxl <- m_first(unit_text(uu, "calyx", parts = c("lobes", "lobe", "teeth")) %>% { .[str_detect(., rx("^(?:calyx )?(?:lobes?|teeth)"))] }, "long|in length", excl("lobes?", "teeth", "calyx"))
      if (!is.null(cxl)) addn("calyx_lobe_length", cxl$v, cxl$txt, ct[1], ct[2])
      # papilionaceous corolla: standard, wing and keel petal sizes on entity_measured rows (flower_petal_length / _width):
      # "Standard 10.8-20.2 mm long, flare white, wing 3.1-8 mm wide", "wings ... c. 7.5 x 2 mm including the 1-1.5 mm claw",
      # "wings and keel c. 4-5 mm long" (both)
      cor_j <- join_units(unit_text(uu, "corolla", parts = "any"))
      if (!is.na(cor_j) && str_detect(cor_j, rx("\\b(?:standard|wings?|keels?)\\b"))) {
        segs <- str_split(str_replace_all(cor_j, rx("\\bwings? and keels?\\b"), "wingsandkeel"), rx("(?=\\b(?:standard|wings?|keels?|wingsandkeel)\\b)"))[[1]]
        pap_done <- character(0)
        for (sg in segs) {
          who <- str_to_lower(str_extract(sg, rx("^(?:standard|wingsandkeel|wings?|keels?)\\b")))
          if (is.na(who)) next
          # (a petal's clause ends at the next clause: Caesalpinia crista "Standard petal c. 5 mm wide on claw 5 mm long ...; other 4 petals ... 8-10 mm long")
          sg <- str_remove(sg, "\\s*\\|.*$")
          who <- if (who == "wingsandkeel") c("wing petals", "keel petals") else c(standard = "standard", wing = "wing petals", wings = "wing petals", keel = "keel petals", keels = "keel petals")[[who]]
          dd <- dims(str_replace(sg, rx("^wingsandkeel"), "wings and keel"), paste0(excl("petals?", "standard", "wings?", "keels?"), "|claws?|calli|callus|auricles?|pockets?|flare"))
          for (w in setdiff(who, pap_done)) {
            if (!is.null(dd$L)) addn("flower_petal_length", dd$L$v, dd$L$txt, "entity_measured", w)
            if (!is.null(dd$W)) addn("flower_petal_width", dd$W$v, dd$W$txt, "entity_measured", w)
            if (!is.null(dd$L) || !is.null(dd$W)) pap_done <- c(pap_done, w)
          }
        }
      }
      # pistil length (candidate pistil_length): Bauhinia, Caesalpinia "Pistil 22-30 mm long"
      pis <- m_first(unit_text(uu, "gynoecium", subj_re = "pistil"), "long|in length", excl("pistils?"))
      if (!is.null(pis)) addn("pistil_length", pis$v, pis$txt, ct[1], ct[2])
    }
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
  # (orchids: the labellum and callus colours are recorded under labellum / callus, not as the flower colour: "pinkish red with a
  # purplish black labellum and callus")
  if (orchid_mode) cor_all <- unit_text(u, "corolla", subj_re = "^(?!(?:the )?labellum)", parts = c(NA, "tube", "limb", "lobes", "lobe", "segments"))
  colour_units <- c(fl_all, cor_all)
  fc_txt <- join_units(colour_units)
  fc <- scan_terms(fc_txt, flower_colour_dict, prep_fun = if (orchid_mode) function(x) colour_prep(str_remove_all(x, "\\b(?:with|and) (?:an? |the )?(?:[a-z-]+,? (?:or |to |and )?){0,7}(?:labell(?:um|a)|call(?:us|i)|columns?|glands?|lips?)\\b[^,;|]*|\\blabellum\\b[^,;|]*|\\bcall(?:us|i)\\b[^,;|]*"))
                   # (Fabaceae: "petal apices often brown to black" is the colour of the petal tips)
                   # (and hair colours: "petals with sparse brown to black appressed hairs")
                   # (and Hovea "Standard 9-10 mm long, flare green-yellow": the basal flare of the standard is a guide mark, recorded
                   # under the candidate standard_flare_colour below)
                   else if (legume_mode) function(x) colour_prep(str_remove_all(x, "[^,;|]*\\bap(?:ex|ices)\\b[^,;|]*|[^,;|]*\\b(?:tips?|margins?|claws?|flares?)\\b[^,;|]*|\\bwith\\b[^,;|]*?\\b(?:hairs|hairlets|trichomes)\\b")) else colour_prep, generic_neg = TRUE)
  if (legume_mode) addc("standard_flare_colour", collapse_text(unlist(str_extract_all(fc_txt %||% "", rx("[^,;|]*\\bflares?\\b[^,;|]*"))), " | "), gen_colour_dict)
  if (!nrow(fc)) {
    # (Fabaceae: the heads / spikes only, not raceme axes "drying dark brown to blackish" or the bracts enclosing young racemes)
    fc_txt <- join_units(unit_text(u, "inflorescence", subj_re = if (legume_mode) "head|capitul|spike" else "head|capitul|spike|raceme|glomerule"))
    fc <- scan_terms(fc_txt, flower_colour_dict, prep_fun = if (legume_mode) function(x) colour_prep(str_remove_all(x, "\\bdrying\\b[^,;|]*|[^,;|]*\\bbracts?\\b[^,;|]*|\\bwith\\b[^,;|]*?\\bhairs\\b")) else colour_prep)
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
  # (a count that starts at 0, "Petals 0-4", says petals may be absent: not a merism)
  mer <- count_after(c(fl_all, cor_all, unit_text(u, "calyx", parts = "any")), "\\b([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?-merous\\b")
  if (is.null(mer)) mer <- count_after(unit_text(u, "corolla", subj_re = "petal|tepal|perianth|corolla", parts = c(NA, "lobes", "limb")),
                                       "^(?:petals?|tepals?|perianth (?:segments|lobes|parts)|corolla lobes|lobes) (?:usually |mostly )?([1-9])(?:\\s*(?:-|or)\\s*([0-9]))?(?=,|$| )(?! ?(?:mm|cm|m)\\b| ?[\u00d7x] ?[0-9.])|^(?:corolla|limb|perianth)\\b[^,;|]{0,20}?\\b([0-9])-lobed\\b")
  if (is.null(mer)) mer <- count_after(unit_text(u, "calyx", parts = "any"), "^(?:calyx (?:segments|lobes)|sepals?|segments|lobes) (?:usually |mostly )?([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?(?=,|$| )(?! ?(?:mm|cm|m)\\b| ?[\u00d7x] ?[0-9.])")
  if (!is.null(mer)) {
    mm <- str_match(mer$txt, "([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?")
    addn("flower_perianth_merism", parse_count(mer$raw), mer$txt)
    # (Fabaceae: "Flowers 5-merous, rarely a few 4-merous": the rarer merism on its own commonness row, the first on a "usually" row)
    if (legume_mode) {
      mq <- str_match_all(str_to_lower(join_units(fl_all) %||% ""), paste0(qual_re, "\\s+(?:a few\\s+|some\\s+)?([0-9])(?:\\s*(?:-|or)\\s*([0-9]))?-merous"))[[1]]
      for (j in seq_len(nrow(mq))) if (mq[j, 2] %in% rare_quals && !str_detect(mer$txt, fixed(mq[j, 1]))) {
        add(rec_num("flower_perianth_merism", count_range(mq[j, 3], mq[j, 4]), mq[j, 1]) %>% mutate(qualifier = mq[j, 2]))
        for (k in seq_along(recs)) if (all(recs[[k]]$trait == "flower_perianth_merism") && all(is.na(recs[[k]]$qualifier))) recs[[k]]$qualifier <- "usually"
      }
    }
  }

  # ---- fruit
  # (an infructescence is not the fruit: "Infructescence a raceme, 5.5-46 cm long" is infructescence_length)
  fr <- unit_text(u, "fruit", subj_re = "^(?!(?:the )?infructescences?\\b).*$")
  fr_txt <- join_units(fr)
  # cycads and conifers: seed and pollen cone length and diameter (candidate traits, user 2026-10-29); seed cones are also
  # "female / ovulate cones", pollen cones "male cones" ("Pollen cones fusiform, 25-45 cm long, 8-12 cm diam.; microsporophyll
  # lamina 20-40 mm long": not the sporophylls, scales, spines or stalks)
  cone_other <- paste0(excl("cones?"), "|sporophylls?|microsporophylls?|megasporophylls?|receptacles?")
  for (ck in list(c("seed_cone", "^(?:the )?(?:seed|ovulate|female|megasporangiate) (?:cones?|strobil(?:i|us))$"),
                  c("pollen_cone", "^(?:the )?(?:pollen|male|microsporangiate) (?:cones?|strobil(?:i|us))$"))) {
    cd <- d_first(unit_text(u, "cone", subj_re = ck[2]), cone_other)
    if (!is.null(cd$L)) addn(paste0(ck[1], "_length"), cd$L$v, cd$L$txt)
    if (!is.null(cd$W)) addn(paste0(ck[1], "_diameter"), cd$W$v, cd$W$txt)
  }
  fr_dim <- unit_text(u, "fruit", subj_re = "^(?!(?:the )?(?:pericarps?|endocarps?|valves|cocci|mericarps?|infructescences?)$).*$")
  fr_dim_txt <- join_units(fr_dim)
  fr_subj <- u$subj[u$sec == "fruit"]
  # (fruit type and fleshiness also from an infructescence clause: "Infructescence ... of berries")
  fr_all <- unit_text(u, "fruit")
  ft_txt <- collapse_text(c(fr_all, unlist(str_extract_all(desc %||% "", rx("\\bfruits? (?:a|an) [^,;.]+")))), " | ")
  ft <- scan_terms(ft_txt, c(fruit_type_dict, if (fam == "Fabaceae") c("pods?" = "legume")), generic_neg = FALSE,
                   prep_fun = function(x) str_remove_all(x, "\\bseeds?\\b[^,;|]*|\\bvalves?\\b"))
  add(rec_cat("fruit_type", ft, ft_txt))
  # "valves" says nothing about dehiscence when the fruit is stated to be indehiscent
  dh_f <- scan_terms(fr_txt, dehisc_dict, generic_neg = FALSE)
  if (nrow(dh_f) && any(dh_f$value == "indehiscent")) dh_f <- dh_f %>% filter(!(value == "dehiscent" & str_detect(term, "^valves?$")))
  add(rec_cat("fruit_dehiscence", dh_f, fr_txt))
  # (Fabaceae: Archidendron "Pod ... fleshy-coriaceous" is a fleshy pod; Ceratonia "pulpy when young but becoming hard at maturity" is dry)
  addc("fruit_fleshiness", join_units(fr_all), fleshy_dict,
       prep_fun = function(x) { x <- str_remove_all(x, "\\b(?:seeds?|endocarp|stone|pyrene|aril)\\b[^,;|]*|[a-z ]*\\bhooks?\\b[^,;|]*")
         if (legume_mode) x <- x %>% str_replace_all(rx("\\bfleshy-coriaceous\\b|\\bcoriaceous-fleshy\\b"), "fleshy") %>% str_remove_all(rx("\\b(?:pulpy|fleshy|succulent) when (?:young|immature)\\b"))
         x })
  addc("fruit_colour", fr_dim_txt, fruit_colour_dict, prep_fun = function(x) colour_prep(x, ageing = FALSE))
  addc("fruit_surface_hairs", fr_txt, hairs_dict %>% { .[. != "glandular_pubescent"] } %>% c("glandular-(?:pubescent|hairy|pilose|puberulous)|glandular hairs" = "hairy"))
  # (Fabaceae pods: coiling, twisting, curvature and constriction between the seeds as well)
  addc("fruit_shape", fr_txt, if (legume_mode) c(fruit_shape_dict, "(?:once-|twice-|openly |loosely |tightly |spirally |semi-)?(?:coiled|circinnate|circinately coiled)|spirally (?:twisted|coiled)" = "coiled",
                                                  "twisted|contorted" = "twisted", "(?:sub)?moniliform" = "moniliform", "constricted (?:between|at)|constrictions" = "constricted",
                                                  "curved|arcuate|recurved|sigmoid" = "curved", "straight|substraight" = "straight") else fruit_shape_dict,
       prep_fun = if (legume_mode) function(x) str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*|\\bstraight-(?:edged|sided|margined)\\b|\\b(?:margins?|sutures?|veins?|nerves?|valves?) [^,;|]*|\\bcurved back\\b|\\bnot (?:or (?:scarcely|barely|slightly) )?constricted\\b[^,;|]*")
                  else function(x) str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*"))
  frd <- dims(fr_txt %||% "", excl("fruits?", "capsules?", "pods?", "legumes?", "drupes?", "berr(?:y|ies)", "nuts?", "nutlets?", "achenes?", "cypselas?", "follicles?", "mericarps?", "samaras?", "valves", "syconi(?:a|um)", "cones?", "endocarps?", "pericarps?"))
  if (length(fr)) {
    fo <- excl("fruits?", "capsules?", "pods?", "legumes?", "drupes?", "nuts?", "nutlets?", "achenes?", "follicles?", "mericarps?", "samaras?", "valves", "cones?", "endocarps?", "pericarps?")
    frd <- d_first(fr_dim, fo)
    # the base-to-apex height is the fruit length when no length is given (Tribulus "Fruit 10-14 mm high, 18-25 mm wide",
    # Leionema "Cocci 4-5 mm high")
    high_len <- function(texts) { for (x in texts) { h <- first_meas(x, "high|tall", fo); if (!is.null(h)) return(h) }; NULL }
    l_from_high <- is.null(frd$L)
    if (is.null(frd$L)) frd$L <- high_len(fr_dim)
    if (!is.null(frd$L)) addn("fruit_length", frd$L$v, frd$L$txt)
    if (!is.null(frd$W)) addn("fruit_width", frd$W$v, frd$W$txt)
    # (Banksia "Follicles broadly elliptic, 20-35 mm long, 10-15 mm high, 14-20 mm wide": the height beside a length)
    fth <- m_first(fr_dim, if (l_from_high) "thick|deep" else "thick|deep|high", fo)
    # fruits per infructescence (candidate fruits_per_infructescence): Banksia "Follicles up to 6 per head"
    fph <- count_after(unit_text(u, "fruit", parts = "any"), paste0("\\b(?:up to |to |c\\. )?", cnt, " per (?:head|infructescence|conflorescence|spike|cone)\\b"))
    if (!is.null(fph)) addn("fruits_per_infructescence", parse_count(fph$raw), fph$txt)
    if (legume_mode) {
      # pod stipe length and marginal wing width (candidates fruit_stipe_length, fruit_wing_width)
      fsl <- str_match(mark_extremes(join_units(unit_text(u, "fruit", parts = "any")) %||% ""), rx(paste0("\\bstipe\\b[^,;|]{0,15}?(", meas, ")\\s*long")))
      if (!is.na(fsl[1])) addn("fruit_stipe_length", meas_of(fsl[2]), unmark(fsl[1]))
      fwg <- str_match(mark_extremes(join_units(unit_text(u, "fruit", parts = "any")) %||% ""), rx(paste0("\\bwing(?:s|ed margins?)?\\b[^,;|]{0,20}?(", meas, ")\\s*wide")))
      if (!is.na(fwg[1])) addn("fruit_wing_width", meas_of(fwg[2]), unmark(fwg[1]))
    }
    if (!is.null(fth)) addn("fruit_height", fth$v, fth$txt)
    # fruits measured only as their mericarps / cocci ("Cocci 5-10 mm long"): entity_measured rows
    if (is.null(frd$L)) {
      mc_txt <- unit_text(u, "fruit", subj_re = "^(?:the )?(?:mericarps?|cocci|coccus|fruitlets?)$")
      mc <- d_first(mc_txt, fo)
      if (is.null(mc$L)) mc$L <- high_len(mc_txt)
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
  if (!legume_mode) addc("seed_shape", sd_txt, seed_shape_dict, prep_fun = function(x) str_remove_all(x, "\\bin (?:cross[- ])?section\\b[^,;|]*|\\b(?:arils?|wings?|hilum|embryo)\\b[^,;|]*"))
  if (legume_mode) {
    # seed_shape is 3-dimensional (traits.yml): the outline terms of legume seeds ("oblong", "elliptic", "ovate", "circular") map to
    # ellipsoid / ovoid / globoid (traits.yml lists obloid and oblong as ellipsoid synonyms), and to the *_flattened levels when the
    # seed is "compressed" / "flattened" (not when "turgid")
    sh_src <- str_remove_all(str_to_lower(sd_txt %||% ""), "\\bin (?:cross[- ])?section\\b[^,;|]*|\\b(?:arils?|wings?|hilum|embryo|areoles?|pleurograms?|funicles?|halo|fold\\w*)\\b[^,;|]*|\\b(?:longitudinal|oblique|transverse)(?:ly)?\\b|\\brounded (?:at|over)\\b[^,;|]*")
    flat <- str_detect(sh_src, "\\b(?:compressed|flattened|flat|dorsiventrally|laterally compressed)\\b|\\bcompressed-") && !str_detect(sh_src, "\\bturgid\\b")
    ss <- scan_terms(sh_src, c("oblong|elliptic(?:al)?|obloid|ellipsoid(?:al)?|oval" = "ellipsoid", "linear" = "", "ovate|obovate|ovoid|obovoid|pyriform|egg-shaped" = "ovoid",
                               "circular|subcircular|orbicular|suborbicular|rotund|round|rounded|globose|subglobose|globular|subglobular|spherical" = "globoid",
                               "reniform|kidney-shaped|heart-shaped" = "reniform", "rhombic|quadrate|subquadrate|square|rectangular|angular" = "polyhedral",
                               "lenticular|lens-shaped" = "lenticular", "discoid|subdiscoid|disc-shaped" = "discoid", "conical|turbinate" = "conical",
                               "cylindrical|cylindric" = "cylindrical", "fusiform" = "fusiform"))
    if (flat && nrow(ss)) ss <- ss %>% mutate(value = ifelse(value %in% c("ellipsoid", "ovoid", "globoid"), paste0(value, "_flattened"), value),
                                              term = paste0(term, ifelse(value %in% c("ellipsoid_flattened", "ovoid_flattened", "globoid_flattened"), " [compressed]", "")))
    add(rec_cat("seed_shape", ss, sd_txt))
    # position of the seeds in the pod (candidate seed_orientation_in_fruit): "Seeds longitudinal", "oblique to longitudinal", "transverse"
    so_tok <- unlist(map(sd, split_tokens))
    so_tok <- so_tok[map_lgl(str_extract_all(str_to_lower(str_remove_all(so_tok, "∅")), "[a-z±-]+"),
                             ~ length(.x) && any(.x %in% c("longitudinal", "oblique", "transverse", "longitudinally", "obliquely", "transversely")) &&
                               all(.x %in% c("seeds", "seed", "longitudinal", "oblique", "transverse", "longitudinally", "obliquely", "transversely", "to", "or", "and",
                                             "mostly", "usually", "sometimes", "rarely", "occasionally", "commonly", "normally", "presumably", "arranged", "in", "the", "pod", "pods", "±", "more", "less", "often", "very", "slightly")))]
    addc("seed_orientation_in_fruit", paste(so_tok, collapse = " | "), c("longitudinal(?:ly)?" = "longitudinal", "oblique(?:ly)?" = "oblique", "transverse(?:ly)?" = "transverse"))
    # seed thickness ("compressed (1-2.5 mm thick)") is the seed height (traits.yml: the shortest axis)
    sth <- m_first(sd, "thick|deep", excl("seeds?"))
    if (!is.null(sth)) addn("seed_height", sth$v, sth$txt)
    # pleurogram (candidate seed_pleurogram, present / absent): "pleurogram U-shaped", "areole open", "without a pleurogram"
    addc("seed_pleurogram", join_units(unit_text(u, "seed", parts = "any")), c("pleurograms?|areoles?|areolate" = "present"),
         neg = c("without (?:a )?pleurogram|pleurograms? (?:absent|lacking|0)|lacking (?:a )?pleurogram|no pleurogram|exareolate" = "absent"), generic_neg = FALSE)
  }
  # (colours of the aril etc. are not the seed's: "with a prominent fleshy white aril")
  addc("seed_colour", sd_txt, seed_colour_dict, prep_fun = function(x) colour_prep(str_remove_all(x, "\\b(?:with )?(?:an? )?(?:[a-z-]+ ){0,3}(?:arils?|arillode|strophioles?|wings?|hilum|elaiosomes?|caruncles?)\\b[^,;|]*") %>%
    # (Fabaceae: halo / areole / pleurogram colours, "yellow around pleurogram", "yellowish at centre")
    { if (legume_mode) str_remove_all(., "[^,;|]*\\b(?:halos?|areoles?|pleurograms?|centre|center|middle)\\b[^,;|]*|\\bwith\\b[^,;|]*\\b(?:mottl\\w*|spots?|spotted|blotch\\w*|markings?|flecks?|streaks?)\\b") else . }))
  addc("seed_surface_texture", sd_txt, seed_texture_dict, prep_fun = function(x) str_remove_all(x, "\\b(?:arils?|wings?|hilum)\\b[^,;|]*"))
  addc("seed_surface_reflectivity", sd_txt, reflect_dict)
  addc("seed_surface_hairs", sd_txt, seed_hairs_dict)
  # (Fabaceae: the wings and margins of a dehiscent pod are not on the dispersal unit, the seed)
  app_txt <- collapse_text(c(if (!legume_mode || any(dh_f$value == "indehiscent")) fr, unit_text(u, "seed", parts = "any")), " | ")
  # chenopod dispersal units: spines, wings, bladder-like appendages (inflated_parts)
  dsp_txt <- join_units(unit_text(u, "diaspore", parts = "any"))
  addc("dispersal_appendage", dsp_txt, c("spines?|spinose|spiny|spine-like" = "spines", "wings?|winged" = "wings",
                                         "bladder(?:-like)?|inflated|vesicular|spongy appendages?" = "inflated_parts", "hooks?|hooked" = "hooks"),
       neg = c("(?:spines|wings?|appendages) (?:absent|0)|wingless|without (?:spines|wings?|appendages)|not winged" = ""))
  # (Fabaceae: a fleshy, thickened or folded funicle around the seed is an arillate funicle, "funicle fleshy, orange ..., twice-encircling seed")
  if (legume_mode) app_txt <- str_replace_all(app_txt, regex("\\bfunicles?\\b(?=[^|;]{0,60}\\b(?:fleshy|thickened|expanded|arillate|aril-like|swollen)\\b)", ignore_case = TRUE), "funicle (aril)")
  addc("dispersal_appendage", app_txt, appendage_dict, appendage_neg,
       prep_fun = function(x) str_remove_all(x, "(?:seed-bearing |curved woody |woody )?hooks? (?:subtending|prominent|present|lacking)[^,;|]*|seed-bearing hooks|retinacul\\w*|\\bwing (?:petals?)\\b"))

  # ---- generic organ x character matrix (vocabularies above); male / female flower parts on entity_measured rows
  u_g <- u %>% mutate(okey = gen_okey(sec, part, subj), pcl = gen_pclass(part)) %>% filter(!is.na(okey), !is.na(pcl))
  # in ferns the indusium covers the sorus (not the Goodeniaceae pollen cup)
  if (fern_mode) u_g <- u_g %>% mutate(okey = if_else(okey == "indusium", "sorus_indusium", okey))
  for (en in c(NA, unique(u_g$entity[!is.na(u_g$entity)]))) {
    ug <- if (is.na(en)) u_g %>% filter(is.na(entity)) else u_g %>% filter(entity == en)
    ct <- if (is.na(en)) c(NA, NA) else if (grass_mode && str_detect(en, rx(paste0("^", grass_qual, " ")))) c("entity_measured", en) else
      c("entity_measured", paste(str_replace(en, "staminate", "male") %>% str_replace("pistillate", "female"), "flowers"))
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
          # (Fabaceae: resin and gland colours, "sepals free, white-resinous", are not the organ's colour)
          colour = if (legume_mode) function(x) colour_prep(str_remove_all(x, paste0("\\b(?:", cw, ")[- ](?:resinous|resin\\w*|glandular|scurfy|waxy)\\b|\\bdrying\\b[^,;|]*")), ageing = FALSE) else function(x) colour_prep(x, ageing = FALSE),
          shape = function(x) str_remove_all(x, "\\b(?:[a-z-]+ )?(?:at|towards|near) (?:the )?(?:base|apex|tip|summit)\\b|\\bin (?:cross[- ])?section\\b[^,;|]*|\\b(?:teeth|lobes?|glands?|veins?|hairs?|scales?|apex|apices|tips?|bases?)\\b[^,;|]*"),
          hairs = function(x) str_remove_all(x, "\\b(?:hairs?|indumentum) (?:of|on) (?:the )?(?:[a-z-]+ )?(?:veins?|midrib|margins?|base)\\b"),
          # ("free basal part of labellum c. 2 mm long" names a part, not a fusion)
          fusion = if (orchid_mode) function(x) str_remove_all(x, "\\bfree (?:basal |apical )?(?:part|portion)\\b[^,;|]*") else NULL,
          NULL)
        so <- gen_strip_other(ok)
        if (ch %in% c("shape", "colour", "texture", "fusion", "persistence", "orientation")) {
          so0 <- so; so <- function(x) str_remove_all(so0(x), "\\b(?:leaving|with|having|bearing|subtended by|surrounded by)\\b[^,;|]*")
        }
        # (Fabaceae: a twisted pod is a pod shape, an incurved / recurved phyllode a curvature: both have their own traits)
        if (legume_mode && ch == "orientation" && ok %in% c("fruit", "leaf")) {
          pf0 <- pf; pf <- function(x) { x <- str_remove_all(x, if (ok == "fruit") "\\b(?:twisted|contorted)\\b" else "\\b(?:incurved|recurved|inflexed)\\b"); if (is.null(pf0)) x else pf0(x) }
        }
        # ("appressed-pubescent", "appressed hairs" describe the hairs, not the organ's orientation: all families, Banksia "Stems
        # appressed-pubescent"; "bracts appressed to the axis" stays an orientation outside Fabaceae)
        if (!legume_mode && ch == "orientation") {
          pf1 <- pf; pf <- function(x) { x <- str_remove_all(x, "\\b(?:sub)?(?:appressed|adpressed)-[a-z]+|\\b(?:sub)?(?:appressed|adpressed|spreading|patent|erect|antrorse|retrorse)\\b(?= (?:[a-z-]+ )?(?:hairs|hairlets|trichomes|bristles|setae))"); if (is.null(pf1)) x else pf1(x) }
        }
        # (Fabaceae: "appressed-puberulous", "appressed hairs" describe the hairs, not the organ's orientation)
        if (legume_mode && ch == "orientation") {
          pf1 <- pf; pf <- function(x) { x <- str_remove_all(x, "\\b(?:sub)?(?:appressed|adpressed)[- ][a-z]+|\\b(?:sub)?(?:appressed|adpressed|spreading|patent|erect|antrorse|retrorse)\\b(?= (?:[a-z-]+ )?(?:hairs|hairlets|trichomes|bristles|setae))"); if (is.null(pf1)) x else pf1(x) }
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
      # (Daviesia "upper 2 lobes united higher and closer than the lower 3": a subset of the lobes, not their number)
      if (legume_mode) tt <- str_remove_all(tt, rx("\\b(?:upper|lower|lateral|outer|inner|other|abaxial|adaxial|longest|shortest) [0-9]+(?:\\s*(?:-|or|to)\\s*[0-9]+)?\\s+(?=[a-z])"))
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
    # Poaceae: vein counts of glumes, lemma and palea ("1-veined", "3-5-nerved"); awns per lemma ("3-awned"); awned or awnless
    # (lemma, glumes); the awn column / bristle measured inside the awn clause ("awn 30-70 mm long, with column 8-15 mm long")
    if (grass_mode) {
      gtx <- function(oks) ug %>% filter(okey %in% oks) %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      for (ok in c("glume", "lemma", "palea")) {
        tt <- gtx(ok)
        if (!length(tt)) next
        v <- count_after(tt, "(?<![0-9.])([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?-?\\s?(?:veined|nerved)\\b")
        if (!is.null(v)) addn(paste0(ok, "_vein_count"), v$v, v$txt, ct[1], ct[2])
        if (ok != "palea") addc(paste0(ok, "_awn_presence"), join_units(tt), c("awnless|muticous|unawned|not awned|without (?:an? )?awns?" = "awnless",
                                                                             "(?<!-)awned|aristate|with (?:an? )?awns?" = "awned", "mucronate|mucronulate|aristulate|shortly mucronate" = "mucronate"))
      }
      # inflorescence branches (candidate inflorescence_branch_length / _count: "branches 2-20, 2-10 cm long")
      tt <- u %>% filter(sec == "inflorescence", part == "branches", is.na(entity) == is.na(en), is.na(en) | entity == en) %>%
        group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      if (length(tt)) {
        dd <- d_first(tt, excl("branches", "branch"))
        if (!is.null(dd$L)) addn("inflorescence_branch_length", dd$L$v, dd$L$txt, ct[1], ct[2])
        v <- count_after(tt, "^(?:the )?(?:[a-z]+ ){0,2}?branch(?:es)?\\s+(?:usually |c\\. |mostly |often |about |± )?([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?(?![0-9.]|\\s*(?:mm|cm|dm|m)\\b|\\s*(?:x|×)|\\s*-\\s*[a-z])")
        if (!is.null(v)) addn("inflorescence_branch_count", v$v, v$txt, ct[1], ct[2])
      }
      # callus hairs ("Callus indumentum tufted, 8-14 mm long", "Callus with hairs to 5 mm long", "Callus hairs to 1 mm long")
      tt <- u %>% filter(sec == "grass_part", str_detect(coalesce(subj, ""), "^call"), is.na(entity) == is.na(en), is.na(en) | entity == en) %>%
        group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      if (length(tt)) {
        hm <- str_match(mark_extremes(join_units(tt)), rx(paste0("\\b(?:hairs?|indumentum|beard)\\b[^;|]{0,30}?(", meas, ")\\s*long")))
        if (!is.na(hm[1])) addn("callus_hair_length", meas_of(hm[2]), unmark(hm[1]), ct[1], ct[2])
      }
      # ("lemma lobes 21-23 mm long", Amphipogon)
      tt <- ug %>% filter(okey == "lemma", pcl == "lobe") %>% group_by(unit) %>% summarise(t = paste(text, collapse = ", "), .groups = "drop") %>% pull(t)
      dd <- if (length(tt)) d_first(tt, excl("lobes?")) else NULL
      if (!is.null(dd$L)) addn("lemma_lobe_length", dd$L$v, dd$L$txt, ct[1], ct[2])
      # (an awn measured inside the lemma or glume clause, "Lemma ..., with a terminal awn, 8-14 mm long", "awns shorter than lemma
      # body to more than twice body length, 6-30 mm long", "awnlet to 1 mm long"; a glume's awn is glume_awn_length, Lepturus "upper
      # glume ..., with awn 10-17 mm long"; not lobes measured with their awns, Triodia "midlobe (including awn) 25-37 mm long", nor
      # hairs on the awn, "awns pilose with hairs up to 1 mm long")
      for (ak in list(c("lemma", "awn_length"), c("glume", "glume_awn_length"))) {
        if (ak[1] == "lemma" && nrow(ug %>% filter(okey == "awn"))) next
        tt <- gtx(ak[1])
        if (!length(tt)) next
        tx <- str_remove_all(mark_extremes(join_units(tt)), rx("\\((?:including|incl\\.?|with) awns?\\)"))
        am <- str_match(tx, rx(paste0("\\b(?<!-)awn(?:let)?s?\\b(?!-)(?!\\s+columns?)[^;|0-9]{0,70}?(", meas, ")\\s*long")))
        if (!is.na(am[1]) && !str_detect(am[1], rx("\\bhairs?\\b"))) addn(ak[2], meas_of(am[2]), unmark(am[1]), ct[1], ct[2])
      }
      tt <- gtx(c("lemma", "awn"))
      v <- if (length(tt)) count_after(tt, "(?<![0-9.])([0-9]+)(?:\\s*(?:-|or|to)\\s*([0-9]+))?-awned\\b") else NULL
      if (!is.null(v)) addn("awn_count", v$v, v$txt, ct[1], ct[2])
      tt <- gtx("awn")
      if (length(tt) && !nrow(ug %>% filter(okey == "awn_column"))) {
        cm <- str_match(mark_extremes(join_units(tt)), rx(paste0("\\bcolumns?\\b[^,;|]{0,15}?(", meas, ")\\s*long")))
        if (!is.na(cm[1])) addn("awn_column_length", meas_of(cm[2]), unmark(cm[1]), ct[1], ct[2])
      }
    }
  }

  # ---- cues: flowering / fruiting / germination triggered by rain, fire, floods ... (user's request: always documented)
  # verbatim clauses in *_cues_description; mapped only when stated (hedged clauses stay text-only)
  cue_src <- collapse_text(c(prep(r[["Phenology"]]), prep(r[["Ecology"]]), prep(r[["Notes"]]), prep(r[["Habitat"]])), " ")
  if (!is.na(cue_src)) {
    cue_re <- "\\b(?:rain\\w*|wet season|the wet|monsoon\\w*|wet conditions|soil moisture|moisture|wetting|(?:bush|wild)?fires?|bush fires?|burn\\w*|smoke|flood\\w*|inundat\\w*|water levels?|drying|dries out|frost|cold|heat|temperature)\\b"
    cue_hedge <- "\\b(?:probably|possibly|perhaps|may|might|appears? to|seems? to|thought to|likely|presumably|suggest\\w*|[Mm]ost species|[Ss]ome species|[Mm]any species)\\b|\\[genus description\\]"
    cue_kw <- list(flowering = "\\b(?:flower\\w*|anthesis|in bloom)\\b", fruiting = "\\b(?:fruit\\w*|seed(?:s|ing)? (?:set|mature|ripen)\\w*|capsules?)\\b",
                   germination = "\\b(?:germinat\\w*|seedlings?|recruit\\w*|regenerat\\w* from seed)\\b")
    # Fabaceae: sentence-initial hedges ("Appears to flower in response to summer rainfall", "Seemingly flowers ...", "it is suspected
    # that ...", "apparently"); "regenerates prolifically from seed following fires"
    if (legume_mode) {
      cue_hedge <- paste0(cue_hedge, "|\\b(?:Probably|Possibly|Perhaps|Appears? to|Seems? to|[Ss]eemingly|Thought to|Likely|Presumably|[Ss]uspected|[Aa]pparently|[Aa]pparent)\\b")
      cue_kw$germination <- "\\b(?:germinat\\w*|seedlings?|recruit\\w*|regenerat\\w* (?:[a-z]+ ){0,2}from seed)\\b"
    }
    # Orchidaceae: flowering only in wet years / poorly in dry years ("Flowers freely only in wet years", "Flowering is profuse in wet
    # years and rare in dry years", "Flowers freely in years of good rainfall", "strongly influenced by rainfall") is a rainfall cue
    orch_cue <- ""
    if (orchid_mode) {
      cue_re <- str_replace(cue_re, "\\)\\\\b$", "|wet years|wet seasons|dry years|wetter than normal seasons)\\\\b")
      orch_cue <- "|\\b(?:in|influenced by)\\b[^.;]{0,25}\\b(?:wet years|wet seasons|dry years|wetter than normal seasons|years of good (?:[a-z]+ )?rain\\w*)|influenced by rain\\w*"
      # (Pterostylis pyramidalis "plants respond to summer fires by flowering strongly")
      orch_cue <- paste0(orch_cue, "|\\brespond\\w* to (?:[a-z]+ ){0,3}(?:(?:bush|wild)?fires?|burns?)\\b[^.;]{0,5}\\bby flowering")
    }
    cue_dict <- c("(?:summer|wet[- ]season|monsoon(?:al)?) rain\\w*|rain\\w* (?:in|during) (?:the )?(?:summer|wet season)|rain\\w* of the (?:wet season|wet|build-up|monsoon)|wet season|the wet\\b|monsoon\\w*" = "rain_summer",
                  "winter rain\\w*|rain\\w* (?:in|during) winter" = "rain_winter", "autumn rain\\w*|rain\\w* (?:in|during) autumn" = "rain_autumn",
                  "spring rain\\w*|rain\\w* (?:in|during) spring" = "rain_spring",
                  "rain\\w* (?:at any time|regardless of season|throughout the year)|any time (?:of (?:the )?year )?after rain\\w*" = "rain_all_year",
                  "(?<!winter )(?<!summer )(?<!autumn )(?<!spring )rain\\w*|wet conditions|soil moisture|moisture|wetting" = "rain", "(?:bush|wild)?fires?|bush fires?|burn\\w*" = "fire", "smoke" = "smoke",
                  "flood\\w*|inundat\\w*|water levels?" = "floods", "heat" = "heat")
    # (temperate "wet seasons" / "wet years" are rainy years, not the tropical wet season)
    if (orchid_mode) { names(cue_dict)[1] <- str_replace_all(names(cue_dict)[1], "season", "season(?!s)")
                       cue_dict <- c(cue_dict, "wet years|wet seasons|dry years|wetter than normal seasons" = "rain")
                       # (Dendrobium crumenatum "flowering simultaneously 8-9 days after ... a sudden drop in temperature following heavy rain":
                       # the level low_temperature_following_rain, also scored rain; user's choice 2026-10-27)
                       # (scan_terms tries the longest patterns first, so this one is matched before the generic "rain")
                       cue_dict <- c("(?:sudden |marked |sharp )?(?:drop|fall|decrease) in (?:air |night )?temperatures? (?:following|after) (?:a |the )?(?:heavy |good |soaking )?(?:rain\\w*|storms?|thunderstorms?)" = "low_temperature_following_rain rain", cue_dict) }
    cl <- unlist(map(sentences(cue_src), ~ str_split(.x, ";\\s*")[[1]]))
    cl <- cl[str_detect(cl, rx(cue_re))]
    # (Diuris dendrobioides "A hardy orchid, even flowering in dry years": flowering despite drought is not a rainfall cue)
    if (orchid_mode) cl <- cl[!str_detect(cl, rx("\\beven (?:flower\\w* )?in dry (?:years|seasons)\\b"))]
    for (k in names(cue_kw)) {
      ck <- cl[str_detect(cl, rx(cue_kw[[k]]))]
      # the fire / rainfall must be the trigger: "flowers after fire", "germinates following rain", "in response to rainfall"
      ck <- ck[str_detect(ck, rx(paste0("\\b(?:after|following|in association with|associated with|in response to|response to|triggered by|stimulated by|induced by|dependent on|depending on|depends on|with|when|once|until|requires?|prompted by|cued by|enhanced by|increased by|promoted by|favoured by|post-?)\\b[^.;]{0,40}", cue_re, "|", cue_re, "[- ](?:stimulated|induced|triggered|dependent|related)", orch_cue)))]
      # capsules exploding "in response to wetting or drying" and fruits "washed ashore during the monsoon" are dehiscence /
      # dispersal, not cues
      ck <- ck[!str_detect(ck, rx("\\b(?:explod\\w*|dehisc\\w*|flung|washed|flotsam|dispers\\w*|carried)\\b"))]
      # (orchids standing in water in wet seasons is habitat, not a cue: Pterostylis falcata "In wet seasons, the flowering plants are often
      # found growing in water", Prasophyllum drummondii "It flowers in wet seasons with the bases of the plants submerged in water")
      if (orchid_mode) ck <- ck[!str_detect(ck, rx("\\bwet (?:seasons|years)\\b[^.;|]{0,60}\\b(?:in water|submerged)\\b"))]
      if (!length(ck)) next
      tr <- paste0(k, "_cues")
      dsc <- collapse_text(ck, " | ")
      # (case-sensitive: the month "May" is not a hedge)
      mapped <- ck[!str_detect(ck, cue_hedge)]
      # (orchids, kept as text only: visibility after fire is not flowering, Pterostylis frenchii "Although this species flowers in unburnt
      # bush, it is easier to see after summer fires" (skill rule); nor is fire acting through the habitat, P. heberlei "The shrubby habitat
      # of this species becomes densely overgrown soon after fire, inhibiting the flowering of the orchid"; Prasophyllum nitidum "This species
      # is more frequently seen after fire", Pterostylis scapula "The exceptionally robust plants are sometimes seen in the season after summer fires")
      # (but not clauses that also state the response: Corybas abditus "Flowering is enhanced by summer fires and the plants are much easier
      # to find in burnt habitat")
      if (orchid_mode) mapped <- mapped[!(str_detect(mapped, rx("\\b(?:easier|more easily) (?:to )?(?:see|seen|find|found|detect\\w*|locate\\w*)\\b|\\bovergrown\\b[^.;|]{0,20}\\bafter\\b[^.;|]{0,15}\\bfires?\\b|\\b(?:seen|noticed|observed|found)\\b[^.;|]{0,20}\\b(?:after|following)\\b[^.;|]{0,20}\\b(?:fires?|burns?)\\b|\\brecorded\\b[^.;|]{0,60}\\b(?:after|following)\\b[^.;|]{0,20}\\b(?:fires?|burns?)\\b")) &
                                            !str_detect(mapped, rx("\\b(?:enhanced|stimulated|promoted|increased|induced|triggered|only|mainly|profuse\\w*|abundan\\w*|en masse)\\b")))]
      d_use <- if (k == "flowering") cue_dict[cue_dict != "smoke" & cue_dict != "heat"] else cue_dict
      # ("mainly after the Wet": the commonness word reaches across the trigger words)
      # "after winter or summer rainfall" -> "winter rain or summer rainfall"
      mtxt <- join_units(mapped)
      if (!is.na(mtxt)) mtxt <- str_remove_all(str_replace_all(str_to_lower(mtxt), "\\b(winter|summer|autumn|spring) ?(or|and|-) ?(winter|summer|autumn|spring) (rain\\w*)", "\\1 rain \\2 \\3 \\4"),
                                             "\\b(?:fire|flood|rainfall|rain)[- ]?(?:breaks?|scar\\w*|history|frequency|regime|plain)\\b|\\b(?:high|low|moderate)[- ]rainfall (?:areas?|regions?|zones?|districts?|sites?)\\b|\\bmonsoonal (?:areas?|regions?|zones?|districts?|parts?|tropics)\\b|\\buntil the rains? (?:finish|end|cease)\\w*|\\bfrom (?:north|south|east|west) to (?:north|south|east|west)\\b")
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
        # "Mainly flowers after fire", "flowering predominantly after fires": fire-dependent, with the source's word as commonness_qualifier
        # (user's choice 2026-10-27; earlier "mainly" was read as fire-enhanced)
        fire_q <- if (orchid_mode) "(?:(?:bush|wild)?fires?|bush fires?|burns?|burning|burnt)" else "(?:(?:bush|wild)?fires?|bush fires?)"
        pf_q <- str_match(mtxt, rx(paste0("\\b(mainly|mostly|predominantly|largely) (?:flowers? |flowering )?(?:in the (?:season|year|spring) )?(?:after|following|in response to) (?:a |the )?(?:[a-z]+ ){0,4}", fire_q)))[, 2]
        # (Orchidaceae, Jones 2021 wording: wider post-fire patterns, "burn" words, and fire-inhibited flowering at a distance)
        if (orchid_mode) {
          fire_n <- "(?:(?:bush|wild)?fires?|bush fires?|burns?|burning|burnt)"
          pf <- case_when(
            str_detect(mtxt, rx("\\b(?:not|never) (?:dependent on|requir\\w+|need\\w*) (?:a )?fire|irrespective of fire|regardless of fire|equally (?:well )?in burnt and unburnt")) ~ "fire_independent_flowering",
            !is.na(pf_q) ~ "fire_dependent_flowering",
            # ("This species only flowers in the year after a summer fire")
            str_detect(mtxt, rx(paste0("\\b(?:only|exclusively|predominantly|largely) (?:after|following|in response to) (?:a |the )?(?:[a-z]+ ){0,4}", fire_n, "|\\bonly (?:flowers? |flowering )?in the (?:first )?(?:year|season|spring|summer|autumn|winter|growing season) (?:after|following) (?:a |the )?(?:[a-z]+ ){0,3}", fire_n, "|rarely (?:flowers?|flowering) (?:in the )?(?:absence of|without) fire|\\brarely without (?:a )?fires?\\b|(?:absent|rare) in unburnt"))) ~ "fire_dependent_flowering",
            # ("Flowering is enhanced in wet seasons, and strongly inhibited following hot summer fires"; Cryptostylis "Flowering is inhibited in
            # the first year or two after a fire")
            str_detect(mtxt, rx(paste0("\\b(?:inhibited|suppressed|reduced|prevented|depressed|impeded)\\b[^.;|]{0,40}\\b(?:by|after|following) (?:a |the )?(?:[a-z]+ ){0,3}", fire_n))) ~ "fire_suppressed_flowering",
            str_detect(mtxt, rx(paste0("\\b(?:best|most prolifically|more prolifically|profusely|prolifically|abundantly|more abundantly|heavily|enhanced|stimulated|promoted|mass(?:ed)?|particularly|especially|increased|freely|more freely) (?:flowering )?(?:in the season )?(?:after|following|in response to|by) (?:a |the )?(?:recent )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?|fire[- ](?:stimulated|enhanced|promoted)|(?:greater|increased) (?:profusion|numbers|flowering) after fire|enhanced,? but not dependent on|promoted when [^.;|]{0,30}\\bburnt|enhanced in [^.;|]{0,40} and by (?:a |the )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?",
                                       # ("strongly enhanced in the year following a fire", "dramatically stimulated by, but not dependant on, summer fires",
                                       # "promoted by burning", "flowers more profusely in years after a fire", "Flowers in abundance after summer fires",
                                       # "flowers sparsely except after fires")
                                       "|\\b(?:enhanced|stimulated|promoted|increased)\\b[^.;|]{0,45}?\\b", fire_n,
                                       "|\\b(?:profusely|strongly|in abundance|abundantly|prolifically|better)\\b[^.;|]{0,30}\\b(?:after|following)\\b[^.;|]{0,25}?\\b", fire_n,
                                       "|\\b(?:sparsely|rarely|poorly|seldom) except (?:after|following) (?:a )?", fire_n,
                                       # ("Flowers in the absence of fire, but is more frequent following a burn", "mass flowering is common after hot summer fires", "usually
                                       # flowering en masse the following season", "plants respond to summer fires by flowering strongly")
                                       "|\\bmore frequent(?:ly)?\\b[^.;|]{0,15}\\b(?:after|following)\\b[^.;|]{0,15}?\\b", fire_n,
                                       "|\\bmass flowering\\b|\\b(?:flowering|flowers?) en masse\\b|\\brespond\\w* to (?:[a-z]+ ){0,3}", fire_n, " by flowering"))) ~ "fire_enhanced_flowering",
            TRUE ~ "")
        } else {
          pf <- case_when(
            str_detect(mtxt, rx("\\b(?:not|never) (?:dependent on|requir\\w+|need\\w*) (?:a )?fire|irrespective of fire|regardless of fire|equally (?:well )?in burnt and unburnt")) ~ "fire_independent_flowering",
            !is.na(pf_q) ~ "fire_dependent_flowering",
            str_detect(mtxt, rx("\\b(?:only|exclusively|predominantly|largely) (?:after|following|in response to) (?:a |the )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?|\\bonly in the (?:first )?(?:year|season|spring|summer|autumn|winter|growing season) (?:after|following) (?:a |the )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?|rarely (?:flowers?|flowering) (?:in the )?(?:absence of|without) fire|(?:absent|rare) in unburnt")) ~ "fire_dependent_flowering",
            str_detect(mtxt, rx("\\b(?:best|most prolifically|more prolifically|profusely|prolifically|abundantly|more abundantly|heavily|enhanced|stimulated|promoted|mass(?:ed)?|particularly|especially|increased|freely|more freely) (?:flowering )?(?:in the season )?(?:after|following|in response to|by) (?:a |the )?(?:recent )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?|fire[- ](?:stimulated|enhanced|promoted)|(?:greater|increased) (?:profusion|numbers|flowering) after fire|enhanced,? but not dependent on|promoted when [^.;|]{0,30}\\bburnt|enhanced in [^.;|]{0,40} and by (?:a |the )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?")) ~ "fire_enhanced_flowering",
            # ("Flowering is enhanced in wet seasons, and strongly inhibited following hot summer fires")
            str_detect(mtxt, rx("\\b(?:inhibited|suppressed|reduced|prevented|depressed) (?:by|after|following) (?:a |the )?(?:[a-z]+ ){0,2}(?:bush|wild)?fires?")) ~ "fire_suppressed_flowering",
            TRUE ~ "")
        }
        add(tibble(trait = "post_fire_flowering", value = pf, desc = dsc, min = NA, max = NA, ctx_type = NA, ctx_value = NA,
                   qualifier = if (pf == "fire_dependent_flowering" && !is.na(pf_q)) pf_q else NA_character_, kind = "cat"))
        # (fire that suppresses flowering is not a flowering cue)
        if (pf == "fire_suppressed_flowering") h <- h %>% filter(value != "fire")
      }
      if (nrow(h)) add(rec_cat(tr, h, dsc) %>% mutate(desc = dsc)) else add(tibble(trait = tr, value = "", desc = dsc, min = NA, max = NA, ctx_type = NA, ctx_value = NA, qualifier = NA_character_, kind = "cat"))
    }
  }

  # ---- Fabaceae: rock types named in the Habitat text (candidate habitat_geology, the source's rock names, in text order;
  # user's request 2026-10-19: "on granite rocks" -> terrestrial + geology granite)
  if (legume_mode) addc("habitat_geology", prep(r[["Habitat"]]),
                        c("granit\\w*" = "granite", "lateri\\w*" = "laterite", "sandstones?" = "sandstone", "limestones?|calcareous|calcrete|calcarenite" = "limestone",
                          "basalt\\w*" = "basalt", "quartzites?" = "quartzite", "shales?" = "shale", "banded iron formations?|bif|ironstones?|iron-rich" = "ironstone",
                          "rhyolit\\w*" = "rhyolite", "serpentin\\w*" = "serpentinite", "dolerit\\w*" = "dolerite", "schists?" = "schist", "gneiss\\w*" = "gneiss",
                          "dolomit\\w*" = "dolomite", "conglomerates?" = "conglomerate", "mudstones?" = "mudstone", "siltstones?" = "siltstone", "cherts?" = "chert",
                          "greenstones?" = "greenstone", "slates?" = "slate", "quartz(?! ?ite)" = "quartz", "volcanic" = "volcanic", "gypsum|gypseous" = "gypsum"))

  # ---- flowering predictability (candidate trait flowering_predictability, user's request 2026-10-27; Orchidaceae for now): whether
  # flowering is irregular in time. Draft values, in text order: sporadic (within a season or the year: "Flowers sporadically",
  # "sporadic bursts / flushes / intervals", "intermittent spasms", "spasmodic intervals"), erratic (between years: "Flowering is erratic
  # from year to year", "Flowering occurs irregularly, sometimes with many years between events", "flowers infrequently") and
  # synchronous (gregarious: "all plants in an area flowering simultaneously", "most plants in an area flowering synchronously").
  # Phenology / Ecology / Notes only (in descriptions "flowers irregular" means zygomorphic); hedged clauses stay text only.
  if (orchid_mode) {
    fp_src <- collapse_text(c(prep(r[["Phenology"]]), prep(r[["Ecology"]]), prep(r[["Notes"]])), " ")
    fp_terms <- c("\\bsporadic(?:ally)?\\b|\\bspasmodic\\w*|\\bintermittent(?:ly)? (?:spasms|flushes|bursts|flowering)\\b|\\bin (?:sporadic )?(?:bursts|flushes|spasms)\\b" = "sporadic",
                  "\\berratic\\w*|\\b(?:flower\\w*|occurs) irregularly\\b|\\bflowers? infrequently\\b|\\bflowering is infrequent\\b" = "erratic",
                  # (synchronous only for plants: Bogoria "Sections of flowers on the racemes open simultaneously" is the raceme)
                  "\\bplants\\b[^.;|]{0,40}\\b(?:synchronous\\w*|simultaneously|at (?:the )?same time)\\b" = "synchronous")
    if (!is.na(fp_src)) {
      fp_cl <- unlist(map(sentences(fp_src), ~ str_split(.x, ";\\s*")[[1]]))
      fp_cl <- fp_cl[str_detect(fp_cl, rx(paste(names(fp_terms), collapse = "|"))) & str_detect(fp_cl, rx("\\b(?:flower\\w*|bloom\\w*|open\\w*)\\b"))]
      if (length(fp_cl)) {
        fp_txt <- paste(fp_cl[!str_detect(fp_cl, rx("\\b(?:probably|probable|possibly|possible|perhaps|apparently|thought to|likely)\\b"))], collapse = " | ")
        hits <- map_dfr(names(fp_terms), function(p) { loc <- str_locate_all(fp_txt, rx(p))[[1]]
          if (nrow(loc)) tibble(pos = loc[, 1], value = fp_terms[[p]]) else NULL })
        vals <- if (nrow(hits)) hits %>% arrange(pos) %>% pull(value) %>% unique() else character(0)
        add(tibble(trait = "flowering_predictability", value = paste(vals, collapse = " "), desc = collapse_text(fp_cl, " | "), min = NA, max = NA,
                   ctx_type = NA, ctx_value = NA, qualifier = NA_character_, kind = "cat"))
      }
    }
  }

  # ---- time of day the flowers open (candidate trait flower_opening_time; draft values day, morning, afternoon, evening,
  # night, in text order): "flowers open during the day, closing at night", "opening in the morning", "day-flowering",
  # "Flowers diurnal". Closing times and queried parts ("(and night?)") are not scored; hedged clauses stay text only.
  ot_src <- collapse_text(c(prep(r[["Description"]]), prep(r[["Phenology"]]), prep(r[["Ecology"]]), prep(r[["Notes"]])), " ")
  if (!is.na(ot_src)) {
    ot_cl <- unlist(map(sentences(ot_src), ~ str_split(.x, ";\\s*")[[1]]))
    ot_cl <- ot_cl[str_detect(ot_cl, rx(paste0("\\bflowers?\\b[^.;]{0,40}\\b(?:open\\w*|diurnal|nocturnal)\\b[^.;]{0,25}\\b(?:day|daytime|morning|afternoon|evening|night|diurnal|nocturnal)\\b",
                                               "|\\bflowers? (?:diurnal|nocturnal)\\b|\\b(?:day|night)[- ](?:blooming|flowering)\\b")))]
    if (length(ot_cl)) {
      ot_terms <- c("\\bday[- ](?:blooming|flowering)\\b|\\b(?:during|in|at|by) (?:the )?(?:day|daytime)\\b|\\bdiurnal\\b|\\bduring day\\b|\\ball day\\b" = "day",
                    "\\bmorning\\b" = "morning", "\\bafternoon\\b" = "afternoon", "\\bevening\\b" = "evening",
                    "\\bnight[- ](?:blooming|flowering)\\b|\\b(?:at|during|by) (?:the )?night\\b|\\bnocturnal\\b" = "night")
      ot_hedge <- "\\b(?:probably|possibly|perhaps|may|might|apparently|thought to|likely)\\b"
      ot_txt <- ot_cl[!str_detect(ot_cl, rx(ot_hedge))] %>%
        str_remove_all("\\([^()]*\\?[^()]*\\)") %>%
        str_remove_all(rx(",?\\s*(?:and |but )?(?:partially |fully )?clos\\w*\\b.*$|\\bnot appreciably fading.*$"))
      ot_txt <- paste(ot_txt, collapse = " | ")
      hits <- map_dfr(names(ot_terms), function(p) { loc <- str_locate_all(ot_txt, rx(p))[[1]]
        if (nrow(loc)) tibble(pos = loc[, 1], value = ot_terms[[p]]) else NULL })
      vals <- if (nrow(hits)) hits %>% arrange(pos) %>% pull(value) %>% unique() else character(0)
      add(tibble(trait = "flower_opening_time", value = paste(vals, collapse = " "), desc = collapse_text(ot_cl, " | "), min = NA, max = NA,
                 ctx_type = NA, ctx_value = NA, qualifier = NA_character_, kind = "cat"))
    }
  }

  # ---- how long each flower stays open (flower_lifespan, days): "The flowers last 1-3 days", "each flower lasting 1 or 2 days",
  # "Each flower lasts a single day", "The flowers last two to three days before self-pollinating" (hours / weeks converted;
  # "a few days", "several days to many weeks" and hedged clauses are not scored)
  if (!is.na(ot_src)) {
    nw <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9, ten = 10, single = 1)
    num_w <- paste0("[0-9]+(?:\\.[0-9]+)?|", paste(names(nw), collapse = "|"))
    fl_re <- paste0("\\bflowers?\\b[^.;]{0,50}?\\b(?:lasts?|lasting|remains? open(?: for)?|remaining open(?: for)?)\\s+(?:for\\s+)?(?:only\\s+|about\\s+|c\\.\\s*|up to\\s+|a\\s+)?(",
                    num_w, ")(?:\\s*(?:-|to|or)\\s*(", num_w, "))?\\s+(hours?|days?|weeks?|months?)\\b")
    ls_cl <- unlist(map(sentences(ot_src), ~ str_split(.x, ";\\s*")[[1]]))
    ls_cl <- ls_cl[str_detect(ls_cl, rx(fl_re)) & !str_detect(ls_cl, rx("\\b(?:probably|possibly|perhaps|may|might|apparently|thought to|likely)\\b|\\bin cultivation\\b"))]
    if (length(ls_cl)) {
      m <- str_match(ls_cl[1], rx(fl_re))
      as_n <- function(z) if (is.na(z)) NA_real_ else if (str_detect(z, "^[0-9]")) as.numeric(z) else unname(nw[str_to_lower(z)])
      k <- c(hour = 1 / 24, day = 1, week = 7, month = 30)[[str_remove(str_to_lower(m[4]), "s$")]]
      lo <- as_n(m[2]) * k; hi <- coalesce(as_n(m[3]) * k, lo)
      add(rec_num("flower_lifespan", c(min = lo, max = hi), m[1]))
    }
  }

  # ---- phenology
  ph_txt <- prep(r[["Phenology"]], keep_q = TRUE)
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

  # hedged calendars ("Flowers June-February or possibly all year", "Flowering apparently occurs between late August and December"):
  # the months a hedged sentence adds to the stated calendar go on their own rows, with the hedge word as the scoring_certainty
  # context (user 2026-10-29); "may occur at any time of the year" counts as stated (user 2026-10-28)
  if (!is.na(ph_txt)) {
    cert_re <- "\\b(probably|possibly|perhaps|likely|almost certainly|apparently|presumably|seemingly|suspected|thought to(?: be)?|is thought(?: to be)?|it may also|appears? to(?: be)?|seems? to(?: be)?)\\b"
    may_re <- "\\b(may|could|might)\\b"
    ym <- function(x) if (is.null(x) || is.na(x)) integer(0) else which(str_split(x, "")[[1]] == "y")
    qx_m <- function(k) if (nrow(ph$q_extra)) unlist(ph$q_extra$m[ph$q_extra$k == k]) else integer(0)
    cert_m <- list(fl = c(ym(ph$fl), ym(ph$fl_x), qx_m("fl")), fr = c(ym(ph$fr), ym(ph$fr_x), qx_m("fr")))
    hrows <- list()
    for (s in sentences(ph_txt)) {
      q_re <- "\\?\\s*(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)|(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]{0,6}\\.?\\s*\\?"
      has_q <- str_detect(s, q_re)
      if (!str_detect(s, regex(cert_re, ignore_case = TRUE)) && !str_detect(s, may_re) && !has_q) next
      anytime <- str_detect(s, regex("\\b(?:may|could|might|can) (?:also )?(?:occur|flower|be found|fruit)\\b[^.;]{0,40}\\b(?:any time|any month|all year|throughout the year)", ignore_case = TRUE)) &&
        !str_detect(s, regex(cert_re, ignore_case = TRUE))
      cw <- str_to_lower(coalesce(str_match(s, regex(cert_re, ignore_case = TRUE))[, 2], str_match(s, may_re)[, 2], if (has_q) "?" else NA_character_))
      if (identical(cw, "?")) cw <- "possibly"
      cw <- if (anytime) NA_character_ else if (str_detect(cw, "^(?:appears?|seems?) to")) "apparently" else if (str_detect(cw, "^(?:thought to|is thought)")) "thought to be" else
        if (cw %in% c("may", "could", "might", "it may also")) "possibly" else cw
      sd <- str_squish(str_remove_all(str_remove_all(s, regex(paste0(cert_re, "\\s*"), ignore_case = TRUE)), paste0(may_re, "(?: also)?\\s*")))
      # (the cause is not a calendar: Triodia mallota "Flowers recorded in March, presumably following summer rainfall")
      sd <- str_squish(str_remove_all(sd, regex(",?\\s*\\b(?:after|following|in response to|dependent on|depending on|due to|as (?:the|a) result of)\\b[^.;,]*?\\brain\\w*(?:\\s+respectively)?", ignore_case = TRUE)))
      sd <- str_squish(str_remove_all(sd, "\\?"))
      hp <- phenology_parse(sd)
      for (k in c("fl", "fr")) {
        hq <- if (!is.null(hp$main_q) && !is.na(hp$main_q[[k]])) hp$main_q[[k]] else NA_character_
        cand <- list(list(m = ym(hp[[k]]), q = hq), list(m = ym(hp[[paste0(k, "_x")]]), q = "rarely"))
        if (nrow(hp$q_extra)) for (j in which(hp$q_extra$k == k)) cand[[length(cand) + 1]] <- list(m = unlist(hp$q_extra$m[j]), q = hp$q_extra$q[j])
        for (cc in cand) {
          m <- setdiff(cc$m, cert_m[[k]])
          q <- if (is.na(cw) && is.na(cc$q) && length(cert_m[[k]])) "occasionally" else cc$q
          if (length(m)) hrows[[length(hrows) + 1]] <- tibble(k = k, q = q, cw = cw, m = list(m), s = s)
        }
      }
    }
    if (length(hrows)) {
      hr <- bind_rows(hrows) %>% group_by(k, q, cw) %>% summarise(m = list(sort(unique(unlist(m)))), s = collapse_text(s, " "), .groups = "drop")
      for (j in seq_len(nrow(hr))) add(tibble(trait = c(fl = "flowering_time", fr = "fruiting_time")[[hr$k[j]]], value = yn(hr$m[[j]]), desc = hr$s[j],
                                              min = NA, max = NA, ctx_type = NA, ctx_value = NA, qualifier = hr$q[j], certainty = hr$cw[j]))
    }
  }

  # Poaceae: the "flowers" measured are florets (candidate floret_length / _width, "Floret 6.5-9 mm long"); the caryopsis embryo and
  # hilum lengths ("embryo 0.7-1.6 mm long; hilum 3-3.3 mm long") are candidate traits
  if (grass_mode) {
    recs <- map(recs, ~ .x %>% mutate(trait = str_replace(trait, "^flower_(length|width|diameter)$", "floret_\\1")))
    gx <- mark_extremes(join_units(c(unit_text(u, c("fruit", "seed"), parts = "any"))) %||% "")
    for (gp in c("embryo", "hilum")) {
      gm <- str_match(gx, rx(paste0("\\b", gp, "s?\\b[^,;|]{0,20}?(", meas, ")\\s*long")))
      if (!is.na(gm[1])) addn(paste0(gp, "_length"), meas_of(gm[2]), unmark(gm[1]))
    }
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
    cone_description = join_units(unit_text(u, "cone", parts = "any")),
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

# taxa are independent, so they are parsed in parallel (forked workers; the mode flags extract_one sets are reset on
# every call)
stop_failed <- function(res) {
  failed <- map_lgl(res, ~ inherits(.x, "try-error"))
  if (any(failed)) stop(paste(map_chr(res[failed], as.character), collapse = "\n"))
}
res <- parallel::mclapply(seq_len(nrow(d)), function(i) {
  tryCatch(extract_one(d[i, ]), error = function(e) stop(paste(d$scientific_name[i], conditionMessage(e))))
}, mc.cores = n_cores)
stop_failed(res)
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
  res <- parallel::mclapply(seq_len(nrow(d_hi_parts)), function(i) extract_one(d_hi_parts[i, ], group_filter = group_filter),
                            mc.cores = n_cores)
  stop_failed(res)
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
  "Casuarinaceae", "leaf_margin", "Allocasuarina", "leaf_margin", "Casuarina", "leaf_margin", "Gymnostoma", "leaf_margin",
  # Iridaceae "evergreen, or leaves and flowers annual": the alternative is not a listed term
  "Iridaceae", "leaf_phenology",
  # Acianthus "forming spreading, clonal colonies" (the colonies spread, not the stems); "held ... on a fleshy stalk" (the leaf
  # stalk is fleshy, not the leaf)
  "Acianthus", "stem_growth_habit", "Acianthus", "leaf_texture", "Acianthus", "plant_succulence",
  # Brackenridgea "Trees or large shrubs, with spreading branches": branch posture of a tree, not a stem growth habit
  "Brackenridgea", "stem_growth_habit",
  # Burnettia "Plants brownish or purplish ... leaf-like purplish bracts": not a leaf colour (the plants are leafless)
  "Burnettia", "leaf_surface_colour",
  # Cadetia "small, compact clumps ...; or spreading patches with creeping rhizomes": the habit varies across the genus
  "Cadetia", "stem_growth_habit",
  # Nepenthes "young plants rosetted": juvenile plants only
  "Nepenthes", "stem_growth_habit", "Nepenthes", "leaf_arrangement", "Nepenthaceae", "stem_growth_habit", "Nepenthaceae", "leaf_arrangement",
  # Arthrochilus "flowering plants consist of a thin, wiry, leafless stem": the flowering stem is leafless, the plants have leaf rosettes
  "Arthrochilus", "leaf_length_type", "Arthrochilus", "leaf_type",
  # Blepharoglossum "dense clumps with thin, spreading roots": root posture
  "Blepharoglossum", "stem_growth_habit",
  # Nymphaea "the floating lamina ... slightly undulate" is N. ondinea only; filaments "either ... membranous, or ... tough or membranous"
  "Nymphaea", "leaf_lamina_posture", "Nymphaea", "filament_texture",
  # Cymbidium "with pseudobulbs or slender woody stems" (C. suave has no pseudobulbs)
  "Cymbidium", "storage_organ",
  # "spreading" roots or colonies, not the stems: Geodorum, Pachystoma and Luisia "spreading roots", Cyrtostylis "spreading clonal
  # colonies" (also handled in the stem_growth_habit text; listed here for the genus descriptions)
  "Geodorum", "stem_growth_habit", "Pachystoma", "stem_growth_habit", "Luisia", "stem_growth_habit", "Cyrtostylis", "stem_growth_habit",
  # Erythrorchis "usually climbing on tree trunks": a climber rooted in the ground, not an epiphyte
  "Erythrorchis", "plant_growth_substrate",
  # Trachoma "flowers ... are sometimes produced in large flushes with all the plants in an area flowering synchronously" (hedged; vetted
  # as "sometimes"); Vanilla "Leaves absent, or if present, ..." (V. planifolia has large leaves)
  "Trachoma", "flowering_predictability", "Vanilla", "leaf_length_type", "Vanilla", "leaf_type",
  # Breynia, Glochidion "Stem leaves scale-like (phyllanthoid branching)": the branchlets bear normal leaves
  "Breynia", "leaf_length_type", "Breynia", "leaf_type", "Glochidion", "leaf_length_type", "Glochidion", "leaf_type",
  # Pittosporaceae "Slender trees, or erect or scandent to procumbent shrubs, sometimes heteroblastic, rhizomatous": not all members;
  # Plantaginaceae "Leaves rosette at base, opposite or alternate, simple, entire to pinnately lobed": alternatives
  # Pittosporaceae "Flowers ... fragrant": too broad a family statement to hold for each species (user, 2026-10-28)
  "Pittosporaceae", "storage_organ", "Pittosporaceae", "stem_growth_habit", "Pittosporaceae", "flower_scent_production",
  "Plantaginaceae", "leaf_arrangement", "Plantaginaceae", "stem_growth_habit", "Plantaginaceae", "leaf_lobation",
  # Agrostis "Annuals or perennials, rhizomatous, stoloniferous, caespitose or decumbent": alternatives across the species (annuals too)
  "Agrostis", "storage_organ",
  # Rubiaceae "annual and perennial herbs, epiphytes, climbers, shrubs or trees": a list of alternatives across the family
  "Rubiaceae", "plant_growth_substrate"
)
# groups whose description stitches together several segregate genera, so no statement holds throughout: Dendrobium sens. lat.
# ("includes the following descriptions from Jones (2021): Australorchis ...: Epiphytic orchids ... Ceraia ...: ... Conostalix ...:
# Terrestrial clumping orchids ..."); likewise Prasophyllum sens. lat. ("Prasophyllum: deciduous, terrestrial ...", then Paraprasophyllum)
# and Pterostylis sens. lat. (Bunochilus, Diplodium, Hymenochilus, Oligochaetochilus, Pharochilum, Plumatichilos, Pterostylis, Ranorchis,
# Speculantha, Stamnorchis, Urochilus)
copydown_exclude_groups <- c("Dendrobium", "Prasophyllum", "Pterostylis")

# ---------------------------------------------------------------- overrides
# split calendars ("in northern populations ... in southern populations") and other phrasing the parser can't read
ph_over_file <- file.path(raw_dir, "phenology_overrides.csv")
if (file.exists(ph_over_file)) {
  pho <- read_csv(ph_over_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(taxon_name %in% main$taxon_name)
  recs <- recs %>% filter(!(taxon_name %in% pho$taxon_name & trait %in% c("flowering_time", "fruiting_time")))
  recs <- bind_rows(recs, pho %>% pivot_longer(c(flowering_time, fruiting_time), names_to = "trait", values_to = "value") %>%
                      filter(!is.na(value)) %>%
                      transmute(taxon_name, trait, value, desc = description, min = NA_real_, max = NA_real_,
                                ctx_type = NA_character_, ctx_value = NA_character_, qualifier = commonness_qualifier, region = population_region,
                                scoring = na_if(trait_scoring_method, ""), certainty = na_if(scoring_certainty, "")))
}
# ecology traits vetted by reading each Ecology / Notes text (pollination, dispersal, clonality, fire, lifespan ...)
eco_file <- file.path(raw_dir, "ecology_vetted.csv")
if (file.exists(eco_file)) {
  ev <- read_csv(eco_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(taxon_name %in% main$taxon_name)
  # (scoring_certainty: the hedge word of a hedged statement, "Probably killed by fire" -> probably; user 2026-10-29)
  if (!"scoring_certainty" %in% names(ev)) ev$scoring_certainty <- NA_character_
  # (trait_scoring_method: inferred_from_genus / inferred_from_family when the evidence is a genus / family profile's
  # statement about all its members, e.g. Opiliaceae "Cansjera and Opilia are root-parasites")
  # (numeric rows, e.g. lifespan in years: "20-30" = range, "max:10" = "less than 10 years", "min:500" = "500 years or longer")
  ev_num <- str_detect(ev$value, "^(?:min:|max:)?[0-9.]+(?:-[0-9.]+)?$")
  num_part <- function(v, k) { v <- str_remove(v, "^(?:min:|max:)"); p <- str_split_fixed(v, "-", 2)
    if (k == "min") suppressWarnings(as.numeric(ifelse(p[, 2] == "", NA, p[, 1]))) else suppressWarnings(as.numeric(ifelse(p[, 2] == "", p[, 1], p[, 2]))) }
  recs <- bind_rows(recs, ev[!ev_num, ] %>% transmute(taxon_name, trait = trait_name, value, desc = evidence, min = NA_real_, max = NA_real_,
                                           ctx_type = NA_character_, ctx_value = NA_character_, qualifier = commonness_qualifier,
                                           scoring = na_if(trait_scoring_method, ""), certainty = na_if(scoring_certainty, "")),
                    # (min / max are read from value before value is cleared: in the other order every numeric vetted row was empty)
                    ev[ev_num, ] %>% transmute(taxon_name, trait = trait_name,
                                               min = ifelse(str_detect(value, "^min:"), num_part(value, "max"), ifelse(str_detect(value, "^max:"), NA, num_part(value, "min"))),
                                               max = ifelse(str_detect(value, "^min:"), NA, num_part(value, "max")),
                                               value = NA_character_, desc = evidence,
                                               ctx_type = NA_character_, ctx_value = NA_character_, qualifier = commonness_qualifier,
                                               scoring = na_if(trait_scoring_method, ""), certainty = na_if(scoring_certainty, ""), kind = "num"))
}

# ecology values that differ by region, vetted by reading the text (user's rule 2026-10-27: one row per value, with the region as a
# population_region context): they replace the values extracted for that taxon and trait
# (Cyanicula gemmata "Flowers profusely only in the spring following a summer burn, although plants from inland localities can flower
# without fire": fire-dependent in other localities, fire-enhanced inland)
evr_file <- file.path(raw_dir, "ecology_vetted_regions.csv")
if (file.exists(evr_file)) {
  evr <- read_csv(evr_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(taxon_name %in% main$taxon_name)
  if (!"scoring" %in% names(recs)) recs$scoring <- NA_character_
  recs <- recs %>% filter(!(paste(taxon_name, trait) %in% paste(evr$taxon_name, evr$trait_name) & is.na(scoring)))
  recs <- bind_rows(recs, evr %>% transmute(taxon_name, trait = trait_name, value, desc = evidence, min = NA_real_, max = NA_real_,
                                            ctx_type = NA_character_, ctx_value = NA_character_, qualifier = NA_character_, region = population_region))
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
  anti_join(copydown_exclude, by = c("group_name", "trait")) %>% filter(!group_name %in% copydown_exclude_groups) %>%
  select(group_name, group_rank, trait, value, desc) %>% distinct(group_name, trait, .keep_all = TRUE)
own_tv <- recs %>% filter(!is.na(value), value != "", !trait %in% c("flowering_time", "fruiting_time")) %>%
  group_by(taxon_name, trait) %>% summarise(vals = paste(value, collapse = " "), .groups = "drop") %>%
  left_join(main %>% transmute(taxon_name, genus = gkey(taxon_name), family), by = "taxon_name")
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
taxa <- main %>% transmute(taxon_name, genus = gkey(taxon_name), family)
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
# orchid "flower stem" lengths (mm): for terrestrial orchids both the height of the flowering plant (plant_height_reproductive, m)
# and the inflorescence length (user's choice); for epiphytes and lithophytes (whose flower stems arch or hang from the
# pseudobulb / stem) and reed-stem orchids with a stated height, the inflorescence length only
if ("orchid_flower_stem_length" %in% recs$trait) {
  epi <- recs %>% filter(trait == "plant_growth_substrate", str_detect(coalesce(value, ""), "epiphyte|lithophyte")) %>% distinct(taxon_name) %>% pull()
  # (and for reed-stem orchids whose own height is given, "Stems 1-1.5 m tall": the flower stem is then a terminal inflorescence)
  own_ht <- recs %>% filter(trait == "plant_height", is.na(scoring)) %>% distinct(taxon_name) %>% pull()
  # (and a flower stem shorter than the plant's own leafy stems is a terminal inflorescence: Tropidia "Stems erect, 100-200 mm long ...
  # Inflorescence terminal. Flower stem 10-20 mm long")
  st_max <- recs %>% filter(trait == "stem_length", is.na(scoring)) %>% group_by(taxon_name) %>% summarise(st = max(c(min, max), na.rm = TRUE), .groups = "drop")
  own_ht <- c(own_ht, recs %>% filter(trait == "orchid_flower_stem_length") %>% inner_join(st_max, by = "taxon_name") %>%
                filter(coalesce(max, min) < st) %>% distinct(taxon_name) %>% pull())
  ofs <- recs %>% filter(trait == "orchid_flower_stem_length") %>% mutate(desc = paste("[flower stem]", desc))
  terr <- ofs %>% filter(!taxon_name %in% c(epi, own_ht))
  # every flower stem is an inflorescence length; a terrestrial orchid's is also the plant's height when flowering
  recs <- bind_rows(recs %>% filter(trait != "orchid_flower_stem_length"),
                    ofs %>% mutate(trait = "inflorescence_length"),
                    terr %>% mutate(trait = "plant_height_reproductive", across(any_of(c("min", "max", "extreme_min", "extreme_max")), ~ .x / 1000)))
  # (a flower stem replaces neither a stated inflorescence length nor a stated height)
  recs <- recs %>% group_by(taxon_name, trait, qualifier, ctx_type, ctx_value, scoring) %>%
    filter(!(str_detect(coalesce(desc, ""), "^\\[flower stem\\]") & n() > 1)) %>% ungroup()
}
# rosette is both a stem growth habit and a leaf arrangement (user's rule): after copy-down too (a genus "basal rosette" fills
# stem_growth_habit of members whose own "Leaves basal" blocks the leaf_arrangement copy), with the same scoring
ros <- recs %>% filter(trait %in% c("stem_growth_habit", "leaf_arrangement"), str_detect(coalesce(value, ""), "\\brosette\\b"))
ros_add <- bind_rows(
  ros %>% filter(trait == "stem_growth_habit") %>% anti_join(ros %>% filter(trait == "leaf_arrangement"), by = "taxon_name") %>% mutate(trait = "leaf_arrangement"),
  ros %>% filter(trait == "leaf_arrangement") %>% anti_join(ros %>% filter(trait == "stem_growth_habit"), by = "taxon_name") %>% mutate(trait = "stem_growth_habit")) %>%
  mutate(value = "rosette") %>% distinct(taxon_name, trait, .keep_all = TRUE)
recs <- bind_rows(recs, ros_add)

# ---------------------------------------------------------------- inferred typical values (user's request, 2026-10-19)
# Substrate, photosynthetic organ, parasitism and leaf type are usually stated only when atypical (epiphyte, aquatic, leafless,
# parasitic, scale leaves). Where the profile gives no value and nothing in its text or its group suggests an atypical state, the
# typical value is recorded on a trait_scoring_method = inferred row, with the evidence in the description:
#   plant_growth_substrate terrestrial: a woody plant (tree, shrub, mallee, subshrub, woody climber) whose text names no aquatic,
#     epiphytic, lithophytic, marine or stem-parasitic habit; other plants also need habitat text naming no wetland or water;
#   plant_photosynthetic_organ leaf: leaves described (sizes or shape), none of leafless / scale / phyllode / cladode /
#     photosynthetic-stem / achlorophyllous wording, not a leafless or scale-leaved group;
#   parasitic not_parasitic: a green plant (leaves, phyllodes, cladodes or photosynthetic stems) outside the parasitic families and genera, its text naming no parasitism,
#     host or mycoheterotrophy;
#   leaf_type broadleaf: a leafy green plant whose own leaf (or leaflet) shapes are all flat-lamina shapes (linear only when the
#     leaves are >= 2 mm wide), or whose leaves are >= 3 mm wide, or a fern with a divided lamina, its leaf text naming no terete,
#     needle, involute or scale leaves.
# (Fabaceae: the description's own "on trunks", "on largest trees", "midrib submerged" are bark, pods and veins; only the habit sentence
# and the Habitat / Ecology / Notes text are read for the substrate)
inf_text <- d %>% left_join(main %>% select(taxon_name, habit_description), by = c("scientific_name" = "taxon_name")) %>%
  transmute(taxon_name = scientific_name,
                            txt = str_to_lower(paste(if_else(family == "Fabaceae", coalesce(habit_description, ""), coalesce(Description, "")), coalesce(Habitat, ""), coalesce(Ecology, ""), coalesce(Notes, ""))),
                            # (user, 2026-10-28: default values unless the text names a special case; the broad water / epiphyte / rock
                            # test reads the habit sentence and the Habitat / Ecology / Notes text, the morphology only for unambiguous
                            # words: "spikelets ... on secondary branches", "persisting on tree" are not epiphyte wording)
                            stxt = str_to_lower(paste(coalesce(habit_description, ""), coalesce(Habitat, ""), coalesce(Ecology, ""), coalesce(Notes, ""))),
                            dtxt = str_to_lower(coalesce(Description, "")),
                            hab = str_to_lower(coalesce(Habitat, "")))
inf_fam_aquatic <- c("Hydrocharitaceae", "Potamogetonaceae", "Ruppiaceae", "Zosteraceae", "Posidoniaceae", "Cymodoceaceae", "Zannichelliaceae",
                     "Najadaceae", "Nymphaeaceae", "Nelumbonaceae", "Cabombaceae", "Ceratophyllaceae", "Aponogetonaceae", "Hydatellaceae",
                     "Marsileaceae", "Salviniaceae", "Azollaceae", "Podostemaceae", "Pontederiaceae", "Lemnaceae", "Menyanthaceae",
                     "Lentibulariaceae", "Juncaginaceae", "Typhaceae", "Sparganiaceae", "Alismataceae", "Elatinaceae", "Isoetaceae",
                     "Butomaceae", "Limnocharitaceae", "Hydroleaceae", "Philydraceae",
                     # (epiphyte-rich groups: substrate only when stated)
                     "Orchidaceae", "Polypodiaceae", "Hymenophyllaceae", "Grammitidaceae", "Davalliaceae", "Psilotaceae", "Bromeliaceae",
                     "Gesneriaceae", "Rhizophoraceae")
inf_fam_parasitic <- c("Loranthaceae", "Viscaceae", "Santalaceae", "Olacaceae", "Opiliaceae", "Balanophoraceae", "Apodanthaceae", "Orobanchaceae",
                       "Cytinaceae", "Rafflesiaceae", "Lennoaceae", "Krameriaceae", "Mitrastemonaceae", "Schoepfiaceae", "Cervantesiaceae",
                       "Thesiaceae", "Amphorogynaceae", "Comandraceae", "Ximeniaceae", "Aptandraceae", "Misodendraceae",
                       # mycoheterotrophs
                       "Corsiaceae", "Triuridaceae", "Burmanniaceae", "Thismiaceae")
inf_gen_parasitic <- c("Cassytha", "Cuscuta", "Exocarpos", "Santalum", "Leptomeria", "Choretrum", "Omphacomeria", "Spirogardnera", "Thesium",
                       "Anthobolus", "Dendrotrophe", "Notothixos", "Korthalsella", "Viscum", "Nuytsia", "Euphrasia", "Striga", "Buchnera",
                       "Centranthera", "Rhamphicarpa", "Parentucellia", "Bartsia", "Odontites", "Orobanche", "Sopubia", "Alectra", "Melasma",
                       "Pilostyles", "Balanophora", "Mystropetalon")
inf_fam_leafless <- c("Casuarinaceae", "Cactaceae", "Restionaceae", "Psilotaceae", "Equisetaceae", "Balanophoraceae", "Apodanthaceae", "Corsiaceae",
                      "Triuridaceae", "Burmanniaceae", "Thismiaceae", "Cytinaceae")
inf_gen_leafless <- c("Tecticornia", "Salicornia", "Sarcocornia", "Halosarcia", "Cassytha", "Cuscuta", "Exocarpos", "Ephedra")
# (Fabaceae, user's choice 2026-10-19: plants "on granite rocks", "in crevices" are scored terrestrial on a CHECK-flagged inferred row;
# "on coastal cliffs" is plain terrestrial; rock words therefore do not block the Fabaceae inference)
inf_rock_re <- "\\b(?:on (?:the )?(?:[a-z-]+ ){0,2}(?:rocks?|boulders?)|among (?:the )?(?:[a-z-]+ ){0,2}(?:rocks?|boulders?)|in (?:rock )?crevices|crevices|(?:granite|rock) (?:outcrops?|pavements?|sheets?))\\b"
inf_water_strong <- "\\b(?:aquatic|submerged|submersed|free-floating|floating|in (?:shallow |still |running |fresh |brackish |deep |standing )?water|semi-?aquatic|epiphyt\\w*|hemi-?epiphyt\\w*|lithophyt\\w*|strangl\\w*|on (?:the )?(?:[a-z-]+ ){0,2}(?:rocks?|boulders?|cliffs?|trees?|branches|trunks?)|in rock crevices|crevices|marine|tidal|intertidal|mangroves?|estuar\\w*|rheophyt\\w*|parasit\\w*|on hosts?)\\b"
# (duckweeds, Araceae since Lemnaceae was merged: "grows in eutrophic, quiet waters")
inf_water_strong <- paste0(inf_water_strong, "|\\b(?:quiet|still|open|stagnant|fresh|eutrophic|mesotrophic) waters\\b|\\b(?:grows|found|floating) in (?:[a-z-]+,? ){0,3}waters\\b")
# ("midrib submerged", "gland submerged ... within the lamina" are morphology; but Myriophyllum "submerged leaves trifid", Ludwigia
# "pneumatophores arising from submerged or buried roots" are water plants)
inf_swamp_re <- "\\b(?:swamps?|swampy|marsh(?:es)?|marshy)\\b"
inf_water_desc <- "\\b(?:submerged|submersed|floating)(?:,? (?:or|and) [a-z]+)? (?:leaves|leaf|stems?|shoots?|plants?|parts|forms?|roots?|lamina)\\b|\\bif submerged\\b|\\b(?:aquatic|free-floating|semi-?aquatic|epiphyt\\w*|hemi-?epiphyt\\w*|lithophyt\\w*|strangl\\w*|marine|seagrass\\w*|mangroves?)\\b"
inf_water_strong_leg <- str_replace(str_replace(inf_water_strong, fixed("rocks?|boulders?|cliffs?|"), ""), fixed("|in rock crevices|crevices"), "")
inf_water_wet <- "\\b(?:swamps?|swampy|marsh\\w*|bogs?|boggy|ponds?|lakes?|lagoons?|billabongs?|waterholes?|creek ?beds?|stream ?beds?|river ?beds?|beds of|inundat\\w*|flooded|flooding|waterlogged|mud\\w*|wetlands?|seepages?|soaks?|claypans?|gilgais?|saline|salt ?lakes?|stream margins|water's edge|edges? of (?:streams|creeks|rivers|pools|lakes))\\b"
inf_noleaf_re <- "\\b(?:phyllod\\w*|cladod\\w*|phyllocla\\w*|(?<!flowering plants )(?<!flowering plants are )(?<!flowering plant is )leafless(?! (?:at|when|while|during) (?:the time of )?(?:flowering|anthesis))(?! (?:flower(?:ing)? )?(?:portions?|parts?|sections?|lengths?|internodes?|stems?|scapes?|peduncles?|inflorescences?))|aphyllous|photosynthetic stems?|stems? (?:green and )?photosynthetic|assimilat\\w* (?:stems?|branchlets?)|scale-?like leaves|(?<!upper |uppermost |floral |cauline |distal |stem |bract-like |inflorescence )leaves (?:reduced|scale-like|minute|vestigial|caducous|early caducous|soon falling|ephemeral)|reduced to (?:minute )?scales|scale leaves|leaves (?:absent|0)|broom-like|switch-like|saprophyt\\w*|mycoheterotroph\\w*|achlorophyll\\w*|(?:without|lacking|lacks) chlorophyll|non-green|holoparasit\\w*)\\b"
inf_par_re <- "\\b(?:parasit\\w*|hemiparasit\\w*|haustor\\w*|host plants?|on the roots of|attached to (?:the )?roots|saprophyt\\w*|mycoheterotroph\\w*|achlorophyll\\w*)\\b"
inf_nonflat_re <- "\\b(?:terete|subterete|semi-?terete|needle\\w*|acicular|subulate|filiform|setaceous|involute|convolute|inrolled|rolled|scale-like|cylindrical|triquetrous|trigonous|quadrangular|channelled|grooved above)\\b"
# (Fabaceae: phyllodes are the leaves of most Acacia; a flat phyllode is a broadleaf, so phyllode wording does not block leaf_type)
inf_noleaf_re_leg <- str_replace(inf_noleaf_re, fixed("phyllod\\w*|"), "")
flat_shapes <- c("cordate", "deltate", "elliptical", "falcate", "lanceolate", "linear_elliptical", "narrowly_elliptical", "narrowly_lanceolate",
                 "narrowly_oblanceolate", "narrowly_oblong", "narrowly_obovate", "narrowly_obtrullate", "narrowly_ovate", "narrowly_rhomboidal",
                 "narrowly_triangular", "obcordate", "oblanceolate", "oblate", "oblong", "obovate", "obtriangular", "obtrullate", "orbicular", "ovate",
                 "peltate", "reniform", "rhomboidal", "spathulate", "triangular", "trullate", "widely_elliptical", "widely_obovate", "widely_obtrullate",
                 "widely_ovate", "ensiform", "strap-shaped")
tv <- function(tr) recs %>% filter(trait %in% tr, !is.na(value), value != "") %>% group_by(taxon_name) %>% summarise(v = paste(unique(value), collapse = " "), .groups = "drop")
has_num <- function(tr) recs %>% filter(trait %in% tr, !is.na(min) | !is.na(max)) %>% distinct(taxon_name) %>% pull()
inf <- main %>% transmute(taxon_name, family, genus = word(taxon_name, 1)) %>%
  left_join(inf_text, by = "taxon_name") %>%
  left_join(tv("plant_growth_form") %>% rename(gf = v), by = "taxon_name") %>%
  left_join(tv("plant_growth_substrate") %>% rename(sub = v), by = "taxon_name") %>%
  left_join(tv("plant_photosynthetic_organ") %>% rename(pho = v), by = "taxon_name") %>%
  left_join(tv("parasitic") %>% rename(par = v), by = "taxon_name") %>%
  left_join(tv("leaf_type") %>% rename(lt = v), by = "taxon_name") %>%
  left_join(tv(c("leaf_length_type")) %>% rename(llt = v), by = "taxon_name") %>%
  left_join(recs %>% filter(trait %in% c("leaf_shape", "leaflet_shape"), is.na(scoring) | scoring == "inferred_from_species", !is.na(value), value != "") %>%
              group_by(taxon_name) %>% summarise(shp = paste(unique(unlist(str_split(value, " "))), collapse = " "), .groups = "drop"), by = "taxon_name") %>%
  left_join(tv("leaf_lamina_division") %>% rename(div = v), by = "taxon_name") %>%
  left_join(recs %>% filter(trait == "leaf_width", !is.na(min) | !is.na(max)) %>% group_by(taxon_name) %>% summarise(lw = min(coalesce(min, max)), .groups = "drop"), by = "taxon_name") %>%
  left_join(main %>% transmute(taxon_name, ltxt = str_to_lower(coalesce(leaf_description, ""))), by = "taxon_name") %>%
  mutate(txt = coalesce(txt, ""), hab = coalesce(hab, ""), stxt = coalesce(stxt, ""), dtxt = coalesce(dtxt, ""),
         leafy = taxon_name %in% has_num(c("leaf_length", "leaf_width", "leaflet_length", "leaflet_width")) | !is.na(shp),
         woody = str_detect(coalesce(gf, ""), "\\b(?:tree|shrub|mallee|subshrub|climber_woody)\\b"))
inf <- inf %>% mutate(
  # (herbs with no Habitat text get the default too; wetland wording in the habitat text still blocks it for herbs: swamp and creek-bed
  # plants may be semiaquatic; a taxon without a growth form is not blocked)
  sub_ok = is.na(sub) & !family %in% inf_fam_aquatic & !genus %in% inf_gen_parasitic & !family %in% c("Loranthaceae", "Viscaceae") &
    !str_detect(stxt, ifelse(family == "Fabaceae", inf_water_strong_leg, inf_water_strong)) & !str_detect(dtxt, inf_water_desc) &
    (woody | !str_detect(stxt, inf_water_wet)) & !str_detect(stxt, inf_swamp_re),
  # (user, 2026-10-28: plants of swamps or marshes, herbs and woody plants alike, are "terrestrial semiaquatic"; other wet wording,
  # creek beds, lake margins, seepages ..., leaves herbs unscored)
  semi_ok = is.na(sub) & !family %in% inf_fam_aquatic & !genus %in% inf_gen_parasitic & !family %in% c("Loranthaceae", "Viscaceae") &
    !str_detect(stxt, ifelse(family == "Fabaceae", inf_water_strong_leg, inf_water_strong)) & !str_detect(dtxt, inf_water_desc) &
    str_detect(stxt, inf_swamp_re),
  rock_chk = family == "Fabaceae" & str_detect(txt, inf_rock_re),
  rock_txt = str_extract(txt, paste0("[^.;]{0,60}", inf_rock_re, "[^.;]{0,30}")),
  pho_ok = is.na(pho) & leafy & !family %in% inf_fam_leafless & !genus %in% inf_gen_leafless &
    !str_detect(coalesce(llt, ""), "leafless|scale_leaves|reduced|bladeless") & !str_detect(coalesce(lt, ""), "leafless|scale") & !str_detect(txt, inf_noleaf_re),
  par_ok = is.na(par) & (pho_ok | coalesce(pho, "") %in% c("leaf", "phyllode", "cladode", "stem")) &
    !family %in% inf_fam_parasitic & !genus %in% inf_gen_parasitic & !str_detect(txt, inf_par_re),
  shp_flat = !is.na(shp) & map2_lgl(str_split(coalesce(shp, ""), " "), coalesce(lw, 0), ~ all(.x %in% c(flat_shapes, "linear", "narrowly_linear")) && (any(.x %in% flat_shapes) || .y >= 2)),
  # (Fabaceae: a terete petiole or rachis says nothing about the blade)
  ltxt = if_else(family == "Fabaceae", str_remove_all(ltxt, "(?:^|\\| )(?:petioles?|rachis|pulvinus|stipules?|glands?)\\b[^|]*"), ltxt),
  lt_ok = is.na(lt) & (pho_ok | coalesce(pho, "") == "leaf" | (family == "Fabaceae" & coalesce(pho, "") == "phyllode")) & !str_detect(ltxt, inf_nonflat_re) &
    !str_detect(txt, ifelse(family == "Fabaceae", inf_noleaf_re_leg, inf_noleaf_re)) &
    (shp_flat | coalesce(lw, 0) >= 3 | (!is.na(div) & str_detect(div, "pinnat|bipinnate|tripinnate|palmat|lobed")))
)
inf_rec <- function(ok, trait, value, why) inf %>% filter(ok) %>%
  transmute(taxon_name, trait = trait, value = value, desc = paste0("[inferred: ", why, "]"), min = NA_real_, max = NA_real_,
            ctx_type = NA_character_, ctx_value = NA_character_, qualifier = NA_character_, region = NA_character_, scoring = "inferred", kind = "cat")
recs <- bind_rows(recs,
  inf_rec(inf$sub_ok & !inf$rock_chk, "plant_growth_substrate", "terrestrial", "no aquatic, epiphytic, lithophytic or marine habit named; growth form and habitat typical of terrestrial plants"),
  inf_rec(inf$semi_ok, "plant_growth_substrate", "terrestrial semiaquatic", "grows in swamps or marshes"),
  inf_rec(inf$sub_ok & inf$rock_chk, "plant_growth_substrate", "terrestrial", "CHECK: grows on or among rocks, assumed rooted in soil") %>%
    mutate(desc = paste0(str_remove(desc, "\\]$"), ": '", str_squish(inf$rock_txt[inf$sub_ok & inf$rock_chk]), "']")),
  inf_rec(inf$pho_ok, "plant_photosynthetic_organ", "leaf", "leaves described; no leafless, scale-leaved, phyllode, cladode or photosynthetic-stem wording"),
  inf_rec(inf$par_ok, "parasitic", "not_parasitic", "green plant outside parasitic families and genera; no parasitism, host or mycoheterotrophy named"),
  inf_rec(inf$lt_ok, "leaf_type", "broadleaf", "leaf or leaflet shapes all flat-lamina shapes, or leaves at least 3 mm wide, or a divided fern lamina; no terete, needle, involute or scale leaves named"))
if (Sys.getenv("FOA_QA") != "") saveRDS(list(main = main, recs = recs, units = map2_dfr(res, d$scientific_name, ~ mutate(.x$units, taxon_name = .y)),
                                             src = d, universal = universal, rejected = rejected, copied = copied), Sys.getenv("FOA_QA"))

# ---------------------------------------------------------------- assemble rows
# one row per taxon x (commonness_qualifier, leaf_division, entity_measured, population_region, trait_scoring_method);
# the main row has no context
for (cc in c("region", "scoring", "certainty", "extreme_min", "extreme_max", "kind")) if (!cc %in% names(recs)) recs[[cc]] <- NA
# phenology, overrides, vetted ecology and copy-down rows are categorical
recs <- recs %>% mutate(kind = coalesce(kind, "cat"))
recs <- recs %>%
  mutate(commonness_qualifier = qualifier,
         leaf_division = ifelse(ctx_type %in% "leaf_division", ctx_value, NA),
         entity_measured = ifelse(ctx_type %in% "entity_measured", ctx_value, NA),
         population_region = coalesce(region, ifelse(ctx_type %in% "population_region", ctx_value, NA)),
         plant_organ_measured = ifelse(ctx_type %in% "plant_organ_measured", ctx_value, NA),
         phenological_stage = ifelse(ctx_type %in% "phenological_stage", ctx_value, NA),
         trait_scoring_method = scoring, scoring_certainty = certainty) %>%
  # categorical values found twice for the same row are merged (stem spines + spiny leaf teeth); for numbers the first statement wins
  group_by(taxon_name, trait, commonness_qualifier, leaf_division, entity_measured, population_region, plant_organ_measured, phenological_stage, trait_scoring_method, scoring_certainty) %>%
  summarise(is_num = first(kind) == "num",
            desc = if (first(kind) == "num") first(desc) else collapse_text(desc, "; "),
            value = if (first(kind) == "num") NA_character_ else collapse_unique(value[!is.na(value) & value != ""]),
            min = first(min), max = first(max), extreme_min = first(extreme_min), extreme_max = first(extreme_max), .groups = "drop")
keys <- c("taxon_name", "commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "phenological_stage", "trait_scoring_method", "scoring_certainty")
cat_w <- recs %>% filter(!is_num) %>% select(all_of(keys), trait, value, desc) %>%
  pivot_wider(names_from = trait, values_from = c(value, desc), names_glue = "{trait}{ifelse(.value == 'desc', '_description', '')}")
num_w <- recs %>% filter(is_num) %>% mutate(across(c(min, max, extreme_min, extreme_max), ~ signif(.x, 6))) %>%
  select(all_of(keys), trait, description = desc, min, max, extreme_min, extreme_max) %>%
  pivot_wider(names_from = trait, values_from = c(description, min, max, extreme_min, extreme_max), names_glue = "{trait}_{.value}")
rows <- full_join(cat_w, num_w, by = keys)

# column order: verbatim description then mapped value for each categorical trait; for each numeric trait the verbatim
# measurement, _min / _max (typical range) and _extreme_min / _extreme_max (parenthetical extremes)
trait_order <- Reduce(union, list(c(
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
  "seed_length", "seed_width", "flowering_time", "fruiting_time", "flowering_cues", "fruiting_cues", "germination_cues", "post_fire_recruitment", "post_fire_flowering"),
  # (traits of the long-format datasets do not reorder the wide files' columns)
  unique(recs$trait[!dataset_of(main$family[match(recs$taxon_name, main$taxon_name)]) %in% long_ids]),
  unique(recs$trait)))
cat_traits <- intersect(trait_order, unique(recs$trait[!recs$is_num]))
num_traits <- intersect(trait_order, unique(recs$trait[recs$is_num]))
cat_cols <- as.vector(rbind(paste0(cat_traits, "_description"), cat_traits))
num_cols <- as.vector(rbind(paste0(num_traits, "_description"), paste0(num_traits, "_min"), paste0(num_traits, "_max"),
                            paste0(num_traits, "_extreme_min"), paste0(num_traits, "_extreme_max")))
num_cols <- num_cols[num_cols %in% names(rows)]
num_cols <- num_cols[!str_detect(num_cols, "_extreme_(min|max)$") | map_lgl(num_cols, ~ any(!is.na(rows[[.x]])))]
for (cc in cat_cols) if (!cc %in% names(rows)) rows[[cc]] <- NA
rows <- rows %>% select(all_of(keys), all_of(cat_cols), all_of(num_cols))

ctx <- c("commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "phenological_stage", "trait_scoring_method", "scoring_certainty")
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
                               !is.na(leaf_division) ~ 3, !is.na(entity_measured) ~ 4, !is.na(plant_organ_measured) ~ 4, !is.na(phenological_stage) ~ 4, TRUE ~ 0),
         fam_order = match(family, sort(unique(family)))) %>%
  arrange(fam_order, taxon_name, row_order, trait_scoring_method, commonness_qualifier, population_region, leaf_division, entity_measured) %>%
  select(-row_order, -fam_order)

# numeric ranges inside text written as "a--b" (Excel reads a hyphenated number pair as a formula / date)
out_df <- out_df %>% mutate(across(where(is.character), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--")))

# ---------------------------------------------------------------- candidate trait register
# raw/candidate_traits.csv: every extracted trait column that is not (yet) mapped in metadata.yml, with its coverage, so
# new traits.yml definitions can be prioritised; rewritten on every full build
meta_var_in <- function(id) tryCatch({
  m <- yaml::read_yaml(file.path("data", id, "metadata.yml"))
  unique(c(map_chr(m$traits, ~ as.character(.x$var_in)), map_chr(m$contexts, ~ as.character(.x$var_in))))
}, error = function(e) character(0))
write_candidates <- function(df, path, id) {
  meta_traits <- unique(str_remove(meta_var_in(id), "_(?:min|max)$"))
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

# ---------------------------------------------------------------- long format (ABRS_Fabaceae_2026, ABRS_NtoZ_2026)
# Fabaceae is written in long format (user's choice, 2026-10-19: the papilionaceous flower parts and phyllode / gland characters
# need many context rows): one row per taxon x trait x value type x context combination, with
#   value              mapped value (categorical: traits.yml level(s) or a draft value for candidate traits, space-delimited, in
#                      text order; numeric: the number, in the units listed in metadata.yml)
#   value_type         mode (categorical; single counts), minimum / maximum (ranges; "to 15 cm" is a maximum), mean (a single
#                      measurement, "c. 6 mm diam.")
#   measurement_range  typical (the range given) or extreme (parenthetical extremes "(2-) 3-7 (-10) m")
#   value_description  the source words (categorical) or the verbatim measurement (numeric)
# plus the context columns, and per-taxon verbatim text (habit_description ... notes, common_name, biostatus) as rows whose
# trait_name is the text field (not mapped in metadata.yml).
count_like <- function(tr) str_detect(tr, "count|merism|per_|_pairs$")
long_of <- function(taxa) {
  r <- recs %>% filter(taxon_name %in% taxa)
  fmt <- function(x) ifelse(is.na(x), NA_character_, format(signif(x, 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  cat_l <- r %>% filter(!is_num) %>%
    transmute(taxon_name, trait_name = trait, value, value_type = "mode", measurement_range = NA_character_, value_description = desc, across(all_of(ctx)))
  num <- r %>% filter(is_num)
  pre <- str_to_lower(str_extract(coalesce(num$desc, ""), "^[^0-9]*"))
  single_type <- case_when(str_detect(pre, "(?:\\b(?:to|up to|less than|under|reaching|not exceeding|not more than|at most)|<)\\s*(?:c\\.\\s*)?$") ~ "maximum",
                           str_detect(pre, "(?:\\b(?:more than|greater than|over|at least|exceeding)|>)\\s*(?:c\\.\\s*)?$") | str_detect(coalesce(num$desc, ""), "^[^a-z]*[0-9.]+\\s*(?:mm|cm|m)?\\s+or more\\b") ~ "minimum",
                           count_like(num$trait) ~ "mode", TRUE ~ "mean")
  nb <- function(v, vt, mr) num %>% mutate(.v = v, .t = vt) %>% filter(!is.na(.v)) %>%
    transmute(taxon_name, trait_name = trait, value = map_chr(.v, fmt), value_type = .t, measurement_range = mr, value_description = desc, across(all_of(ctx)))
  num_l <- bind_rows(
    nb(num$min, "minimum", "typical"),
    nb(ifelse(is.na(num$min), NA, num$max), "maximum", "typical"),
    nb(ifelse(is.na(num$min), num$max, NA), single_type, "typical"),
    nb(num$extreme_min, "minimum", "extreme"),
    nb(num$extreme_max, "maximum", "extreme"))
  txt_l <- main %>% filter(taxon_name %in% taxa) %>% select(-family, -taxon_rank, -foa_url) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(-taxon_name, names_to = "trait_name", values_to = "value") %>% filter(!is.na(value), value != "") %>%
    mutate(value_type = NA_character_, measurement_range = NA_character_, value_description = NA_character_)
  # (traits outside the core list are sorted by name, so the row order does not depend on which other datasets were run)
  core_t <- trait_order[seq_len(match("post_fire_flowering", trait_order))]
  ord_t <- c(core_t, sort(setdiff(unique(c(cat_l$trait_name, num_l$trait_name)), core_t)))
  bind_rows(cat_l %>% mutate(.k = 1), num_l %>% mutate(.k = 1), txt_l %>% mutate(.k = 2)) %>%
    left_join(main %>% select(taxon_name, family, taxon_rank, foa_url), by = "taxon_name") %>%
    mutate(.fo = match(family, sort(unique(family))), .to = coalesce(match(trait_name, ord_t), 9999L),
           .ctx = rowSums(!is.na(across(all_of(ctx)))), .vt = match(value_type, c("mode", "mean", "minimum", "maximum")),
           .mr = match(coalesce(measurement_range, "typical"), c("typical", "extreme"))) %>%
    arrange(.fo, taxon_name, .k, .to, .ctx, trait_scoring_method, commonness_qualifier, population_region, leaf_division, entity_measured, plant_organ_measured, .mr, .vt) %>%
    select(taxon_name, family, taxon_rank, foa_url, trait_name, value, value_type, measurement_range, value_description, all_of(ctx)) %>%
    mutate(across(c(value, value_description), ~ str_replace_all(.x, "(?<=[0-9])\\s*[-–—]\\s*(?=[0-9])", "--")))
}
write_candidates_long <- function(df, path, id) {
  meta_traits <- meta_var_in(id)
  # (before the dataset has a metadata.yml, traits already in config/traits.yml are not candidates; once it has one, a
  # traits.yml trait it does not map is still listed, as in the wide registers, e.g. petiole_shape with unlisted values)
  if (!length(meta_traits)) meta_traits <- names(yaml::read_yaml("config/traits.yml")$traits$elements)
  top <- function(x, k = 8) { x <- str_squish(x[!is.na(x) & x != ""]); if (!length(x)) return(NA_character_)
    t <- sort(table(x), decreasing = TRUE); paste0(names(t)[seq_len(min(k, length(t)))], " (", t[seq_len(min(k, length(t)))], ")", collapse = "; ") }
  reg <- df %>% filter(!is.na(value_type), !trait_name %in% meta_traits) %>%
    group_by(trait = trait_name) %>%
    summarise(type = if (all(value_type == "mode" & is.na(measurement_range))) "categorical" else "numeric",
              n_taxa = n_distinct(taxon_name), n_values = n(), n_copied = sum(!is.na(trait_scoring_method)),
              range = if (type == "numeric") { v <- suppressWarnings(as.numeric(value)); paste(signif(min(v, na.rm = TRUE), 3), "-", signif(max(v, na.rm = TRUE), 3)) } else NA_character_,
              suggested_values = if (type == "categorical") top(unlist(str_split(value, " "))) else NA_character_,
              source_terms = if (type == "categorical") top(unlist(str_split(str_remove(value_description, "^\\[[a-z ]+\\]\\s*"), ";\\s*"))) else NA_character_,
              .groups = "drop") %>%
    mutate(organ = str_extract(trait, "^[a-z]+")) %>% select(trait, type, organ, n_taxa, n_values, n_copied, range, suggested_values, source_terms) %>%
    arrange(desc(n_taxa), trait)
  write_csv(reg, path, na = "")
  invisible(reg)
}

if (Sys.getenv("FOA_FAMILIES") == "") {
  # one data.csv per dataset; each keeps the id / context columns, the columns its metadata.yml maps, and any other
  # column with a value for its families (all-empty extraction columns are dropped)
  keep_always <- c("taxon_name", "family", "taxon_rank", "foa_url", ctx, "common_name", "biostatus", "profile_author")
  for (id in sort(unique(dataset_of(out_df$family)))) {
    if (id %in% long_ids) {
      df <- long_of(main$taxon_name[dataset_of(main$family) == id])
      dir.create(file.path("data", id, "raw"), recursive = TRUE, showWarnings = FALSE)
      write_csv(df, file.path("data", id, "data.csv"), na = "")
      write_candidates_long(df, file.path("data", id, "raw", "candidate_traits.csv"), id)
      next
    }
    df <- out_df %>% filter(dataset_of(family) == id)
    has_value <- map_lgl(df, ~ any(!is.na(.x) & .x != ""))
    df <- df[, names(df) %in% c(keep_always, meta_var_in(id)) | has_value]
    write_csv(df, file.path("data", id, "data.csv"), na = "")
    write_candidates(df, file.path("data", id, "raw", "candidate_traits.csv"), id)
  }
} else {
  write_csv(out_df, Sys.getenv("FOA_OUT", file.path(tempdir(), "foa_chunk.csv")), na = "")
  if (Sys.getenv("FOA_OUT_LONG") != "") write_csv(long_of(main$taxon_name), Sys.getenv("FOA_OUT_LONG"), na = "")
}
