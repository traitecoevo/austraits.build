# ATRP_2026/metadata.yml, first creation (Australian Tropical Rainforest Plants online edition, species pages scraped 2026-11-08, read by
# the ABRS parser in ATRP mode; user 2026-11-09). Afterwards the file is only updated add-only. Trait blocks
# reused by var_in from the other 2026 flora datasets (their cross-name mappings included); contexts (with their value lists) and
# substitutions from those datasets for the traits mapped here.
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/ATRP_2026/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/ATRP_2026/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026", "NT_2026", "Cooper_2004_2", "Brophy_2013"]
blocks, ctx_all, subs_all = {}, {}, []
for d in ALL:
    m = yaml.safe_load(open(f"data/{d}/metadata.yml"))
    for b in m["traits"]: blocks.setdefault(b["var_in"], {k: v for k, v in b.items() if not k.startswith("question")})
    for c in m.get("contexts") or []:
        if c["var_in"] not in ctx_all: ctx_all[c["var_in"]] = dict(c)
        elif c.get("values") and ctx_all[c["var_in"]].get("values"):
            have = {v.get("find", v.get("value")) for v in ctx_all[c["var_in"]]["values"]}
            ctx_all[c["var_in"]]["values"] = ctx_all[c["var_in"]]["values"] + [v for v in c["values"] if v.get("find", v.get("value")) not in have]
    subs_all += [s for s in (m.get("substitutions") or []) if isinstance(s, dict)]
icon = ("The page's habit icons (Tree; Shrub, woody or herbaceous, 1-6 m tall; Herb, herbaceous or woody, under 1 m tall; Grass; Palm, pandan "
        "or cycad; Vine; Slender Vine), recorded first: Grass -> graminoid, Palm, pandan or cycad -> palmoid, Vine (stems recorded to several "
        "cm diameter) -> climber_woody, Slender Vine (stem diameter up to c. 2 cm) -> climber_herbaceous; growth forms named in the description "
        "that the icons lack are added (verbatim in value_description, the icons as \"[ATRP habit icon] ...\"). ")
if "plant_growth_form" in blocks: blocks["plant_growth_form"] = dict(blocks["plant_growth_form"], methods=icon + blocks["plant_growth_form"]["methods"])
blocks["seedling_germination_location"] = {"var_in": "seedling_germination_location", "unit_in": ".na", "trait_name": "seedling_germination_location",
    "entity_type": "species", "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
    "methods": "Germination type (epigeal or hypogeal) as given in the caption of the page's cotyledon-stage seedling photograph (\"Cotyledon stage, "
               "epigeal germination\"; verbatim in value_description; raw/seedlings.R)."}
blocks["seed_germination_time"] = {"var_in": "seed_germination_time", "unit_in": "d", "trait_name": "seed_germination_time",
    "entity_type": "species", "value_type": "value_type", "basis_of_value": "measurement", "replicates": ".na",
    "methods": "Seed germination time as given at the end of the page's Seedlings paragraph (\"Seed germination time 15 to 19 days\"; verbatim in "
               "value_description; raw/seedlings.R): a range is recorded as minimum and maximum values, a single value as a mean and \"to X\" as a maximum. "
               "The germination conditions are not stated."}
mapped = sorted(t for t in present if t in blocks)
ctx = [ctx_all[c] for c in ["commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "phenological_stage",
                            "cue_scored_from", "trait_scoring_method", "scoring_certainty"] if c in cols and c in ctx_all]
tn = {blocks[t]["trait_name"] for t in mapped}
seen, subs = set(), []
for x in subs_all:
    k = (x["trait_name"], x["find"])
    if x["trait_name"] in tn and k not in seen: seen.add(k); subs.append(x)
head = """source:
  primary:
    key: ATRP_2026
    bibtype: Online
    year: '2020'
    author: Frank A. Zich and Bernard P.M. Hyland and Trevor Whiffin and Raelee A. Kerrigan
    title: Australian Tropical Rainforest Plants, Edition 8
    publisher: CSIRO
    url: https://apps.lucidcentral.org/rainforest/text/intro/index.html
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the online species pages
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = """  collection_date: '2026'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the species pages of Australian Tropical Rainforest Plants (online edition 8),
    for the rainforest plants of tropical Australia.
  basis_of_record: literature
  life_stage: adult
  sampling_strategy: 'The species pages of the online edition were scraped on 2026-11-08 (Stem, Leaves, Flowers, Fruit, Seedlings,
    Distribution and Ecology, and Natural History & Notes paragraphs, with each page''s habit icons and photo captions). Trait values were
    extracted with the scripted parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R, ATRP mode): the
    Stem, Leaves, Flowers and Fruit paragraphs are the description; each is split into sentences, clauses and comma-separated segments,
    each segment is assigned to the organ it describes, and categorical terms and measurements are read from the segments of the relevant
    organ. The page''s habit icons give the growth form (with the description''s growth forms added), the Mistletoe icon stem parasitism
    and the Epiphyte icon the epiphytic substrate. The habitat is read from the Distribution and Ecology sentences on where the plant
    grows; introduced status from "An introduced species" or "naturalised". The germination type (epigeal or hypogeal) is read from the
    caption of the seedling photograph and the germination time from the Seedlings paragraph (raw/seedlings.R). Categorical terms are
    mapped to AusTraits trait values, with the source terms kept verbatim in value_description; several values are listed in the order
    they appear in the text, and alternatives qualified by a commonness word are recorded on separate rows. A measured range is recorded
    as minimum and maximum values, a single measurement as a mean, a single count as a mode and "to X" as a maximum. Values that
    descriptions state only when atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are recorded on
    trait_scoring_method = inferred rows when nothing in the page suggests otherwise. The family pages, which describe each family
    world-wide, are not read.'
  original_file: atrp_fact_sheets.csv (the species pages, scraped 2026-11-08) and atrp_page_extras.csv (their habit icons and
    germination captions)
  notes: 'The page link, RFK code, synonyms and habit icons of each taxon are in raw/taxon_info.csv. Candidate traits not yet in
    traits.yml are listed with their coverage in raw/candidate_traits.csv; custom_R_code keeps only the traits mapped below. Rows whose
    trait_name is a text field (habit_description, leaf_description, distribution_description, ...) carry the verbatim paragraph text
    and are unmapped. ATRP_2020 holds growth form and a few other traits from a 2022 scrape of the same site.'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\n"
out += "exclude_observations: .na\n"
# (user 2026-11-05: extremes stay as measurement_range = extreme rows in data.csv, not mapped)
out += "questions: .na\n"
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
