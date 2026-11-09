# Brophy_2013/metadata.yml, first creation (Brophy, Craven & Doran 2013, Melaleucas: their botany, essential oils and uses, the species
# accounts read by the ABRS parser in Brophy mode; user 2026-11-09). Afterwards the file is only updated add-only. Trait blocks
# reused by var_in from the other 2026 flora datasets (their cross-name mappings included); contexts (with their value lists) and
# substitutions from those datasets for the traits mapped here.
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/Brophy_2013/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/Brophy_2013/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026", "NT_2026", "Cooper_2004_2"]
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
cal = ("Months of flowering given in the species account's Flowering time paragraph (\"Recorded as flowering from May to November\"; "
       "verbatim in value_description), as a 12-character y/n string starting in January.")
if "flowering_time" in blocks: blocks["flowering_time"] = dict(blocks["flowering_time"], methods=cal)
# (oil yields: not in the other 2026 flora datasets; the fresh-mass trait added to traits.yml 2026-11-09)
oil_m = ("Leaf essential oil yield as given in the species account's Oil yield paragraph, as a percentage of {b} leaf weight (w/w) "
         "(verbatim in value_description; raw/oil_yield.R). Each yield is recorded under the trait for the basis the book states (dry or "
         "fresh weight). Where a statement gives yields for several chemotypes, forms or collections, the range over them is recorded; "
         "\"<0.1%\" is recorded as a maximum, as is a range whose lower figure is \"<\" or \"trace\"; \"nil\" is recorded as 0. The two "
         "yields the book describes as unreliable (very small samples) are not recorded.")
for b in ("dry", "fresh"):
    t = f"leaf_essential_oil_per_{b}_mass"
    blocks[t] = {"var_in": t, "unit_in": "%", "trait_name": t, "entity_type": "species", "value_type": "value_type",
                 "basis_of_value": "measurement", "replicates": ".na", "methods": oil_m.format(b=b)}
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
    key: Brophy_2013
    bibtype: Book
    author: Joseph J. Brophy and Lyndley A. Craven and John C. Doran
    year: '2013'
    title: 'Melaleucas: their botany, essential oils and uses'
    series: ACIAR Monograph No. 156
    publisher: Australian Centre for International Agricultural Research
    address: Canberra, Australia
    isbn: '9781922137517'
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the book's species accounts
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = """  collection_date: '2013'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the species accounts of Melaleucas, their botany, essential oils and uses (Brophy,
    Craven & Doran 2013), which describe every species of Melaleuca (Myrtaceae) in Australia, New Guinea, New Caledonia and South-East Asia.
  basis_of_record: literature
  life_stage: adult
  sampling_strategy: 'Each species account of the book (Description, Natural occurrence, Ecology, Flowering time, Essential oils, Oil
    yield and Notes paragraphs) was read from the text layer of the published PDFs, column by column, and split into its paragraphs.
    Trait values were extracted with the scripted parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R,
    Brophy mode): each description is split into sentences, clauses and comma-separated segments, each segment is assigned to the organ
    it describes, and categorical terms and measurements are read from the segments of the relevant organ. The Ecology paragraph gives
    the habitat and the Flowering time paragraph the flowering months. For a species with subspecies or varieties, the book gives one
    Natural occurrence, Ecology, Flowering time and Oil yield paragraph per infraspecific taxon, and a short key to them in the Notes; each
    infraspecific taxon is recorded from its own paragraphs and its key text, and takes the values of its species where these are silent
    (inferred_from_species). Leaf essential oil yields are recorded from the Oil yield paragraph, per dry or fresh mass as stated (raw/oil_yield.R).
    Categorical terms are mapped to AusTraits trait values, with the source terms kept verbatim in value_description; several values are
    listed in the order they appear in the text, and alternatives qualified by a commonness word are recorded on separate rows. A measured
    range is recorded as minimum and maximum values, a single measurement as a mean, a single count as a mode and "to X" as a maximum.
    Values that descriptions state only when atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are
    recorded on trait_scoring_method = inferred rows when nothing in the account suggests otherwise. Taxon names are given as in the book,
    without authorities.'
  original_file: Melaleucas_mn156-species-a-c_1.pdf, ...-species-d-k_1.pdf, mn156-species-l-o_1.pdf, Melaleucas_mn156-species-p-r_1.pdf
    and ...-species-s-z_1.pdf (the species accounts, chapter 7), transcribed to brophy2013_profiles.csv
  notes: 'The book page of each taxon is in the source_page column; the synonyms the book gives are in raw/taxon_info.csv. Stamen counts
    given per bundle ("Stamens 6-7 per bundle") are recorded per flower, multiplied by the number of bundles (5, unless the account
    states otherwise), with the multiplier in value_description. The book also treats the
    Melaleuca species of New Caledonia and South-East Asia; these stay in data.csv and are excluded under exclude_observations. Candidate traits not yet in traits.yml are listed
    with their coverage in raw/candidate_traits.csv; custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text
    field (habit_description, leaf_description, distribution_description, ...) carry the verbatim paragraph text and are unmapped.'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\n"
# (user 2026-11-09: the book's non-Australian taxa stay in data.csv and are excluded here - those whose distribution names no Australian
# state or territory)
import re
aus = re.compile(r"Western Australia|Northern Territory|Queensland|New South Wales|Victoria|Tasmania|South Australia|Australian Capital|Torres Strait|Lord Howe")
dist = {}
for r in rows:
    if r["trait_name"] == "distribution_description": dist[r["taxon_name"]] = r["value"]
non_aus = sorted(t for t, d in dist.items() if not aus.search(d))
out += yaml.safe_dump({"exclude_observations": [{"variable": "taxon_name", "find": ", ".join(non_aus),
                                                  "reason": "excluding taxa not native to Australia (New Caledonia, South-East Asia)"}]},
                      sort_keys=False, allow_unicode=True, width=120) if non_aus else "exclude_observations: .na\n"
print("excluded:", non_aus)
# (user 2026-11-05: extremes stay as measurement_range = extreme rows in data.csv, not mapped)
out += "questions: .na\n"
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
