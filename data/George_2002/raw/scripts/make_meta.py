# George_2002/metadata.yml, first creation (E.A. George 2002, Verticordia: the turner of hearts, the species accounts OCR'd from page
# photographs and read by the ABRS parser in Verticordia mode; user 2026-11-09). Afterwards the file is only updated add-only. Trait blocks
# reused by var_in from the other 2026 flora datasets (their cross-name mappings included); contexts (with their value lists) and
# substitutions from those datasets for the traits mapped here.
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/George_2002/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/George_2002/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026", "NT_2026", "Cooper_2004_2", "Brophy_2013", "ATRP_2026"]
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
cal = ("Months of flowering given in the taxon's Flowering period note (\"August to November\"; verbatim in value_description), as a "
       "12-character y/n string starting in January.")
if "flowering_time" in blocks: blocks["flowering_time"] = dict(blocks["flowering_time"], methods=cal)
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
    key: George_2002
    bibtype: Book
    author: Elizabeth A. George
    year: '2002'
    title: 'Verticordia: the turner of hearts'
    publisher: University of Western Australia Press
    address: Crawley, Western Australia
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
mid = """  collection_date: '2002'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the species accounts of Verticordia, the turner of hearts (George 2002), which
    describes every species, subspecies and variety of Verticordia (Myrtaceae).
  basis_of_record: literature
  life_stage: adult
  sampling_strategy: 'The species accounts (101 species, 23 subspecies and 44 varieties) were transcribed from photographs of the book
    pages by optical character recognition (Apple Vision, the whole page and each text column and full-width band read separately) and
    split into their parts: each species'' description and key to its infraspecific taxa, and each taxon''s notes (an opening paragraph,
    Distribution, Habitat, Flowering period, Distinguishing characteristics, Related species, Other names used, Conservation status and
    Cultivation). Trait values were extracted with the scripted parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/
    build_data_csv.R, Verticordia mode): each description is split into sentences, clauses and comma-separated segments, each segment is
    assigned to the organ it describes, and categorical terms and measurements are read from the segments of the relevant organ. A
    subspecies or variety is described by its opening note and the leads on its path through the species'' key, and takes the values of
    its species where these are silent (inferred_from_species). The Habitat note gives the habitat and the Flowering period note the
    flowering months; the Cultivation notes (plants in gardens) are not read. Categorical terms are mapped to AusTraits trait values,
    with the source terms kept verbatim in value_description; several values are listed in the order they appear in the text, and
    alternatives qualified by a commonness word are recorded on separate rows. A measured range is recorded as minimum and maximum
    values, a single measurement as a mean, a single count as a mode and "to X" as a maximum. Values that descriptions state only when
    atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are recorded on trait_scoring_method =
    inferred rows when nothing in the account suggests otherwise.'
  original_file: Verticordia George compressed_Part1.pdf to _Part6.pdf (photographs of the species accounts); OCR'd from the same pages
    at full resolution (Verticordia George.pdf) and transcribed to verticordia_profiles.csv
  notes: 'The page of each taxon''s entry in the photographed PDF is in the source_page column; the other names used are in
    raw/taxon_info.csv. Candidate traits not yet in traits.yml are listed with their coverage in raw/candidate_traits.csv;
    custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description, leaf_description,
    distribution_description, ...) carry the verbatim paragraph text and are unmapped.'
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
