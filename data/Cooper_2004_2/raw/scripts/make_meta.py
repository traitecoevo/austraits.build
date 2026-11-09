# Cooper_2004_2/metadata.yml, first creation (Cooper & Cooper 2004, Fruits of the Australian Tropical Rainforest, OCR'd and read by the
# ABRS parser in Cooper mode; user 2026-11-08). Afterwards the file is only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks
# reused by var_in from the other 2026 flora datasets (their cross-name mappings included); contexts (with their value lists) and
# substitutions from those datasets for the traits mapped here.
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/Cooper_2004_2/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/Cooper_2004_2/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026", "NT_2026"]
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
cal = ("Months of {w} given at the end of the book's {f} paragraph (\"Sep.-Dec.\", \"Jan.-Apr. & Aug.\", \"Any month\"; verbatim in "
       "value_description), as a 12-character y/n string starting in January.")
for t, w, f in [("flowering_time", "flowering", "Flower"), ("fruiting_time", "fruiting", "Fruit")]:
    if t in blocks: blocks[t] = dict(blocks[t], methods=cal.format(w=w, f=f))
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
    key: Cooper_2004
    bibtype: Book
    author: Wendy Cooper and William T. Cooper
    year: '2004'
    title: Fruits of the Australian Tropical Rainforest
    publisher: Nokomis Editions
    address: Melbourne, Victoria, Australia
    isbn: '9780958174213'
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the book's species descriptions
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = """  collection_date: '2004'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the species descriptions of Fruits of the Australian Tropical Rainforest (Cooper &
    Cooper 2004), for the rainforest plants of tropical Queensland.
  basis_of_record: literature
  life_stage: adult
  sampling_strategy: 'Each species entry of the book (a habit sentence, then Leaves, Inflorescence, Flower, Fruit, Notes and Distribution
    paragraphs, with flowering and fruiting months at the end of the Flower and Fruit paragraphs) was transcribed by optical character
    recognition from photographs of the pages (Apple Vision, each text column read separately) and split into its paragraphs. Trait
    values were extracted with the scripted parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R,
    Cooper mode): each description is split into sentences, clauses and comma-separated segments, each segment is assigned to the organ
    it describes, and categorical terms and measurements are read from the segments of the relevant organ. The Distribution paragraph
    gives the habitat. The animals named in the Notes as eating the fruit, aril or flesh are recorded as dispersers (as in Cooper_2013),
    those eating the seeds alone as the candidate trait seed_predators (raw/vet_notes.R). Categorical terms are mapped to AusTraits trait
    values, with the source terms kept verbatim in value_description; several values are listed in the order they appear in the text,
    and alternatives qualified by a commonness word (including the book''s "may be" and "may have") are recorded on separate rows. A
    measured range is recorded as minimum and maximum values, a single measurement as a mean, a single count as a mode and "to X" as a
    maximum. The genera and families of the book have no morphological description, so no values are copied down; subspecies and
    varieties silent on a trait take the values of their species (inferred_from_species). Values that descriptions state only when
    atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are recorded on trait_scoring_method =
    inferred rows when nothing in the entry suggests otherwise. Taxon names are given as in the book (2004 names, without authorities);
    phrase names are kept as written.'
  original_file: Photographs of the book pages (Cooper_2004_pages001-100.pdf, ...101-200, ...201-400, ...401-560, and two sets of
    rescanned pages), transcribed to cooper2004_profiles.csv
  notes: 'The book page of each taxon is in the source_page column; the book''s family, the other names (synonyms) it gives and the page
    are also in raw/taxon_info.csv. Candidate traits not yet in traits.yml are listed with their coverage in raw/candidate_traits.csv;
    custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description, leaf_description,
    distribution_description, ...) carry the verbatim paragraph text and are unmapped. Cooper_2004 holds the dispersal modes of 21
    taxa from the same book, scored from a spreadsheet.'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\nexclude_observations: .na\n"
# (user 2026-11-05: extremes stay as measurement_range = extreme rows in data.csv, not mapped)
out += "questions: .na\n"
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
