# Chinnock_2007_2/metadata.yml, first creation (R.J. Chinnock 2007, Eremophila and allied genera, the taxon accounts OCR'd from page
# photographs and read by the ABRS parser in Chinnock mode; user 2026-11-09). Afterwards the file is only updated add-only. Trait blocks
# reused by var_in from the other 2026 flora datasets (their cross-name mappings included); contexts (with their value lists) and
# substitutions from those datasets for the traits mapped here.
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/Chinnock_2007_2/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/Chinnock_2007_2/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026", "NT_2026", "Cooper_2004_2", "Brophy_2013", "ATRP_2026", "George_2002"]
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
cal = ("Months of flowering given in the taxon's Distribution and ecology section (verbatim in value_description), as a 12-character y/n "
       "string starting in January.")
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
    key: Chinnock_2007_2
    bibtype: Book
    author: R. J. Chinnock
    year: '2007'
    title: 'Eremophila and allied genera: A monograph of the plant family Myoporaceae'
    publisher: Rosenberg
    address: Dural, NSW
    isbn: '9781877058165'
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the book's taxon accounts
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = """  collection_date: '2007'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the taxon descriptions of Eremophila and allied genera (Chinnock 2007), the monograph
    of the Myoporaceae (now in Scrophulariaceae).
  basis_of_record: literature
  life_stage: adult
  sampling_strategy: 'The taxon accounts (251 species and 102 subspecies of Myoporum, Bontia, Pentacoelium, Diocirea, Glycocystis,
    Calamphoreus and Eremophila) were transcribed from photographs of the book pages by optical character recognition (Apple Vision,
    the whole page and each text column and full-width band read separately) and split into their parts: citations, type, description,
    and the Distribution and ecology, Conservation status, Derivation of epithet, Notes and Selected specimens sections. Book page 145
    (the heading and opening of Myoporum degeneri) was not among the photographs. Trait values were extracted with the scripted parser
    written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R, Chinnock mode): each description is split into
    sentences, clauses and comma-separated segments, each segment is assigned to the organ it describes, and categorical terms and
    measurements are read from the segments of the relevant organ. The habitat is read from the Distribution and ecology sentences on
    soils, vegetation and situation, and the flowering months from its sentences on flowering. Subspecies silent on a trait take the
    values of their species (inferred_from_species). Categorical terms are mapped to AusTraits trait values, with the source terms kept
    verbatim in value_description; several values are listed in the order they appear in the text, and alternatives qualified by a
    commonness word are recorded on separate rows. A measured range is recorded as minimum and maximum values, a single measurement as a
    mean, a single count as a mode and "to X" as a maximum; parenthetical extremes are on measurement_range = extreme rows (not mapped).
    Values that descriptions state only when atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf)
    are recorded on trait_scoring_method = inferred rows when nothing in the account suggests otherwise.'
  original_file: Eremophile 093-300.pdf, Eremophile 301-500.pdf, Eremophile 501-650.pdf and Eremophila 2.pdf (rescans of ten pages),
    photographs of the book pages, transcribed to chinnock2007_profiles.csv
  notes: 'The book page of each taxon''s entry is in the source_page column. Chinnock_2007 holds 25 traits of the same book, from a
    spreadsheet. Candidate traits not yet in traits.yml are listed with their coverage in raw/candidate_traits.csv; custom_R_code keeps
    only the traits mapped below. Rows whose trait_name is a text field (habit_description, leaf_description, distribution_description,
    ...) carry the verbatim paragraph text and are unmapped.'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\n"
# (user 2026-11-09: the book's non-Australian taxa stay in data.csv and are excluded here; New Caledonia, Bonin Islands - Taiwan, New Zealand,
# Mauritius, Polynesia, Hawaii, Caribbean, Japan - China. Norfolk Island (M. obscurum) is Australian)
non_aus = ["Bontia daphnoides", "Myoporum boninense subsp. boninense", "Myoporum crassifolium", "Myoporum degeneri", "Myoporum laetum",
           "Myoporum mauritianum", "Myoporum rapense", "Myoporum rapense subsp. kermadecense", "Myoporum rapense subsp. rapense",
           "Myoporum rimatarense", "Myoporum sandwicense", "Myoporum sandwicense subsp. lanaiense", "Myoporum sandwicense subsp. sandwicense",
           "Myoporum stellatum", "Myoporum stokesii", "Myoporum tenuifolium", "Myoporum wilderi", "Pentacoelium bontioides"]
out += yaml.safe_dump({"exclude_observations": [{"variable": "taxon_name", "find": ", ".join(non_aus),
                                                  "reason": "excluding taxa not native to Australia (New Caledonia, Pacific islands, New Zealand, Mauritius, Caribbean, eastern Asia)"}]},
                      sort_keys=False, allow_unicode=True, width=120)
# (user 2026-11-05: extremes stay as measurement_range = extreme rows in data.csv, not mapped)
out += yaml.safe_dump({"questions": {
    "question1": "Book page 145 is missing from the photographed scans (Eremophile 093-300.pdf runs from p. 144 to p. 146, and the rescans in "
                 "Eremophila 2.pdf do not include it). It holds the heading and the opening of the description of 24. Myoporum degeneri "
                 "(the end of 23b. M. sandwicense subsp. lanaiense may also be on it); M. degeneri's data here start partway through its "
                 "description, at the top of p. 146. Please photograph p. 145 so that it can be added."}}, sort_keys=False, allow_unicode=True, width=120)
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
