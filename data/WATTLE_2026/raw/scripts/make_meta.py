# WATTLE_2026/metadata.yml, first creation (WATTLE: Acacias of Australia profiles on Lucid, read by the ABRS parser in WATTLE mode).
# Afterwards the file is only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks reused by var_in from the 2026 flora
# datasets (their cross-name mappings included); contexts (with their value lists) and substitutions from ABRS_Fabaceae_2026, the
# same Acacia text. Written without reference to Maslin_2012 (the comparison case afterwards).
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/WATTLE_2026/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/WATTLE_2026/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_Fabaceae_2026", "ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026", "NHNSW_2026"]
blocks = {}
for d in ALL:
    for b in yaml.safe_load(open(f"data/{d}/metadata.yml"))["traits"]:
        blocks.setdefault(b["var_in"], {k: v for k, v in b.items() if not k.startswith("question")})
mapped = sorted(t for t in present if t in blocks)
ntoz = yaml.safe_load(open("data/ABRS_Fabaceae_2026/metadata.yml"))
ctx = [c for c in ntoz["contexts"] if c["var_in"] in cols]
if "cue_scored_from" in cols and not any(c["var_in"] == "cue_scored_from" for c in ctx):
    ctx += [c for c in yaml.safe_load(open("data/ABRS_AtoM_2026/metadata.yml"))["contexts"] if c["var_in"] == "cue_scored_from"]
subs = [x for x in (ntoz.get("substitutions") or []) if x["trait_name"] in {blocks[t]["trait_name"] for t in mapped}]
head = """source:
  primary:
    key: WATTLE_2026
    bibtype: Online
    author: Bruce R. Maslin
    year: '2026'
    title: 'WATTLE: Acacias of Australia'
    publisher: Lucidcentral
    url: https://apps.lucidcentral.org/wattle/
    note: accessed 2 November 2026
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the WATTLE profiles
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
  description: To compile plant trait values from the taxon profiles of WATTLE (Acacias of Australia) for Acacia, Vachellia,
    Senegalia and Acaciella.
  basis_of_record: preserved_specimen
  life_stage: adult
  sampling_strategy: 'The species, infraspecific, hybrid and variant profiles of WATTLE (apps.lucidcentral.org/wattle) were
    downloaded; most of their text derives from the Flora of Australia Acacia treatment. Trait values were extracted from the
    Description, Habitat, Distribution, Phenology and Notes text with the scripted parser written for the Flora of Australia online
    (ABRS_AtoM_2026/raw/build_data_csv.R, WATTLE mode): each description is split into sentences, clauses and comma-separated segments, each segment is assigned to the
    organ it describes, and categorical terms and measurements are read from the segments of the relevant organ, with the family
    rules of that parser (Fabaceae phyllodes, orchids, grasses and other graminoids, ferns, Asteraceae heads ...). Categorical terms
    are mapped to AusTraits trait values, with the source terms kept verbatim in value_description; several values are listed in the
    order they appear in the text, and alternatives qualified by a commonness word or a region are recorded on separate rows. A
    measured range is recorded as minimum and maximum values, a single measurement as a mean, a single count as a mode, "to X" as a
    maximum and "X or more" as a minimum; parenthetical extremes are kept on separate rows (measurement_range = extreme). WATTLE has
    no genus profiles, so nothing is copied down; subspecies and varieties silent on a trait take the values of their species. Values that descriptions state only when atypical (terrestrial substrate, leaf as
    photosynthetic organ, not parasitic, broadleaf) are recorded on trait_scoring_method = inferred rows when nothing in the profile
    suggests otherwise. Ecological traits (fire response, dispersal, pollination, clonality) were scored by reading each statement of
    the Habitat and Notes text; hedged statements are scored with the hedge word as scoring_certainty. Taxon names
    are given without authorities; hybrid formulae, informal variants and phrase names are kept as written.'
  original_file: wattle_fact_sheets.csv (WATTLE profiles, downloaded 2 November 2026)
  notes: 'The profile URL of each taxon is in the wattle_url column. Candidate traits not yet in traits.yml are listed with their coverage in raw/candidate_traits.csv;
    custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description, leaf_description,
    habitat_description, ...) carry the verbatim organ-level text and are unmapped. Parenthetical extremes (measurement_range =
    extreme) are in data.csv but not mapped.'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs or ".na"}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\nexclude_observations: .na\n"
out += yaml.safe_dump({"questions": {"question1": "Parenthetical extremes (measurement_range = extreme) are filtered out in custom_R_code. Should they be mapped, e.g. as separate minimum / maximum records with a context?", "question2": "Source reference: please confirm the WATTLE citation (author / coordinator, edition, publisher); taken from the Lucid site as accessed 2 November 2026."}}, sort_keys=False, width=120)
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
