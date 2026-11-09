# NT_2026/metadata.yml, first creation (NT Flora online factsheets, read by the ABRS parser in NT Flora mode; user 2026-11-07). Afterwards
# the file is only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks reused by var_in from the other 2026 flora datasets
# (their cross-name mappings included); contexts (with their value lists) and substitutions from those datasets for the traits mapped here.
# Written without reference to NTH_2014 / NTH_2022 / NTH_2023 / NTH_2025 (the comparison cases afterwards).
import csv, os, sys, yaml
csv.field_size_limit(10**9)
path = "data/NT_2026/metadata.yml"
if os.path.exists(path) and "--force" not in sys.argv: sys.exit("metadata.yml exists: use meta_update.py (add-only)")
rows = list(csv.DictReader(open("data/NT_2026/data.csv")))
cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
ALL = ["ABRS_NtoZ_2026", "ABRS_AtoM_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026"]
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
cal = ("Months of {w} from the {f} field of the factsheet's Ecological Attributes (month lists, \"Mar, Apr, May, Jun, Aug, Sep\", or "
       "ranges, \"Feb - July\", \"throughout the year, mainly Oct-Dec\"; verbatim in value_description), as a 12-character y/n string starting "
       "in January; qualified alternatives are on commonness_qualifier rows.")
for t, w, f in [("flowering_time", "flowering", "Flowering"), ("fruiting_time", "fruiting", "Fruiting")]:
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
    key: NT_2026
    bibtype: Online
    author: '{Northern Territory Herbarium}'
    year: '2026'
    title: 'FloraNT: Northern Territory Flora Online'
    publisher: Northern Territory Government
    url: https://eflora.nt.gov.au
    note: accessed 7 November 2026
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the FloraNT factsheets
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
url_note = ("The profile URL of each taxon is in raw/taxon_urls.csv (one row per taxon). " if os.path.exists("data/NT_2026/raw/taxon_urls.csv")
            else "The profile URL of each taxon is in the ntflora_url column. ")
mid = f"""  collection_date: '2026'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the taxon descriptions of the Northern Territory flora online (FloraNT, Northern
    Territory Herbarium), for all vascular plant families.
  basis_of_record: preserved_specimen
  life_stage: adult
  sampling_strategy: 'The genus, species and infraspecific factsheets of FloraNT (eflora.nt.gov.au) were downloaded, phrase names and
    hybrids included, introduced and native taxa alike. Their descriptions come from several floras and unpublished treatments,
    named for each taxon in the Flora Description Source field (Flora of the Darwin Region, Flora of Central Australia, Flora of the
    Kimberley Region, Floodplain Flora, unpublished notes prepared for FloraNT ...). Trait values were extracted from the Description
    and Habitat text, and from the Notes, Diagnostic Characters and Similar Taxa text for ecological statements, with the scripted
    parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R, NT Flora mode): each description is split
    into sentences, clauses and comma-separated segments, each segment is assigned to the organ it describes, and categorical terms
    and measurements are read from the segments of the relevant organ, with the family rules of that parser (Fabaceae phyllodes,
    orchids, grasses and other graminoids, ferns, Asteraceae heads ...). For the factsheets without a Description, the Diagnostic
    Characters were read as the description, without their sentences that name or compare another taxon. The Flowering and Fruiting
    fields (Ecological Attributes) give the flowering and fruiting times. Categorical terms are mapped to AusTraits trait values, with
    the source terms kept verbatim in value_description; several values are listed in the order they appear in the text, and
    alternatives qualified by a commonness word or a region are recorded on separate rows. A measured range is recorded as minimum
    and maximum values, a single measurement as a mean, a single count as a mode, "to X" as a maximum and "X or more" as a minimum;
    parenthetical extremes are kept on separate rows (measurement_range = extreme). Categorical values stated in a genus description
    are copied to member taxa whose own description is silent on that trait, unless most of the member taxa that describe the trait
    contradict it (trait_scoring_method = inferred_from_genus), with the hedge word of a hedged genus statement as
    commonness_qualifier; subspecies and varieties silent on a trait take the values of their species (inferred_from_species). Values
    that descriptions state only when atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are
    recorded on trait_scoring_method = inferred rows when nothing in the factsheet suggests otherwise. Taxon names are given as in the
    source (without authorities); phrase names and hybrid formulae are kept as written.'
  original_file: ntflora_profiles.csv (FloraNT factsheets, downloaded 7 November 2026; the descriptions are those of the 2022 download
    split_sentences_NT_Flora.csv, updated where the site has since revised them)
  notes: '{url_note}Candidate traits not yet in traits.yml are listed with their coverage in raw/candidate_traits.csv;
    custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description, leaf_description,
    habitat_description, ...) carry the verbatim organ-level text and are unmapped. Parenthetical extremes (measurement_range =
    extreme) are in data.csv but not mapped. The common names are those of the factsheet without the genus name that opens every
    list; biostatus introduced is from Introduced Status ("Introduced to NT").'
"""
out = head + cust + mid + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) + "traits:\n"
for t in mapped: out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
out += "taxonomic_updates: .na\nexclude_observations: .na\n"
# (user 2026-11-05: extremes stay as measurement_range = extreme rows in data.csv, not mapped)
out += "questions: .na\n"
yaml.safe_load(out); open(path, "w").write(out)
print(len(mapped), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx], ";", len(subs), "substitutions")
