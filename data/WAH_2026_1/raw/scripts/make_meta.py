# WAH_2026_1 .. _4 metadata.yml, first creation (Florabase profiles; user 2026-11-04: one dataset per kind of text). Afterwards the
# files are only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks reused by var_in from the 2026 flora datasets (new
# blocks for traits.yml traits no earlier dataset maps); contexts (with their value lists) and substitutions from those datasets.
# Written without reference to WAH_1998 / WAH_2022 / WAH_2023 (the comparison cases afterwards).
# usage: python3 ignore/jobs/fb1/make_meta.py WAH_2026_1 [WAH_2026_2 ...] [--force]
import csv, os, sys, yaml
csv.field_size_limit(10**9)
def find(d):
    if isinstance(d, dict):
        if "post_fire_recruitment" in d: return d
        for v in d.values():
            r = find(v)
            if r: return r
TY = find(yaml.safe_load(open("config/traits.yml")))
ALL = ["ABRS_NtoZ_2026", "ABRS_Fabaceae_2026", "ABRS_AtoM_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "ABRS_Asteraceae_2026", "CPBR_2026",
       "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026"]
blocks, ctx_all, subs_all = {}, {}, []
for d in ALL:
    m = yaml.safe_load(open(f"data/{d}/metadata.yml"))
    for b in m["traits"]: blocks.setdefault(b["var_in"], {k: v for k, v in b.items() if not k.startswith("question")})
    for c in m.get("contexts") or []:
        if c["var_in"] not in ctx_all: ctx_all[c["var_in"]] = c
        elif c.get("values") and ctx_all[c["var_in"]].get("values"):
            have = {v.get("find", v.get("value")) for v in ctx_all[c["var_in"]]["values"]}
            ctx_all[c["var_in"]]["values"] += [v for v in c["values"] if v.get("find", v.get("value")) not in have]
    subs_all += [s for s in (m.get("substitutions") or []) if isinstance(s, dict)]
CONT = """contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the Florabase profiles
  dataset_curators: Elizabeth Wenk
"""
FB = """    key: {key}
    bibtype: Online
    author: '{{Western Australian Herbarium}}'
    year: '2026'
    title: 'Florabase: the Western Australian Flora'
    publisher: Department of Biodiversity, Conservation and Attractions
    url: https://florabase.dbca.wa.gov.au
    note: accessed 1 October 2026
"""
PARSER = ("Trait values were extracted with the scripted parser written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R, "
          "Florabase mode, FB_TEXT = {mode}): each description is split into sentences, clauses and comma-separated segments, each segment is assigned to the "
          "organ it describes, and categorical terms and measurements are read from the segments of the relevant organ, with the family rules of that parser "
          "(Fabaceae, orchids, grasses and other graminoids, ferns, Asteraceae heads ...). Categorical terms are mapped to AusTraits trait values, with the source "
          "terms kept verbatim in value_description; several values are listed in the order they appear in the text, and alternatives qualified by a commonness "
          "word or a region are recorded on separate rows. A measured range is recorded as minimum and maximum values, a single measurement as a mean, a single "
          "count as a mode, \"to X\" as a maximum and \"X or more\" as a minimum; parenthetical extremes are kept on separate rows (measurement_range = extreme). "
          "Subspecies and varieties silent on a trait take the values of their species. Values that descriptions state only when atypical (terrestrial substrate, "
          "leaf as photosynthetic organ, not parasitic, broadleaf) are recorded on trait_scoring_method = inferred rows when nothing in the profile suggests "
          "otherwise; plant_height_type records why a taxon has no plant_height. Taxon names are given without authorities; current and not-current Florabase "
          "names are both included (one profile per name, the current one first).")
SPEC = {
  "WAH_2026_1": dict(
    desc="To compile plant trait values from the brief descriptions (Descriptive Catalogue) of the Florabase profiles of the Western Australian flora.",
    strat="The brief descriptions of the Florabase profiles (source: Descriptive Catalogue) were downloaded: \"<habit>, <height>[, organ clauses]. Fl. <colours>, "
          "<months>. <Soils>. <Habitats>.\". The text before the \"Fl.\" sentence is read as the description, the flower colours and months of the \"Fl.\" sentence "
          "as flower colour and flowering time (colours joined by -, / or & are each scored), and the soil and habitat sentences as the habitat text. " + PARSER.format(mode="brief"),
    file="florabase_profiles.csv (Florabase profiles, downloaded 1 October 2026), field brief_description",
    q={"question2": "Source reference: the brief descriptions are credited to the Descriptive Catalogue (author shown as Amanda Spooner and others): should the "
                    "Descriptive Catalogue (Paczkowska & Chapman 2000) be added as a secondary source?"}),
  "WAH_2026_2": dict(
    desc="To compile plant trait values from the key-based scientific descriptions (C. Hollister & K.R. Thiele) of the Florabase profiles of the Western Australian flora.",
    strat="The scientific descriptions written by C. Hollister and K.R. Thiele from the Florabase identification keys (Fabaceae, Proteaceae, Malvaceae, Goodeniaceae, "
          "Rutaceae, Haemodoraceae) were downloaded. Most are generated prose; 943 profiles (mostly Fabaceae) are the raw character list the prose is made from "
          "(\"Stems Indumentum hairy, Pustules or glands absent, Cross section terete ...\"), which was rewritten as that prose before parsing. States the key "
          "codes as uncertain (\"(?)\") are scored with scoring_certainty = uncertain: each such profile is read a second time with every state, and the values "
          "only that reading finds go on the uncertain rows. The \"Flowers in\" / \"Flowering time\" sentence is the flowering time. " + PARSER.format(mode="key"),
    file="florabase_profiles.csv (Florabase profiles, downloaded 1 October 2026), field Scientific Description (C. Hollister and K.R. Thiele)",
    q={"question2": "Keys code '(?)' for states the coder was unsure of, often every state of a character ('hairy (?) or glabrous (?)'); these are on "
                    "scoring_certainty = uncertain rows (user's choice). Keep them all?"}),
  "WAH_2026_3": dict(
    desc="To compile plant trait values from the DELTA genus and family descriptions and the prose species descriptions of the Florabase profiles of the Western Australian flora.",
    strat="The DELTA-generated genus and family descriptions of Florabase (L. Watson, H.R. Coleman, J. Gathe, B. Richardson and others; sections Habit and leaf "
          "form, Reproductive type and pollination, Inflorescence and flower features, Fruit and seed features, Physiology, Special features) and the prose species "
          "descriptions (R. Davis, Ptilotus) were downloaded; anatomy, seedling and pollen sentences and the geography, etymology and uses sections were left out. "
          "DELTA wording is read as in the other sections (\"Entomophilous\" = insect pollination, \"Photosynthetic pathway: C3\", \"Dispersal unit the fruit\"). "
          "The genus and family descriptions are scored on the genus or family itself, and a categorical value stated singly in them is copied to every Florabase "
          "taxon of that genus (else family) whose own Florabase texts (this dataset, WAH_2026_1 brief descriptions and WAH_2026_2 key descriptions) are silent on "
          "the trait (trait_scoring_method = inferred_from_genus / inferred_from_family), unless at least two members contradict it and they outnumber those that "
          "agree. " + PARSER.format(mode="delta"),
    file="florabase_profiles.csv (Florabase profiles, downloaded 1 October 2026), field Scientific Description (DELTA and prose descriptions)",
    q={"question2": "Copy-down reaches taxa that have no DELTA or prose text of their own (their only rows here are the genus / family copies): is that the intended scope?"}),
  "WAH_2026_4": dict(
    desc="To compile plant trait values from the Management Notes (Swan NRM Region) of the Florabase profiles of weeds of the Swan region.",
    strat="The Management Notes (for the Swan NRM Region) of the Florabase profiles (K. Brown, K. Bettink and others) were downloaded. Their General Biology "
          "fields (Growth form, Life form, Reproduction, Dispersal, Photosynthetic Pathway, Time to first flowering, Seedbank persistence, Fire response, "
          "Toxicity, Vegetative regeneration strategy, Woody structure) and their calendars (Flowering, Fruiting, Germination, Active Growth, Dormant; \"(O)\" = "
          "occasionally) were read field by field with the script raw/build_data_csv.R, which maps each field's terms to AusTraits values (the field's text is "
          "kept in value_description); fire responses were mapped from the statements as read. Management (herbicide, removal, treatment months) is not a trait "
          "and is left out. Taxon names are given without authorities.",
    file="florabase_profiles.csv (Florabase profiles, downloaded 1 October 2026), field Management Notes (for the Swan NRM Region)",
    q={"question2": "Reproduction 'Seed' alone is not scored as not_vegetative (only 'reproduces only by seed' is). Agree?"}),
    # (user 2026-11-05: 'Generally survives fire' = resprouts, usually: agreed, so no longer a question)
}
def new_block(t, rows):
    ty = TY[t]
    vts = sorted({r["value_type"] for r in rows if r["trait_name"] == t and r["value_type"]})
    num = ty.get("type") == "numeric"
    return {"var_in": t, "unit_in": ty.get("units", ".na") if num else ".na", "trait_name": t, "entity_type": "species", "value_type": "value_type",
            "basis_of_value": "measurement" if num else "expert_score", "replicates": ".na",
            "methods": "Scored from the taxon profile (verbatim source terms in value_description)." if not num else
                       "Value given in the taxon profile (verbatim in value_description): a range is recorded as minimum and maximum values, a single value as a mean, "
                       "\"to X\" as a maximum and \"X or more\" as a minimum."}
args = [a for a in sys.argv[1:] if not a.startswith("--")]
for ds in args:
    path = f"data/{ds}/metadata.yml"
    if os.path.exists(path) and "--force" not in sys.argv: sys.exit(f"{path} exists: use meta_update.py (add-only)")
    rows = list(csv.DictReader(open(f"data/{ds}/data.csv")))
    cols = list(rows[0].keys()); present = {r["trait_name"] for r in rows}
    # (a new categorical block only when some value fits the traits.yml levels or a substitution: petiole_shape reniform/oblong does not
    # fit decurrent/slender, and the ABRS datasets leave it a candidate; meta_update would copy a block here to them, 2026-11-05)
    ok = lambda t, w: w in (TY[t].get("allowed_values_levels") or {}) or any(s["trait_name"] == t and s["find"] == w for s in subs_all)
    fits = lambda t: TY[t].get("type") == "numeric" or any(ok(t, w) for r in rows if r["trait_name"] == t for w in r["value"].split())
    mapped = sorted(t for t in present if t in blocks or (t in TY and TY[t].get("type") in ("categorical", "numeric") and fits(t)))
    ctx = [ctx_all[c] for c in ["commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "phenological_stage",
                                "cue_scored_from", "trait_scoring_method", "scoring_certainty"] if c in cols and c in ctx_all]
    tn = {blocks[t]["trait_name"] if t in blocks else t for t in mapped}
    seen, subs = set(), []
    for x in subs_all:
        k = (x["trait_name"], x["find"])
        if x["trait_name"] in tn and k not in seen: seen.add(k); subs.append(x)
    sp = SPEC[ds]
    url_col = "florabase_url" if "florabase_url" in cols else "foa_url"
    out = "source:\n  primary:\n" + FB.format(key=ds) + CONT + "dataset:\n  data_is_long_format: yes\n"
    out += "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
           ",\n".join("          " + ", ".join(f'"{t}"' for t in mapped[i:i + 4]) for i in range(0, len(mapped), 4)) + "\n        )" + \
           (",\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n" if "measurement_range" in cols else "\n      )\n    '\n")
    out += yaml.safe_dump({"collection_date": "2026", "taxon_name": "taxon_name", "trait_name": "trait_name", "value": "value", "description": sp["desc"],
                           "basis_of_record": "preserved_specimen", "life_stage": "adult", "sampling_strategy": sp["strat"], "original_file": sp["file"],
                           "notes": f"The profile URL of each taxon is in the {url_col} column. Rows whose trait_name is not mapped below (candidate traits, and text "
                                    "fields such as habit_description or habitat_description with the verbatim organ-level text) are in data.csv but filtered out in "
                                    "custom_R_code" + ("; candidate traits are listed with their coverage in raw/candidate_traits.csv" if os.path.exists(f"data/{ds}/raw/candidate_traits.csv") else "") + "."},
                          sort_keys=False, width=120, allow_unicode=True).replace("\n", "\n  ").rstrip() .join(["  ", "\n"])
    out += "identifiers: .na\nlocations: .na\n"
    out += yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120, allow_unicode=True) if ctx else "contexts: .na\n"
    out += "traits:\n"
    for t in mapped: out += yaml.safe_dump([blocks[t] if t in blocks else new_block(t, rows)], sort_keys=False, allow_unicode=True, width=120)
    out += yaml.safe_dump({"substitutions": subs}, sort_keys=False, allow_unicode=True, width=120) if subs else "substitutions: .na\n"
    out += "taxonomic_updates: .na\nexclude_observations: .na\n"
    # (user 2026-11-05: parenthetical extremes stay in data.csv as measurement_range = extreme rows and are not mapped: no question)
    q = {}
    q.update(sp["q"])
    out += yaml.safe_dump({"questions": q}, sort_keys=False, width=120)
    yaml.safe_load(out); open(path, "w").write(out)
    print(ds, len(mapped), "traits mapped (", sum(t not in blocks for t in mapped), "new blocks );", len(ctx), "contexts;", len(subs), "substitutions")
