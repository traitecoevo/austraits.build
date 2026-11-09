# Add-only metadata update for the ABRS_*_2026 / CPBR_2026 datasets (replaces the regenerating make_meta scripts, which dropped
# hand-made mappings - var_in != trait_name, plant_organ_measured, context values, substitutions; 2026-10-30). Every existing
# block, context, substitution and question is kept verbatim. A trait newly present in data.csv gets a block only if another of
# these datasets already maps that same var_in (the block is copied), and its var_in is added to the custom_R_code whitelist.
# Usage: python3 meta_update.py ABRS_AtoM_2026 [ABRS_NtoZ_2026 ...]
import csv, re, sys, yaml
csv.field_size_limit(10**9)
ALL = ["ABRS_AtoM_2026", "ABRS_NtoZ_2026", "ABRS_Asteraceae_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "CPBR_2026", "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026"]
# newly defined traits.yml traits the parser now writes under their own name, with no block in any dataset yet (2026-10-30)
NEW_BLOCKS = {
    "flower_colour_detailed": {"var_in": "flower_colour_detailed", "unit_in": ".na", "trait_name": "flower_colour_detailed", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
        "methods": "Scored from the flower, corolla and perianth colour words of the taxon description (verbatim in value_description): the same words as flower_colour, mapped to the detailed colour levels, with the head colour of a compound (greenish yellow -> yellow)."},
    "fruit_surface_reflectivity": {"var_in": "fruit_surface_reflectivity", "unit_in": ".na", "trait_name": "fruit_surface_reflectivity", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
        "methods": "Scored from the fruit surface words of the taxon description (shiny, dull, glaucous; verbatim in value_description)."},
    "leaf_secondary_vein_angle": {"var_in": "leaf_secondary_vein_angle", "unit_in": "deg", "trait_name": "leaf_secondary_vein_angle", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
        "methods": "Stated angle of the side (secondary, lateral) veins to the midrib in the taxon description (\"lateral veins at 30-45 deg to the midrib\", \"side-veins about 45 deg to midrib\"; verbatim in value_description); thresholds (\"greater than 45 deg\") are in the candidate leaf_secondary_vein_angle_categorical."},
    # (2026-10-31: in NHNSW_2023 but scored by none of the 2026 datasets)
    "petiole_width": {"var_in": "petiole_width", "unit_in": "mm", "trait_name": "petiole_width", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "measurement", "replicates": ".na",
        "methods": 'Measurement given in the taxon description (verbatim in value_description): a range is recorded as minimum and maximum values, a single measurement as a mean, a single count as a mode, "to X" as a maximum and "X or more" as a minimum; parenthetical extremes are on measurement_range = extreme rows (not mapped).'},
    "leaf_pendulousness": {"var_in": "leaf_pendulousness", "unit_in": ".na", "trait_name": "leaf_pendulousness", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
        "methods": "Scored from the leaf clause of the taxon description that names hanging leaves (pendulous, pendent, drooping, weeping; verbatim in value_description); an erect or spreading alternative in the same clause (\"Leaves spreading to pendent\") is recorded as not_pendulous. Taxa whose leaves are not described as hanging are not scored."},
    # (2026-11-02: post-scraping step, only for taxa without plant_height)
    "plant_height_type": {"var_in": "plant_height_type", "unit_in": ".na", "trait_name": "plant_height_type", "entity_type": "species",
        "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
        "methods": "Inferred for taxa whose profile gives no plant height, from the taxon's own scored traits (evidence in value_description): marine or aquatic (floating, submerged) substrate; epiphyte or lithophyte substrate; climbing (climber growth form or a climbing-plant height); geophyte (a reproductive height and a bulb, corm or tuber, or a terrestrial orchid); rosette or leaf_tuft (a reproductive height and a rosette, or a tufted habit or graminoid); prostrate (prostrate or creeping, not also erect); mat-forming; rhizome (ferns with a rhizome). The description names the trait that holds the plant's length instead (plant_height_reproductive, plant_height_climbing_plant, stem_length). Not scored when no reason applies."},
}
# the context of taxa with both a flowering and a fruiting cue (pending_changes.md 1)
CUE_CTX = """- context_property: cue scored from
  category: method_context
  var_in: cue_scored_from
  values:
  - find: flowering
    value: flowering
    description: Cue scored from a statement about the taxon's flowering.
  - find: fruiting
    value: fruiting
    description: Cue scored from a statement about the taxon's fruiting.
"""
blocks = dict(NEW_BLOCKS)
for d in ALL:
    for b in yaml.safe_load(open(f"data/{d}/metadata.yml"))["traits"]:
        if b["var_in"] not in NEW_BLOCKS: blocks.setdefault(b["var_in"], b)
for d in sys.argv[1:]:
    p = f"data/{d}/metadata.yml"; s = open(p).read(); m = yaml.safe_load(s)
    present = {r["trait_name"] for r in csv.DictReader(open(f"data/{d}/data.csv"))}
    have = {b["var_in"] for b in m["traits"]}
    new = sorted(t for t in present - have if t in blocks)
    cm = re.search(r"(        trait_name %in% c\(\n)(.*?)(\n        \),?)", s, re.S)
    wl = set(re.findall(r'"([^"]+)"', cm.group(2)))
    add_wl = sorted(set(new) - wl)
    if add_wl:
        # (one new line at the top of the list: the existing lines are left as they are)
        s = s[:cm.start(2)] + "          " + ", ".join(f'"{t}"' for t in add_wl) + ",\n" + s[cm.start(2):]
    if new:
        txt = "".join(yaml.safe_dump([{k: v for k, v in blocks[t].items() if not k.startswith("question")}], sort_keys=False, allow_unicode=True, width=120) for t in new)
        i = s.index("\nsubstitutions:") + 1
        s = s[:i] + txt + s[i:]
    rows = list(csv.DictReader(open(f"data/{d}/data.csv")))
    if rows and "cue_scored_from" in rows[0] and any(r["cue_scored_from"] for r in rows) and "var_in: cue_scored_from" not in s:
        i = s.index("\ntraits:\n") + 1
        s = s[:i] + CUE_CTX + s[i:]
        print(f"{d}: added context cue_scored_from")
    yaml.safe_load(s)
    open(p, "w").write(s)
    print(f"{d}: added {new}")
