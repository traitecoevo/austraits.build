# SUPERSEDED 2026-10-30 by ignore/jobs/cp1/meta_update.py (add-only): regenerating drops hand-made mappings. Do not run on an existing metadata.yml.
# ABRS_Myrtaceae_2026/metadata.yml (long format) from the NtoZ one: same source / contributors / contexts, the dataset text adapted, and a
# traits block holding exactly the traits.yml traits present in the Myrtaceae data.csv (trait blocks reused from the long-format
# datasets NtoZ and Fabaceae, else from AtoM / Myrtaceae with value_type read from the data). Rerun after every Myrtaceae rebuild.
import csv, re, yaml

def find(d):
    if isinstance(d, dict):
        if "post_fire_recruitment" in d: return d
        for v in d.values():
            r = find(v)
            if r: return r
defined = find(yaml.safe_load(open("config/traits.yml")))
csv.field_size_limit(10**9)
present = sorted({r["trait_name"] for r in csv.DictReader(open("data/ABRS_Myrtaceae_2026/data.csv"))} & set(defined))

blocks = {}
for d in ["NtoZ", "Fabaceae", "Proteaceae", "Asteraceae", "AtoM", "Myrtaceae"]:
    m = yaml.safe_load(open(f"data/ABRS_{d}_2026/metadata.yml"))
    for b in m["traits"]:
        t = b["trait_name"]
        if t in blocks: continue
        b = {k: v for k, v in b.items() if not k.startswith("question")}
        b["var_in"] = t
        b["value_type"] = "value_type"
        blocks[t] = b
# (map only traits mapped in another ABRS dataset, or new here on purpose: a traits.yml trait never mapped elsewhere, such as
# petiole_shape, has source terms outside its levels and stays a candidate)
new_ok = {"mycorrhizal_association_type"}
present = [t for t in present if t in blocks or t in new_ok]
missing = [t for t in present if t not in blocks]
for t in missing:
    u = defined[t].get("units")
    blocks[t] = {"var_in": t, "unit_in": u if defined[t].get("type") == "numeric" else ".na", "trait_name": t, "entity_type": "species",
                 "value_type": "value_type", "basis_of_value": "expert_score", "replicates": ".na",
                 "methods": "Scored from the taxon description, Ecology and Notes text (verbatim source terms in value_description)."}

src = open("data/ABRS_NtoZ_2026/metadata.yml").read()
head = src[:src.index("  custom_R_code:")]
tail_ctx = src[src.index("identifiers:"):src.index("traits:\n")]
head = head.replace("key: ABRS_NtoZ_2026", "key: ABRS_Myrtaceae_2026")
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in present[i:i + 4]) for i in range(0, len(present), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = src[src.index("\n  collection_date:") + 1:src.index("\n  notes:") + 1]
mid = mid.replace("families N to Z, except Proteaceae.", "family Myrtaceae.")
notes = ("  notes: 'The Flora of Australia extraction is split across datasets by family, to keep each data.csv within GitHub\n"
         "    file-size limits: ABRS_AtoM_2026 (families A to M), ABRS_NtoZ_2026 (families N to Z), and ABRS_Asteraceae_2026,\n"
         "    ABRS_Fabaceae_2026, ABRS_Myrtaceae_2026 and ABRS_Proteaceae_2026. One parser, ABRS_AtoM_2026/raw/build_data_csv.R,\n"
         "    builds all of them; this dataset is written in long format (since October 2026). Family in this dataset: Myrtaceae.\n"
         "    Candidate traits not yet in traits.yml (organ x character traits such as calyx_hairs, petal_count, ovary_shape) are\n"
         "    listed with their coverage in raw/candidate_traits.csv, which the build script rewrites on every full build;\n"
         "    custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description,\n"
         "    leaf_description, common_name, ...) carry the verbatim organ-level text and are unmapped. Parenthetical extremes\n"
         "    (measurement_range = extreme) are in data.csv but not mapped.'\n")
# keep hand edits in the existing file: its dataset fields (collection_date ... original_file) and questions
cur_path = "data/ABRS_Myrtaceae_2026/metadata.yml"
cur = open(cur_path).read()
if "\n  collection_date:" in cur: mid = cur[cur.index("\n  collection_date:") + 1:cur.index("\n  notes:") + 1]
cur_q = yaml.safe_load(cur).get("questions")
out = head + cust + mid + notes + tail_ctx + "traits:\n"
for t in present:
    out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
out += "substitutions: .na\ntaxonomic_updates: .na\nexclude_observations: .na\n"
if cur_q: out += yaml.safe_dump({"questions": cur_q}, sort_keys=False, allow_unicode=True, width=120)
open("data/ABRS_Myrtaceae_2026/metadata.yml", "w").write(out)
print(len(present), "traits mapped;", len(missing), "new blocks:", missing)
