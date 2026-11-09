# SUPERSEDED 2026-10-30 by ignore/jobs/cp1/meta_update.py (add-only): regenerating drops hand-made mappings. Do not run on an existing metadata.yml.
# ABRS_AtoM_2026/metadata.yml in long format, from its wide metadata (ignore/jobs/am1/metadata_wide.yml: source, contributors,
# dataset text, questions) and the NtoZ contexts; one traits block per traits.yml trait present in data.csv, reused from the
# long-format datasets, else the wide AtoM block with var_in = trait_name, value_type read from the data and its methods text
# reworded for long format. Rerun after every AtoM rebuild.
import csv, re, yaml
def find(d):
    if isinstance(d, dict):
        if "post_fire_recruitment" in d: return d
        for v in d.values():
            r = find(v)
            if r: return r
defined = find(yaml.safe_load(open("config/traits.yml")))
csv.field_size_limit(10**9)
present = sorted({r["trait_name"] for r in csv.DictReader(open("data/ABRS_AtoM_2026/data.csv"))} & set(defined))
wide_src = "ignore/jobs/am1/metadata_wide.yml"
num_methods = next(b["methods"] for b in yaml.safe_load(open("data/ABRS_NtoZ_2026/metadata.yml"))["traits"] if b["trait_name"] == "leaf_length")
def reword(b):
    m = b.get("methods", "")
    if b.get("unit_in") not in (None, ".na") and m.startswith("Typical range given in the taxon description"):
        return num_methods
    m = re.sub(r"parenthetical extremes are in [a-z0-9_]+_extreme_min and [a-z0-9_]+_extreme_max", "parenthetical extremes are on measurement_range = extreme rows", m)
    return re.sub(r"\b[a-z0-9_]+_description\b", "value_description", m)
blocks = {}
for d, path in [("NtoZ", None), ("Fabaceae", None), ("Proteaceae", None), ("Asteraceae", None), ("Myrtaceae", None), ("AtoM", wide_src)]:
    m = yaml.safe_load(open(path or f"data/ABRS_{d}_2026/metadata.yml"))
    for b in m["traits"]:
        t = b["trait_name"]
        if t in blocks: continue
        b = {k: v for k, v in b.items() if not k.startswith("question")}
        if d == "AtoM": b["methods"] = reword(b)
        b["var_in"] = t
        b["value_type"] = "value_type"
        blocks[t] = b
present = [t for t in present if t in blocks]
# (pairs of pinnae / leaflets: the NtoZ block maps var_in leaflet_count_pairs to leaflet_count, unit pairs)
pair_block = next(b for b in yaml.safe_load(open("data/ABRS_NtoZ_2026/metadata.yml"))["traits"] if b["var_in"] == "leaflet_count_pairs")
has_pairs = any(r["trait_name"] == "leaflet_count_pairs" for r in csv.DictReader(open("data/ABRS_AtoM_2026/data.csv")))
filt = sorted(present + (["leaflet_count_pairs"] if has_pairs else []))

src = open(wide_src).read()
ntoz = open("data/ABRS_NtoZ_2026/metadata.yml").read()
head = src[:src.index("  custom_R_code:")].replace("  data_is_long_format: no\n", "  data_is_long_format: yes\n")
assert "data_is_long_format: yes" in head
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in filt[i:i + 4]) for i in range(0, len(filt), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = src[src.index("\n  collection_date:") + 1:src.index("\n  notes:") + 1]
w = yaml.safe_load(open(wide_src))
notes = w["dataset"]["notes"]
old1 = "One parser, raw/build_data_csv.R in this dataset, builds all of them."
assert old1 in notes
notes = notes.replace(old1, "One parser, raw/build_data_csv.R in this dataset, builds all of them; this dataset is written in long format (since October 2026).")
i = notes.index("Candidate traits in data.csv")
notes = notes[:i] + ("Candidate traits not yet in traits.yml (mostly organ x character traits such as calyx_hairs, petal_count, ovary_shape) "
                     "are listed with their coverage in raw/candidate_traits.csv, which the build script rewrites on every full build; "
                     "custom_R_code keeps only the traits mapped below. Rows whose trait_name is a text field (habit_description, "
                     "leaf_description, common_name, ...) carry the verbatim organ-level text and are unmapped. Parenthetical extremes "
                     "(measurement_range = extreme) are in data.csv but not mapped.")
notes_y = "  " + yaml.safe_dump({"notes": notes}, width=120, allow_unicode=True).rstrip("\n").replace("\n", "\n  ") + "\n"
tail_ctx = ntoz[ntoz.index("identifiers:"):ntoz.index("traits:\n")]
q = w["questions"]
q["question2"] = "Parenthetical extremes (measurement_range = extreme rows) are not mapped. Should they be added as separate minimum / maximum records with a context (e.g. commonness_qualifier = rarely)?"
# keep hand edits in the existing file: its dataset fields (collection_date ... original_file) and questions
cur_path = "data/ABRS_AtoM_2026/metadata.yml"
cur = open(cur_path).read()
if "\n  collection_date:" in cur: mid = cur[cur.index("\n  collection_date:") + 1:cur.index("\n  notes:") + 1]
cur_q = yaml.safe_load(cur).get("questions")
out = head + cust + mid + notes_y + tail_ctx + "traits:\n"
for t in present:
    out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
if has_pairs: out += yaml.safe_dump([pair_block], sort_keys=False, allow_unicode=True, width=120)
out += "substitutions: .na\ntaxonomic_updates: .na\nexclude_observations: .na\n"
out += yaml.safe_dump({"questions": cur_q or q}, sort_keys=False, allow_unicode=True, width=120)
yaml.safe_load(out)
open("data/ABRS_AtoM_2026/metadata.yml", "w").write(out)
from_atom = [t for t in present if all(t not in {b["trait_name"] for b in yaml.safe_load(open(f"data/ABRS_{d}_2026/metadata.yml"))["traits"]} for d in ["NtoZ", "Fabaceae", "Proteaceae", "Asteraceae", "Myrtaceae"])]
print(len(present), "traits mapped;", len(from_atom), "blocks from the wide AtoM metadata:", from_atom)
