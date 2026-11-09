# Myrtaceae wide -> long equivalence: every mapped value (and its parenthetical extremes) in the wide data.csv must appear in the
# long data.csv with the same taxon, contexts, trait, value_type and range, and vice versa (argv: wide.csv long.csv metadata_wide.yml)
import csv, sys, yaml
from collections import Counter
csv.field_size_limit(10**9)
wide, long_, meta = sys.argv[1:4]
m = yaml.safe_load(open(meta))
ctx = ["commonness_qualifier", "leaf_division", "entity_measured", "population_region", "plant_organ_measured", "phenological_stage", "trait_scoring_method", "scoring_certainty"]
def num(v):
    try: return repr(float(v))
    except: return v
W = Counter()
for r in csv.DictReader(open(wide)):
    k = tuple(r.get(c, "") or "" for c in ctx)
    for b in m["traits"]:
        v = r.get(b["var_in"], "")
        if v not in ("", None):
            vt = b["value_type"]
            for vv in ([v] if vt != "mode" else [v]): W[(r["taxon_name"], k, b["trait_name"], vt, "", num(vv))] += 1
        base = b["var_in"].rsplit("_", 1)[0] if b["var_in"].endswith(("_min", "_max")) else None
        if base and b["var_in"].endswith("_min") and r.get(base + "_extreme_min"): W[(r["taxon_name"], k, b["trait_name"], "minimum", "extreme", num(r[base + "_extreme_min"]))] += 1
        if base and b["var_in"].endswith("_max") and r.get(base + "_extreme_max"): W[(r["taxon_name"], k, b["trait_name"], "maximum", "extreme", num(r[base + "_extreme_max"]))] += 1
traits = {b["trait_name"] for b in m["traits"]}
NUMT = {b["trait_name"] for b in m["traits"] if b["value_type"] != "mode"}
L = Counter()
for r in csv.DictReader(open(long_)):
    if r["trait_name"] not in traits or r["value"] == "": continue
    rng = "extreme" if r["measurement_range"] == "extreme" else ""
    vt = r["value_type"] if r["value_type"] in ("minimum", "maximum", "mode") else r["value_type"]
    # (a single number is a "mean" in long format; the wide file has it in *_max with value_type maximum)
    L[(r["taxon_name"], tuple(r.get(c, "") or "" for c in ctx), r["trait_name"], vt, rng, num(r["value"]))] += 1
Lm = Counter({(t, k, tr, ("maximum" if vt == "mean" or (vt == "mode" and tr in NUMT) else vt), rng, v): n for (t, k, tr, vt, rng, v), n in L.items()})
only_w, only_l = W - Lm, Lm - W
print("wide tuples", sum(W.values()), " long tuples", sum(Lm.values()), " only in wide", sum(only_w.values()), " only in long", sum(only_l.values()))
print("only-wide by trait", Counter(k[2] for k in only_w.elements()).most_common(12))
print("only-long by trait", Counter(k[2] for k in only_l.elements()).most_common(12))
for k in list(only_w)[:10]: print("  W", k)
for k in list(only_l)[:10]: print("  L", k)
