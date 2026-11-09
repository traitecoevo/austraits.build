# One candidate-trait register across every dataset the parser writes (user 2026-10-30: a trait proposed in any family or group is
# checked for all other taxa; per-dataset lists hid that): data/ABRS_AtoM_2026/raw/candidate_traits_all.csv
import csv, yaml
from collections import defaultdict, Counter
csv.field_size_limit(10**9)
ds = ["ABRS_AtoM_2026", "ABRS_NtoZ_2026", "ABRS_Asteraceae_2026", "ABRS_Fabaceae_2026", "ABRS_Myrtaceae_2026", "ABRS_Proteaceae_2026", "CPBR_2026", "NHNSW_2026", "WATTLE_2026", "AusGrass3_2026", "WAH_2026_1", "WAH_2026_2", "WAH_2026_3", "WAH_2026_4", "RBGV_2026", "SAH_2026"]
defined = set(yaml.safe_load(open("config/traits.yml"))["traits"]["elements"])
taxa = defaultdict(lambda: defaultdict(set)); fams = defaultdict(lambda: defaultdict(set)); vals = defaultdict(Counter); kind = {}
for d in ds:
    for r in csv.DictReader(open(f"data/{d}/data.csv")):
        t = r["trait_name"]
        if t in defined or t.endswith("_description") or t in ("notes", "common_name", "profile_author", "biostatus", "phenology_text"): continue
        if not (r["value"] or r["value_type"]): continue
        taxa[t][d].add(r["taxon_name"]); fams[t][r["family"]].add(r["taxon_name"])
        if r["value"] and not r["value"].replace(".", "").replace("-", "").isdigit():
            kind.setdefault(t, "categorical"); vals[t].update(r["value"].split())
        else: kind.setdefault(t, "numeric")
rows = []
for t in taxa:
    n_all = len(set().union(*taxa[t].values()))
    f = sorted(((k, len(v)) for k, v in fams[t].items()), key=lambda x: -x[1])
    rows.append({"trait": t, "type": kind.get(t, ""), "n_taxa": n_all, "n_families": len(f),
                 "family_confined": "yes" if len(f) <= 2 else "",
                 "datasets": "; ".join(f"{d} ({len(v)})" for d, v in sorted(taxa[t].items())),
                 "families": "; ".join(f"{k} ({n})" for k, n in f[:12]) + ("; ..." if len(f) > 12 else ""),
                 "suggested_values": "; ".join(f"{k} ({n})" for k, n in vals[t].most_common(12))})
rows.sort(key=lambda r: (-r["n_taxa"], r["trait"]))
with open("data/ABRS_AtoM_2026/raw/candidate_traits_all.csv", "w", newline="") as fh:
    w = csv.DictWriter(fh, list(rows[0])); w.writeheader(); w.writerows(rows)
print(len(rows), "candidate traits;", sum(r["family_confined"] == "yes" for r in rows), "confined to 1-2 families")
