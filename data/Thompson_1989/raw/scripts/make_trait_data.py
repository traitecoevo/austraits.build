"""Step 2 of the Thompson_1989 extraction (run extract_text.py first): writes raw/trait_data.csv from
  - spec.txt, the hand transcription of each taxon's habit and leaves (growth form, habit, branching, height, leaf shape and size), and
  - extra.py, the scripted characters (stems, bark, flowers, fruit, seeds, flowering period, distribution and habitat),
then copies species values down to subspecies for characters the subspecies text does not mention."""
import csv, re, sys
from pathlib import Path

here = Path(__file__).parent          # raw/scripts
raw = here.parent                     # raw
out = raw / "trait_data.csv"

TRAITS = {"gf": "plant_growth_form", "hab": "stem_growth_habit", "br": "stem_branching_form", "ls": "leaf_shape",
          "h": "plant_height", "ll": "leaf_length", "lw": "leaf_width"}
LEAF = {"ls", "ll", "lw"}

ocr_fixes = {"m m": "mm", "m ore": "more", "m ostly": "mostly", "m ost": "most", "m uch": "much", "m aturity": "maturity",
             "m idrib": "midrib", "m idvein": "midvein", "m argins": "margins", "sm ooth": "smooth", "ultim ately": "ultimately",
             "w ithout": "without", "w ith": "with", "w ide": "wide", "alm ost": "almost", "num erous": "numerous",
             "Com pact": "Compact", "com pact": "compact", "M ultistem m ed": "Multistemmed", "arom atic": "aromatic",
             "rem aining": "remaining", "prom inent": "prominent", "m inute": "minute", "um bo": "umbo", "acum inate": "acuminate",
             "R ather": "Rather", "M uch": "Much", "R obust": "Robust", "interm ediate": "intermediate", "a n d": "and",
             "rath er": "rather", "betw een": "between", "o f": "of", "lam ina": "lamina", "som ew hat": "somewhat",
             "obovateelliptical": "obovate-elliptical", "linearlanceolate": "linear-lanceolate", "ovatelanceolate": "ovate-lanceolate",
             "obovateoblanceolate": "obovate-oblanceolate", "sim ilar": "similar", "lh as wide": "1/2 as wide"}


def clean_text(s):
    for a, b in ocr_fixes.items():
        s = re.sub(r"(?<![A-Za-z])" + re.escape(a) + r"(?![A-Za-z])", b, s)
    s = re.sub(r"(\d)\s*-\s*(\d)", r"\1--\2", s)          # ranges as a--b
    s = re.sub(r"(\d)\s*[—–]\s*(\d)", r"\1--\2", s)
    s = re.sub(r"\(\s*(\d[\d.]*)\s*[-—–]+\s*\)", r"(\1--)", s)
    s = re.sub(r"\(\s*[-—–]+\s*(\d[\d.]*)\s*\)", r"(--\1)", s)
    s = s.replace("usually 12 m tall", "usually 1--2 m tall")   # incanum, checked on the page image
    return re.sub(r"\s+", " ", s).strip()


# descriptions (habit + leaves) in paper order, and the page each starts on
descs = [b.split("\n", 1) for b in (raw / "text" / "desc.txt").read_text().split("### ")[1:]]
pages = (raw / "text" / "t.txt").read_text(encoding="utf-8").split("\f")
norm_pages = [" ".join(p.split()) for p in pages]


def page_of(text):
    probe = " ".join(text.split())[:60]
    for i, p in enumerate(norm_pages):
        if probe in p:
            return i + 1 + 300   # pdf page 1 = Telopea p. 301
    probe = " ".join(text.split())[:25]
    for i, p in enumerate(norm_pages):
        if probe in p:
            return i + 1 + 300
    return None


# spec blocks
blocks, cur = [], None
for line in (here / "spec.txt").read_text().splitlines():
    if not line.strip() or line.startswith("#"):
        continue
    if line.startswith("== "):
        cur = {"name": line[3:].strip(), "rows": []}
        blocks.append(cur)
    else:
        cur["rows"].append([x.strip() for x in line.split("|")])
assert len(blocks) == len(descs) == 87, (len(blocks), len(descs))

rows = []
for blk, (head, body) in zip(blocks, descs):
    epithet = blk["name"].split()[-1]
    assert epithet.replace("-", "").lower()[:5] in head.replace(" ", "").lower().replace("sphacrocarpum", "sphaerocarpum").replace("continental", "continentale"), (blk["name"], head)
    taxon = "Leptospermum " + blk["name"]
    body = body.strip().lstrip(":, ").strip()
    # the Latin / type text left in front of a few descriptions
    m = re.search(r"\b(Shrub|Spinescent|Leaves|Rather|Dense|Bushy|Spreading|Slender|Compact|Com pact|Erect|Low|Open|Sparsely|Several|Multistem|M ultistem|Tall|Straggling|Rigid|Leafy|Dwarfed|A shrub|Prostrate|Much|M uch|R obust|R ather|Broadly)\b", body)
    if m and m.start() > 0 and not body.startswith(("Shrub", "Leaves")):
        body = body[m.start():]
    page = page_of(body[:80]) or {"incanum": 377}.get(blk["name"])   # incanum: text layer reads "usually 12 m"; page checked on the image
    text = clean_text(body)
    habit_clause = text.split(";")[0]
    if blk["name"].count(" ") == 0:
        leaf_part = text[text.find("Leaves"):] if "Leaves" in text else ""
        # up to and including the shape clause
        parts = leaf_part.split(", ")
        shape_re = r"ovate|obovate|ellip|lanceol|oblanceol|linear|orbicular|oblong|oval|cuneate|obcordate|clavate|obcuneate"
        k = next((i for i, p in enumerate(parts) if re.search(shape_re, p)), len(parts) - 1)
        while k + 1 < len(parts) and re.match(r"(occasionally|often|rarely|or |to |and |very broadly|narrow)", parts[k + 1]):
            k += 1
        leaf_clause = ", ".join(parts[:k + 1])
        section = f"Description, p.{page}"
    else:
        # subspecies: habit sentence, then 'Leaves ...' sentence
        habit_clause = text.split(". Leaves")[0] if ". Leaves" in text else ""
        leaf_clause = ("Leaves" + text.split("Leaves", 1)[1]).split(", the apex")[0].split(", tapering")[0] if "Leaves" in text else ""
        section = f"Subspecies description, p.{page}"
    for r in blk["rows"]:
        code, val = r[0], r[1]
        opts = dict(x.split("=", 1) for x in r[2:] if "=" in x)
        pos = [x for x in r[2:] if "=" not in x]
        trait = TRAITS[code]
        if code in ("h", "ll", "lw"):
            clean, vtype, unit = val, pos[0], pos[1]
        else:
            clean, vtype, unit = (pos[0] if pos else ""), "mode", ""
        rows.append({
            "taxon_name": taxon, "entity_type": "species", "rank_scored_from": "",
            "commonness_qualifier": opts.get("q", ""), "population_region": opts.get("p", ""),
            "entity_measured": opts.get("e", ""), "trait": trait, "in_traits.yml": "x",
            "trait_value_raw": leaf_clause if code in LEAF else habit_clause,
            "trait_value": val, "trait_value_clean": clean, "value_type": vtype, "unit": unit,
            "source_section": section, "notes": opts.get("n", ""),
        })

# scripted characters (extra.py) from the full species text
import json
from extra import extract, dejoin
full = list(json.load(open(raw / "text" / "blocks.json")).values())
for blk, txt in zip(blocks, full):
    taxon = "Leptospermum " + blk["name"]
    # start at the description (after Latin diagnosis, type and synonymy)
    t = dejoin(txt)
    first = next(r for r in rows if r["taxon_name"] == taxon)
    probe = re.sub(r"\W", "", first["trait_value_raw"])[:25] if first["trait_value_raw"] else ""
    flat = re.sub(r"\W", "", t)
    start = 0
    if probe and probe in flat:
        # map the flat offset back to the text
        k = flat.index(probe); n = 0
        for i, ch in enumerate(t):
            if re.match(r"\w", ch):
                if n == k: start = i; break
                n += 1
    page = int(first["source_section"].split("p.")[1])
    rows += extract(taxon, t[start:], page, " subsp. " in taxon)

# characters only in the key to subspecies (they stop the species value being copied down)
def key_row(taxon, trait, raw, val, vtype, unit, note=""):
    return {"taxon_name": taxon, "entity_type": "species", "rank_scored_from": "", "commonness_qualifier": "", "population_region": "",
            "entity_measured": "", "trait": trait, "in_traits.yml": "x", "trait_value_raw": raw, "trait_value": val,
            "trait_value_clean": val, "value_type": vtype, "unit": unit, "source_section": "Key to subspecies, p.396", "notes": note}
k_raw = "placenta high and with c. 20-60 ovules"
rows += [key_row("Leptospermum polygalifolium subsp. tropicum", "flower_ovules_per_functional_carpel_count", k_raw, "20", "minimum", "count"),
         key_row("Leptospermum polygalifolium subsp. tropicum", "flower_ovules_per_functional_carpel_count", k_raw, "60", "maximum", "count"),
         key_row("Leptospermum polygalifolium subsp. transmontanum", "flower_ovules_per_functional_carpel_count",
                 "placenta not high and with c. 80 ovules", "80", "mean", "count")]

# ecology stated in the introduction and the discussion of groups (only unhedged statements become values)
def eco_row(taxon, trait, raw, val, section, vtype="mode", unit="", rank="", note=""):
    return {"taxon_name": "Leptospermum " + taxon, "entity_type": "species", "rank_scored_from": rank, "commonness_qualifier": "",
            "population_region": "", "entity_measured": "", "trait": trait, "in_traits.yml": "x", "trait_value_raw": raw,
            "trait_value": val, "trait_value_clean": val, "value_type": vtype, "unit": unit, "source_section": section, "notes": note}
raw_tri = ("The disproportionately stout trunks of L. trinervium are obviously associated with its possession of epicormic buds "
           "(protected by corky layered bark) ready to shoot after fire in its dry forest habitat.")
raw_sub = ("L. trinervium and its close relative L. subglabratum have a \"paper\" bark with many layers that obviously protect the "
           "epicormic shoots from which these species can regenerate after fire")
raw_myr = "L. myrsinoides is said (Burrell 1981) to regenerate from underground parts."
raw_via = ("Samples from a number of species have been germinated to reveal a very low percentage viability, from +/- 3.5% in "
           "L. glabrescens to 43% in L. myrsinoides, with an average of 20% for the species sampled.")
raw_nec = "All species of Group 2 have the surface [of the ovary-top] glabrous and often shining with nectar."
rows += [
    eco_row("trinervium", "resprouting_capacity", raw_tri, "resprouts", "Introduction: Habit, p.304",
            note="the description (p.366) adds: trunk stout 'probably owing to regeneration from epicormic shoots after fire'"),
    eco_row("trinervium", "bud_bank_location", raw_tri, "epicormic_buds", "Introduction: Habit, p.304"),
    eco_row("subglabratum", "resprouting_capacity", raw_sub, "resprouts", "Putative natural groups, p.331"),
    eco_row("subglabratum", "bud_bank_location", raw_sub, "epicormic_buds", "Putative natural groups, p.331"),
    eco_row("myrsinoides", "resprouting_capacity", raw_myr, "resprouts", "Introduction: Habit, p.304",
            note="Thompson citing Burrell (1981): secondary source"),
    eco_row("myrsinoides", "bud_bank_location", raw_myr, "underground_buds", "Introduction: Habit, p.304",
            note="'regenerate from underground parts'; Thompson citing Burrell (1981): secondary source"),
    eco_row("glabrescens", "seed_viability", raw_via, "0.035", "Introduction: Seed, p.323", "mean", "{count}/{count}",
            note="germination of a seed sample; printed as +/- 3.5%"),
    eco_row("myrsinoides", "seed_viability", raw_via, "0.43", "Introduction: Seed, p.323", "mean", "{count}/{count}",
            note="germination of a seed sample"),
]
group2 = [b["name"] for b in blocks[38:]]          # 39. L. emarginatum to 79. L. epacridoideum, with their subspecies
rows += [eco_row(n, "flower_nectar_production", raw_nec, "nectar_produced", "Introduction: Top of the ovary, p.316",
                 rank="species_group", note="statement about all Group 2 species (nos 39-79)") for n in group2 if " subsp. " not in n]

# hedged ecological remarks: kept as notes on the nearest character, not as values
hedged = {
    ("confertum", "seed_shape"): "p.331: the similarity of the seeds of L. confertum to L. spinescens 'may be convergence associated with distribution by ants'",
    ("spinescens", "seed_shape"): "p.331: the similarity of the seeds of L. confertum to L. spinescens 'may be convergence associated with distribution by ants'",
    ("laevigatum", "fruit_persistence"): "p.321: sclerified cells lining the loculus are 'perhaps' the mechanism by which this species, whose fruits are soon shed, survives fire",
    ("liversidgei", "fruit_persistence"): "p.333: very woody fruit with reduced seed-set and an extended woody rim that 'does appear to have become modified for survival in its very fire-prone habitat'",
    ("scoparium", "fruit_persistence"): "p.323: seed viable when the fruit is about six months old and germinates normally after several years' retention on the plant (Burrell 1965)",
    ("nitidum", "habitat_description"): "p.336: 'it has been most successful in the fire-ravaged mountains'",
}
myrt_subgroup = ["rupestre", "myrtifolium", "gregarium", "obovatum", "argenteum", "micromyrtus", "scoparium", "continentale",
                 "juniperinum", "rupicola", "squarrosum", "rotundifolium", "grandiflorum"]
for n in myrt_subgroup:
    note = "p.334 (L. myrtifolium subgroup): much woodier fruits, 'a character that perhaps allows recovery from fire'"
    hedged[(n, "fruit_persistence")] = (hedged.get((n, "fruit_persistence"), "") + "; " + note).strip("; ")
for r in rows:
    k = (r["taxon_name"].replace("Leptospermum ", ""), r["trait"])
    if k in hedged and not r["rank_scored_from"]:
        r["notes"] = (r["notes"] + "; " + hedged[k]).strip("; ")

# page of each row: where its source clause is printed (searched from the taxon's first page onwards)
flat_pages = [re.sub(r"\W", "", p) for p in pages]
for r in rows:
    p0 = int(r["source_section"].split("p.")[1])
    probe = re.sub(r"\W", "", r["trait_value_raw"].replace("--", "-"))[:40]
    for k in range(p0 - 300 - 1, min(p0 - 300 + 4, len(flat_pages))):
        if probe and probe in flat_pages[k]:
            r["source_section"] = re.sub(r"p\.\d+", f"p.{k + 301}", r["source_section"])
            break

# subspecies: characters the subspecies text does not mention are those of the species (copied down)
own = {"distribution_description", "habitat_description", "soil_terms", "geology_terms", "habitat_terms", "plant_growth_substrate", "flowering_time"}
new = []
for r in rows:
    new.append(r)
for sp in ["Leptospermum petersonii", "Leptospermum polygalifolium"]:
    sp_rows = [r for r in rows if r["taxon_name"] == sp]
    for sub in sorted({r["taxon_name"] for r in rows if r["taxon_name"].startswith(sp + " subsp.")}):
        have = {(r["trait"], r["entity_measured"]) for r in rows if r["taxon_name"] == sub}
        for r in sp_rows:
            if (r["trait"], r["entity_measured"]) in have or r["trait"] in own:
                continue
            c = dict(r, taxon_name=sub, rank_scored_from=r["rank_scored_from"] or "species",
                     notes=("Copied down from the species description: the subspecies description gives only the characters that differ. " + r["notes"]).strip())
            new.append(c)
rows = new

# commonness: once a trait has a qualified alternative, its unqualified rows are the usual values
from collections import defaultdict
quals = defaultdict(set)
for r in rows:
    if r["commonness_qualifier"] and r["commonness_qualifier"] != "usually":
        quals[(r["taxon_name"], r["trait"])].add(r["commonness_qualifier"])
for r in rows:
    if not r["commonness_qualifier"] and quals[(r["taxon_name"], r["trait"])] and not r["population_region"]:
        r["commonness_qualifier"] = "usually"

# in_traits.yml flag and candidate notes from the branch's traits.yml
import subprocess
known = set(subprocess.run(["Rscript", "-e", 'cat(names(yaml::read_yaml("config/traits.yml")$traits$elements), sep = "\\n")'],
                           cwd=raw.parents[2], capture_output=True, text=True).stdout.split())
cand = "Candidate trait, not in config/traits.yml (checked 2026-11)."
for r in rows:
    r["in_traits.yml"] = "x" if r["trait"] in known else ""
    r["notes"] = r["notes"].replace(cand, "").strip(" ;")
    if r["trait"] not in known:
        r["notes"] = (cand + " " + r["notes"]).strip()

out.parent.mkdir(parents=True, exist_ok=True)
with open(out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0]))
    w.writeheader()
    w.writerows(rows)
print(len(rows), "rows,", len({r["taxon_name"] for r in rows}), "taxa ->", out)

# data.csv (what the build reads): trait_data.csv plus trait_scoring_method from rank_scored_from, and units in the
# traits.yml form for the count traits
method = {"": "", "species": "inferred_from_species", "species_group": "inferred_from_species_group"}
unit_fix = {"flowers_per_inflorescence": "{count}/{count}", "seed_viability": "{count}/{count}"}
with open(raw.parent / "data.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0]) + ["trait_scoring_method"])
    w.writeheader()
    for r in rows:
        d = dict(r)
        d["trait_scoring_method"] = "inferred" if r["notes"].startswith("inferred") else method[r["rank_scored_from"]]
        if d["unit"] == "count":
            d["unit"] = unit_fix.get(d["trait"], "{count}")
        w.writerow(d)
print("data.csv written")
missing_page = {r["taxon_name"] for r in rows if r["source_section"].endswith("None")}
print("taxa without a page:", sorted(missing_page))
