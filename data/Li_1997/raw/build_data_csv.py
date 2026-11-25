"""Assemble data/Li_1997/data.csv from the full thesis extraction (raw/trait_data.csv) and the
Li et al. (1997) Table 3 wax yields (raw/Li_1997_table3_wax_yield.csv).

Keeps only the traits added to AusTraits (oil yield, 1,8-cineole % and absolute content, wax yield,
wax crystal form, wax density, juvenile glaucousness, DBH, plus preferred habitat as a context) and drops
summary tables that restate other tables (Tables 3.4, 6.1, 11.4, 11.10, 11.7 summary rows, 10.1 except
oil yields, 11.6A cineole %, species-level Ch 5.3 prose).
Wax yields of the 17 Symphyomyrtus species are taken from Li et al. (1997) Table 3 (which splits E. ovata into
east- and west-coast populations and uses a slightly different population set for E. cordata and E. viminalis)
in place of thesis Table 5.1.
Natural-survey values are means across populations: entity_type metapopulation, located at Tasmania's centroid,
with the populations sampled listed in measurement_remarks; species sampled at a single site are population.
Run from the repo root: python3 data/Li_1997/raw/build_data_csv.py
"""
import csv
import re
from collections import OrderedDict

SRC = "data/Li_1997/raw/trait_data.csv"
LOC = "data/Li_1997/raw/location_data.csv"
LI1997 = "data/Li_1997/raw/Li_1997_table3_wax_yield.csv"
OUT = "data/Li_1997/data.csv"

rows = list(csv.DictReader(open(SRC, encoding="utf-8")))
locs = {l["location_code"]: l for l in csv.DictReader(open(LOC, encoding="utf-8"))}
li1997 = list(csv.DictReader(open(LI1997, encoding="utf-8")))
LI1997_TAXA = {r["taxon_name"] for r in li1997}

TASMANIA = "Tasmania (multiple natural populations)"
SINGLE_SITE = {"Eucalyptus radiata": "RA", "Eucalyptus perriniana": "Pe"}

# natural populations sampled per species (Appendix 4.1), attributed by population-code prefix
code_taxon = {}
for l in locs.values():
    if l["location_role"].startswith("natural") and "/" not in l["species_sampled"]:
        code_taxon.setdefault(l["population_code"].split()[0].upper(), l["species_sampled"].split(" (")[0])
pops = {}
for l in locs.values():
    taxon = code_taxon.get(l["population_code"].split()[0].upper()) if l["population_code"] else None
    if l["location_role"].startswith("natural") and taxon:  # V/D intermediates are not in the thesis species means
        pops.setdefault(taxon, []).append(l["location_name"])

COLS = ["source_id", "study_part", "source_table", "taxon_name", "location_name", "entity_type", "basis_of_record",
        "life_stage", "leaf_type", "leaf_age", "seedlot_or_family", "sampling_time", "collection_date",
        "oil_yield", "oil_yield_SD", "oil_yield_n",
        "oil_yield_internal_standard", "oil_yield_internal_standard_SD", "oil_yield_internal_standard_n",
        "cineole_percent", "cineole_percent_SD", "cineole_percent_n",
        "cineole_absolute", "cineole_absolute_SD", "cineole_absolute_n",
        "wax_yield", "wax_yield_SD", "wax_yield_n",
        "wax_crystal_form", "wax_density", "glaucousness_juvenile_leaves",
        "DBH", "DBH_n", "preferred_habitat", "population_group", "measurement_remarks"]

TRAITS = {"leaf_essential_oil_per_dry_mass", "leaf_oil_1_8_cineole_percent", "leaf_oil_1_8_cineole_per_dry_mass",
          "leaf_wax_per_dry_mass", "leaf_wax_structure", "leaf_wax_cover_density", "leaf_glaucousness",
          "stem_diameter", "habitat"}

DROP_SECTIONS = ("Table 3.4", "Table 6.1", "Table 11.4", "Table 11.10", "Table 11.7 (t-test", "Table 2.1")

table = OrderedDict()


def leaf_phase(ctx):
    if ctx.startswith("adult leaves"):
        return "adult leaves"
    if ctx.startswith("juvenile leaves"):
        return "juvenile leaves"
    return ""


def plant_stage(part, leaf):
    # natural survey: adult foliage from reproductively mature trees, juvenile foliage from young plants;
    # trials: both foliage types from the same trees, except the two young F1 hybrid trials
    if part.startswith("natural"):
        return {"adult leaves": "adult", "juvenile leaves": "sapling"}.get(leaf, "unknown")
    if part.startswith(("E. ovata x E. globulus", "E. nitens x E. globulus")):
        return "sapling"
    return "adult"


def part_of(sec):
    if sec.startswith(("Table 4", "Table 5", "Table 2", "Ch. 5.3", "Li_1997")):
        return "natural populations survey (Ch 4-5)"
    if sec.startswith("Table 3.2"):
        return "provenance trial, provenance comparison (Ch 3 experiment 1)"
    if sec.startswith("Appendix 3.1"):
        return "provenance trial, site comparison (Ch 3 experiment 2)"
    if sec.startswith("Appendix 3.2"):
        return "provenance trial, seasonal variation (Ch 3 experiment 3)"
    if sec.startswith("Appendix 3.3"):
        return "provenance trial, leaf age and growth season (Ch 3 experiment 3)"
    if sec.startswith(("Table 10", "Appendix 10")):
        return "E. ovata x E. globulus F1 hybrid trial (Ch 10)"
    if sec.startswith(("Table 11.3", "Table 11.7")):
        return "E. nitens provenance trial (Ch 11 experiment 1)"
    if sec.startswith("Appendix 11.1"):
        return "E. nitens family trials (Ch 11 experiment 2)"
    if sec.startswith(("Table 11.6", "Appendix 11.2")):
        return "E. nitens x E. globulus F1 hybrid trial (Ch 11 experiment 3)"
    if sec.startswith("Table 11.8"):
        return "E. nitens and E. globulus wax comparison, Ridgley (Ch 11)"
    raise ValueError(sec)


DATES = {"Li_1997": "1989-05/1989-11", "Table 2": "1989-05/1989-12", "Table 4": "1989-05/1989-12", "Table 5": "1989-05/1989-12", "Ch. 5.3": "1989-05/1989-12",
         "Table 3.2": "1989-04", "Appendix 3.1": "1991-05", "Appendix 10": "1990-12", "Table 10": "1990-12",
         "Table 11.3": "1988-11", "Table 11.7": "1988-11", "Appendix 11.1": "1990-11",
         "Table 11.6": "1989-04", "Appendix 11.2": "1989-04", "Table 11.8": "1987-06"}
SEASON_MONTHS = {"1": "1987-11", "2": "1988-01", "3": "1988-03", "4": "1988-05", "5": "1988-07",
                 "6": "1988-09", "7": "1988-11"}


def key_and_meta(x):
    sec, ctx = x["source_section"], x["context"]
    part = part_of(sec)
    lc = x["location_code"]
    loc = TASMANIA if lc == "TAS" else (locs[lc]["location_name"] if lc else "")
    entity = x["entity_type"]
    leaf = leaf_phase(ctx)
    life = plant_stage(part, leaf)
    remarks = x.get("measurement_remarks", "")
    group = x.get("population_group", "")
    leaf_age, samp, date = "", "", ""
    for k, v in DATES.items():
        if sec.startswith(k):
            date = v
    m = re.search(r"sample time (\d)", ctx)
    if sec.startswith("Appendix 3.2"):
        samp = f"sample time {m.group(1)}"
        date = SEASON_MONTHS[m.group(1)]
    if sec.startswith("Appendix 3.3"):
        samp = f"sample time {m.group(1)}"
        date = re.search(r"sampled (\d{4}-\d\d-\d\d)", ctx).group(1)
        leaf_age = re.search(r"leaf age code ([A-G] \([^)]*\))", ctx).group(1)
    if sec.startswith(("Table 11.6", "Appendix 11.2")):
        leaf_age = ctx.split(";")[0]
    if sec.startswith(("Table 10", "Appendix 10")):
        leaf_age = "young adult leaves"
    fam = x["entity_context"]
    basis = "field" if part.startswith("natural") else "field_experiment"
    source = x.get("source_id", "Li_1993")
    key = (source, part, x["taxon_name"], loc, entity, leaf, leaf_age, fam, samp, group, remarks)
    meta = dict(source_id=source, study_part=part, taxon_name=x["taxon_name"], location_name=loc,
                entity_type=entity, basis_of_record=basis, life_stage=life, leaf_type=leaf, leaf_age=leaf_age,
                seedlot_or_family=fam, sampling_time=samp, collection_date=date, population_group=group,
                measurement_remarks=remarks)
    return key, meta


def put(x, col, value, sd=None, n=None):
    key, meta = key_and_meta(x)
    rec = table.setdefault(key, dict(meta, source_table=set()))
    rec["source_table"].add(x["source_section"].split(",")[0])
    rec[col] = value
    if sd is not None:
        rec[col + "_SD"] = sd
    if n is not None:
        rec[col + "_n"] = n


# Table 10.1 oil yields (correct F1 assignment, more precise) and Appendix 10.1 SDs for parent families only
t101 = {x["entity_context"]: x["trait_value_raw"] for x in rows
        if x["source_section"].startswith("Table 10.1 (A)") and x["trait"] == "leaf_essential_oil_per_dry_mass"}

for x in rows:
    sec, t = x["source_section"], x["trait"]
    if t not in TRAITS or sec.startswith(DROP_SECTIONS) or sec.startswith("Table 10.1"):
        continue
    if sec.startswith("Table 11.6 (A)") and t == "leaf_oil_1_8_cineole_percent":
        continue
    if sec.startswith("Ch. 5.3") and (x["entity_type"] == "species" or not x["location_code"]):
        continue
    if t == "leaf_wax_per_dry_mass" and sec.startswith("Table 5.1") and x["taxon_name"] in LI1997_TAXA:
        continue
    if part_of(sec).startswith("natural") and x["entity_type"] == "species":
        if x["taxon_name"] in SINGLE_SITE:
            x = dict(x, entity_type="population", location_code=SINGLE_SITE[x["taxon_name"]])
        else:
            x = dict(x, entity_type="metapopulation", location_code="TAS",
                     measurement_remarks="Mean across natural populations (number in replicates); survey sites for this species: "
                     + "; ".join(pops[x["taxon_name"]]))
    v, sd, n = x["trait_value_raw"], x["error_value"], x["n"]
    if t == "leaf_essential_oil_per_dry_mass":
        if sec.startswith("Appendix 3.2") or sec.startswith("Appendix 3.3"):
            put(x, "oil_yield_internal_standard", v, sd, n)
        elif sec.startswith("Appendix 10.1"):
            fam = x["entity_context"]
            if fam in t101:
                put(x, "oil_yield", t101[fam], "" if fam.startswith("F1") else sd, n)
            else:
                put(x, "oil_yield", v, sd, n)
        else:
            put(x, "oil_yield", v, sd, n)
    elif t == "leaf_oil_1_8_cineole_percent":
        put(x, "cineole_percent", v, sd, n)
    elif t == "leaf_oil_1_8_cineole_per_dry_mass":
        put(x, "cineole_absolute", v, sd, n)
    elif t == "leaf_wax_per_dry_mass":
        if sec.startswith("Table 11.7") and not n:
            n = "10" if x["context"].startswith("adult") else "5"
        put(x, "wax_yield", v, sd, n)
    elif t == "leaf_wax_structure":
        put(x, "wax_crystal_form", v)
    elif t == "leaf_wax_cover_density":
        put(x, "wax_density", v)
    elif t == "leaf_glaucousness":
        put(x, "glaucousness_juvenile_leaves", v)
    elif t == "stem_diameter":
        put(dict(x, context="adult leaves"), "DBH", v, None, "unknown")

# Li et al. (1997) Table 3 wax yields (Symphyomyrtus species)
for r in li1997:
    group = f"{r['population_group']} populations; " if r["population_group"] else ""
    x = dict(source_section="Li_1997 Table 3", context=r["leaf_type"], taxon_name=r["taxon_name"],
             entity_context="", source_id="Li_1997",
             population_group=f"{r['population_group']} populations" if r["population_group"] else "")
    if r["pop_no"] == "1":
        x.update(entity_type="population", location_code=SINGLE_SITE[r["taxon_name"]])
    else:
        x.update(entity_type="metapopulation", location_code="TAS",
                 measurement_remarks=f"Mean across {group}natural populations (number in replicates); "
                                     f"sites: {r['populations_sampled']}")
    put(x, "wax_yield", r["wax_yield"], r["wax_yield_SD"], r["pop_no"])

# Table 2.1 Barber (1955) glaucousness phenotype: juvenile-leaf species rows only
for x in rows:
    if x["source_section"].startswith("Table 2.1") and x["trait"] == "leaf_glaucousness":
        y = dict(x, context="juvenile leaves", entity_type="species", location_code="", entity_context="")
        put(y, "glaucousness_juvenile_leaves", x["trait_value_raw"])

# Table 2.4 preferred habitat (Davidson et al. 1981) as a context on the Ch 4-5 species rows
zone = {x["taxon_name"]: x["trait_value_raw"] for x in rows if x["trait"] == "habitat_vegetation_zone"}
hab = {}
for x in rows:
    if x["trait"] == "habitat" and x["source_section"].startswith("Table 2.4"):
        taxon = x["taxon_name"]
        if taxon == "Eucalyptus vernicosa" and x["trait_value_raw"].startswith("replaces E. vernicosa"):
            taxon = "Eucalyptus subcrenulata"
        hab[taxon] = f"{zone[x['taxon_name']]}: {x['trait_value_raw']}"
for key, rec in table.items():
    if rec["study_part"].startswith("natural") and rec["entity_type"] in ("species", "metapopulation", "population"):
        rec["preferred_habitat"] = hab.get(rec["taxon_name"], "")

with open(OUT, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=COLS)
    w.writeheader()
    for rec in table.values():
        rec["source_table"] = "; ".join(sorted(rec["source_table"]))
        w.writerow({c: rec.get(c, "") for c in COLS})
print(len(table), "rows written to", OUT)
