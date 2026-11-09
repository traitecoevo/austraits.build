# AusGrass3_2026 vetted ecology (raw/ecology_vetted.csv), 2026-11-02:
# 1. the Notes / Habitat sentences on spread, storage organs, breeding system, salt / frost tolerance and fire, read one by one
#    (candidates from eco_cand.py); values in the ABRS_*_2026 vetted vocabulary, the WATTLE / ABRS conventions (a statement about
#    another species is scored on that species with an "[x profile:]" prefix; hedges as scoring_certainty);
# 2. photosynthetic_pathway from the genus "Kranz Anatomy" field (genus profiles only): copied to every species of the genus
#    (trait_scoring_method inferred_from_genus) when the genus states a single pathway with no exception ("C3.", "C4, biochemical
#    type NADP-ME (3 species)."); "C4 or C3 ...", "C4 (... except ...)" are not copied.
import csv, re
csv.field_size_limit(10**9)
SRC = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/AusGrass3_fact_sheets.csv", encoding="utf-8-sig")))
nm = lambda n: re.sub(r"\*+\s*$", "", n).strip()
R = {nm(r["taxon_name"]): r for r in SRC}
TXT = {t: " ".join([r["Notes"], r["Habitat"], r["Distribution and habitat"], r["Comments"]]) for t, r in R.items()}
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
out = []
def ev(t, ph):
    s = TXT[t]; i = s.find(ph); assert i >= 0, (t, ph)
    st = max(s.rfind(". ", 0, i), s.rfind("| ", 0, i)); st = st + 2 if st >= 0 else 0
    en = s.find(".", i + len(ph)); return s[st:en + 1 if en >= 0 else len(s)].strip()
def add(t, tr, v, ph, cert="", q="", src=None, method=""):
    e = ev(src or t, ph)
    if src: e = f"[{src} profile:] " + e
    out.append(dict(zip(H, [t, tr, v, q, method, e, cert])))
def stolon(t, ph, q="", cert=""):
    add(t, "stem_growth_habit", "stoloniferous", ph, cert, q); add(t, "clonal_spread_mechanism", "stolon", ph, cert, q); add(t, "vegetative_reproduction_ability", "vegetative", ph, cert, q)
def rhizome(t, ph, form="", clonal=False, q="", cert=""):
    add(t, "storage_organ", "rhizome", ph, cert, q); add(t, "stem_growth_habit", "rhizomatous", ph, cert, q)
    if form: add(t, "rhizome_form", form, ph, cert, q)
    if clonal: add(t, "clonal_spread_mechanism", "rhizome", ph, cert, q); add(t, "vegetative_reproduction_ability", "vegetative", ph, cert, q)

# spread by stolons
stolon("Cynodon radiatus", "spread profusely by stolons", cert="reportedly")
stolon("Dactyloctenium aegyptium", "frequently assumes a perennial, stoloniferous habit in Australia", q="often")
stolon("Brachyachne convergens", "usually stoloniferous habit", q="usually")
add("Brachyachne prostrata", "clonal_spread_mechanism", "stem_suckers", "its prostrate habit, pseudo-stolons")
add("Brachyachne prostrata", "vegetative_reproduction_ability", "vegetative", "its prostrate habit, pseudo-stolons")
stolon("Chloris gayana", "drought- and salt-tolerant and stoloniferous")
stolon("Eragrostis stenostachya", "erect or stoloniferous habit")
stolon("Lepturus repens", "The long stolons allow the formation of large, loose, green mats")
add("Lepturus repens", "stem_growth_habit", "mat-forming", "The long stolons allow the formation of large, loose, green mats")
stolon("Paspalum conjugatum", "quickly spreading via highly productive stolons")
stolon("Sporobolus mitchellii", "smaller spikelets and stoloniferous")
stolon("Thuarea involuta", "where it forms extensive stolons")
stolon("Triodia secunda", "Distinguished by its stoloniferous running habit")
stolon("Urochloa decumbens", "the stoloniferous habit")
stolon("Urochloa mutica", "the presence of pronounced stolons")
stolon("Cynodon nlemfuensis var. nlemfuensis", "the presence of woody stolons")
add("Cynodon nlemfuensis var. nlemfuensis", "stem_growth_habit", "mat-forming", "vigorous, forming dense swards")
add("Elytrophorus spicatus", "stem_growth_habit", "mat-forming", "forming extensive mats of bright-green plants", q="sometimes")
add("Micraira tenuis", "stem_growth_habit", "mat-forming", "grows in small clumps or dense mats")
# rhizomes (storage organ, rhizomatous habit; clonal spread only for spreading / long rhizomes, as for the DELTA "elongated")
rhizome("Eragrostis infecunda", "rhizomatous and stoloniferous habit"); stolon("Eragrostis infecunda", "rhizomatous and stoloniferous habit")
rhizome("Echinochloa pyramidalis", "its perennial habit with well-developed rhizomes")
rhizome("Elytrigia repens", "its perennation by rhizomes", clonal=True)
rhizome("Eragrostis xerophila", "rootbase rhizomatous")
rhizome("Eragrostis desertorum", "bulbous or rhizomatous, woolly or hirsute base")
rhizome("Panicum antidotale", "pronounced rhizomes with hairy cataphylls")
rhizome("Panicum repens", "the pronounced scaly rhizomes")
rhizome("Phragmites karka", "growing with the rhizomes in water")
rhizome("Poa cheelii", "the nature of the rhizome and its enveloping scales")
rhizome("Poa halmaturina", "the long scaly rhizomes", form="long_creeping", clonal=True, cert="seemingly")
rhizome("Rytidosperma lepidopodum", "its spreading rhizomes", form="long_creeping", clonal=True)
rhizome("Setaria parviflora", "a perennial with a short rhizome", form="short_creeping")
rhizome("Spartina anglica", "being rhizomatous and a profuse seeder", clonal=True)
rhizome("Spartina x townsendii", "it spreads only slowly, by rhizomes and fragments", clonal=True)
add("Spartina x townsendii", "clonal_spread_mechanism", "aboveground_clonal", "it spreads only slowly, by rhizomes and fragments")
rhizome("Sporobolus latzii", "together with the short rhizome", form="short_creeping")
rhizome("Sporobolus virginicus", "being rhizomatous as opposed to stoloniferous")
rhizome("Symplectrodia lanosa", "the unusually robust, rhizomatous, clump-forming habit")
rhizome("Zoysia macrantha subsp. walshii", "rhizomes more frequent")
rhizome("x Cynochloris macivori", "from rhizomes collected at Ipswich")
add("x Cynochloris macivori", "vegetative_reproduction_ability", "vegetative_obligate", "no caryopses are formed")
add("Phragmites australis", "vegetative_reproduction_ability", "vegetative", "Dominant or common clonal plant")
add("Phragmites australis", "clonal_spread_mechanism", "clonal", "Dominant or common clonal plant")
add("Paspalum conjugatum", "vegetative_reproduction_ability", "vegetative", "a high dependence on vegetative reproduction", "possibly")
# storage organs other than rhizomes
add("Poa bulbosa", "storage_organ", "bulb", "the swollen, fleshy basal sheaths")
# dispersal
add("Spartina anglica", "dispersers", "water", "seeds widely dispersed by water"); add("Spartina anglica", "dispersal_syndrome", "hydrochory", "seeds widely dispersed by water")
add("Thuarea involuta", "dispersal_syndrome", "hydrochory", "which is able to float")
# breeding system
for t, ph, cert in [("Astrebla pectinata", "Cleistogamy is common", ""), ("Calyptochloa gracillima", "The cleistogamous spikelets", ""),
                    ("Cleistochloa sclerachne", "The cleistogamous spikelets", ""), ("Cleistochloa subjuncea", "cleistogamous spikelets", ""),
                    ("Dimorphochloa rigida", "cleistogamous spikelets", ""), ("Eragrostis barrelieri", "chasmogamous and cleistogamous florets", ""),
                    ("Eragrostis cumingii", "cleistogamous species", ""), ("Eragrostis fallax", "chasmogamous and cleistogamous spikelets", ""),
                    ("Eragrostis hirticaulis", "chasmogamous and cleistogamous florets", ""), ("Eragrostis parviflora", "chasmogamous and cleistogamous, loose florets", ""),
                    ("Eriachne axillaris", "axillary and cleistogamous inflorescences", ""), ("Thellungia advena", "cleistogamous spikelets", ""),
                    ("Pentapogon quadrifidus var. quadrifidus", "apparently cleistogamous", "apparently")]:
    add(t, "breeding_system", "cleistogamy", ph, cert)
add("Paspalum scrobiculatum", "breeding_system", "apomixis", "possibly an aggregate swarm of apomicts", "possibly")
# salt and frost tolerance, fire
add("Chloris gayana", "plant_tolerance_salt", "salinity_tolerance_undefined", "drought- and salt-tolerant")
add("Dactyloctenium radulans", "plant_tolerance_salt", "salinity_tolerance_undefined", "tolerates saline and swampy conditions")
for t in ["Eragrostis australasica", "Eragrostis dielsii", "Eragrostis falcata"]: add(t, "plant_tolerance_salt", "salinity_tolerance_undefined", "a useful saline tolerant species")
add("Periballia minuta", "plant_tolerance_salt", "salinity_tolerance_undefined", "tolerating somewhat saline conditions")
add("Sporobolus virginicus", "plant_tolerance_salt", "halophyte", "A halophytic polymorphic species")
add("Chloris truncata", "plant_tolerance_frost", "frost_sensitive", "frost tender")
add("Digitaria fumida", "post_fire_recruitment", "post_fire_recruitment", "often abundant in the summer season following fire", q="often")

# 2. genus Kranz anatomy -> species photosynthetic_pathway
gen_sp = {}
for t, r in R.items():
    if r["taxon_rank"] != "genus" and R.get(t.split()[0], {}).get("taxon_rank") == "genus": gen_sp.setdefault(t.split()[0], []).append(t)
nk = 0
for g, r in R.items():
    if r["taxon_rank"] != "genus" or not r["Kranz Anatomy"].strip(): continue
    k = r["Kranz Anatomy"].split("|")[0].strip()
    if re.search(r"\bor\b|except|\bC3\b.*\bC4\b|\bC4\b.*\bC3\b", k): continue
    v = "c4" if re.match(r"^C4\b", k) else "c3" if re.match(r"^C3\b", k) else None
    if not v: continue
    for t in gen_sp.get(g, []):
        out.append(dict(zip(H, [t, "photosynthetic_pathway", v, "", "inferred_from_genus", f"[genus description: Kranz anatomy] {k}", ""]))); nk += 1

seen, rows = set(), []
for r in out:
    key = (r["taxon_name"], r["trait_name"], r["value"], r["commonness_qualifier"])
    if key not in seen: seen.add(key); rows.append(r)
with open("data/AusGrass3_2026/raw/ecology_vetted.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=H); w.writeheader(); w.writerows(rows)
print(len(rows), "rows;", nk, "photosynthetic_pathway rows from", len({r['taxon_name'].split()[0] for r in rows if r['trait_name'] == 'photosynthetic_pathway'}), "genera")
