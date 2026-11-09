# NHNSW_2026 vetted ecology (raw/ecology_vetted.csv, ecology_vetted_regions.csv) from the PlantNET "Distribution and occurrence",
# Notes and Flowering text, read sentence by sentence (2026-10-31); each value with its sentence as evidence
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/plantnet_nsw_flora.csv")))
def clean(n):
    m = re.match(r"((?:x )?[A-Z][a-z-]+)(?: (x))? ([a-z][a-z-]+)", n); out = " ".join(x for x in [m.group(1), m.group(2), m.group(3)] if x)
    ir = re.search(r" (subsp\.|var\.|f\.) ([a-z][a-z-]+)", n)
    return out + (f" {ir.group(1)} {ir.group(2)}" if ir else "")
txt = {}
for r in rows:
    if r[list(r)[0]] in ("species", "infraspecific"):
        txt.setdefault(clean(r["taxon_name"]), " ".join([r["Distribution and occurrence"], r["Notes"], r["Flowering"]]))
def ev(t, ph):
    s = txt[t]; i = s.find(ph); assert i >= 0, (t, ph)
    st = max(s.rfind(". ", 0, i) + 2, 0) if s.rfind(". ", 0, i) >= 0 else 0
    en = s.find(".", i + len(ph)); return s[st:en + 1 if en >= 0 else len(s)].strip()
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
out = []
def add(t, tr, v, ph, cert="", q=""):
    out.append(dict(zip(H, [t, tr, v, q, "", ev(t, ph), cert])))
# fire
add("Acacia doratoxylon", "resprouting_capacity", "resprouts", "reshoots from the roots"); add("Acacia doratoxylon", "bud_bank_location", "bud-bearing_root", "reshoots from the roots")
for t in ["Commersonia amystia", "Commersonia breviseta"]:
    add(t, "resprouting_capacity", "fire_killed", "obligate seeder", "apparently"); add(t, "post_fire_recruitment", "post_fire_recruitment", "germinates in abundance", "apparently")
    add(t, "germination_cues", "fire", "germinates in abundance", "apparently")
# dispersal (fruit / seed eaten or carried by birds: dispersers + endozoochory; tumbleweed: wind)
for t in ["Actinidia chinensis", "Actinidia chinensis var. deliciosa"]: add(t, "dispersers", "birds", "Seeds distributed by birds"); add(t, "dispersal_syndrome", "endozoochory", "Seeds distributed by birds")
add("Billardiera mutabilis", "dispersers", "birds", "Dispersal of seeds by birds"); add("Billardiera mutabilis", "dispersal_syndrome", "endozoochory", "Dispersal of seeds by birds")
add("Nitraria billardierei", "dispersers", "birds", "distributed by emus and other birds"); add("Nitraria billardierei", "dispersal_syndrome", "endozoochory", "distributed by emus and other birds")
for t in ["Psidium cattleyanum", "Psidium cattleyanum var. cattleyanum"]: add(t, "dispersers", "birds mammals", "spread by birds and animals"); add(t, "dispersal_syndrome", "endozoochory", "spread by birds and animals")
add("Trachyandra divaricata", "dispersers", "wind", "disperses as a tumbleweed"); add("Trachyandra divaricata", "dispersal_syndrome", "anemochory", "disperses as a tumbleweed"); add("Trachyandra divaricata", "dispersal_appendage", "tumbleweed", "disperses as a tumbleweed")
add("Gladiolus gueinzii", "dispersal_appendage", "wings", "winged seeds"); add("Gladiolus gueinzii", "dispersal_syndrome", "anemochory hydrochory", "winged seeds")
add("Scirpus polystachyus", "dispersers", "birds", "introduction by birds", "apparently")
# pollination and breeding
add("Ptilotus nobilis", "pollination_syndrome", "moth", "Pollination is likely to be by night-flying moths", "likely")
add("Danhatchia australis", "pollination_syndrome", "self", "self–pollinating"); add("Danhatchia australis", "breeding_system", "autogamy", "self–pollinating")
add("Thelymitra pauciflora", "pollination_syndrome", "self", "self-pollinating"); add("Thelymitra pauciflora", "breeding_system", "autogamy", "self-pollinating")
add("Coleus australis", "breeding_system", "cleistogamy", "cleistogamous flowers")
add("Echium plantagineum", "flower_nectar_production", "nectar_produced", "source of nectar and pollen")
# clonality: suckering (unspecified suckers = clonal, root suckers = root_buds), stolons, rhizomes, vegetative spread
cl = [("Acacia difformis", "clonal", "suckers freely", ""), ("Acacia maidenii", "root_buds", "spread by suckering", "possibly"), ("Acacia saligna", "clonal", "suckers freely", ""),
      ("Ailanthus altissima", "clonal", "suckering habit", ""), ("Citrus glauca", "clonal", "often suckers", ""), ("Gleditsia triacanthos", "clonal", "freely suckers", ""),
      ("Hicksbeachia pinnatifolia", "clonal", "regenerates by suckering", ""), ("Populus alba", "clonal", "naturalised by suckering", ""), ("Searsia lancea", "clonal", "travels by sucker", ""),
      ("Brachyscome foliosa", "stolon", "stoloniferous patch", ""), ("Colocasia esculenta", "stolon", "vigorous stolons", ""), ("Crocosmia crocosmiiflora", "stolon", "spreading by stolons", ""),
      ("Pistia stratiotes", "stolon", "daughter plants on the end of stolons", ""), ("Sparganium erectum", "stolon", "long stolons", ""),
      ("Reynoutria x bohemica", "rhizome", "spreading vegetatively by its stout rhizomes", ""), ("Persicaria chinensis", "rhizome", "strongly rhizomatous", ""),
      ("Centipeda crateriformis subsp. compacta", "rhizome stolon", "rhizomatous and/or stoloniferous", ""),
      ("Arisarum vulgare", "clonal", "propagate vegetatively", ""), ("Arisarum vulgare subsp. vulgare", "clonal", "propagate vegetatively", ""),
      ("Cylindropuntia pallida", "aboveground_clonal", "spreads vegetatively by segments", ""), ("Erythrina x sykesii", "aboveground_clonal", "propagates vegetatively", ""),
      ("Nymphaea alba", "clonal", "maintaining itself vegetatively", ""), ("Salix babylonica", "clonal", "spreads vegetatively", ""),
      ("Salix glaucophylloides", "clonal", "probably vegetatively", "probably"), ("Caladenia latifolia", "clonal", "clonal clumps", "")]
for t, v, ph, cert in cl:
    add(t, "clonal_spread_mechanism", v, ph, cert); add(t, "vegetative_reproduction_ability", "vegetative", ph, cert)
add("Hakea pulvinifera", "clonal_spread_mechanism", "root_buds", "reproduces only by root suckers", "apparently")
add("Hakea pulvinifera", "vegetative_reproduction_ability", "vegetative_obligate", "reproduces only by root suckers", "apparently")
# colony-forming perennials (skill rule: colony-forming -> vegetative + clonal; not annuals, biennials or palms, whose colonies are seedlings)
col = ["Adiantum aethiopicum", "Adiantum formosum", "Arachniodes aristata", "Blechnum nudum", "Gleichenia dicarpa", "Gleichenia microphylla", "Histiopteris incisa",
       "Hypolepis glandulifera", "Hypolepis rugosula", "Lastreopsis munita", "Polystichum proliferum", "Pteridium esculentum", "Pteris umbrosa", "Pteris platyzomopsis",
       "Sticherus flabellatus", "Sticherus lobatus", "Deparia petersenii subsp. congrua", "Corybas diemenicus", "Microtis parviflora", "Thelymitra megcalyptra",
       "Thelymitra nuda", "Prasophyllum australe", "Arthrochilus prolixus", "Caleana major", "Caleana minor", "Celmisia tomentella", "Hypolepis amaurorachis"]
for t in col:
    ph = re.search(r"[^.;]*colon(?:y|ies)[^.;]*", txt[t]).group(0).strip()
    q = "rarely" if "rarely forms colonies" in ph else ("sometimes" if "sometimes forming colonies" in ph else "")
    add(t, "clonal_spread_mechanism", "clonal", ph, q=q); add(t, "vegetative_reproduction_ability", "vegetative", ph, q=q)
with open("data/NHNSW_2026/raw/ecology_vetted.csv", "w", newline="") as f:
    w = csv.DictWriter(f, H); w.writeheader(); w.writerows(out)
R = ["taxon_name", "trait_name", "value", "population_region", "evidence"]
reg = [dict(zip(R, ["Grevillea sericea", "clonal_spread_mechanism", "rhizome", "NW of the range", ev("Grevillea sericea", "rhizomatous")])),
       dict(zip(R, ["Grevillea sericea", "vegetative_reproduction_ability", "vegetative", "NW of the range", ev("Grevillea sericea", "rhizomatous")])),
       dict(zip(R, ["Ranunculus collinus", "clonal_spread_mechanism", "stolon", "around Corricudgee (Northern Tablelands)", ev("Ranunculus collinus", "stoloniferous")])),
       dict(zip(R, ["Ranunculus collinus", "vegetative_reproduction_ability", "vegetative", "around Corricudgee (Northern Tablelands)", ev("Ranunculus collinus", "stoloniferous")]))]
with open("data/NHNSW_2026/raw/ecology_vetted_regions.csv", "w", newline="") as f:
    w = csv.DictWriter(f, R); w.writeheader(); w.writerows(reg)
print(len(out), "vetted rows;", len(reg), "regional rows")
