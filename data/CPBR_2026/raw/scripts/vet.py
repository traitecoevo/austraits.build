# CPBR_2026 vetted ecology (raw/ecology_vetted.csv, ecology_vetted_regions.csv): each value with its EUCLID sentence as evidence
import csv, re
eu = {r["scientific_name"]: r for r in csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/euclid_species_profiles.csv"))}
def sentence(text, ph):
    i = text.find(ph); assert i >= 0, ph
    ends = [m.end() for m in re.finditer(r"(?<!\b[A-Z])(?<!subsp)(?<!var)(?<!\be\.g)(?<!\bc)(?<!\bi\.e)\.(?=\s*[A-Z(]|\s*$)", text)]
    st = max([e for e in ends if e <= i], default=0); en = min([e for e in ends if e > i], default=len(text))
    return text[st:en].strip()
def ev(src, ph):
    # (some notes run on in the flowering_time field: "Flowering has been recorded in ... Corymbia ficifolia is very common in horticulture ...")
    for field in ("notes", "flowering_time", "description"):
        if ph in eu[src][field]: return sentence(eu[src][field], ph)
    raise AssertionError(ph)
H = ["taxon_name","trait_name","value","commonness_qualifier","trait_scoring_method","evidence","scoring_certainty"]
rows = [r for r in csv.DictReader(open("data/CPBR_2026/raw/ecology_vetted.csv")) if r["trait_name"] == "plant_growth_substrate"]
def add(t, tr, v, evid, q="", sm="", cert=""):
    rows.append(dict(zip(H, [t, tr, v, q, sm, evid, cert])))
# obligate seeders (non-lignotuberous; FoA rule: "obligate seeder" -> fire_killed)
# (E. urna: FoA quotes "obligate seeder", but EUCLID says only "lack of a lignotuber", which alone is not scored)
for t in ["Eucalyptus distuberosa subsp. aerata", "Eucalyptus distuberosa subsp. distuberosa"]:
    add(t, "resprouting_capacity", "fire_killed", ev(t, "seed-only means of regeneration"))
# kondininensis: "may either have or lack a lignotuber (i.e. is either a tree capable of resprouting basally, or alternatively, is a mallet)"
k = ev("Eucalyptus kondininensis", "capable of resprouting basally")
add("Eucalyptus kondininensis", "resprouting_capacity", "resprouts fire_killed", k)
add("Eucalyptus kondininensis", "bud_bank_location", "basal_stem_buds", k)
# Corymbia porrecta: rhizomes and their role in regrowth after fire; juvenile clumps (C. dichromophloia profile)
p = ev("Corymbia porrecta", "develops rhizomes") + " " + ev("Corymbia porrecta", "their role in regrowth")
add("Corymbia porrecta", "clonal_spread_mechanism", "rhizome", p + " [C. dichromophloia profile:] " + ev("Corymbia dichromophloia", "well known for being rhizomatous"))
add("Corymbia porrecta", "vegetative_reproduction_ability", "vegetative", p)
add("Corymbia porrecta", "resprouting_capacity", "resprouts", p)
add("Corymbia porrecta", "bud_bank_location", "rhizome", p)
# flood-cued seedling regeneration
add("Eucalyptus largiflorens", "germination_cues", "floods", ev("Eucalyptus largiflorens", "Seedling regeneration"))
# flower visitors ("highly attractive to ...") and nectar
for t in ["Eucalyptus densa subsp. densa", "Eucalyptus densa subsp. improcera"]:
    add(t, "flower_visitor", "honeyeater", ev(t, "attractive to honeyeaters"))
add("Corymbia ficifolia", "flower_visitor", "parrot honeyeater", ev("Corymbia ficifolia", "lorikeets and honeyeaters"))
add("Corymbia eximia", "flower_visitor", "insect bird", ev("Corymbia eximia", "flower feeding insects and birds"))
add("Eucalyptus torquata", "flower_nectar_production", "nectar_produced", ev("Eucalyptus torquata", "abundant nectar producer"))
# salt tolerance (FoA rule: salt-tolerant -> halophyte_moderate)
add("Eucalyptus litorea", "plant_tolerance_salt", "halophyte_moderate", ev("Eucalyptus litorea", "obvious salt tolerance"))
add("Eucalyptus famelica", "plant_tolerance_salt", "halophyte_moderate", ev("Eucalyptus famelica", "appears to tolerate saline"), cert="apparently")
add("Eucalyptus kondininensis", "plant_tolerance_salt", "halophyte_moderate", ev("Eucalyptus kondininensis", "salty flats") + " " + ev("Eucalyptus kondininensis", "reclamation of saline soils"))
for t in ["Eucalyptus spathulata subsp. spathulata", "Eucalyptus spathulata subsp. salina"]:
    add(t, "plant_tolerance_salt", "halophyte_moderate", "[E. alipes profile:] " + ev("Eucalyptus alipes", "salt-tolerant mallet"))
add("Eucalyptus myriadena", "plant_tolerance_salt", "halophyte_moderate", "[E. baudiniana profile:] " + ev("Eucalyptus baudiniana", "somewhat salt tolerant"), cert="apparently")
# E. rudis (user 2026-10-30): "may be anticipated along streams and in swampy areas" -> swampy as a secondary substrate, hedged
for t in ["Eucalyptus rudis subsp. rudis", "Eucalyptus rudis subsp. cratyantha"]:
    if any(r["taxon_name"] == t for r in rows): continue
    s_ = ev(t, "anticipated along streams")
    add(t, "plant_growth_substrate", "terrestrial", "[inferred: no aquatic, epiphytic, lithophytic or marine habit named] " + s_, sm="inferred")
    add(t, "plant_growth_substrate", "swampy", "[inferred: grows in swamps or marshes] " + s_, sm="inferred", cert="possibly")
with open("data/CPBR_2026/raw/ecology_vetted.csv", "w", newline="") as f:
    w = csv.DictWriter(f, H); w.writeheader(); w.writerows(rows)
# region-specific: plants at the type locality of E. sargentii; frost in cultivation
R = ["taxon_name","trait_name","value","population_region","evidence"]
reg = [r for r in csv.DictReader(open("data/CPBR_2026/raw/ecology_vetted_regions.csv")) if r["trait_name"] == "plant_growth_substrate"]
reg.append(dict(zip(R, ["Eucalyptus sargentii subsp. sargentii", "resprouting_capacity", "fire_killed", "type locality (Mears Lake)", ev("Eucalyptus sargentii subsp. sargentii", "obligate-seeder")])))
for t in ["Eucalyptus caesia subsp. caesia", "Eucalyptus caesia subsp. magna"]:
    reg.append(dict(zip(R, [t, "plant_tolerance_frost", "frost_hardy", "in cultivation", ev(t, "heavily frosted areas")])))
with open("data/CPBR_2026/raw/ecology_vetted_regions.csv", "w", newline="") as f:
    w = csv.DictWriter(f, R); w.writeheader(); w.writerows(reg)
for r in rows[-22:]: print(r["taxon_name"], "|", r["trait_name"], r["value"], r["scoring_certainty"], "|", r["evidence"][:150])
for r in reg[-3:]: print(r["taxon_name"], "|", r["trait_name"], r["value"], r["population_region"], "|", r["evidence"][:150])
