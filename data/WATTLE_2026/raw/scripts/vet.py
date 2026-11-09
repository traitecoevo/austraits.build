# WATTLE_2026 vetted ecology (raw/ecology_vetted.csv) from the WATTLE Habitat, Notes, Distribution and Description text, read
# sentence by sentence (2026-11-02; candidates from eco_cand.py). Values follow the ABRS_*_2026 vetted vocabulary; each row's evidence
# is the WATTLE sentence; hedged statements carry the hedge word as scoring_certainty; a statement in one profile about another
# species is scored on that species, its evidence prefixed "[A. x profile:]". Rows of the ABRS vetting whose evidence sentence is
# verbatim in WATTLE are kept as well (seed file written by the first step of this script).
import csv, re
csv.field_size_limit(10**9)
W = {r["taxon_name"].strip(): r for r in csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/wattle_fact_sheets.csv", encoding="utf-8-sig"))}
TXT = {t: " ".join(r[f] for f in ("Habitat", "Notes", "Distribution", "Description")) for t, r in W.items()}
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
norm = lambda s: re.sub(r"\s+", " ", (s or "").replace("–", "-").replace("--", "-")).lower()

# 1. ABRS vetted rows whose evidence is verbatim in the WATTLE text of the same taxon
out = []
for r in csv.DictReader(open("data/ABRS_AtoM_2026/raw/ecology_vetted.csv")):
    t = r["taxon_name"]
    if t in TXT and norm(re.sub(r"^\[[^\]]*\]\s*", "", r["evidence"]))[:60] in norm(TXT[t]): out.append({k: r.get(k, "") for k in H})

def ev(t, ph):
    s = TXT[t]; i = s.find(ph); assert i >= 0, (t, ph)
    st = s.rfind(". ", 0, i); st = st + 2 if st >= 0 else 0
    en = s.find(".", i + len(ph)); return s[st:en + 1 if en >= 0 else len(s)].strip()
def add(t, tr, v, ph, cert="", q="", src=None):
    e = ev(src or t, ph)
    if src: e = f"[{src.replace('Acacia ', 'A. ')} profile:] " + e
    out.append(dict(zip(H, [t, tr, v, q, "", e, cert])))
FK, PFR, RS = "resprouting_capacity", "post_fire_recruitment", "resprouting_capacity"
def killed_seed(t, ph, cert="", src=None): add(t, FK, "fire_killed", ph, cert, src=src); add(t, PFR, "post_fire_recruitment", ph, cert, src=src)
def resprout(t, ph, bud, cert="", src=None): add(t, RS, "resprouts", ph, cert, src=src); add(t, "bud_bank_location", bud, ph, cert, src=src)
def sucker(t, ph, mech="root_buds", cert="", src=None): add(t, "vegetative_reproduction_ability", "vegetative", ph, cert, src=src); add(t, "clonal_spread_mechanism", mech, ph, cert, src=src)
NF = "resprouting_capacity_non_fire_disturbance"

# fire response
killed_seed("Acacia acradenia", "killed by hot summer fires; it regenerates from seed")
add("Acacia acradenia", "seedling_establishment_conditions", "establish_post_disturbance", "may form localized colonies in areas following disturbance")
add("Acacia adoxa var. adoxa", PFR, "post_fire_recruitment", "regenerates prolifically from seed following fire")
add("Acacia adsurgens", PFR, "post_fire_recruitment", "regenerates from seed following fire or other disturbance")
add("Acacia adsurgens", "seedling_establishment_conditions", "establish_post_fire establish_post_disturbance", "regenerates from seed following fire or other disturbance")
add("Acacia ammobia", FK, "fire_killed", "fire-sensitive rare plant")
resprout("Acacia anaticeps", "regenerates from base of stem following fire", "basal_stem_buds")
killed_seed("Acacia ancistrocarpa", "Usually killed by fire but regenerates profusely from seed")
resprout("Acacia arida", "regenerates from a basal root stock following fire", "basal_buds")
sucker("Acacia arida", "appears to spread vegetatively by subterranean runners", "root_buds", "apparently")
killed_seed("Acacia orthocarpa", "normally killed by fire, regenerating from seed", src="Acacia arida")
killed_seed("Acacia bivenosa", "generally killed by fire and regenerates from seed")
add("Acacia bivenosa", RS, "resprouts", "under very cool burn conditions large plants may regenerate from root stock", q="rarely")
add("Acacia bivenosa", "bud_bank_location", "basal_buds", "under very cool burn conditions large plants may regenerate from root stock", q="rarely")
resprout("Acacia dictyophleba", "resprouts vigorously from the root stock", "basal_buds")
add("Acacia dictyophleba", PFR, "post_fire_recruitment", "regenerates from seed after fire")
resprout("Acacia effusa", "Regenerates from the base of main stem following fire", "basal_stem_buds")
add("Acacia effusa", NF, "resprouts_non_fire_disturbance", "Regenerates from the base of main stem following fire and after clearing")
resprout("Acacia exigua", "Resprouts from the base following fire", "basal_buds")
add("Acacia fecunda", PFR, "post_fire_recruitment", "they regenerate from seed following fire or other disturbance")
add("Acacia fecunda", "seedling_establishment_conditions", "establish_post_fire establish_post_disturbance", "they regenerate from seed following fire or other disturbance")
resprout("Acacia inaequilatera", "resprouts from the base and/or develops epicormic growth following fire", "basal_stem_buds epicormic_buds")
add("Acacia inaequilatera", PFR, "post_fire_recruitment", "rapidly regenerates from seed")
add("Acacia maitlandii", FK, "fire_killed", "is killed by hot fire")
resprout("Acacia maitlandii", "regenerates from a woody root-stock following cool fire", "basal_buds")
resprout("Acacia marramamba", "Can resprout from the base following fire", "basal_buds")
add("Acacia melleodora", FK, "fire_killed", "in most instances it is killed", q="usually")
resprout("Acacia melleodora", "Can resprout from the base following fire", "basal_buds")
add("Acacia monticola", RS, "resprouts", "has the capacity to resprout advantageously after mild fires")
add("Acacia mulganeura", FK, "fire_killed", "appear to be killed by fire", "apparently")
add("Acacia murrayana", RS, "resprouts", "resprouts readily after wildfires")
resprout("Acacia oligoneura", "thin stems arise from a woody rootstock after fire", "basal_buds")
resprout("Acacia pachyacra", "resprout advantageously from the base of the stem", "basal_stem_buds")
add("Acacia pachyacra", NF, "resprouts_non_fire_disturbance", "resprout advantageously from the base of the stem after mechanical disturbance")
add("Acacia pachycarpa", RS, "resprouts", "regenerates from coppice growth and from seed"); add("Acacia pachycarpa", "bud_bank_location", "basal_buds", "regenerates from coppice growth")
sucker("Acacia pachycarpa", "it also root sucker after fire or other disturbance")
resprout("Acacia pruinocarpa", "both coppices from the base and produce root suckers following damage", "basal_buds bud-bearing_root")
sucker("Acacia pruinocarpa", "produce root suckers following damage")
add("Acacia pyrifolia var. pyrifolia", PFR, "post_fire_recruitment", "regenerates well (from seed) following fire")
add("Acacia pyrifolia var. pyrifolia", "seedling_establishment_conditions", "establish_post_disturbance", "readily colonises disturbed sites")
killed_seed("Acacia rhodophloia", "Acacia rhodophloia is killed by hot fire")
add("Acacia rossei", "seedling_establishment_conditions", "establish_post_disturbance establish_post_fire", "especially in disturbed sites, e.g. road verges, regeneration following fire")
resprout("Acacia sericophylla", "ability to coppice from the base", "basal_stem_buds")
sucker("Acacia sericophylla", "to regenerate by root suckers")
add("Acacia silvestris", PFR, "post_fire_recruitment", "Regeneration of this long-lived species is almost always after fire")
add("Acacia silvestris", "seedling_establishment_conditions", "establish_post_fire", "Regeneration of this long-lived species is almost always after fire")
add("Acacia sp. Marble Bar (J.G. & M.H. Simmons 3499)", FK, "fire_killed", "seemingly has been killed by fire", "seemingly")
add("Acacia aneura", FK, "fire_killed", "are fire sensitive", src="Acacia sp. Mulga Blue Crown (D.E. Albrecht 9573)")
resprout("Acacia steedmanii subsp. borealis", "can resprout from a basal woody rootstock following fire", "basal_buds")
add("Acacia thomsonii", PFR, "post_fire_recruitment", "regenerates prolifically from seed following fires")
add("Acacia trachycarpa", FK, "fire_killed", "Plants are killed by intense fires")
add("Acacia trachycarpa", RS, "resprouts", "have the ability to resprout after cool fires")
add("Acacia trachycarpa", NF, "resprouts_non_fire_disturbance", "resprout after cool fires or mechanical clearing")
resprout("Acacia trudgeniana", "A fire-tolerant species", "epicormic_buds")
add("Acacia trudgeniana", NF, "resprouts_non_fire_disturbance", "resprout after the above ground biomass has been removed")
resprout("Acacia tumida var. kulparn", "plants resprouting rapidly from base when burnt", "basal_stem_buds")
add("Acacia tumida var. pilbarensis", FK, "fire_killed", "This variety is fire-sensitive and does not coppice following fire")
killed_seed("Acacia tumida var. tumida", "it is killed by fire (regeneration from seed)")
add("Acacia tumida var. tumida", "lifespan", "max:20", "a life span probably rarely exceeding 20 years", "probably")
add("Acacia urophylla", PFR, "post_fire_recruitment", "commonly forms dense regrowth populations following fire")
# suckering / clonality
sucker("Acacia ampliceps", "frequently spreads by root-suckering")
sucker("Acacia boormanii", "its suckering habit")
add("Acacia bromilowiana", "vegetative_reproduction_ability", "vegetative", "represent a series of clonal populations", "apparently")
add("Acacia bromilowiana", "clonal_spread_mechanism", "clonal", "represent a series of clonal populations", "apparently")
sucker("Acacia sporadica", "a low, root-suckering shrub", src="Acacia caerulescens")
sucker("Acacia carneorum", "gregarious, suckering habit")
sucker("Acacia citrinoviridis", "a propensity to root sucker")
add("Acacia citrinoviridis", NF, "resprouts_non_fire_disturbance", "good coppicing ability")
add("Acacia citrinoviridis", "lifespan", "min:40", "a long life span (over 40 years)")
sucker("Acacia daphnifolia", "commonly forms localized clonal clumps", "clonal")
sucker("Acacia amblyophylla", "its more aggressively suckering habit", src="Acacia daphnifolia")
sucker("Acacia daviesii", "reproducing almost exclusively by root suckering")
sucker("Acacia falciformis", "forming thickets by root suckers following fire or disturbance")
sucker("Acacia hakeoides", "produces suckers if its roots are distur")
sucker("Acacia harpophylla", "Because of its suckering habit")
sucker("Acacia infecunda", "A slow growing dwarf species that s", "clonal")
sucker("Acacia melanoxylon", "vigorous regrowth from root suckers")
sucker("Acacia murrayana", "It forms colonies from subsurface adventitious sprouts")
sucker("Acacia nanopravissima", "A slow growing dwarf species that spreads by suckering", "clonal")
sucker("Acacia obtusifolia", "appears to propagate principally by suckering", "root_buds", "apparently")
sucker("Acacia rostellifera", "often clonal", "clonal")
sucker("Acacia salicina", "often clonal due to suckering habit")
sucker("Acacia saligna", "often root-suckering")
sucker("Acacia stenophylla", "has a vigorous root suckering habit")
add("Acacia stenophylla", NF, "resprouts_non_fire_disturbance", "that coppices and has a vigorous root suckering habit")
sucker("Acacia tabula", "A slow-growing dwarf species that spreads by suckering", "clonal")
sucker("Acacia victoriae", "it can readily regenerate from suckers")
sucker("Vachellia valida", "Suckers freely from roots")
# establishment, tolerance, lifespan
add("Acacia cardiophylla", "seedling_establishment_conditions", "establish_post_disturbance", "the seed readily germinates in disturbed sites")
add("Acacia ampliceps", "plant_tolerance_salt", "salinity_tolerance_undefined", "great potential for use in reclamation of salt-affected areas")
add("Acacia cyclops", "plant_tolerance_salt", "salt_spray_tolerant halophyte", "It is tolerant of highly saline soils")
add("Acacia inceana subsp. conformis", "plant_tolerance_salt", "halophyte", "A highly salt-tolerant species")
add("Acacia sclerosperma subsp. sclerosperma", "plant_tolerance_salt", "salinity_tolerance_undefined", "is somewhat salt tolerant")
add("Acacia stenophylla", "plant_tolerance_salt", "halophyte", "highly salt-tolerant")
add("Acacia stenophylla", "plant_tolerance_frost", "frost_hardy", "moderately drought- and frost-tolerant")
add("Acacia baileyana", "plant_tolerance_frost", "frost_hardy", "frost resistant")
add("Acacia glaucocarpa", "plant_tolerance_frost", "frost_sensitive", "sensitive to frost")
add("Acacia microbotrya", "plant_tolerance_frost", "frost_hardy", "drought- and frost-tolerant")
add("Acacia microbotrya", "lifespan", "20-30", "a life span of about 20–30 years")
add("Acacia pycnantha", "plant_tolerance_frost", "frost_sensitive", "somewhat frost-sensitive")
add("Acacia spectabilis", "plant_tolerance_frost", "frost_hardy", "moderately frost resistant")
add("Acacia tetragonophylla", "plant_tolerance_frost", "frost_hardy", "drought- and frost-resistant")

# one row per taxon x trait x value x qualifier (the ABRS copy and a hand row may coincide)
seen, rows = set(), []
for r in out:
    k = (r["taxon_name"], r["trait_name"], r["value"], r["commonness_qualifier"])
    if k not in seen: seen.add(k); rows.append(r)
with open("data/WATTLE_2026/raw/ecology_vetted.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=H); w.writeheader(); w.writerows(rows)
missing = sorted({r["taxon_name"] for r in rows} - set(W))
print(len(rows), "rows,", len({r["taxon_name"] for r in rows}), "taxa; not WATTLE taxa:", missing)
