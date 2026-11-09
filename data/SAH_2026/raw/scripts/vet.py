# SAH_2026 vetted ecology (raw/ecology_vetted.csv) from the SA Flora (Flora of South Australia online) Habitat, Notes, Biological Notes,
# Taxonomic Notes, Cultivation and Uses text, read sentence by sentence (2026-11-05; candidates from eco_cand.py, 388 sentences). Values
# follow the ABRS / NHNSW / RBGV vetted vocabulary; each row's evidence is the SA Flora sentence; hedged statements carry the hedge word as
# scoring_certainty and frequency words ("often", "sometimes") as commonness_qualifier. A genus statement is copied to the members
# (trait_scoring_method inferred_from_genus) only when it states one value that clearly applies to every member (user 2026-11-05): copied
# Calochilus "The S.Aust. species are all self-pollinated", Chiloglottis "Pollinated by sexually attracted male thynnid wasps" (not to
# C. cornuta, "Self-pollinated in the S. Aust. forms"), Corybas "Pollinated by small fungus flies", Pterostylis "pollinated by small flies
# or fungus gnats"; not copied: Acianthus "mainly by flies", Caladenia (bees or wasps by flower colour), Diuris / Glossodia "chiefly by
# native bees", Microtis / Thelymitra / Oxalis "most", Prasophyllum (by flowering season), Nicotiana "may be cleistogamous", Fumaria
# (shaded / late plants), Amphibromus "fairly common", Ulmus "Several of the European taxa sucker", Lythrum heterostyly ("in L. salicaria,
# L. junceum and other species"), Eucalyptus (defines obligate seeders only).
# Not scored: germination experiments with seed treatments (scarification, hot water), seed eaten by birds (granivory is not dispersal),
# host lists of mistletoes and dodders (the parser scores parasitism from the descriptions), plants "regenerating around planted trees",
# horticultural notes on cultivated plants, "drought resistant" (no drought tolerance trait), colonies of annuals and of shrubs that are not
# stated to be clonal ("forming thickets", "dense colonies").
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/saflora_profiles.csv", encoding="utf-8-sig")))
nm = lambda n: re.sub(r"\s+", " ", n).strip()
FIELDS = ("Habitat", "Notes", "Biological Notes", "Taxonomic Notes", "Cultivation", "Uses")
TXT, SRC = {}, {}
for r in rows:
    TXT[nm(r["taxon_name"])] = " | ".join(r[f] for f in FIELDS if r[f].strip()); SRC[nm(r["taxon_name"])] = r
SPP = [t for t, r in SRC.items() if r["taxon_rank"] not in ("genus",) and (r["Description"].strip() or r["Phenology"].strip())]
def members(g): return [t for t in SPP if t.split()[0] == g]
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
out = []
def ev(t, ph):
    s = TXT[t]; i = s.find(ph); assert i >= 0, (t, ph)
    st = max(s.rfind(". ", 0, i), s.rfind(" | ", 0, i), s.rfind(".", 0, i) if re.search(r"\.[A-Z]", s[max(0, i - 200):i]) else -1)
    st = st + 2 if st >= 0 and s[st:st + 2] in (". ", "| ") else (st + 1 if st >= 0 else 0)
    en = s.find(".", i + len(ph)); return s[st:en + 1 if en >= 0 else len(s)].strip(" |")
def add(t, tr, v, ph, cert="", q="", src=None, method=""):
    e = ev(src or t, ph)
    if src and not method: e = f"[{src} profile:] " + e
    if method: e = f"[genus notes:] " + e
    out.append(dict(zip(H, [t, tr, v, q, method, e, cert])))
def group(g, tr, v, ph, skip=()):
    for t in members(g):
        if t not in skip: add(t, tr, v, ph, src=g, method="inferred_from_genus")
VR, CS, PFR, RS, BB, SEC, PFF = "vegetative_reproduction_ability", "clonal_spread_mechanism", "post_fire_recruitment", "resprouting_capacity", "bud_bank_location", "seedling_establishment_conditions", "post_fire_flowering"
def clonal(t, ph, mech="clonal", cert="", q="", src=None): add(t, VR, "vegetative", ph, cert, q, src); add(t, CS, mech, ph, cert, q, src)
def obligate(t, ph, mech="clonal", cert="", q=""): add(t, VR, "vegetative_obligate", ph, cert, q); add(t, CS, mech, ph, cert, q)
def colonies(t, ph, cert="", q=""): add(t, VR, "vegetative", ph, cert, q)
def no_veg(t, ph, cert=""): add(t, VR, "not_vegetative", ph, cert)
def fire_rec(t, ph, cert="", q="", dist=False):
    add(t, PFR, "post_fire_recruitment", ph, cert, q)
    add(t, SEC, "establish_post_fire establish_post_disturbance" if dist else "establish_post_fire", ph, cert, q)
def birds(t, ph, cert=""): add(t, "dispersers", "birds", ph, cert); add(t, "dispersal_syndrome", "endozoochory", ph, cert)
def ants(t, ph, cert=""): add(t, "dispersers", "ants", ph, cert); add(t, "dispersal_syndrome", "myrmecochory", ph, cert)
def selfp(t, ph, cert="", q=""): add(t, "pollination_syndrome", "self", ph, cert, q); add(t, "breeding_system", "autogamy", ph, cert, q)
def sexdec(t, ph, visitor="thynnine_wasp", cert=""):
    add(t, "pollination_syndrome", "wasp", ph, cert); add(t, "flower_visitor", visitor, ph, cert); add(t, "flower_pollinator_reward", "deceit", ph, cert)
def fire_fl(t, ph): add(t, PFF, "fire_enhanced_flowering", ph)

# ---------------------------------------------------------------- Acacia (Whibley & Symon, Acacias of South Australia, notes)
no_veg("Acacia burkittii", "Vegetative reproduction from suckers does not occur")
add("Acacia burkittii", "germination_cues", "fire rain_summer", "Germination seems stimulated by fire and by summer rain", "apparently")
add("Acacia cyclops", "dispersers", "birds ants", "The aril contains up to 40% oil and is sought by both birds and ants")
add("Acacia cyclops", "dispersers", "mammals", "In addition to the birds, seeds were also taken by a rat and a baboon", q="occasionally")
add("Acacia cyclops", "dispersal_syndrome", "endozoochory myrmecochory", "Seeds extracted from bird faeces had enhanced germination")
add("Acacia cyclops", "dispersal_appendage", "aril", "The aril contains up to 40% oil and is sought by both birds and ants")
add("Acacia cyclops", SEC, "establish_post_disturbance establish_post_fire", "The species invade disturbed areas particularly after fire")
add("Acacia glandulicarpa", VR, "vegetative", "It is probable that the species is sparingly clonal", "probably")
add("Acacia glandulicarpa", "plant_tolerance_frost", "frost_hardy", "tolerant of frost in Canberra")
fire_rec("Acacia melanoxylon", "regenerates freely from soil-stored seed after fire or when the soil is exposed or disturbed", dist=True)
add("Acacia melanoxylon", "seedbank_location", "soil_seedbank", "regenerates freely from soil-stored seed after fire")
fire_rec("Acacia pulchella", "Dense populations may grow following fires")
add("Acacia pulchella", "lifespan", "max:13", "It was believed that 13 years was near the normal maximum life span of the species", "thought to be")
add("Acacia pulchella", "germination_cues", "rain_winter", "Seedlings emerged after the first winter rains")
add("Acacia pycnantha", "pollination_syndrome", "bird", "In a more extended study of bird pollination and nectar secretion Vanstone & Paton (1988) confirmed")
add("Acacia pycnantha", "pollination_syndrome", "bee", "Bees however may be less reliable than birds due to reduced activity in cold weather", q="occasionally")
add("Acacia pycnantha", "flower_visitor", "honeyeater silvereye", "Honeyeaters and silvereyes take the nectar and brush against the flowers")
for t in ["Acacia mitchellii", "Acacia myrtifolia"]:
    add(t, "flower_visitor", "bee", "native bees foraging on A. " + t.split()[1])
add("Acacia retinodes", "pollination_syndrome", "bee", "solitary bees of the Colletidae and Halictidae are the major pollen collectors")
add("Acacia retinodes", "flower_visitor", "colletidae sweat_bee", "solitary bees of the Colletidae and Halictidae are the major pollen collectors")
add("Acacia retinodes", "flower_pollinator_reward", "pollen", "The flowers are nectar-less but insects are attracted to the flowers by colour and fragrance")
clonal("Acacia salicina", "The suckering habit can be useful for soil conservation", "root_buds")
clonal("Acacia saligna", "Due to its suckering habit plants once established may persist", "root_buds")
ants("Acacia saligna", "Ants were most active in removing seed")
add("Acacia suaveolens", "germination_cues", "fire heat", "Soil heating through fire caused flushes of seed germination")
add("Acacia suaveolens", RS, "fire_killed", "As the adults are killed by fire and do not regenerate vegetatively")
no_veg("Acacia suaveolens", "As the adults are killed by fire and do not regenerate vegetatively")
fire_rec("Acacia suaveolens", "all individuals represent plants established from seed in, often, even-aged stands after fire")
ants("Acacia suaveolens", "Of mature seed about 83% was dispersed by ants")
add("Acacia suaveolens", "seedbank_location", "soil_seedbank", "Of the seed taken by ants 62% was in the top 0-5 cm of soil")
add("Acacia suaveolens", "seedbank_longevity_class", "long_persistent", "though seeds persist well beyond this point")
add("Acacia suaveolens", "lifespan", "20-25", "Auld estimates that adult plants would disappear from the flora 20-25 years after a fire")
clonal("Acacia victoriae", "A. victoriae can regenerate from suckers", "root_buds", q="sometimes")

# ---------------------------------------------------------------- clonal spread, colonies, apomixis
add("Aerva javanica", "breeding_system", "apomixis", "the plants are assumed to be apomictic", "presumably")
clonal("Ailanthus altissima", "difficult to eradicate because of the suckers", "root_buds")
colonies("Acianthus caudatus", "Forming small to extensive colonies")
clonal("Asplenium flabellifolium", "Often forming large colonies by proliferation", "aboveground_clonal", q="often")
clonal("Caladenia latifolia", "Forming small to large colonies by vegetative increase")
colonies("Caleana major", "Forms small colonies")
colonies("Chiloglottis cornuta", "where it forms small colonies")
for t, ph, q in [("Corybas despectans", "Forms small to very extensive colonies", ""), ("Corybas diemenicus", "Forms large colonies", ""),
                 ("Corybas unguiculatus", "Forms small colonies", ""), ("Cryptostylis subulata", "Forms small colonies", ""),
                 ("Cyrtostylis reniformis", "Forming small to large, often dense colonies", ""), ("Gladiolus tristis", "Forming large colonies", ""),
                 ("Gleichenia microphylla", "It can form dense thickets", "sometimes"), ("Glycine tabacina", "usually in small colonies", "usually"),
                 ("Leporella fimbriata", "Forms small to quite extensive colonies", ""), ("Microtis atrata", "Forms small dense colonies", ""),
                 ("Microtis parviflora", "forming small to extensive colonies", ""), ("Microtis unifolia", "Forms small to very extensive colonies", ""),
                 ("Ophioglossum lusitanicum", "It often forms large colonies", "often"), ("Paracaleana minor", "Forms small colonies", ""),
                 ("Prasophyllum australe", "Forms small colonies", ""), ("Pterostylis alata", "Forms small to extensive colonies", ""),
                 ("Pterostylis cucullata", "Forms small colonies", ""), ("Pterostylis curta", "Forms small to extensive colonies", ""),
                 ("Pterostylis dolichochila", "Forms extensive colonies", ""), ("Pterostylis erythroconcha", "Forms small to extensive colonies", ""),
                 ("Pterostylis mutica", "Occurs singly or in scattered colonies", "sometimes"), ("Pterostylis nana", "Forms small colonies", ""),
                 ("Pterostylis nutans", "Forms small to rather extensive colonies", ""), ("Pterostylis pedunculata", "Forms small often dense colonies", ""),
                 ("Pterostylis robusta", "Forms small to extensive colonies", ""), ("Pterostylis tenuissima", "Forms large colonies", "")]:
    if t in SPP: colonies(t, ph, q=q)
add("Daviesia sejugata", VR, "vegetative", "This species grows in apparently clonal colonies", "apparently")
clonal("Drosera aberrans", "stoloniferous, colony forming habit", "stolon")
clonal("Drosera pygmaea", "the water-dispersed gemmae also allow the plant to form large clonal populations", "aboveground_clonal")
add("Drosera pygmaea", "breeding_system", "self-compatible", "Although self-fertile and producing seed abundantly")
add("Drosera glanduligera", "breeding_system", "self-compatible", "Although the plant is abundantly self-fertile")
obligate("Hakea aenigma", "completely dependent for reproduction upon its capacity to sucker", "root_buds")
add("Microtis orbicularis", "breeding_system", "apomixis", "S.Aust. forms are mainly apomictic", q="mostly")
add("Paracaleana minor", "breeding_system", "apomixis", "The South Australian specimens are largely apomictic", q="mostly")
add("Prasophyllum fitzgeraldii", "breeding_system", "apomixis", "the smaller form is restricted to drier often alkaline soils and appears to be apomictic", "apparently", "sometimes")
clonal("Passiflora caerulea", "known to sucker", "root_buds")
if "Rosa gallica" in SPP: add("Rosa gallica", VR, "vegetative", "forming sparsely clonal almost thornless shrubs")
clonal("Sparaxis bulbifera", "the abundant bulbils allow the formation of dense pure stands", "aboveground_clonal")
add("Tradescantia fluminensis", VR, "vegetative", "Most plants in Australia are from vegetative material", q="mostly")
clonal("Ulmus minor", "most of the wild collections of U. minor in S.A. come from suckering plants", "root_buds")
add("Ulmus parvifolia", "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", "This species does not sucker unless the roots are disturbed", src="Ulmus")
add("Ulmus parvifolia", "dispersers", "wind", "A single tree produces an abundance of wind-dispersed fruit"); add("Ulmus parvifolia", "dispersal_syndrome", "anemochory", "A single tree produces an abundance of wind-dispersed fruit")
add("Oxalis pes-caprae", "dispersers", "water_currents", "The latter play an important part in dispersal"); add("Oxalis pes-caprae", "dispersal_syndrome", "hydrochory", "The latter play an important part in dispersal")

# ---------------------------------------------------------------- fire
fire_rec("Anthocercis angustifolia", "sometimes locally abundant following fire", q="sometimes")
fire_rec("Codonocarpus cotinifolius", "often common in disturbed areas especially after fires", q="often", dist=True)
add("Eucalyptus conferruminata subsp. recherche", RS, "fire_killed", "This species is killed by fire and other crown-destructive events")
fire_rec("Eucalyptus conferruminata subsp. recherche", "regenerates prolifically from seed following fire")
add("Grammosolen dixonii", RS, "resprouts", "Mechanical damage or burning may stimulate production of suckers from the stem base", "possibly")
add("Grammosolen dixonii", BB, "basal_buds", "Mechanical damage or burning may stimulate production of suckers from the stem base", "possibly")
add("Grammosolen dixonii", "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", "Mechanical damage or burning may stimulate production of suckers", "possibly")
add("Grammosolen truncatus", RS, "resprouts", "Regenerates from the stem base following fire"); add("Grammosolen truncatus", BB, "basal_buds", "Regenerates from the stem base following fire")
fire_rec("Hovea trisperma", "The species regenerated strongly from seed following bushfires")
fire_fl("Leporella fimbriata", "flowering more prolifically after bushfires")
fire_rec("Lobelia rhombifolia", "often common after fires", q="often")
fire_fl("Microtis rara", "flowering more freely after bushfires")
add("Monotaxis luteiflora", PFR, "post_fire_recruitment", "often collected from burnt areas", q="often")
fire_fl("Prasophyllum australe", "Flowering more freely after fires")
fire_fl("Prasophyllum elatum", "Flowers more prolifically after fires")
fire_rec("Senecio hypoleucus", "locally abundant along roadsides or after fire", dist=True)
fire_rec("Senecio pterophorus", "spreading aggressively in burnt areas")
fire_rec("Senecio quadridentatus", "In disturbed situations in grasslands, shrublands, and open woodlands, especially after fire", dist=True)
add("Cassinia arcuata", SEC, "establish_post_disturbance", "Often forming dense stands as a colonist of overgrazed land and of disturbed soil", q="often")

# ---------------------------------------------------------------- pollination, breeding systems, dispersal
group("Calochilus", "pollination_syndrome", "self", "The S.Aust. species are all self-pollinated")
group("Calochilus", "breeding_system", "autogamy", "The S.Aust. species are all self-pollinated")
for tr, v in [("pollination_syndrome", "wasp"), ("flower_visitor", "thynnine_wasp"), ("flower_pollinator_reward", "deceit")]:
    group("Chiloglottis", tr, v, "Pollinated by sexually attracted male thynnid wasps", skip=("Chiloglottis cornuta",))
selfp("Chiloglottis cornuta", "Self-pollinated in the S. Aust. forms")
group("Corybas", "pollination_syndrome", "fly", "Pollinated by small fungus flies")
group("Pterostylis", "pollination_syndrome", "fly", "Flowers pollinated by small flies or fungus gnats")
selfp("Caladenia bicalliata", "A self-pollinating species")
add("Caladenia bicalliata", "breeding_system", "cleistogamy", "some (cleistogamous) forms have flowers which never open", q="sometimes")
add("Caleana major", "pollination_syndrome", "wasp", "Pollinated by sexually attracted male wasps"); add("Caleana major", "flower_pollinator_reward", "deceit", "Pollinated by sexually attracted male wasps")
sexdec("Cryptostylis subulata", "Pollination is effected only by pseudo-copulating male wasps", "ichneumon_wasp")
add("Gastrodia sesamoides", "pollination_syndrome", "bee", "Pollinated by native bees")
add("Hydrilla verticillata", "pollination_syndrome", "wind", "Wind-pollinated")
add("Lamium amplexicaule", "breeding_system", "cleistogamy", "Cleistogamous plants of this species", q="sometimes")
add("Leporella fimbriata", "pollination_syndrome", "insect", "Pollinated by sexually attracted winged male ants")
add("Leporella fimbriata", "flower_visitor", "ant", "Pollinated by sexually attracted winged male ants"); add("Leporella fimbriata", "flower_pollinator_reward", "deceit", "Pollinated by sexually attracted winged male ants")
add("Microtis unifolia", "pollination_syndrome", "wasp", "Pollinated mainly by tiny wasps", q="mostly")
add("Microtis unifolia", "pollination_syndrome", "insect", "more rarely by ants", q="rarely"); add("Microtis unifolia", "flower_visitor", "wasp ant", "Pollinated mainly by tiny wasps or more rarely by ants")
add("Microtis unifolia", "pollination_syndrome", "self", "but also self-pollinated", q="sometimes"); add("Microtis unifolia", "breeding_system", "autogamy", "but also self-pollinated", q="sometimes")
add("Microtis unifolia", "breeding_system", "apomixis", "often apomictic", q="often")
add("Plantago australis", "breeding_system", "cleistogamy", "Both species have closed cleistogamous flowers")
selfp("Thelymitra carnea", "Self-pollinated")
selfp("Thelymitra ixioides", "Self-pollinated forms occur in S.Aust.", q="sometimes")
add("Fumaria muralis subsp. muralis", "breeding_system", "cleistogamy", "late-season plants with small, pale, cleistogamous flowers", q="often")
add("Viola odorata", "breeding_system", "cleistogamy", "Cleistogamous flowers with much-reduced petals and short peduncles are common", q="often")
birds("Lycium australe", "the seeds are probably distributed by birds", "probably")
birds("Lycium ferocissimum", "Distributed by birds consuming the edible berries")

# ---------------------------------------------------------------- germination, tolerances
add("Senecio gregorii", "germination_cues", "rain", "Germination dependent on rainfall")
for t in ["Enneapogon avenaceus", "Enneapogon polyphyllus"]: add(t, "germination_cues", "rain", "regenerat")
add("Tamarix aphylla", "plant_tolerance_salt", "halophyte_moderate", "as it is very drought and salt tolerant")

seen, rows_out = set(), []
for r in out:
    k = tuple(r[h] for h in H[:5])
    if k not in seen: seen.add(k); rows_out.append(r)
miss = sorted({r["taxon_name"] for r in rows_out if r["taxon_name"] not in SPP})
assert not miss, miss
import os; os.makedirs("data/SAH_2026/raw", exist_ok=True)
w = csv.DictWriter(open("data/SAH_2026/raw/ecology_vetted.csv", "w", newline=""), fieldnames=H, lineterminator="\n"); w.writeheader(); w.writerows(rows_out)
from collections import Counter
print(len(rows_out), "rows,", len({r["taxon_name"] for r in rows_out}), "taxa"); print(Counter(r["trait_name"] for r in rows_out).most_common())
