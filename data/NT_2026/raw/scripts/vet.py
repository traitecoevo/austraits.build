# NT_2026 vetted ecology (raw/ecology_vetted.csv) from the FloraNT factsheets' Habitat, Notes, Diagnostic Characters, Similar Taxa and
# Distribution Notes, read sentence by sentence (2026-11-07; candidates from eco_cand.py, 585 sentences). Values follow the ABRS / NHNSW /
# RBGV / SAH vetted vocabulary; each row's evidence is the FloraNT sentence; hedged statements carry the hedge word as scoring_certainty and
# frequency words ("often", "most frequently") as commonness_qualifier. "Seen in recently burnt areas" is post-fire recruitment with
# post-fire establishment, as "often common after fires" in the other datasets. A genus statement is copied to the members
# (trait_scoring_method inferred_from_genus) only when it states one value that clearly applies to every member: copied Casuarina "Species
# are wind-pollinated", Macaranga "The local species are fast growing, short lived colonizers of gaps"; not copied: Terminalia (dispersal by
# species, scored on the species named), Nymphaea (pollinators reported for the genus worldwide), Pittosporum "typically dispersed by birds",
# Corymbia "a number of species produce large clonal patches", Cheilanthes "some species are apomictic", Hibiscus "frequently regenerate
# vigorously after fire" (seed or resprouting not said).
# A Similar Taxa statement about the other species is scored on that species ("Acacia salicina ... tendency to root sucker" from the A.
# ligulata factsheet; "[Acacia salicina factsheet:]" in the evidence).
# Not scored: mistletoe host lists (the parser scores parasitism from the descriptions), "vine thicket" habitats, colonies not stated to be
# clonal ("growing in colonies", "in dense colonies", "may form dense thickets"), fire-protected habitats and fire-regime susceptibility
# ("some susceptibility to a high frequency fire regime", "not very fire tolerant"), "regenerating after fire" without seed or resprouting,
# seasonal reshooting from a rootstock after the dry season (not a disturbance), timber, firewood and fire-stick uses, germination in
# cultivation, seeds "not suited to water dispersal", and statements about the forms of a taxon that contradict each other (Tephrosia
# remotiflora annual and resprouting forms).
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/ntflora_profiles.csv", encoding="utf-8")))
nm = lambda n: re.sub(r"\s+", " ", n).strip()
FIELDS = ("Habitat", "Notes", "Diagnostic Characters", "Similar Taxa", "Distribution Notes")
TXT, SRC = {}, {}
for r in rows:
    TXT[nm(r["taxon_name"])] = " | ".join(r[f] for f in FIELDS if r[f].strip()); SRC[nm(r["taxon_name"])] = r
SPP = [t for t, r in SRC.items() if r["rank"] != "genus" and any(r[f].strip() for f in ("Description", "Diagnostic Characters", "Flowering", "Fruiting"))]
def members(g): return [t for t in SPP if t.split()[0] == g]
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
out = []
# sentence ends: a full stop before a capital (with or without a space: "bats.Flower morphology"), or " | " between fields, but not after an
# abbreviation ("A. salicina", "S. alba", "e.g. T. ferdinandiana", "et al. 2001", "cf. sheath")
END = re.compile(r"(?<!\b[A-Z])(?<!\be\.g)(?<!\bet al)(?<!\bcf)(?<!\bc)(?<!\bsp)(?<!\bvar)(?<!\bsubsp)\.(?=\s*[A-Z(\u201c\"])| \| ")
def ev(t, ph):
    s = TXT[t]; i = s.find(ph); assert i >= 0, (t, ph)
    ends = [m.end() for m in END.finditer(s)]
    st = max([e for e in ends if e <= i], default=0); en = min([e for e in ends if e >= i + len(ph)], default=len(s))
    return s[st:en].strip(" |")
def add(t, tr, v, ph, cert="", q="", src=None, method=""):
    e = ev(src or t, ph)
    if src and not method: e = f"[{src} factsheet:] " + e
    if method: e = f"[genus notes:] " + e
    out.append(dict(zip(H, [t, tr, v, q, method, e, cert])))
def group(g, tr, v, ph, skip=()):
    for t in members(g):
        if t not in skip: add(t, tr, v, ph, src=g, method="inferred_from_genus")
VR, CS, PFR, RS, BB, SEC, PFF = "vegetative_reproduction_ability", "clonal_spread_mechanism", "post_fire_recruitment", "resprouting_capacity", "bud_bank_location", "seedling_establishment_conditions", "post_fire_flowering"
PS, FV, BS, DS, DI, SALT = "pollination_syndrome", "flower_visitor", "breeding_system", "dispersal_syndrome", "dispersers", "plant_tolerance_salt"
def clonal(t, ph, mech="clonal", cert="", q="", src=None): add(t, VR, "vegetative", ph, cert, q, src); add(t, CS, mech, ph, cert, q, src)
def fire_rec(t, ph, cert="", q="", dist=False):
    add(t, PFR, "post_fire_recruitment", ph, cert, q)
    add(t, SEC, "establish_post_fire establish_post_disturbance" if dist else "establish_post_fire", ph, cert, q)
def water(t, ph, cert="", by="water"): add(t, DS, "hydrochory", ph, cert); add(t, DI, by, ph, cert)
def birds(t, ph, cert=""): add(t, DI, "birds", ph, cert); add(t, DS, "endozoochory", ph, cert)

# ---------------------------------------------------------------- fire: recruitment after fire ("seen in recently burnt areas")
fire_rec("Brachyscome blackii", "often in higher numbers after fire", q="often")
fire_rec("Lobelia heterophylla", "Often seen in recently burnt vegetation", q="often")
fire_rec("Polymeria ambigua", "Mostly recorded from recently burnt vegetation", q="mostly")
fire_rec("Euphorbia boophthona", "Commonly seen after fire or soil disturbance", q="often", dist=True)
fire_rec("Monotaxis luteiflora", "Occurs in recently burnt vegetation")
fire_rec("Acacia sp. Krichauff Range", "probably the result of an individual wildfire", cert="probably")
fire_rec("Goodenia connata", "Most frequently seen in recently burnt vegetation", q="usually")
fire_rec("Codonocarpus cotinifolius", "Seen more frequently in areas that have experienced good rains following a fire")
fire_rec("Hibiscus solanifolius", "most commonly seen in areas regenerating after fire", q="usually")
fire_rec("Triumfetta micracantha", "Typically associated with recently burnt vegetation", q="usually")
fire_rec("Dendrophyllanthus erwinii", "seen mostly in recently burnt areas", q="usually")
fire_rec("Psydrax suaveolens", "where there is evidence of Mulga having burnt away fairly recently", q="often")
fire_rec("Nicotiana excelsior", "Seen most frequently in recently burnt areas", q="usually")
fire_rec("Solanum chippendalei", "Most frequently seen in recently burnt areas and disturbed roadsides", q="usually", dist=True)
fire_rec("Pimelea trichostachya", "Often found in recently burnt areas", q="often")
fire_rec("Crotalaria calycina", "Appears to be a fire weed", cert="apparently")
fire_rec("Sida sp. Wakaya Desert", "particuarly common in post-fire regrowth", q="often")
add("Jacksonia divisa", PFR, "post_fire_recruitment", "after which plants regenerate from seed")
add("Calytrix exstipulata", SEC, "establish_post_disturbance", "regenerating in groves on disturbed areas")
add("Desmodium pycnotrichum", SEC, "establish_post_disturbance", "Colonising and forming mats on disturbed")
add("Eremophila prostrata", "germination_cues", "disturbance", "Germination is possibly stimulated by environmental perturbation", cert="possibly")
group("Macaranga", SEC, "establish_post_disturbance", "short lived colonizers of gaps")

# ---------------------------------------------------------------- fire: flowering after fire
add("Flemingia parviflora", PFF, "fire_enhanced_flowering", "flowers near ground level in the late dry season and after dry season fires")
add("Haemodorum brevicaule", PFF, "fire_enhanced_flowering", "Frequently flowers after early dry season fires", q="often")
add("Eulophia bicallosa", PFF, "fire_enhanced_flowering", "Often flowers after dry season fires", q="often")

# ---------------------------------------------------------------- fire: resprouting / fire-killed
add("Terminalia fitzgeraldii", RS, "resprouts", "Sucker regrowth occurs after fire")
add("Euphorbia muelleri", RS, "resprouts", "often on new growth after fire", q="often")
add("Callitris glaucophylla", RS, "fire_killed", "This species is fire sensitive")
add("Boronia decumbens", RS, "resprouts", "Individuals are observed to tolerate annual fires")
for v, b in [("resprouts", None), ("basal_stem_buds epicormic_buds", BB)]:
    add("Carissa ovata", b or RS, v, "That species is highly fire tolerant, resprouting from basal stem buds and epicormic shoots after fire", cert="likely")
add("Blumea integrifolia", RS, "resprouts", "the facility to reshoot from the base of the plant after dry season fires")
add("Blumea integrifolia", BB, "basal_buds", "the facility to reshoot from the base of the plant after dry season fires")
add("Terminalia ferdinandiana", RS, "resprouts", "frequently exists as low regrowth less than 1 m tall", q="often")
add("Acacia setulifera", RS, "fire_killed", "Thought to be an obligate-seeder", cert="thought to be")
add("Acacia sp. El Sharana", RS, "resprouts", "resprouting from base following fire"); add("Acacia sp. El Sharana", BB, "basal_buds", "resprouting from base following fire")
add("Tephrosia oblongata", RS, "resprouts", "appears to be resprouting perennial", cert="apparently")
add("Helicteres isora", RS, "resprouts", "often a lower multi-stemmed shrub where it has been burned")
add("Calytrix megaphylla", RS, "resprouts", "Plants on laterite near Bloomfield Springs were 'resprouters'", q="sometimes")
add("Grevillea angulata", RS, "fire_killed", "Habit single-stemmed at base (obligate seeder)")
add("Grevillea aurea", RS, "fire_killed", "habit single-stemmed at base (obligate seeder)")
add("Grevillea longicuspis", RS, "resprouts", "Resprouting shrub to 1 m")
add("Grevillea prasina", RS, "resprouts", "Resprouts after fire")
add("Grevillea prasina", "storage_organ", "lignotuber", "lignotuberous resprouter"); add("Grevillea prasina", BB, "basal_stem_buds", "lignotuberous resprouter")
add("Boronia filicifolia", RS, "fire_killed", "A short-lived, perennial obligate-seeding subshrub")
add("Boronia lanuginosa", RS, "resprouts fire_killed", "low multi-stemmed resprouting plants with pale flowers occur on laterite substrates with more hairy, darker-flowered obligate seeding plants")
add("Eucalyptus miniata", "storage_organ", "lignotuber", "when suppressed as lignotuberous seedlings")

# ---------------------------------------------------------------- vegetative reproduction
clonal("Colocasia esculenta", "forms extensive colonies by means of stolons", "stolon")
add("Goodenia heterochila", VR, "vegetative", "disturbance encourages vegetative spread")
add("Goodenia lunata", VR, "vegetative", "disturbance encourages vegetative spread")
clonal("Hoya australis subsp. oramicola", "propagates vegetatively by producing roots from nodes", "adventitious_roots")
add("Ceratophyllum demersum", VR, "vegetative", "reproduction is primarily by vegetative fragmentation", q="mostly")
add("Dioscorea bulbifera", VR, "vegetative", "Bulbils present")
clonal("Acacia salicina", "tendency to root sucker", "root_buds", src="Acacia ligulata")
add("Acacia ligulata", VR, "not_vegetative", "non-suckering habit", src="Acacia salicina")
clonal("Acacia murrayana", "the clonal habit"); clonal("Acacia validinervia", "the clonal habit")
clonal("Erythrophleum chlorostachys", "The species is a prolific root suckerer", "root_buds")
add("Androcalva loxophylla", RS, "resprouts", "Regenerates strongly from root suckers after fire or other disturbance")
add("Androcalva loxophylla", "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", "Regenerates strongly from root suckers after fire or other disturbance")
add("Androcalva loxophylla", BB, "bud-bearing_root", "Regenerates strongly from root suckers after fire or other disturbance")
clonal("Androcalva loxophylla", "Regenerates strongly from root suckers after fire or other disturbance", "root_buds")
clonal("Malvastrum americanum", "Can produce root suckers", "root_buds")
clonal("Eucalyptus tetrodonta", "One of only two species in the genus which reproduces by root suckers", "root_buds")
add("Stenocarpus acacioides", VR, "vegetative", "Typically reproducing by suckers", q="usually")
add("Salvinia molesta", VR, "vegetative_obligate", "spread of plants is by vegetative fragmentation")
clonal("Eremophila longifolia", "clonal growth")

# ---------------------------------------------------------------- pollination and breeding system
add("Sesuvium portulacastrum", FV, "bee moth", "Small pollen collecting bees (Trigona hockingsi) visit flowers of S. portulacastrum in Qld, day-flying moths visit flowers in the NT")
add("Sesuvium portulacastrum", PS, "moth", "day-flying moths visit flowers in the NT and are likely pollinators", cert="likely")
add("Suaeda arbusculoides", PS, "insect", "The small white flowers are insect pollinated")
add("Vincetoxicum carnosum", PS, "insect self", "The small green-yellow flowers appear suited to insect or self-pollination", cert="apparently")
add("Nypa fruticans", PS, "fly", "pollination appears to occur via drosophilial flies", cert="apparently")
group("Casuarina", PS, "wind", "Species are wind-pollinated")
add("Lumnitzera littorea", FV, "bird", "observe birds visiting the bright red flowers to obtain nectar and pollen")
add("Lumnitzera littorea", "flower_pollinator_reward", "nectar pollen", "observe birds visiting the bright red flowers to obtain nectar and pollen")
add("Lumnitzera racemosa", PS, "insect", "nectariferous flowers are suited to insect pollination")
add("Lumnitzera racemosa", FV, "bee", "the flowers are a good source of nectar for wild bees"); add("Lumnitzera racemosa", "flower_pollinator_reward", "nectar", "the flowers are a good source of nectar for wild bees")
add("Murdannia cryptantha", BS, "cleistogamy", "the cleistogamous flowers which are sessile")
add("Bergia occultipetala", BS, "cleistogamy", "The flowers are almost certainly cleistogamous", cert="almost certainly")
add("Barringtonia acutangula subsp. acutangula", PS, "bat", "They are probably bat-pollinated", cert="probably")
add("Lysiana maritima", PS, "bird", "display several factors related to bird pollination")
add("Lysiana maritima", FV, "honeyeater", "observed white-throated honey-eaters (Melithreptus albogularis) as being principal visitors and pollinators")
add("Pemphis acidula", FV, "bee", "Native bees visit and appear to pollinate flowers"); add("Pemphis acidula", PS, "bee", "Native bees visit and appear to pollinate flowers", cert="apparently")
add("Sonneratia alba", BS, "allogamy", "is a protogynous outbreeder")
add("Sonneratia alba", PS, "bat", "mainly pollinated by bats", q="mostly")
add("Sonneratia alba", PS, "moth bird", "Whilst hawk-moths have been observed visiting and pollinating flowers, birds and fruit bats are also important pollinators")
add("Sonneratia alba", FV, "moth bird bat", "Whilst hawk-moths have been observed visiting and pollinating flowers, birds and fruit bats are also important pollinators")
add("Abutilon otocarpum", BS, "cleistogamy", "Some plants also appear to be cleistogamous", cert="apparently", q="sometimes")
add("Camptostemon schultzii", PS, "insect wind", "insects and wind are suspected as pollinating agents", cert="suspected")
add("Hibiscus sturtii var. grandiflorus", BS, "cleistogamy", "flowers occasionally cleistogamous", q="occasionally")
add("Xylocarpus moluccensis", PS, "insect", "Pollinators of X. moluccensis are insects")
add("Xylocarpus moluccensis", PS, "bee", "probably native bees", cert="probably")
add("Osbornia octodonta", PS, "insect", "flowers, with silvery indumentum and exserted stamens are insect pollinated")
add("Aegialitis annulata", FV, "bee ant", "Native bees have been observed visiting flowers")
add("Aegialitis annulata", PS, "bee", "appear to be major pollinators of this species", cert="apparently")
add("Ardisia elliptica", BS, "self-compatible autogamy", "is known to be self-compatible and highly autogamous")
add("Grevillea versicolor", FV, "bird", "Nectar feeding birds use the broad leaves as landing platforms while feeding from the flowers")
add("Grevillea versicolor", "flower_pollinator_reward", "nectar", "Nectar feeding birds use the broad leaves as landing platforms while feeding from the flowers")
add("Scyphiphora hydrophylacea", PS, "insect self", "Flowers may be insect or self-pollinated", cert="possibly")

# ---------------------------------------------------------------- dispersal
add("Vincetoxicum carnosum", DS, "anemochory hydrochory", "enhances dispersal by wind and water"); add("Vincetoxicum carnosum", DI, "wind water", "enhances dispersal by wind and water")
water("Nypa fruticans", "air cavities in the seed testa and fruit coat aid water dispersal")
water("Thespidium basiflorum", "adapted for dispersal by water")
add("Thespidium basiflorum", DS, "anemochory", "the whole plant becomes the dispersal agent, being carried away by wind or water", cert="suggests")
water("Lumnitzera littorea", "The corky, buoyant fruit of L. littorea is well adapted to water dispersal")
water("Lumnitzera racemosa", "The fibrous, buoyant fruit are suited to water dispersal")
birds("Terminalia microcarpa", "The fruit are a favourite with frugivorous birds")
for t in ["Terminalia ferdinandiana", "Terminalia latipes", "Terminalia carpentariae"]:
    if t in SPP: add(t, DS, "endozoochory", "are probably dispersed by larger birds such as emus", cert="probably", src="Terminalia"); add(t, DI, "birds", "are probably dispersed by larger birds such as emus", cert="probably", src="Terminalia")
add("Terminalia grandiflora", DS, "endozoochory", "may be dispersed by emus", cert="possibly", src="Terminalia"); add("Terminalia grandiflora", DI, "birds", "may be dispersed by emus", cert="possibly", src="Terminalia")
birds("Diospyros compacta", "they also note that birds eat the fruit")
birds("Diospyros littorea", "note that birds eat the fruit")
add("Eriocaulon tricornum", DS, "hydrochory", "The inflated petals appear to be an adaptation to allow dispersal of seed by water", cert="apparently")
water("Caesalpinia bonduc", "The fruits and seeds are buoyant and water dispersed")
water("Derris trifoliata", "The pods and seeds of D. trifoliata are adapted to water dispersal")
water("Intsia bijuga", "The seed pods are widely dispersed by ocean currents", by="water_currents")
add("Gyrocarpus americanus", DS, "anemochory", "This twirling motion aids dispersal from parent trees")
add("Gyrocarpus americanus", DS, "hydrochory", "presumably reflects effective dispersal by sea", cert="presumably")
water("Hernandia nymphaeifolia", "the presence of a spongy mesocarp allows fruit to float")
water("Barringtonia asiatica", "Fruit are extremely buoyant and can be widely dispersed in the sea", by="water_currents")
water("Pemphis acidula", "The corky margin of the seed aids buoyancy and hence water dispersal")
add("Camptostemon schultzii", DS, "hydrochory anemochory", "the woolly tomentum covering the seed aids both water and wind dispersal")
add("Camptostemon schultzii", DI, "water wind", "the woolly tomentum covering the seed aids both water and wind dispersal")
water("Xylocarpus granatum", "Seed of X. granatum is adapted to water dispersal")
water("Xylocarpus moluccensis", "The corky testa of X. moluccensis seed allows it to remain afloat")
water("Osbornia octodonta", "The indehiscent fruit of O. octodonta is suited to water dispersal")
water("Syzygium armstrongii", "The fruit float and are so dispersed by the stream")
add("Pandanus spiralis", DS, "endozoochory", "attractive to flying foxes which probably serve to disperse the fruit", cert="probably")
add("Pandanus spiralis", DI, "flying_foxes", "attractive to flying foxes which probably serve to disperse the fruit", cert="probably")
birds("Passiflora foetida", "as the fruit is relished by birds")
add("Plumbago zeylanica", DS, "epizoochory", "well adapted to dispersal on the feathers of migratory birds"); add("Plumbago zeylanica", DI, "birds", "well adapted to dispersal on the feathers of migratory birds")
birds("Ardisia elliptica", "The species is believed to be spread by fruit-eating birds", cert="believed")
water("Scyphiphora hydrophylacea", "The fruit is well adapted to water dispersal")
water("Aegiceras corniculatum", "The propagule is suited to water dispersal")

# ---------------------------------------------------------------- tolerances
add("Lotus cruentus", SALT, "halophyte_moderate", "apparently salt-tolerant", cert="apparently")
add("Swainsona unifoliolata", SALT, "halophyte_moderate", "Salt-tolerant")
add("Dysphania simulans", SALT, "halophyte_moderate", "Dysphania simulans appears to be the most salt-tolerant species", cert="apparently")
add("Cynodon dactylon", SALT, "halophyte_moderate", "Cynodon dactylon is a very hardy, salt tolerant species")

seen, rows_out = set(), []
for r in out:
    k = tuple(r[h] for h in H[:5])
    if k not in seen: seen.add(k); rows_out.append(r)
miss = sorted({r["taxon_name"] for r in rows_out if r["taxon_name"] not in SPP})
assert not miss, miss
import os; os.makedirs("data/NT_2026/raw", exist_ok=True)
w = csv.DictWriter(open("data/NT_2026/raw/ecology_vetted.csv", "w", newline=""), fieldnames=H, lineterminator="\n"); w.writeheader(); w.writerows(rows_out)
from collections import Counter
print(len(rows_out), "rows,", len({r["taxon_name"] for r in rows_out}), "taxa"); print(Counter(r["trait_name"] for r in rows_out).most_common())
