# RBGV_2026 vetted ecology (raw/ecology_vetted.csv) from the VicFlora "Distribution and habitat" and Notes text, read sentence by
# sentence (2026-11-05; candidates from eco_cand.py, 651 sentences). Values follow the ABRS / NHNSW / WATTLE vetted vocabulary; each
# row's evidence is the VicFlora sentence; hedged statements carry the hedge word as scoring_certainty and frequency words ("often",
# "sometimes") as commonness_qualifier. A statement in one profile about another taxon is scored on that taxon, its evidence prefixed
# "[<taxon> profile:]"; a genus / family statement about all its members is copied to them with trait_scoring_method inferred_from_genus /
# inferred_from_family (statements about "most", "many" or "several" species are not copied).
# (user 2026-11-05: a group statement is copied only when it holds for every member - not when it is hedged ("usually open only on the
# death of the branch"), conditional ("plants grown in shaded sites"), about some species ("several have become naturalised"), or
# contradicted by a member's own text or the same note ("Some species apparently producing seed without fertilization"). Dropped on that
# rule: Hakea serotiny / canopy seedbank, Prasophyllum insect pollination / nectar, Salix vegetative, Fumaria cleistogamy, Cotoneaster
# bird dispersal, Gyrostemonaceae lifespan and fire germination (one sentence opening "Mostly short-lived"); nor the Taraxacum "all introduced taxa investigated to date" apomixis.)
# Conventions (as in the earlier vetting): "common / abundant after fire" -> post_fire_recruitment; colonising disturbed ground ->
# seedling_establishment_conditions establish_post_disturbance; "forms colonies" counts as vegetative only for perennial herbs, ferns,
# orchids and sedges; flowering enhanced by fire -> post_fire_flowering fire_enhanced_flowering, "rarely flowering without fire" ->
# fire_dependent_flowering; bird-dispersed -> dispersers birds + endozoochory; self-pollinating -> pollination_syndrome self + breeding_system
# autogamy.
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/vicflora_profiles.csv", encoding="utf-8-sig")))
by = {r["taxon_name"]: r for r in rows}
def phylum(r):
    for _ in range(15):
        if not r: return None
        if r["taxon_rank"] == "phylum": return r["taxon_name"]
        r = by.get(r["parent"])
nm = lambda n: re.sub(r"\s+", " ", re.sub(r"×\s*", "x ", n)).strip()
TXT, SRC = {}, {}
for r in rows:
    if r["taxon_rank"] in ("species", "subspecies", "variety", "form", "genus", "family") and phylum(r) not in ("Bryophyta", "Marchantiophyta", "Anthocerotophyta"):
        TXT[nm(r["taxon_name"])] = " ".join([r["Distribution and habitat"], r["Notes"]]); SRC[nm(r["taxon_name"])] = r
SPP = [t for t, r in SRC.items() if r["taxon_rank"] in ("species", "subspecies", "variety", "form") and r["Description"]]
def members(g, rank="genus"):
    return [t for t in SPP if (t.split()[0] == g if rank == "genus" else SRC[t]["family"] == g)]
H = ["taxon_name", "trait_name", "value", "commonness_qualifier", "trait_scoring_method", "evidence", "scoring_certainty"]
out = []
def ev(t, ph):
    s = TXT[t]; i = s.find(ph); assert i >= 0, (t, ph)
    st = max(s.rfind(". ", 0, i), s.rfind(" | ", 0, i)); st = st + 2 if st >= 0 else 0
    en = s.find(".", i + len(ph)); return s[st:en + 1 if en >= 0 else len(s)].strip(" |")
def add(t, tr, v, ph, cert="", q="", src=None, method=""):
    e = ev(src or t, ph)
    if src and not method: e = f"[{src} profile:] " + e
    if method: e = f"[{SRC[src]['taxon_rank']} Notes:] " + e
    out.append(dict(zip(H, [t, tr, v, q, method, e, cert])))
def group(g, tr, v, ph, rank="genus", cert="", q="", skip=()):
    for t in members(g, rank):
        if t not in skip: add(t, tr, v, ph, cert, q, src=g, method="inferred_from_" + rank)
VR, CS, PFR, RS, BB, SEC, PFF = "vegetative_reproduction_ability", "clonal_spread_mechanism", "post_fire_recruitment", "resprouting_capacity", "bud_bank_location", "seedling_establishment_conditions", "post_fire_flowering"
def clonal(t, ph, mech="clonal", cert="", q="", src=None): add(t, VR, "vegetative", ph, cert, q, src); add(t, CS, mech, ph, cert, q, src)
def obligate(t, ph, mech="clonal", cert="", q="", src=None): add(t, VR, "vegetative_obligate", ph, cert, q, src); add(t, CS, mech, ph, cert, q, src)
def colonies(t, ph, cert="", q=""): add(t, VR, "vegetative", ph, cert, q)
def no_veg(t, ph, cert="", src=None): add(t, VR, "not_vegetative", ph, cert, src=src)
def resprout(t, ph, bud="", cert="", q=""):
    add(t, RS, "resprouts", ph, cert, q)
    if bud: add(t, BB, bud, ph, cert, q)
def fire_rec(t, ph, cert="", q="", est=True, dist=False):
    add(t, PFR, "post_fire_recruitment", ph, cert, q)
    if est: add(t, SEC, "establish_post_fire establish_post_disturbance" if dist else "establish_post_fire", ph, cert, q)
def colonise(t, ph, cert="", q="", fire=False): add(t, SEC, "establish_post_disturbance establish_post_fire" if fire else "establish_post_disturbance", ph, cert, q)
def birds(t, ph, cert="", extra="", src=None): add(t, "dispersers", ("birds " + extra).strip(), ph, cert, src=src); add(t, "dispersal_syndrome", "endozoochory", ph, cert, src=src)
def selfp(t, ph, cert="", q="", src=None, method=""):
    if method: group(t, "pollination_syndrome", "self", ph); group(t, "breeding_system", "autogamy", ph); return
    add(t, "pollination_syndrome", "self", ph, cert, q, src); add(t, "breeding_system", "autogamy", ph, cert, q, src)
def fire_fl(t, ph, v="fire_enhanced_flowering", cert="", q=""): add(t, PFF, v, ph, cert, q)

# ---------------------------------------------------------------- clonal spread and vegetative reproduction
add("Abutilon pictum", VR, "vegetative_obligate", "apparently sterile, spreading vigorously via root suckers", "apparently"); add("Abutilon pictum", CS, "root_buds", "spreading vigorously via root suckers")
obligate("Acacia daviesii", "only known to reproduce vegetatively, forming dense stands")
clonal("Acacia dealbata subsp. dealbata", "clonal populations of subsp. dealbata developing from root-suckers", "root_buds", q="sometimes")
clonal("Acacia hakeoides", "produces suckers if its roots are disturbed", "root_buds"); add("Acacia hakeoides", "resprouting_capacity_non_fire_disturbance", "resprouts_non_fire_disturbance", "produces suckers if its roots are disturbed")
obligate("Acacia infecunda", "apparently sterile, known only to reproduce asexually via root suckers", "root_buds", "apparently")
clonal("Acacia nanopravissima", "spreads by suckering", "root_buds")
clonal("Acacia obtusifolia", "appear to propagate principally by suckering", "root_buds", "apparently", "sometimes")
clonal("Acacia rostellifera", "known to spread via suckers", "root_buds")
clonal("Acacia sporadica", "often root-suckering growth-habit", "root_buds", q="often")
clonal("Acacia tabula", "spreads by suckering", "root_buds")
clonal("Acacia uncifolia", "by its suckering habit", "root_buds")
add("Agave americana", VR, "vegetative", "often produce bulbils instead of setting seed", q="often"); add("Agave americana", CS, "viviparous", "often produce bulbils instead of setting seed", q="often")
clonal("Albuca bracteata", "spread rapidly via asexually produced bulbils", "bulb")
clonal("Allium vineale", "freely produced bulbils", "viviparous")
clonal("Allocasuarina diminuta subsp. diminuta", "reproducing via seed and suckers", "root_buds")
clonal("Aloe arborescens", "Plants spread vegetatively")
colonies("Amphibromus fluitans", "Sometimes forming loose turf-like colonies", q="sometimes")
clonal("Arrhenatherum elatius var. bulbosum", "These swellings separate readily on disturbance and are able to regenerate anew")
clonal("Arundo donax", "The primary mode of reproduction reported over majority of world distribution is vegetative")
clonal("Asplenium flabellifolium", "rapidly colonize suitable sites by vegetative reproduction")
if "Asplenium x lucrosum" in SPP: obligate("Asplenium x lucrosum", "is perpetuated by bulbils", "aboveground_clonal", src="Asplenium gracillimum")
colonies("Astelia australiana", "Plants form large colonies")
colonies("Adiantum diaphanum", "It forms small colonies")
for t in ["Astrotricha asperifolia subsp. 1", "Astrotricha asperifolia subsp. 2", "Astrotricha linearis subsp. 1", "Astrotricha linearis subsp. 2", "Astrotricha sp. 1 subsp. 1",
          "Astrotricha sp. 1 subsp. 2", "Astrotricha sp. 4", "Astrotricha sp. 5"]: no_veg(t, "not known to root-sucker")
no_veg("Astrotricha parvifolia subsp. 2", "not known to root-sucker")
no_veg("Astrotricha sp. 2", "Regenerates apparently only from seed, not known to root-sucker", "apparently")
clonal("Astrotricha ledifolia", "Regenerates from seed and/or root-suckers", "root_buds")
colonies("Blechnum neohollandicum", "frequently forming extensive colonies", q="often")
clonal("Blechnum parrisiae", "spreads rapidly by runners", "stolon")
colonies("Blechnum penna-marina subsp. alpina", "forming small colonies")
add("Brachyscome ciliaris", "breeding_system", "apomixis", "mostly apomictic entities", q="mostly")
for t in ["Bryophyllum delagoense", "Bryophyllum x houghtonii"]:
    clonal(t, "reproducing vegetatively from plantlets produced on leaf margins", "aboveground_clonal", src=None if t == "Bryophyllum delagoense" else "Bryophyllum delagoense")
clonal("Caladenia latifolia", "often forms dense colonies by vegetative reproduction", q="often")
clonal("Casuarina glauca", "populations establishing via root suckers", "root_buds")
clonal("Cheilanthes austrotenuifolia", "spreads by means of a creeping rhizome", "rhizome")
for t, ph, q in [("Corybas aconitiflorus", "Colonies grow in sheltered positions", ""), ("Corybas despectans", "Colony-forming", ""), ("Corybas diemenicus", "often forming extensive colonies", "often"),
                 ("Corybas fimbriatus", "Usually forming colonies", "usually"), ("Corybas fordhamii", "This species can be solitary or grow in colonies", "sometimes")]: colonies(t, ph, q=q)
colonies("Dennstaedtia davallioides", "it can form extensive colonies", q="sometimes")
colonies("Diplaspis nivis", "often forming small colonies", q="often")
clonal("Eleocharis atricha", "It spreads by proliferating from the spikelet or producing tubers", "viviparous stem_tuber")
clonal("Gladiolus gueinzii", "capable of spreading by buoyant corms", "corm"); add("Gladiolus gueinzii", "dispersers", "water", "capable of spreading by buoyant corms"); add("Gladiolus gueinzii", "dispersal_syndrome", "hydrochory", "capable of spreading by buoyant corms")
clonal("Gleditsia triacanthos", "reproduce from seed and suckers", "root_buds")
colonies("Gleichenia microphylla", "Forming scrambling thickets")
no_veg("Grevillea chrysophaea", "which does not root-sucker", src="Grevillea celata")
if "Grevillea alpina" in SPP: no_veg("Grevillea alpina", "(which does not root-sucker, has a more prominent tongue-like nectary", src="Grevillea celata")
clonal("Grevillea celata", "references in that treatment to root-suckering habit, red coloration on the perianth, and occurrence in the Buchan area refer to G. celata", "root_buds", src="Grevillea chrysophaea")
# (sterile and suckering: "apparently does not set fruit" -> vegetative_obligate; not apomixis, as no seed is set)
obligate("Grevillea infecunda", "consistently root-suckering in habit", "root_buds")
clonal("Grevillea lavandulacea subsp. lavandulacea", "tendency to root-sucker", "root_buds", q="sometimes")
clonal("Gymnocoronis spilanthoides", "parts of stems detaching and being dispersed by water flow to new sites where they can then become established by rooting at nodes", "adventitious_roots")
add("Gymnocoronis spilanthoides", "dispersers", "water_currents", "being dispersed by water flow"); add("Gymnocoronis spilanthoides", "dispersal_syndrome", "hydrochory", "being dispersed by water flow")
obligate("Hakea asperma", "only reproduce asexually via root-suckers", "root_buds")
add("Hypericum perforatum subsp. veronense", "breeding_system", "apomixis", "Most plants reproduce apomictically", q="mostly")
colonies("Hypolepis amaurorachis", "rarely forming colonies", q="rarely")
colonies("Hypolepis glandulifera", "Tall thickets can form in forest clearings", q="sometimes")
colonies("Hypolepis rugosula", "Forms dense thickets")
add("Isoetes drummondii subsp. anomala", "breeding_system", "apomixis", "the megaspores are presumed to be apomictic", "presumably")
clonal("Iva axillaris subsp. robustior", "by its extensively rhizomatous habit", "rhizome"); add("Iva axillaris subsp. robustior", "allelopathy", "allelopathic", "its tendency to chemically inhibit germination of other species")
colonies("Lagenophora montana", "Plants generally form small colonies", q="usually")
add("Lissanthe strigosa subsp. subulata", VR, "vegetative", "A low, clonal variant occurs", q="sometimes")
add("Ludwigia arcuata x Ludwigia repens", VR, "vegetative", "plants apparently spreading vegetatively", "apparently")
clonal("Lysimachia nummularia", "largely reproducing vegetatively, forming dense mats"); add("Lysimachia nummularia", "stem_growth_habit", "mat-forming", "forming dense mats")
add("Microlaena stipoides var. stipoides", VR, "vegetative", "small colonies or natural lawns", q="sometimes")
clonal("Myriophyllum aquaticum", "It spreads mainly by vegetative means with broken stem fragments rooting adventitiously", "adventitious_roots")
clonal("Nymphaea 'Colonel A.J. Welch'", "viviparous formation of robust, fully formed plantlets", "viviparous"); add("Nymphaea 'Colonel A.J. Welch'", "dispersal_syndrome", "hydrochory", "downstream dispersal accelerated by viviparous formation")
add("Nymphaea alba", VR, "vegetative", "reproducing vegetatively"); add("Nymphaea alba", "dispersers", "birds", "possibly with the assistance of water fowl", "possibly")
clonal("Olearia passerinoides subsp. glutescens", "plants extensively spreading via suckers", "root_buds", "apparently")
clonal("Olearia ramulosa var. tomentosa", "consist largely of root-suckering plants", "root_buds", q="sometimes")
add("Phylloglossum drummondii", VR, "vegetative", "occasionally forming extensive colonies", q="occasionally")
add("Poa bulbosa", VR, "vegetative", "vegetatively proliferating inflorescences", q="rarely"); add("Poa bulbosa", CS, "viviparous", "vegetatively proliferating inflorescences", q="rarely")
clonal("Polystichum proliferum", "The bulbils", "aboveground_clonal")
clonal("Populus tremula", "Plants produce many suckers, forming large, dense clonal stands", "root_buds")
clonal("Populus x canescens", "spread via suckers", "root_buds")
clonal("Prunus domestica", "plants suckering, forming dense colonies", "root_buds")
clonal("Pteridium esculentum subsp. esculentum", "even small pieces of rhizome may resume growth", "rhizome")
for t in ["Pterostylis x ingens", "Pterostylis x toveyana"]: clonal(t, "forming clonal colonies")
clonal("Robinia pseudoacacia", "vigorous suckering habit", "root_buds")
add("Salix purpurea", VR, "vegetative", "Vegetative reproduction is limited", q="rarely")
add("Salix x sepulcralis nothovar. chrysocoma", VR, "vegetative", "Reproduction is mostly vegetative", q="mostly")
add("Salix nigra", VR, "vegetative", "probably also reproducing vegetatively", "probably")
# ("Plants are thought to be of hybrid origin, reproducing vegetatively by fragmentation": the hedge is on the hybrid origin)
clonal("Salvinia molesta", "reproducing vegetatively by fragmentation", "aboveground_clonal")
colonies("Schoenus calyptratus", "sometimes forming extensive colonies", q="sometimes")
colonies("Thelymitra antennifera", "often in colonies", q="often")
colonies("Thelymitra carnea", "solitary or in small colonies", q="sometimes")
for t in ["Thelymitra basaltica", "Thelymitra exigua"]: colonies(t, "Often grows in clumps by vegetative reproduction", q="often")
colonies("Tritonia gladiolaris", "Forms colonies on roadsides")

# ---------------------------------------------------------------- fire: resprouting, recruitment, flowering
resprout("Acrotriche serrulata", "This species resprouts after burning")
resprout("Callistemon wimmerensis", "apparently capable of resprouting from a lignotuber", cert="apparently"); add("Callistemon wimmerensis", "storage_organ", "lignotuber", "resprouting from a lignotuber", "apparently")
resprout("Dampiera lanceolata var. lanceolata", "A resprouter from rootstock following fire", "basal_buds")
resprout("Hakea teretifolia subsp. hirsuta", "At least some populations of this species are lignotuberous", q="sometimes"); add("Hakea teretifolia subsp. hirsuta", "storage_organ", "lignotuber", "At least some populations of this species are lignotuberous", q="sometimes")
resprout("Isopogon ceratophyllus", "Plants may regenerate after fire from a woody lignotuber"); add("Isopogon ceratophyllus", "storage_organ", "lignotuber", "from a woody lignotuber")
add("Isopogon prostratus", "storage_organ", "lignotuber", "Older plants usually have a substantial lignotuber", q="usually")
resprout("Piptochaetium uruguense", "The plants resprouted after burning")
resprout("Pteridium esculentum subsp. esculentum", "its growth is stimulated by fire", "rhizome")
resprout("Scaevola depauperata", "mostly resprouting from rootstock", "basal_buds", q="mostly")
add("Banksia cunninghamii", "storage_organ", "lignotuber_absent", "B. cunninghamii differs from B. spinulosa in lacking a lignotuber")
add("Pomaderris lanigera", "storage_organ", "lignotuber", "semi-lignotuberous", q="sometimes")
add("Commersonia breviseta", RS, "fire_killed", "thought to be an obligate seeder", "probably"); fire_rec("Commersonia breviseta", "recruiting abundantly following fire", "probably")
# Hakea: "Lignotuberous species resprout from the base after fire but non-lignotuberous species are killed", read for each species from its
# own description (a lignotuber, or "non-lignotuberous"); not the serotiny ("usually open only on the death of the branch": hedged, and
# serotiny varies between Hakea species, user 2026-11-05)
for t in members("Hakea"):
    d = SRC[t]["Description"]
    if re.search(r"\bnon-lignotuberous|\b(?:lacking|without) a lignotuber|lignotuber absent", d, re.I): add(t, RS, "fire_killed", "non-lignotuberous species are killed", src="Hakea", method="inferred_from_genus")
    elif re.search(r"\blignotuber", d, re.I): add(t, RS, "resprouts", "Lignotuberous species resprout from the base after fire", src="Hakea", method="inferred_from_genus")
for t, ph, q in [("Austrostipa drummondii", "Often common following fire", "often"), ("Austrostipa macalpinei", "may be locally abundant following fires", "sometimes"),
                 ("Calandrinia corrigioloides", "Often abundant following fire", "often"), ("Calotis cymbacantha", "sometimes locally common following fire", "sometimes"),
                 ("Cassytha pubescens", "often abundant following bushfire", "often"), ("Codonocarpus cotinifolius", "Common after fires", ""),
                 ("Convolvulus crispifolius", "often only appearing after fire", "often"), ("Coronidium adenophorum", "sometimes locally abundant for a season or two following fire", "sometimes"),
                 ("Dampiera rosmarinifolia", "particularly prevalent following fires", ""), ("Exocarpos sparteus", "particularly following fires", "sometimes"),
                 ("Gyrostemon australasicus", "Generally common only in the first few years following fire", "usually"), ("Kennedia rubicunda", "Particularly abundant following fires", ""),
                 ("Lordhowea pilosicrista", "Usually appears after fire", "usually"), ("Muehlenbeckia diclina subsp. diclina", "Often abundant only after fire", "often"),
                 ("Orianthera nuda", "often appearing following fire", "often"), ("Panaetia tepperi", "Prevalent the season following fire", ""),
                 ("Pigea floribunda", "most abundant following fires", ""), ("Pimelea williamsonii", "several plants were observed in 1990 following wildfire in 1988", ""),
                 ("Poranthera corymbosa", "Often conspicuous following bushfires", "often"), ("Teucrium thieleanum", "may be locally common following fires", "sometimes"),
                 ("Trachymene thysanocarpa", "usually noted soon after fire", "usually"), ("Commersonia rugosa", "each species only seen following fire", ""),
                 ("Swainsona pyrophila", "usually found only after fire", "usually"), ("Hakea drupacea", "particularly through regeneration following 1983 bushfires", ""),
                 ("Melaleuca armillaris subsp. armillaris", "sometimes giving rise to seedlings, particularly after fire", "sometimes"),
                 ("Platysace heterophylla var. heterophylla", "appearing reliably only after fire", "")]: fire_rec(t, ph, q=q)
add("Poranthera leiosperma", PFR, "post_fire_recruitment", "it is likely that the species is at least locally common following fire", "likely")
for t, ph, q in [("Barbarea grayi", "sometimes locally abundant following fire or local disturbance", "sometimes"), ("Calandrinia eremaea", "particularly after fire or other disturbance", ""),
                 ("Cassinia aculeata subsp. aculeata", "often a pioneer species following fire or other disturbance", "often"),
                 ("Cassinia longifolia", "often forming near-pure stands following fire or other disturbance", "often"), ("Cassinia sifton", "Often a pioneer species following fire or other disturbance", "often"),
                 ("Coronidium elatum subsp. elatum", "More common following fire or ground disturbance", ""), ("Cynoglossum australe", "often abundant in disturbed sites or following fire", "often"),
                 ("Dryopoa dives subsp. dives", "Locally common after disturbance by fire or roadworks", ""), ("Goodia lotifolia", "Usually commoner following disturbance or bushfire", "usually"),
                 ("Irenepharsus magicus", "locally common after disturbance from fire or timber-harvesting", ""), ("Leptorhynchos tenuifolius", "Sometimes strongly colonizing following fire or soil disturbance", "sometimes"),
                 ("Lobelia dentata", "It often appears abundantly after fires or other forms of disturbance", "often"),
                 ("Lobelia rhombifolia", "often appearing abundantly after fires or following bulldozing of tracks", "often"),
                 ("Lobelia simplicicaulis", "often ephemeral after fire or other disturbance", "often"),
                 ("Muehlenbeckia diclina subsp. stenophylla", "often becoming more frequent following disturbance such as fire", "often"),
                 ("Olearia rudis", "Commonest following fire or clearing", ""), ("Ozothamnus ferrugineus", "regenerating prolifically after fire or other disturbance", ""),
                 ("Ozothamnus thyrsoideus", "often prolific after fire or other disturbance", "often"), ("Pelargonium inodorum", "generally uncommon except following fire or other disturbance", ""),
                 ("Phyllanthus gunnii", "Sometimes forming thickets after fire or soil disturbance", "sometimes"), ("Senecio hispidulus", "particularly following soil disturbance or fire", ""),
                 ("Senecio linearifolius", "a common colonizer following fire and/or soil disturbance", ""), ("Senecio minimus", "A common colonizer following fire or soil disturbance", ""),
                 ("Senecio pterophorus", "appearing following disturbance, particularly fire", ""), ("Solanum linearifolium", "often appearing following disturbance or bushfire", "often"),
                 ("Stenanthemum leucophractum", "particularly following fire or other disturbance", ""), ("Tetrarrhena juncea", "Often particularly abundant in disturbed areas, e.g. following fire or clearing", "often"),
                 ("Tetrarrhena turfosa", "often conspicuous only after disturbance (e.g. fire, mechanical clearing)", "often"),
                 ("Trachymene composita var. composita", "Frequently appearing only after fire or on newly exposed soil", "often")]: fire_rec(t, ph, q=q, dist=True)
add("Goodenia connata", PFR, "post_fire_recruitment", "Generally a short-lived fire-follower", q="usually"); add("Goodenia connata", SEC, "establish_post_fire", "Generally a short-lived fire-follower", q="usually")
for t, q in [("Lobelia rhombifolia", ""), ("Lobelia simplicicaulis", "often")]: add(t, "life_history_ephemeral_class", "disturbance_ephemeral", "A disturbance ephemeral" if not q else "often ephemeral after fire or other disturbance", q=q)
add("Stenanthemum notiale subsp. notiale", SEC, "establish_post_disturbance establish_post_fire", "perhaps appearing only following disturbance (e.g. clearing, fire)", "possibly")
add("Olearia rudis", "lifespan", "3-5", "apparently surviving for only c. 3–5 years", "apparently")
add("Calomeria amaranthoides", SEC, "establish_post_disturbance establish_post_fire", "usually a pioneer species of flood-prone river banks or wettish forests regenerating after fire", q="usually")
add("Calotis erinacea", SEC, "establish_post_fire establish_post_disturbance", "in areas of regeneration following fire or other disturbance")
add("Sicyos australis", SEC, "establish_post_disturbance establish_post_fire", "usually following disturbance (flood, canopy break, fire)", q="usually")
add("Solanum aviculare", SEC, "establish_post_disturbance establish_post_fire", "A colonizing species of disturbed or burnt sites")
add("Actinotus forsythii", "germination_cues", "fire disturbance", "requires disturbances such as fire or soil upheaval to stimulate germination and flowering")
add("Actinotus forsythii", PFR, "post_fire_recruitment", "requires disturbances such as fire or soil upheaval to stimulate germination and flowering")
add("Actinotus forsythii", "flowering_cues", "fire", "requires disturbances such as fire or soil upheaval to stimulate germination and flowering")
add("Astrotricha parvifolia subsp. 1", PFR, "post_fire_recruitment", "Capable of regenerating after fire from soil-stored seed"); add("Astrotricha parvifolia subsp. 1", "germination_cues", "fire", "Capable of regenerating after fire from soil-stored seed")
add("Astrotricha parvifolia subsp. 1", "seedbank_location", "soil_seedbank", "Capable of regenerating after fire from soil-stored seed")
add("Chenopodium erosum", "germination_cues", "fire", "requires fire to promote germination", "apparently"); add("Chenopodium erosum", PFR, "post_fire_recruitment", "It was also common post-fire")
add("Chenopodium erosum", "seedbank_longevity_class", "long_persistent", "the seed is very long-lived", "apparently")
add("Coleonema pulchellum", "seedbank_longevity_class", "long_persistent", "Seed appears to be long-lived", "apparently"); add("Coleonema pulchellum", "seedbank_location", "soil_seedbank", "seedlings have been observed many years after removal of parent plants")
add("Commersonia dasyphylla", "germination_cues", "fire", "possibly dependent on fire for germination", "possibly")
add("Commersonia prostrata", "germination_cues", "floods fire disturbance", "Germination of soil-stored seeds usually follows flood, fire or earthworks", q="usually")
add("Commersonia prostrata", "seedbank_location", "soil_seedbank", "Germination of soil-stored seeds"); add("Commersonia prostrata", PFR, "post_fire_recruitment", "Germination of soil-stored seeds usually follows flood, fire or earthworks", q="usually")
add("Radyera farragei", "germination_cues", "disturbance fire rain", "Establishment from seed appears to require some disturbance (e.g. fire) followed by good rains", "apparently")
add("Radyera farragei", SEC, "establish_post_disturbance establish_post_fire", "Establishment from seed appears to require some disturbance (e.g. fire) followed by good rains", "apparently")
add("Ceratogyne obionoides", "germination_cues", "rain fire", "sometimes locally abundant following good rains and/or fires", q="sometimes"); add("Ceratogyne obionoides", PFR, "post_fire_recruitment", "sometimes locally abundant following good rains and/or fires", q="sometimes")
add("Senecio gregorii", "germination_cues", "rain", "locally prolific following favourable rains"); add("Senecio gregorii", PFR, "post_fire_recruitment", "particularly following fires")
add("Hyalosperma demissum", "germination_cues", "rain_spring", "often occurring with other short-lived annuals following spring rains", q="often")
add("Ranunculus pumilio var. pumilio", "germination_cues", "rain_spring rain_summer", "mostly in drier areas following spring or summer rains", q="mostly")
fire_fl("Austrostipa nivicola", "flowering profusely in first seasons following fires")
fire_fl("Burnettia cuneata", "Seldom or never flowering except in the season following Summer fires", "fire_dependent_flowering")
fire_fl("Caladenia oenochila", "Flowering can be enhanced dramatically by summer fires")
fire_fl("Chrysocephalum semipapposum", "Flowering is often promoted by fire or other disturbance", q="often")
fire_fl("Chrysocephalum semipapposum subsp. asperum", "apparently often produced as a response to fire or other disturbance", cert="apparently", q="often")
fire_fl("Corunastylis arrecta", "Flowering may be enhanced by fires during the previous summer", "fire_enhanced_flowering", "possibly")
fire_fl("Diplarrena moraea", "Often shy-flowering except after bushfires")
fire_fl("Imperata cylindrica", "Flowering is infrequent (more commonly following fire)")
fire_fl("Microtis rara", "most abundant after bushfires")
fire_fl("Pheladenia deformis", "Flowering is strongly stimulated by fire the previous season")
fire_fl("Prasophyllum aff. parviflorum", "Apparently flowering is stimulated by fire", cert="apparently")
for t, ph in [("Prasophyllum appendiculatum", "Flowering is enhanced by fire"), ("Prasophyllum australe", "Flowering is observed more often after fires"),
              ("Prasophyllum barnettii", "Flowering in this species is stimulated dramatically by summer fires"), ("Prasophyllum elatum", "Flowering is observed more often after fires"),
              ("Prasophyllum spicatum", "Flowers freely after fire or similar disturbance"), ("Prasophyllum suttonii", "Particularly frequent following fires"),
              ("Prasophyllum sylvestre", "Flowering is enhanced by fire and other disturbance"), ("Thelymitra azurea", "Plants flower most abundantly the season following fires"),
              ("Thelymitra benthamiana", "Flowers most prolifically following hot summer fires or other habitat disturbance")]: fire_fl(t, ph)
fire_fl("Prasophyllum lindleyanum", "Flowering is generally enhanced by fire, but some populations flower well without fire", q="usually")
fire_fl("Prasophyllum brevilabre", "Flowering freely only after fires", "fire_dependent_flowering")
fire_fl("Pyrorchis nigricans", "rarely flowering without hot bushfires in the previous summer", "fire_dependent_flowering")
fire_fl("Rytidosperma pallidum", "rarely flowering unless recently burnt", "fire_dependent_flowering")
# (Gyrostemonaceae family Notes: "Mostly short-lived, usually surviving less than 10 years (sometimes to 20), but germinating readily following fire")

# ---------------------------------------------------------------- establishment on disturbed ground
for t, ph, q in [("Abutilon grandifolium", "occasionally colonising disturbed sites", "occasionally"), ("Argyrotegium mackayi", "sometimes colonizing bare ground", "sometimes"),
                 ("Carex hebes", "sometimes colonizing disturbed ground", "sometimes"), ("Dactyloctenium radulans", "Sometimes colonising cleared or disturbed ground", "sometimes"),
                 ("Euchiton japonicus", "often colonizing bare ground", "often"), ("Euchiton sphaericus", "often colonizing disturbed ground", "often"),
                 ("Histiopteris incisa", "readily colonizing forest track margins and ground disturbed", ""), ("Hydrocotyle laxiflora", "often colonizing bare and disturbed areas", "often"),
                 ("Isolepis montivaga", "usually colonizing bare earth", "usually"), ("Lawrencia glomerata", "sometimes colonizing disturbed or recently inundated ground", "sometimes"),
                 ("Maireana appressa", "Occasionally colonizing disturbed ground", "occasionally"), ("Maireana brevifolia", "Often colonizing disturbed sites", "often"),
                 ("Marrubium vulgare", "sometimes colonising bare, eroded ground", "sometimes"), ("Pteridium esculentum subsp. esculentum", "An aggressive colonizer of disturbed areas", ""),
                 ("Scaevola aemula", "often colonizing bare ground", "often"), ("Scaevola albida", "sometimes colonizing bare ground", "sometimes"),
                 ("Sclerolaena intricata", "Usually a colonising species", "usually"), ("Silene coronaria", "occasionally colonising disturbed land", "occasionally")]: colonise(t, ph, q=q)

# ---------------------------------------------------------------- dispersal
birds("Actinidia chinensis var. deliciosa", "Seeds are presumably spread by birds or other animals such as deer", "presumably", "mammals")
birds("Arbutus unedo", "probably from bird-dispersed seed", "probably")
birds("Berberis darwinii", "from bird-dispersed seed")
birds("Billardiera fusiformis", "escaping via bird-dispersed seed")
birds("Celtis australis", "the seeds probably spread by birds", "probably")
birds("Cornus capitata", "undoubtedly resulting in the spread of seed by birds")
birds("Euonymus europaeus", "Seeds are dispersed by birds attracted to the bright arils"); add("Euonymus europaeus", "dispersal_appendage", "aril", "attracted to the bright arils")
birds("Ficus macrophylla f. macrophylla", "birds, possums and fruit-bats have distributed seed", extra="mammals flying_foxes")
birds("Pittosporum undulatum", "dispersal of seed from cultivated sources (chiefly by Blackbirds)")
birds("Ribes uva-crispa", "probably from bird-dispersed seed", "probably")
birds("Solanum oligacanthum", "possibly the result of transport of seed by birds", "possibly")
birds("Sorbus aucuparia", "Seeds of cultivated plants are apparently spread by birds", "apparently")
add("Malus pumila", "dispersers", "animals", "animal-dispersed seed"); add("Malus pumila", "dispersal_syndrome", "endozoochory", "naturalizing from discarded cores and animal-dispersed seed")
add("Allium triquetrum", "dispersers", "ants", "attractive to ants which are regarded as important dispersal vectors"); add("Allium triquetrum", "dispersal_syndrome", "myrmecochory", "attractive to ants which are regarded as important dispersal vectors")
add("Allium triquetrum", "dispersal_appendage", "aril", "The arillate seeds are attractive to ants")
add("Eleusine indica", "dispersers", "mammals_domestic", "the seed probably spread by stock", "probably")
add("Scandix pecten-veneris", "dispersal_syndrome", "ballistic", "This structure acts as a spring dispersal mechanism for the seed")
group("Amphibolis", "dispersal_unit", "seedling", "the developing seedling remains attached to the parent plant")

# ---------------------------------------------------------------- pollination and breeding systems
add("Amelichloa caudata", "breeding_system", "cleistogamy", "The presence of cleistogenes within the leaf-sheaths")
add("Caladenia mentiens", "pollination_syndrome", "self", "Flowers are short-lived and self-pollinating"); add("Caladenia mentiens", "breeding_system", "autogamy", "Flowers are short-lived and self-pollinating")
add("Caladenia mentiens", "breeding_system", "cleistogamy", "sometimes without opening", q="sometimes")
add("Caleana minor", "breeding_system", "apomixis", "A rare apomictic form", q="rarely")
add("Ceratophyllum demersum", "pollination_syndrome", "water", "Pollination is achieved by stamens becoming detached from the plant and rising to the surface")
# (Chiloglottis: "Pollinated by sexually attracted male thynnine wasps, or self-pollinating in C. cornuta")
for tr, v in [("pollination_syndrome", "wasp"), ("flower_visitor", "thynnine_wasp"), ("flower_pollinator_reward", "deceit")]:
    group("Chiloglottis", tr, v, "Pollinated by sexually attracted male thynnine wasps", skip=("Chiloglottis cornuta",))
selfp("Chiloglottis cornuta", "self-pollinating in C. cornuta", src="Chiloglottis")
for tr, v in [("pollination_syndrome", "wasp"), ("flower_visitor", "ichneumon_wasp"), ("flower_pollinator_reward", "deceit")]:
    group("Cryptostylis", tr, v, "Pollinated by sexually attracted male ichneumon wasps")
add("Cullen discolor", "breeding_system", "cleistogamy", "Plants with cleistogamous flowers occur throughout the range", q="sometimes")
group("Cymodoceaceae", "pollination_syndrome", "water", "Pollination occurs within the water", "family")
selfp("Disa bracteata", "Flowers are self-pollinating")
add("Eupomatia laurina", "pollination_syndrome", "beetle", "Pollination is by beetles")
add("Fumaria muralis subsp. muralis", "breeding_system", "cleistogamy", "many specimens having, pale, cleistogamous flowers", q="often")
group("Glossodia", "pollination_syndrome", "bee", "Pollination by native bees")
add("Glossostigma diandrum", "breeding_system", "cleistogamy", "evidence of cleistogamous flowers", q="sometimes")
add("Hydrilla verticillata", "pollination_syndrome", "wind", "The flowers are air-pollinated")
add("Juncus homalocaulis", "breeding_system", "cleistogamy", "Some plants appear to have cleistogamous flowers", "apparently", "sometimes")
add("Lamium amplexicaule", "breeding_system", "cleistogamy", "Apparently cleistogamous plants", "apparently", "sometimes")
for tr, v in [("pollination_syndrome", "insect"), ("flower_visitor", "ant"), ("flower_pollinator_reward", "deceit")]:
    add("Leporella fimbriata", tr, v, "Pollinated by male flying ants attempting copulation with the labellum")
add("Microtis parviflora", "pollination_syndrome", "insect", "Flowers are frequently pollinated by ants", q="often"); add("Microtis parviflora", "flower_visitor", "ant", "Flowers are frequently pollinated by ants", q="often")
group("Najas", "pollination_syndrome", "water", "Pollination takes place completely submerged")
add("Ottelia ovalifolia subsp. ovalifolia", "breeding_system", "cleistogamy", "cleistogamous flowers remain submerged")
add("Pauridia glabella var. glabella", "breeding_system", "apomixis cleistogamy", "suggesting apomixy or cleistogamy", "possibly", "sometimes")
for t in ["Plantago australis"] + (["Plantago myosuros"] if "Plantago myosuros" in SPP else []):
    add(t, "breeding_system", "cleistogamy", "Both P. australis and P. myosuros have cleistogamous flowers", src=None if t == "Plantago australis" else "Plantago australis")
group("Posidoniaceae", "pollination_syndrome", "water", "Pollination occurs within the water", "family")
group("Ruppiaceae", "pollination_syndrome", "water", "Pollination occurs below the water surface", "family")
for g in ["Stylidium", "Levenhookia"]:
    for t in members(g): add(t, "pollination_syndrome", "insect", "pollinating insects' bodies" if g == "Stylidium" else "when triggered by an insect", src="Stylidiaceae", method="inferred_from_family")
# (Taraxacum: "The 2 native species in Victoria ... are self-compatible and sexual whereas all introduced taxa investigated to date have been
# triploid and apomictic")
for t in members("Taraxacum"):
    if SRC[t]["establishment_means"] == "native": add(t, "breeding_system", "self-compatible", "are self-compatible and sexual", src="Taraxacum", method="inferred_from_genus")
    # (introduced: "all introduced taxa investigated to date have been triploid and apomictic" is not copied - not every taxon was
    # investigated, user 2026-11-05)
add("Taraxacum sp. group 1", "breeding_system", "apomixis", "produce seeds without pollination", "probably")
# Thelymitra
for t in ["Thelymitra arenaria", "Thelymitra bracteata", "Thelymitra brevifolia", "Thelymitra holmesii", "Thelymitra inflata", "Thelymitra lucida", "Thelymitra mucida",
          "Thelymitra orientalis", "Thelymitra pallidiflora", "Thelymitra pauciflora", "Thelymitra peniculata"]: selfp(t, "Flowers are self-pollinating")
for t in ["Thelymitra improcera", "Thelymitra longiloba"]: selfp(t, "are self-pollinating")
selfp("Thelymitra matthewsii", "but are self-pollinating")
selfp("Thelymitra circumsepta", "almost obligately self–pollinating")
selfp("Thelymitra flexuosa", "primarily self-pollinating", q="mostly")
for t in ["Thelymitra carnea", "Thelymitra rubra"]: selfp(t, "sometimes self–pollinating", q="sometimes")
selfp("Thelymitra juncifolia", "mostly self-pollinated flowers", q="mostly"); add("Thelymitra juncifolia", "pollination_syndrome", "insect", "insect-pollinated variants occur", q="sometimes")
for t in ["Thelymitra aristata", "Thelymitra epipactoides"]: add(t, "pollination_syndrome", "insect", "are insect-pollinated")
add("Thelymitra azurea", "pollination_syndrome", "insect", "are most likely insect-pollinated", "likely")
add("Thelymitra ixioides", "pollination_syndrome", "insect", "insect-pollinated flowers")

# ---------------------------------------------------------------- tolerances, lifespan, other
add("Coprosma repens", "plant_tolerance_salt", "halophyte_moderate", "planted as a salt-tolerant hedge")
add("Hakea drupacea", "plant_tolerance_salt", "halophyte_moderate", "a hardy, salt-tolerant shrub")
for t in ["Melilotus albus", "Melilotus indicus"]: add(t, "plant_tolerance_salt", "halophyte_moderate", "moderately salt tolerant")
add("Grevillea lanigera", "plant_tolerance_salt", "salt_spray_tolerant", "appear to be tolerant of salt spray", "apparently", "sometimes")
add("Poa infirma", "plant_tolerance_salt", "salinity_tolerance_undefined", "perhaps more salt-tolerant", "possibly")
add("Thinopyrum obtusiflorum", "plant_tolerance_salt", "salinity_tolerance_undefined", "often selected for salted land")
add("Hymenophyllum cupressiforme", "desiccation_tolerance", "desiccation_tolerant", "comparatively drought resistant (curled, partly dried fronds can be rejuvenated)")
add("Todea barbara", "lifespan", "min:200", "200 year old specimens can be found")
add("Malvella leprosa", "recruitment_time", "nnnnnnnnyyyn", "the seeds of this species germinate in spring")

seen, rows_out = set(), []
for r in out:
    k = tuple(r[h] for h in H[:5])
    if k not in seen: seen.add(k); rows_out.append(r)
miss = sorted({r["taxon_name"] for r in rows_out if r["taxon_name"] not in SPP})
assert not miss, miss
w = csv.DictWriter(open("data/RBGV_2026/raw/ecology_vetted.csv", "w", newline=""), fieldnames=H, lineterminator="\n"); w.writeheader(); w.writerows(rows_out)
from collections import Counter
print(len(rows_out), "rows,", len({r["taxon_name"] for r in rows_out}), "taxa"); print(Counter(r["trait_name"] for r in rows_out).most_common())
