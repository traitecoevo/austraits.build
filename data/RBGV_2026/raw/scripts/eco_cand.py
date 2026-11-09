# VicFlora (RBGV_2026) sentences with ecology wording (fire, regeneration, suckering, dispersal, pollination, tolerance, lifespan,
# germination), from "Distribution and habitat" and Notes of the vascular species-level profiles; for vetting into
# RBGV_2026/raw/ecology_vetted.csv (ignore/jobs/vf1/vet.py)
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/vicflora_profiles.csv", encoding="utf-8-sig")))
by = {r["taxon_name"]: r for r in rows}
def phylum(r):
    for _ in range(15):
        if not r: return None
        if r["taxon_rank"] == "phylum": return r["taxon_name"]
        r = by.get(r["parent"])
kw = re.compile(r"fire|burn|regenerat|resprout|reshoot|coppic|sucker|lignotuber|seeder|clonal|colon(?:y|ies|ising|izing)|thicket|\bants?\b|\bbirds?\b|dispers|pollinat|\binsects?\b|\bbees?\b|\bwasps?\b|(?:salt|frost|drought|flood|inundation|grazing|shade)[- ](?:tolera|resist|sensitiv|intolera|hardy)|tolera(?:nt|tes) (?:of )?(?:salt|frost|drought|flood|inundation|waterlogging)|short-lived|long-lived|lifespan|live[sd]? (?:for|to)|years old|germinat|seedlings?|cleistogam|self-poll|apomict|vegetativ|bulbils?|layering|(?<!wide)(?<!wide-)\bspread(?:s|ing)? (?:by|via|from|vegetativ|clonal|underground|rapidly|readily)|forming (?:dense )?(?:mats|patches|clumps|clones|stands)|mycorrh|parasit|\bhosts?\b", re.I)
out = csv.writer(open("ignore/jobs/vf1/eco_candidates.tsv", "w"), delimiter="\t")
out.writerow(["taxon_name", "rank", "field", "sentence"])
n = 0
for r in rows:
    if r["taxon_rank"] not in ("species", "subspecies", "variety", "form", "genus", "family") or phylum(r) in ("Bryophyta", "Marchantiophyta", "Anthocerotophyta"): continue
    name = re.sub(r"×\s*", "x ", r["taxon_name"]).strip()
    for fld in ("Distribution and habitat", "Notes"):
        for s in re.split(r"(?<=[.;])\s+(?=[A-Z])", r[fld] or ""):
            if kw.search(s): out.writerow([name, r["taxon_rank"], fld, s.strip()]); n += 1
print(n, "candidate sentences")
