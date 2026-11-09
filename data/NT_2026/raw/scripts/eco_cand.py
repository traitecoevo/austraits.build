# FloraNT (NT_2026) sentences with ecology wording (fire, regeneration, suckering, dispersal, pollination, tolerance, lifespan,
# germination), from Habitat, Notes, Diagnostic Characters, Similar Taxa and Distribution Notes of the genus and species-level factsheets; for vetting into
# NT_2026/raw/ecology_vetted.csv
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/ntflora_profiles.csv", encoding="utf-8-sig")))
kw = re.compile(r"fire|burn|regenerat|resprout|reshoot|coppic|sucker|lignotuber|seeder|clonal|colon(?:y|ies|ising|izing)|thicket|\bants?\b|\bbirds?\b|dispers|pollinat|\binsects?\b|\bbees?\b|\bwasps?\b|(?:salt|frost|drought|flood|inundation|grazing|shade)[- ](?:tolera|resist|sensitiv|intolera|hardy)|tolera(?:nt|tes) (?:of )?(?:salt|frost|drought|flood|inundation|waterlogging)|short-lived|long-lived|lifespan|live[sd]? (?:for|to)|years old|germinat|seedlings?|cleistogam|self-poll|apomict|vegetativ|bulbils?|layering|(?<!wide)(?<!wide-)\bspread(?:s|ing)? (?:by|via|from|vegetativ|clonal|underground|rapidly|readily)|forming (?:dense )?(?:mats|patches|clumps|clones|stands)|mycorrh|parasit|\bhosts?\b", re.I)
out = csv.writer(open("ignore/jobs/nt1/eco_candidates.tsv", "w"), delimiter="\t")
out.writerow(["taxon_name", "rank", "field", "sentence"])
n = 0
for r in rows:
    name = re.sub(r"\s+", " ", r["taxon_name"]).strip()
    for fld in ("Habitat", "Notes", "Diagnostic Characters", "Similar Taxa", "Distribution Notes"):
        for s in re.split(r"(?<=[.;])\s+(?=[A-Z])|\s+\|\s+", r[fld] or ""):
            if kw.search(s): out.writerow([name, r["rank"], fld, s.strip()]); n += 1
print(n, "candidate sentences")
