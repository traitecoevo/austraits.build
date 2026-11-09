# WATTLE sentences with ecology wording (fire, regeneration, suckering, dispersal, pollination, tolerance, lifespan), with the ABRS vetted
# values of the same taxon beside them; for vetting into WATTLE_2026/raw/ecology_vetted.csv
import csv, re
from collections import defaultdict
csv.field_size_limit(10**9)
w = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/wattle_fact_sheets.csv", encoding="utf-8-sig")))
abrs = defaultdict(list)
for r in csv.DictReader(open("data/ABRS_AtoM_2026/raw/ecology_vetted.csv")): abrs[r["taxon_name"]].append(f'{r["trait_name"]}={r["value"]}{"("+r["scoring_certainty"]+")" if r["scoring_certainty"] else ""}')
kw = re.compile(r"fire|burn|regenerat|resprout|reshoot|coppic|sucker|lignotuber|seeder|root[- ]?sucker|clonal|colon(?:y|ies)|thicket|\bants?\b|\bbirds?\b|dispers|pollinat|\binsects?\b|\bbees?\b|salt|saline|frost|drought|flood|inundat|short-lived|long-lived|lifespan|live[sd]? (?:for|to)|years old|weed|invasive|naturali[sz]ed|germinat|seedlings?|grazing|\bstock\b|toxic|poison|nitrogen|nodul", re.I)
out = csv.writer(open("ignore/jobs/wt1/eco_candidates.tsv", "w"), delimiter="\t")
out.writerow(["taxon_name", "field", "sentence", "abrs_vetted"])
n = 0
for r in w:
    for fld in ("Habitat", "Notes", "Distribution"):
        for s in re.split(r"(?<=[.;])\s+(?=[A-Z])", r[fld] or ""):
            if kw.search(s): out.writerow([r["taxon_name"], fld, s.strip(), "; ".join(abrs.get(r["taxon_name"], []))]); n += 1
print(n, "candidate sentences")
