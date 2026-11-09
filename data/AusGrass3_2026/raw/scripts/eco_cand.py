# AusGrass3 Notes / Habitat / Distribution-and-habitat sentences with ecology or spread wording, for vetting into
# AusGrass3_2026/raw/ecology_vetted.csv (species and infraspecific profiles; genus sentences listed separately)
import csv, re
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/AusGrass3_fact_sheets.csv", encoding="utf-8-sig")))
kw = re.compile(r"fire|burn|regenerat|resprout|sucker|stolon|rhizom|runner|rooting at|roots at|creeping|mat-forming|\bmats?\b|sward|colon(?:y|ies)|clonal|vegetative|viviparous|bulbil|tuber|corm|bulb|swollen|knotty|storage|\bseeds? (?:dispers|spread)|dispers|\bants?\b|\bbirds?\b|wind|grazing|grazed|palatab|fodder|stock|toxic|poison|weed|invasive|salt|saline|halophyt|frost|drought|flood|inundat|waterlogg|cleistogam|apomict|self-pollinat|short-lived|long-lived|ephemeral|lifespan|C4|C3", re.I)
out = csv.writer(open("ignore/jobs/ag1/eco_candidates.tsv", "w"), delimiter="\t"); out.writerow(["taxon_name", "rank", "field", "sentence"])
n = 0
for r in rows:
    for fld in ("Notes", "Habitat", "Distribution and habitat", "Comments"):
        for s in re.split(r"(?<=[.;|])\s+(?=[A-Z])|\s*\|\s*", r[fld] or ""):
            if kw.search(s): out.writerow([re.sub(r"\*+\s*$", "", r["taxon_name"]).strip(), r["taxon_rank"], fld, s.strip()]); n += 1
print(n)
