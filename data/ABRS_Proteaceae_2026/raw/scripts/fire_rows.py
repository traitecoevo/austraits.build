# Proteaceae fire response and serotiny from Ecology / Notes / habit text (Myrtaceae conventions; user's serotiny rules 2026-10-29:
# both kinds of follicle -> "serotinous not_serotinous"; hedged statements scored with the hedge word as scoring_certainty)
# usage: fire_rows.py <dir with taxa.txt>   (or a taxa file path as the second argument)
import csv, re, sys
csv.field_size_limit(10**9)
S = sys.argv[1]
taxa = set(open(sys.argv[2] if len(sys.argv) > 2 else f"{S}/taxa.txt").read().split("\n")) - {""}
H = re.compile(r"\b(probably|possibly|apparently|perhaps|presumably|likely|may|might)\b|(\?)", re.I)
def sents(t):
    t = re.sub(r"\s+", " ", t or "").strip()
    return [s for s in re.split(r"(?<=[.;])\s+", t) if s]
rows = []
cw = ""
def add(t, tr, v, q, ev): rows.append((t, tr, v, q, "", ev, cw))
for r in csv.DictReader(open('/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/foa_profiles.csv', encoding='utf-8-sig')):
    t = r["scientific_name"]
    if t not in taxa: continue
    ss = sents(r["Ecology"]) + sents(r["Notes"]) + sents((r["Description"] or "").split(". ")[0] + ".")
    seen = set()
    for s in ss:
        sl = s.lower()
        if not re.search(r"fire|burnt|sprout|follicles", sl): continue
        hm = H.search(s)
        cw = "" if not hm else ("possibly" if (hm.group(1) or "?").lower() in ("may", "might", "?") else hm.group(1).lower())
        if re.search(r"related to|differs|similar to|response unknown|eliminated|too-frequent", sl): continue
        if "killed by fire" in sl or "fire-sensitive" in sl or "fire sensitive" in sl:
            if ("rc", "fk") not in seen: add(t, "resprouting_capacity", "fire_killed", "", s); seen.add(("rc", "fk"))
        if re.search(r"regenerat\w* (?:from|by) seed", sl):
            if "pfr" not in seen: add(t, "post_fire_recruitment", "post_fire_recruitment", "", s); add(t, "germination_cues", "fire", "", s); seen.add("pfr")
        if re.search(r"fire[- ]tolerant|fire tolerant|sprout\w* (?:from|by|after)|sprouts? after fire|regenerat\w* (?:after fire )?from (?:a |the )?(?:lignotuber|rootstock)", sl):
            if ("rc", "rs") not in seen: add(t, "resprouting_capacity", "resprouts", "", s); seen.add(("rc", "rs"))
            bb = []
            for m in re.finditer(r"lignotuber|epicormic|trunk|branches|rootstock|stems", sl):
                w = m.group(0)
                if w == "stems" and re.search(r"underground[^.;]{0,30}stems|stems[^.;]{0,10}underground", sl): continue
                b = {"lignotuber": "basal_stem_buds", "rootstock": "root_crown"}.get(w, "epicormic_buds")
                if b not in bb: bb.append(b)
            if bb and "bb" not in seen: add(t, "bud_bank_location", " ".join(bb), "", s); seen.add("bb")
        if "follicles" in sl and "fire" in sl or "until burnt" in sl or "when burnt" in sl:
            if "ser" in seen: continue
            if re.search(r"\bsome\b[^.;]{0,40}open", sl) and re.search(r"closed until burnt", sl): v, q = "serotinous not_serotinous", ""
            elif re.search(r"remain\w* closed until burnt|only (?:with fire|when burnt)", sl): v, q = "serotiny_high", ""
            elif re.search(r"spontaneously or with fire|with or without fire|open\w* (?:without fire|spontaneously)|open after [0-9]", sl): v, q = "serotinous not_serotinous", ""
            elif re.search(r"open\w* with fire", sl):
                v = "serotinous"; m = re.search(r"\b(usually|typically|mostly|most)\b", sl); q = {"most": "mostly"}.get(m.group(1), m.group(1)) if m else ""
            else: continue
            add(t, "serotiny", v, q, s); add(t, "seedbank_location", "canopy_seedbank", "", s); seen.add("ser")
w = csv.writer(sys.stdout, lineterminator="\n"); w.writerows(rows)
