# NT_2026 source (user 2026-11-07: re-scrape the full factsheets): every genus, species and infraspecific factsheet of eflora.nt.gov.au
# (NT Flora online, NT Herbarium), read from the browse pages (family letters -> families -> genera -> taxon lists). The 2022 scrape
# (split_sentences_NT_Flora.csv) kept only the Description field and cut infraspecific names back to the species; the factsheets also
# hold Habitat, Flowering / Fruiting months, Introduced Status, Notes, Diagnostic Characters and Similar Taxa. Pages are cached in
# data_extra/NTFlora_html (0.5 s between requests); writes data_extra/ntflora_profiles.csv, one row per factsheet.
import os, re, sys, csv, html, json
sys.path.insert(0, os.path.dirname(__file__)); from crawl import get
OUT = os.path.expanduser("~/GitHub/austraits.build-data.scraping.scripts/data_extra/ntflora_profiles.csv")
def clean(s):
    s = re.sub(r"(?s)<(script|style)\b.*?</\1>", " ", s)
    s = re.sub(r"<br\s*/?>", " ", s)
    return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", "", s))).strip()
def fields(page):
    # each field is <div class="fielddiv"><span class="fieldlabel">Label:</span><span class="fieldbody"> ... </span>... </div>
    out = {}
    for m in re.finditer(r'(?s)<div class="fielddiv">\s*<span class="fieldlabel">([^<]+?):?\s*</span>(.*?)</div>', page):
        lab = clean(m.group(1)).rstrip(":"); body = " ".join(clean(b) for b in re.findall(r'(?s)<span class="fieldbody">(.*?)</span>', m.group(2)))
        if lab and lab not in out: out[lab] = body.strip()
    return out
def title(page):
    m = re.search(r"(?s)<h1[^>]*>(.*?)</h1>", page)
    return clean(m.group(1)) if m else ""
idx = {"families": []}
for L in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
    s = get(f"browse?heading=browseNTFlora&viewLevel=family&exactMatches&familyLetter={L}")
    idx["families"] += [f for f in re.findall(r"viewLevel=genus&amp;exactMatches&amp;family=([A-Z]+)\"", s) if f not in idx["families"]]
rows = []
for fam in idx["families"]:
    s = get(f"browse?heading=browseNTFlora&viewLevel=genus&exactMatches&family={fam}")
    gens = []
    for g in re.findall(rf"viewLevel=all&amp;exactMatches&amp;family={fam}&amp;genus=([^\"&]+)\"", s):
        g = html.unescape(g)
        if g not in gens: gens.append(g)
    for g in gens:
        gs = get(f"browse?heading=browseNTFlora&viewLevel=all&exactMatches&family={fam}&genus={g}")
        seen = set()
        for gid in re.findall(r'href="factsheetGenus\?id=(\d+)"', gs):
            if gid in seen: continue
            seen.add(gid); p = get(f"factsheetGenus?id={gid}")
            rows.append({"family": fam.title(), "genus": g, "rank": "genus", "taxon_id": gid, "title": title(p), "url": f"https://eflora.nt.gov.au/factsheetGenus?id={gid}", **fields(p)})
        for tr in re.findall(r'(?s)<tr class="[^"]*rowReal[^"]*">(.*?)</tr>', gs):
            td1 = tr.split("</td>")[0]
            # (phrase names are <species normal >sp. Watarrka</species>: allow attributes)
            get_t = lambda tag: (re.search(rf"<{tag}\b[^>]*>\s*([^<]*?)\s*</{tag}>", td1) or [None, ""])[1].strip()
            sp, rk, nm = get_t("species"), get_t("infrasprank"), get_t("infraspname")
            fid = re.search(r'href="factsheet\?id=(\d+)"', tr)
            if not sp or not fid or fid.group(1) in seen: continue
            seen.add(fid.group(1)); p = get(f"factsheet?id={fid.group(1)}")
            name = f"{g} {html.unescape(sp)}" + (f" {rk} {html.unescape(nm)}" if rk and nm else "")
            rows.append({"family": fam.title(), "genus": g, "rank": {"subsp.": "subspecies", "var.": "variety", "f.": "form"}.get(rk, "infraspecific" if rk else "species"),
                         "taxon_id": fid.group(1), "taxon_name": name, "title": title(p), "url": f"https://eflora.nt.gov.au/factsheet?id={fid.group(1)}", **fields(p)})
    print(fam, len(rows), flush=True)
cols = ["taxon_name", "rank", "genus", "family", "taxon_id", "url", "title"]
cols += sorted({k for r in rows for k in r} - set(cols), key=lambda k: -sum(1 for r in rows if r.get(k)))
for r in rows:
    if r["rank"] == "genus": r["taxon_name"] = r["genus"]
w = csv.DictWriter(open(OUT, "w", newline="", encoding="utf-8"), fieldnames=cols, lineterminator="\n"); w.writeheader(); w.writerows(rows)
print(len(rows), "factsheets ->", OUT)
