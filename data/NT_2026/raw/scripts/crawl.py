# NT_2026 names (user 2026-11-07): the 2022 scrape (split_sentences_NT_Flora.csv) cut subspecies / variety names back to the species and
# left the family descriptions unnamed, so the true names are read from eflora.nt.gov.au: family letters -> families -> genera ->
# taxon lists (name + factsheet id), then the factsheets of the genera that need them. Pages are cached in html/ (0.5 s between requests).
import os, re, sys, time, html, json, urllib.request
B = "https://eflora.nt.gov.au/"; H = os.path.expanduser("~/GitHub/austraits.build-data.scraping.scripts/data_extra/NTFlora_html")
def get(path):
    fn = os.path.join(H, re.sub(r"[^A-Za-z0-9=&_.-]+", "_", path)[:200] + ".html")
    if os.path.exists(fn): return open(fn, encoding="utf-8", errors="replace").read()
    for k in range(3):
        try:
            s = urllib.request.urlopen(B + path, timeout=60).read().decode("utf-8", errors="replace"); break
        except Exception as e:
            print("retry", path, e, file=sys.stderr); time.sleep(5)
    else: raise SystemExit("failed " + path)
    open(fn, "w", encoding="utf-8").write(s); time.sleep(0.5); return s
def text(s):
    s = re.sub(r"(?s)<(script|style)\b.*?</\1>", " ", s); return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", s))).strip()
if __name__ == "__main__":
    fams = {}
    for L in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
        s = get(f"browse?heading=browseNTFlora&viewLevel=family&exactMatches&familyLetter={L}")
        for f in re.findall(r"viewLevel=genus&amp;exactMatches&amp;family=([A-Z]+)\"", s): fams[f] = 1
    gen_fam = {}; fam_sheet = {}
    for f in fams:
        s = get(f"browse?heading=browseNTFlora&viewLevel=genus&exactMatches&family={f}")
        for g in re.findall(rf"viewLevel=all&amp;exactMatches&amp;family={f}&amp;genus=([^\"&]+)\"", s): gen_fam[html.unescape(g)] = f
        m = re.search(r'href="(factsheetFamily\?id=\d+)"', s)
        if m: fam_sheet[f] = m.group(1)
    json.dump({"gen_fam": gen_fam, "fam_sheet": fam_sheet}, open(os.path.join(os.path.dirname(__file__), "nt_index.json"), "w"), indent=0)
    print(len(fams), "families,", len(gen_fam), "genera,", len(fam_sheet), "family factsheets")
