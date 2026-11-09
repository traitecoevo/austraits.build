# NT_2026 names, step 2: for every name of split_sentences_NT_Flora.csv that carries more than one distinct text (subspecies / varieties cut
# back to the species in the 2022 scrape), read its genus' taxon list on eflora.nt.gov.au (name + factsheet id), fetch the factsheets of that
# species and its infraspecific taxa, and match each row's text to a factsheet Description (word-set overlap). Writes nt_name_matches.csv.
import csv, io, re, json, html, os, sys, collections
sys.path.insert(0, os.path.dirname(__file__)); from crawl import get, text
SRC = "/Users/z3524079/Library/CloudStorage/OneDrive-UNSW/Documents/data_for_GitHub_repos/austraits.build-data.scraping.scripts.data/floras/split_sentences_NT_Flora.csv"
rows = list(csv.DictReader(io.StringIO(open(SRC, "rb").read().decode("utf-8", errors="replace"))))
idx = json.load(open(os.path.join(os.path.dirname(__file__), "nt_index.json")))
gen_fam = idx["gen_fam"]
by = collections.defaultdict(list)
for i, r in enumerate(rows): by[r["taxon_name"]].append(i)
amb = {k: v for k, v in by.items() if k and len({rows[i]["original_text"] for i in v}) > 1}
def taxa_of(genus):
    f = gen_fam.get(genus)
    if not f: return []
    s = get(f"browse?heading=browseNTFlora&viewLevel=all&exactMatches&family={f}&genus={genus}")
    out = []
    for tr in re.findall(r'(?s)<tr class="[^"]*rowReal[^"]*">(.*?)</tr>', s):
        td1 = re.split(r"</td>", tr)[0]
        sp = re.search(r"<species\s*>\s*([^<]*?)\s*</species>", td1); rk = re.search(r"<infrasprank>\s*([^<]*?)\s*</infrasprank>", td1)
        nm = re.search(r"<infraspname\s*>\s*([^<]*?)\s*</infraspname>", td1); fid = re.search(r'href="factsheet\?id=(\d+)"', tr)
        if not sp or not sp.group(1).strip(): continue
        name = f"{genus} {sp.group(1).strip()}" + (f" {rk.group(1).strip()} {nm.group(1).strip()}" if rk and rk.group(1).strip() and nm and nm.group(1).strip() else "")
        out.append((html.unescape(name), fid.group(1) if fid else None))
    return out
def desc_of(fid):
    t = text(get(f"factsheet?id={fid}"))
    m = re.search(r"Description:\s*(.*?)(?:\s+img\.active|\s+Distribution\b|\s+Notes?:|\s+Habitat:|$)", t)
    return m.group(1).strip() if m else ""
import difflib
N = lambda s: re.sub(r"[^a-z0-9]+", " ", s.lower()).strip()
sim = lambda a, b: difflib.SequenceMatcher(None, a, b, autojunk=False).ratio()
out = []
for name, ii in sorted(amb.items()):
    cands = [(n, f) for n, f in taxa_of(name.split()[0]) if (n == name or n.startswith(name + " ")) and f]
    descs = {n: desc_of(f) for n, f in cands}
    for i in ii:
        w = N(rows[i]["original_text"]); best = []
        for n, d in descs.items():
            if not d or d.startswith("Not available"): continue
            best.append((sim(w, N(d)), n))
        best.sort(reverse=True)
        out.append({"row": i, "taxon_name": name, "best": best[0][1] if best else "", "score": round(best[0][0], 3) if best else "",
                    "second": best[1][1] if len(best) > 1 else "", "score2": round(best[1][0], 3) if len(best) > 1 else "",
                    "candidates": "; ".join(f"{n} [{'none' if not descs[n] or descs[n].startswith('Not available') else len(descs[n])}]" for n in descs),
                    "text": rows[i]["original_text"][:150]})
w = csv.DictWriter(open(os.path.join(os.path.dirname(__file__), "nt_name_matches.csv"), "w", newline=""), fieldnames=list(out[0]), lineterminator="\n")
w.writeheader(); w.writerows(out); print(len(amb), "names,", len(out), "rows")
