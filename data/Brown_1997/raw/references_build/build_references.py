"""Build Brown_1997's numbered reference table (raw/references.csv).

refs 135-160: references_135-160_standardised.bib (already matched to CrossRef/OpenAlex)
other refs:   transcribed.txt (from the photographed Appendix Five), matched to CrossRef
              (crossref.json) when title, year (+-1) and first author agree.
"""
import csv, json, re, unicodedata
from collections import defaultdict

COLS = ["refnumber", "key", "see", "bibtype", "year", "author", "title", "journal", "volume", "number",
        "pages", "booktitle", "editor", "publisher", "place", "institution", "type", "doi", "url", "note", "origin"]


def norm(s):
    s = re.sub(r"<[^>]+>", " ", s or "")
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^a-z0-9]", "", s.lower())


def ascii_stem(s):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^A-Za-z]", "", s)


def cap_surname(s):
    s = s.strip()
    if not re.fullmatch(r"(Mc|Mac)?[A-Z][A-Z' -]+", s):
        return s
    m = re.match(r"^(Mc|Mac)(.*)$", s)
    if m:
        return m.group(1) + cap_surname(m.group(2)).capitalize()
    out = []
    for w in s.split():
        w = "-".join(p.capitalize() for p in w.lower().split("-"))
        w = re.sub(r"^Mc([a-z])", lambda m: "Mc" + m.group(1).upper(), w)
        out.append(w)
    return " ".join(out)


def person(p):
    # "WHITLOCK, F.L." -> "F. L. Whitlock"; "van LEEUWEN, S.J." -> "S. J. van Leeuwen"
    p = p.strip()
    if "," not in p:
        return p
    sur, giv = [x.strip() for x in p.split(",", 1)]
    giv = re.sub(r"\.(?=[A-Z])", ". ", giv).replace("(Ed)", "").strip()
    particle = ""
    m = re.match(r"^(van|de|von)\s+(.*)$", sur)
    if m:
        particle, sur = m.group(1) + " ", m.group(2)
    return f"{giv} {particle}{cap_surname(sur)}".strip()


def authors(s):
    s = s.replace(" - ", " and ").replace("(Ed)", "")
    if s.isupper() and "," not in s:          # institution
        return "{" + " ".join(w.capitalize() if len(w) > 3 else w for w in s.split()).replace("PTY LTD", "Pty Ltd").replace("Pty", "Pty").replace("LTD", "Ltd") + "}"
    parts = re.split(r",?\s+and\s+|(?<=\.),\s+(?=[A-Za-z])", s)
    return " and ".join(person(p) for p in parts if p.strip())


def first_surname(a):
    if a.startswith("{"):
        return "".join(w for w in a.strip("{}").split()[:2])     # {Worsley Alumina Pty Ltd} -> WorsleyAlumina
    first = a.split(" and ")[0].strip("{}")
    m = re.search(r"\b(van|de|von) (\S+)$", first)       # S. J. van Leeuwen -> vanLeeuwen
    if m:
        return m.group(1) + m.group(2)
    return first.split()[-1] if " " in first and not first.startswith("{") else first


def clean_latex(s):
    s = re.sub(r"\\textit\{([^}]*)\}", r"\1", s)
    s = s.replace("\\&", "&").replace("{", "").replace("}", "")
    return re.sub(r"\s+", " ", s).strip()


rows = []

# ---- refs 135-160 from the bib
bib = open("references_135-160_standardised.bib").read()
for m in re.finditer(r"@(\w+)\{([^,]+),\n(.*?)\n\}", bib, re.S):
    typ, bkey, body = m.groups()
    f = {}
    for fm in re.finditer(r"^\s*(\w+)\s*=\s*\{(.*)\},?\s*$", body, re.M):
        f[fm.group(1)] = fm.group(2)
    row = dict.fromkeys(COLS, "")
    row["refnumber"] = f["refnumber"]
    row["origin"] = "bib 135-160"
    row["bibtype"] = {"article": "Article", "book": "Book", "incollection": "InBook", "phdthesis": "Thesis",
                      "misc": "Unpublished"}[typ]
    for k in ["year", "title", "journal", "volume", "number", "pages", "booktitle", "publisher", "doi"]:
        if k in f:
            row[k] = clean_latex(f[k])
    if "author" in f:
        a = clean_latex(f["author"])
        if f["author"].startswith("{{"):
            row["author"] = "{" + a + "}"
        else:
            row["author"] = " and ".join(
                (lambda sur, giv: f"{giv} {sur}")(*[x.strip() for x in p.split(",", 1)]) if "," in p else p
                for p in a.split(" and "))
    row["place"] = clean_latex(f.get("address", ""))
    if typ == "phdthesis":
        row["institution"] = clean_latex(f["school"]); row["type"] = "PhD thesis"
    if typ == "misc":
        row["title"] = clean_latex(f.get("howpublished", "")) if f.get("howpublished", "").startswith(("Unpublished", "Personal", "Photographic")) and "title" not in f else row["title"]
        if "Lochman" in bkey:
            row["title"] = "Lochman Transparencies: photographic transparency collection"
        if bkey.endswith("unpubl_obs"):
            row["title"] = "Unpublished observations"
        if bkey == "Keighery_slides":
            row["title"] = "Personal slide collection"
    if "Bowyer-Bowler" in row["title"]:
        row["title"] = row["title"].replace("Bowyer-Bowler", "Bowyer-Bower")   # printed: Bowyer-Bower
    row["note"] = clean_latex(f.get("annote", ""))
    if bkey == "Keighery_1979_unpubl":   # howpublished: "Unpublished. King's Park and Botanic Gardens, West Perth"
        row["institution"] = "King's Park and Botanic Gardens, West Perth"
        row["note"] = "Unpublished"
    row["key"] = bkey   # re-keyed below
    # 155 is a personal communication "also see" a paper, like 7: records cite the paper (155b)
    if bkey == "Lepschi_1997":
        row["refnumber"] = "155b"
    if bkey == "Lepschi_1997_perscomm":
        row.update(origin="see", see="155b", author="B. J. Lepschi")
    rows.append(row)

# ---- the rest from the transcription (+ CrossRef)
cr = json.load(open("crossref.json"))
by_eye = {"5", "11", "21", "65", "66", "68", "92", "100", "102", "134", "162", "173", "200", "201",
          "234", "237", "248"}
for l in open("appendix5_transcribed.txt"):
    if not l[0].isdigit():
        continue
    num, typ, au, yr, title, cont, vol, pages, extra = (l.rstrip("\n").split("|") + [""] * 9)[:9]
    row = dict.fromkeys(COLS, "")
    row.update(refnumber=num, origin="transcribed")
    year = "" if yr == "undated" else re.sub(r"[a-z]$", "", yr)
    row["year"] = year
    row["author"] = authors(au) if au else ""
    if typ == "S":
        row["see"] = extra; row["origin"] = "see"
    elif typ == "P":
        row["see"] = "7b"; row["origin"] = "see"
    elif typ == "U":
        row.update(bibtype="Unpublished", title=extra or "Unpublished observations",
                   note=f"Reported in Brown et al. (1997), Appendix Five, ref. {num}")
    elif typ == "M" and num == "12":
        # a society bulletin: published
        row.update(bibtype="Article", title=title, journal="Western Australian Native Orchid Study and Conservation Group Bulletin",
                   note="June 1985 Bulletin")
    elif typ == "M":
        row.update(bibtype="Unpublished", title=title or extra,
                   note="; ".join(x for x in [cont, f"Cited in Brown et al. (1997), Appendix Five, ref. {num}"] if x))
    elif typ == "T":
        row.update(bibtype="Thesis", title=title, institution=cont, type=extra)
    elif typ == "R":
        row.update(bibtype="Report", type="Report", title=title, institution=cont, number=vol, pages=pages)
    elif typ == "B":
        pub, _, place = cont.rpartition(", ")
        row.update(bibtype="Book", title=title, publisher=pub or cont, place=place if pub else "",
                   note="" if year else "Undated")
        if not year:
            row["year"] = "n.d."     # dataset_test() needs a year; printed as undated
    elif typ == "C":
        eds, _, rest = extra.partition(";")
        row.update(bibtype="InBook", title=title, booktitle=cont, pages=pages.replace("-", "--"),
                   editor=authors(eds) if eds.strip() else "", publisher=rest.strip(" ;"))
    elif typ == "A":
        row.update(bibtype="Article", title=title, journal=cont, volume=vol,
                   pages=pages if ";" in pages else pages.replace("-", "--"), note=extra)
    # CrossRef: accept when title, year and first author agree; or the top hit
    # where checked by eye (record title misspelt, e.g. "Eutyglossina", "Qualap",
    # "Sedgwich"): DOI, volume and pages from the record, printed title kept
    items = cr.get(num) or []
    if num in by_eye and items:
        it = items[0]
        row["doi"] = it["DOI"].lower()
        if it.get("volume"): row["volume"] = it["volume"]
        if it.get("issue"): row["number"] = it["issue"]
        if it.get("page"): row["pages"] = it["page"].replace("-", "--")
        row["origin"] = "transcribed, matched to CrossRef by eye"
        items = []
    for it in items:
        t = (it.get("title") or [""])[0]
        if len(norm(t)) < 15 or not (norm(t) in norm(title) or norm(title) in norm(t)):
            continue
        iy = ((it.get("published-print") or it.get("issued") or {}).get("date-parts") or [[None]])[0][0]
        if year and iy and abs(int(year) - iy) > 1:
            continue
        fam = ((it.get("author") or [{}])[0]).get("family", "")
        if fam and norm(fam) not in norm(au):
            continue
        row["doi"] = it["DOI"].lower()
        crt = re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", t)).strip()
        crt = re.sub(r"\s+([,.;:)])", r"\1", crt)
        if norm(crt) != norm(title):
            row["note"] = (row["note"] + "; " if row["note"] else "") + f'Printed title: "{title}"'
        row["title"] = crt
        if it.get("volume"): row["volume"] = it["volume"]
        if it.get("issue"): row["number"] = it["issue"]
        if it.get("page") and ";" not in row["pages"]: row["pages"] = it["page"].replace("-", "--")
        if iy and str(iy) != year:
            row["note"] = (row["note"] + "; " if row["note"] else "") + f"Printed year {yr}; the DOI record gives {iy}"
            row["year"] = str(iy)
        row["origin"] = "transcribed, matched to CrossRef"
        break
    row["key"] = ""
    if num in ("220", "221"):
        # printed "Memoirs Mus. Vict. 52: Part 1: 1-124 / Part 2: 215-423"; the volume is 55
        row["note"] = f"Printed: Memoirs Mus. Vict. 52: Part {'1: 1-124' if num == '220' else '2: 215-423'}"
        row["volume"] = "55"
        row["number"] = "1" if num == "220" else "2"
        if num == "221":
            row.update(title="Revision of the Australian native bee subgenus Lasioglossum (Chilalictus) continued",
                       doi="10.24199/j.mmv.1995.55.02", pages="215--423", origin="transcribed, matched to CrossRef by eye")
    if num == "11":
        row["author"] = "G. Barrett"   # printed BARRET; the paper (doi 10.5962/p.310333) is by G. Barrett
        row["note"] = "Printed author: BARRET, G."
    rows.append(row)

# ---- keys: first author's surname + year; unpublished: surname + _unpub; suffixes in reference order
def stem(r):
    s = ascii_stem(first_surname(r["author"])) if r["author"] else "Unknown"
    if r["bibtype"] == "Unpublished" and not r["year"]:
        return f"{s}_unpub"
    return f"{s}_{r['year'].replace('n.d.', 'nd') or 'nd'}"

order = lambda r: (int(re.sub(r"\D", "", r["refnumber"])), r["refnumber"])
rows.sort(key=order)
groups = defaultdict(list)
for r in rows:
    if r["origin"] != "see":
        groups[stem(r)].append(r)
for s, rs in groups.items():
    for i, r in enumerate(rs, 1):
        r["key"] = s if len(rs) == 1 else f"{s}_{i}"
by_num = {r["refnumber"]: r for r in rows if r["origin"] != "see"}

# "In <work>": the observation is reported in another numbered reference
see_targets = {"In Frith (1976)": "82", "In Sargent (1928)": "189", "In Keighery (1996a)": "143",
               "In Keast (1975)": "131", "In North (1898)": "174",
               "In Storr, G.M. (unpubl. ms. notes 1)": "214", "In Storr, G.M. (unpub. ms. notes 2)": "215", "In Storr, G.M. (unpubl. ms. notes 2)": "215",
               "7b": "7b", "155b": "155b"}
for r in rows:
    if r["origin"] == "see":
        t = see_targets[r["see"]]
        r["note"] = f'{r["author"]}, {r["see"]}' if not r["see"].endswith("b") else \
            f'{r["author"]}, personal communication to E. M. Brown; also see ref. {r["see"]}'
        r["see"] = t
        r["key"] = by_num[t]["key"]

with open("../references.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=COLS)
    w.writeheader()
    w.writerows(rows)
from collections import Counter
print(len(rows), Counter(r["origin"] for r in rows))
