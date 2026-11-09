"""Assemble the Vision OCR lines of Cooper & Cooper (2004) "Fruits of the Australian Tropical Rainforest" into one row per taxon.

reads  ocr2/<tag>_<page>.tsv (col x0 y0 x1 y1 conf src text)
writes cooper2004_profiles.csv, pages.csv (scan page -> book page, chosen scan) and assemble_log.txt
"""
import csv, glob, os, re, sys
from collections import Counter, defaultdict

SP = "/private/tmp/claude-502/-Users-z3524079-GitHub-traits-build-inst-support/fb9a03f2-b5ee-4e53-a6f9-9631bd56b1ab/scratchpad/cooper"
OUT = sys.argv[1] if len(sys.argv) > 1 else SP
TAGS = ["pages001-100", "pages101-200", "pages201-400", "pages401-560", "pages_rescanned", "pages_rescanned2"]
# book page = scan page + offset for the four main scans (checked against the printed page numbers below)
OFFSET = {"pages001-100": -1, "pages101-200": 100, "pages201-400": 200, "pages401-560": 400}
log = open(f"{OUT}/assemble_log.txt", "w", encoding="utf-8")


def L(*a):
    print(*a, file=log)


OCR_GENUS = {"Elacocarpus": "Elaeocarpus", "ELACOCARPUS": "ELAEOCARPUS", "Excoccaria": "Excoecaria", "EXCOCCARIA": "EXCOECARIA",
             "Ricinocarpus": "Ricinocarpos"}
# ---- read the pages ----
pages = {}
for f in sorted(glob.glob(f"{SP}/ocr2/*.tsv")):
    base = os.path.basename(f)[:-4]
    tag, pg = base.rsplit("_", 1)
    rows = []
    for ln in open(f, encoding="utf-8"):
        p = ln.rstrip("\n").split("\t")
        if len(p) < 8:
            continue
        rows.append(dict(col=int(p[0]), x0=float(p[1]), y0=float(p[2]), x1=float(p[3]), y1=float(p[4]), conf=float(p[5]), src=p[6], text="\t".join(p[7:]).strip()))
    for r in rows:
        r["text"] = re.sub(r"^(\*?\s*)Nex\b", r"\1Ilex", r["text"])
        r["text"] = re.sub(r"\b(var|subsp),\s", r"\1. ", r["text"])
        r["text"] = re.sub(r"\bJulv\b", "July", re.sub(r"\bMav\b", "May", r["text"]))
        # Vision slips in genus names ("ae" read "ac"; found against config/taxon_list.csv; the book's own older names are kept)
        for a, b in OCR_GENUS.items():
            r["text"] = re.sub(rf"\b{a}\b", b, r["text"])
    pages[(tag, int(pg))] = rows


def printed_number(rows):
    c = [r for r in rows if re.fullmatch(r"\d{1,3}", r["text"]) and (r["y0"] > 0.88 or r["y1"] < 0.07)]
    return int(c[0]["text"]) if len(c) == 1 else None


# ---- scan page -> book page ----
book = defaultdict(list)
unplaced = []
for (tag, pg), rows in pages.items():
    num = printed_number(rows)
    if tag in OFFSET:
        bp = pg + OFFSET[tag]
        if num is not None and num != bp:
            L(f"page number mismatch: {tag} p{pg} printed {num}, offset gives {bp}")
    else:
        bp = num
    if bp is None:
        unplaced.append((tag, pg))
        continue
    book[bp].append((tag, pg))
# rescans without a readable page number: the main-scan page with the most words in common
def wordset(rows):
    return set(re.findall(r"[a-z]{4,}", " ".join(r["text"] for r in rows).lower()))
for k in unplaced:
    ws = wordset(pages[k])
    best = max(((len(ws & wordset(pages[m])) / max(1, len(ws | wordset(pages[m]))), m) for m in pages if m[0] in OFFSET), default=(0, None))
    if best[0] > 0.5:
        bp = best[1][1] + OFFSET[best[1][0]]
        book[bp].append(k)
        L(f"{k[0]} p{k[1]}: no page number; matched to book page {bp} ({best[0]:.2f} of words shared)")
    else:
        L(f"no book page for {k[0]} p{k[1]} ({len(pages[k])} lines; best match {best[0]:.2f}): left out")


def body_chars(rows):
    return sum(len(r["text"]) for r in rows if r["col"] >= 0)


chosen = {}
with open(f"{OUT}/pages.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["book_page", "scan", "scan_page", "chars", "chosen"])
    for bp in sorted(book):
        cands = sorted(book[bp], key=lambda k: -body_chars(pages[k]))
        chosen[bp] = cands[0]
        for k in book[bp]:
            w.writerow([bp, k[0], k[1], body_chars(pages[k]), k == cands[0]])
        if len(cands) > 1:
            L(f"book page {bp}: {len(cands)} scans {[(k, body_chars(pages[k])) for k in cands]}, using {cands[0]}")
missing = [p for p in range(min(book), max(book) + 1) if p not in book]
L(f"book pages {min(book)}-{max(book)}; missing: {missing}")

# ---- clean lines and make one stream ----
FAM_RE = re.compile(r"^[A-Z]{4,}ACEAE$")
GENUS_RE = re.compile(r"^([A-Z]{3,})\s*(.*)$")
stream = []
for bp in sorted(chosen):
    rows = pages[chosen[bp]]
    has_run = any("FRUITS OF" in r["text"].upper() or "TROPICAL RAINFOREST" in r["text"].upper() for r in rows)
    # a strip of the facing page caught at the photo's edge ("Wightia pube", "Lares simple, 30-." beside p. 56): 5+ narrow lines against
    # the left or right edge are dropped
    # (outside the page's own text: left of where its long lines start, right of where they end)
    longl = [r for r in rows if len(r["text"]) >= 25 and r["col"] >= 0]
    lmin = min((r["x0"] for r in longl), default=0.0); rmax = max((r["x1"] for r in longl), default=1.0)
    right = [r for r in rows if r["x0"] > max(0.85, rmax + 0.005) and r["x1"] - r["x0"] < 0.08]
    left = [r for r in rows if r["x1"] < min(0.15, lmin - 0.005) and r["x1"] - r["x0"] < 0.08 and not re.fullmatch(r"\d{1,3}", r["text"])]
    sliver = set()
    for grp, side in ((right, "right"), (left, "left")):
        if len(grp) >= 5:
            sliver |= {id(r) for r in grp}
            L(f"book page {bp}: {len(grp)} lines of the facing page at the {side} edge dropped")
    cols = defaultdict(list)
    for r in rows:
        t = r["text"]
        if id(r) in sliver:
            continue
        if r["col"] < 0 or r["y1"] - r["y0"] > 0.025:
            continue
        if re.fullmatch(r"\d{1,3}", t) and (r["y0"] > 0.88 or r["y1"] < 0.07):
            continue
        if re.search(r"FRUITS OF|AUSTRALIAN TROPICAL|TROPICAL RAINFOREST|OF THE AUSTRALIAN", t.upper()) and r["y1"] < 0.08:
            continue
        # (pieces of the running head: "CAL RAINFOREST", "FRUITS OF THE")
        if r["y1"] < 0.08 and t.isupper() and len(t.replace(" ", "")) >= 3 and t.replace(" ", "") in "FRUITSOFTHEAUSTRALIANTROPICALRAINFOREST":
            continue
        # running head: the family name at the top of the page, beside "FRUITS OF THE AUSTRALIAN TROPICAL RAINFOREST"
        if FAM_RE.match(t.replace(" ", "")) and r["y1"] < 0.06 and has_run:
            r["running"] = True
        cols[r["col"]].append(r)
    # pieces of one visual line returned separately (the bold field label "Leaves" a little lower than the rest of its line, which then
    # sorted after it: Parsonsia ventricosa p. 52) are joined, left to right
    for c in list(cols):
        rs = sorted(cols[c], key=lambda r: r["x0"])
        out = []
        for r in rs:
            yc = (r["y0"] + r["y1"]) / 2
            hit = None
            for q in out:
                qc = (q["y0"] + q["y1"]) / 2
                if abs(qc - yc) < 0.0045 and -0.006 <= r["x0"] - q["x1"] < 0.02 and q["src"] != "span" and r["src"] != "span":
                    hit = q
                    break
            if hit is not None:
                hit["text"] = hit["text"] + " " + r["text"]
                hit["x1"] = max(hit["x1"], r["x1"]); hit["y0"] = min(hit["y0"], r["y0"]); hit["y1"] = max(hit["y1"], r["y1"])
            else:
                out.append(r)
        cols[c] = out
    # a full-width band (a genus introduction set across the page, Syzygium p. 356: its heading, etymology, "World: ..." lines): text
    # above it is read first (columns left to right), then the band (top to bottom), then the columns below it
    spans = [r for c in cols for r in cols[c] if r["src"] == "span"]
    groups = []
    if spans:
        top, bot = min(r["y0"] for r in spans), max(r["y1"] for r in spans)
        # (the band's centred genus heading just above its first spanning line)
        heads = [r for c in cols for r in cols[c] if top - 0.07 < r["y0"] < top and GENUS_RE.match(r["text"]) and not FAM_RE.match(r["text"].split()[0])
                 and len(r["text"]) < 45 and re.search(r"[a-z.]", r["text"])]
        if heads:
            top = min(r["y0"] for r in heads)
        allr = [r for c in cols for r in cols[c]]
        band = [r for r in allr if top - 0.002 <= r["y0"] <= bot + 0.002]
        above = defaultdict(list); below = defaultdict(list)
        for r in allr:
            if r in band:
                continue
            (above if r["y0"] < top else below)[r["col"]].append(r)
        groups = [above[c] for c in sorted(above)] + [band] + [below[c] for c in sorted(below)]
    else:
        groups = [cols[c] for c in sorted(cols)]
    for grp in groups:
        rs = sorted(grp, key=lambda r: (r["y0"], r["x0"]))
        # gaps to the nearest line above / below that overlaps horizontally (not a caption or key answer beside the line)
        def over(a, b):
            return min(a["x1"], b["x1"]) - max(a["x0"], b["x0"]) > 0.3 * min(a["x1"] - a["x0"], b["x1"] - b["x0"])
        for k, r in enumerate(rs):
            r["page"] = bp
            up = [q for q in rs[:k] if q["y1"] <= r["y0"] + 0.004 and over(q, r)]
            dn = [q for q in rs[k + 1:] if q["y0"] >= r["y1"] - 0.004 and over(q, r)]
            r["gap_above"] = (r["y0"] - max(q["y1"] for q in up)) if up else 1.0
            r["gap_below"] = (min(q["y0"] for q in dn) - r["y1"]) if dn else 1.0
        stream.extend(rs)

# ---- entries ----
LABELS = ["Leaves", "Leaf", "Inflorescence", "Inflorescences", "Flowers", "Flower", "Male cones", "Female cones", "Male flowers", "Female flowers",
          "Fruit", "Fruits", "Figs", "Notes", "Distribution"]
LABEL_RE = re.compile(r"^(" + "|".join(sorted(LABELS, key=len, reverse=True)) + r")\b\s*(.*)$")
NAME_RE = re.compile(r"^(\*\s*)?([A-Z][a-z]+)\s+(?:(x)\s+)?(sp\.|[a-z][a-z-]+)(.*)$")
GENUS_RE = re.compile(r"^([A-Z]{3,})\s*(.*)$")
NOT_EPITHET = {"to", "or", "and", "with", "from", "after", "species", "spp", "is", "are", "was", "has", "have", "in", "on", "of", "may", "can", "up", "about"}
NOT_EPITHET |= {"tree", "trees", "shrub", "shrubs", "twiner", "tendril", "climber", "scrambler", "hook", "herb", "palm", "vine", "epiphyte", "lily", "stem", "stems"}
NOT_GENUS = set(LABELS) | {"Vine", "Tree", "Shrub", "Herb", "Palm", "Lily", "Climber", "Small", "Climbing", "Scrambling", "Sedge", "Grass", "Cycad", "Mistletoe", "Epiphyte"}
NOT_GENUS |= {"Fig", "Figs", "Domatia", "Aril", "Arils", "Petioles", "Petiole", "Rainforest", "Funicle", "Lateral", "Stipules", "Pedicels", "Seeds", "Seed",
              "Calyx", "Petals", "Sepals", "Tepals", "Bracts", "Spines", "Stems", "Twigs", "Bark", "Exudate", "Branchlets", "Midrib", "Tendrils",
              "Pods", "Pod", "Capsule", "Capsules", "Any", "Season", "Leaflets", "Pinnae", "Plants", "Female", "Male", "Australia", "World"}
NOT_EPITHET |= {"thought", "believed", "said", "commemorating", "named"}
NOT_EPITHET |= {"possibly", "probably", "according", "derivation", "named", "name", "the", "honour", "honours", "commemorates", "origin",
                "meaning", "refers", "referring", "latin", "greek", "derived", "unknown", "perhaps", "an", "a"}
NOT_EPITHET |= {"axillary", "terminal", "ramiflorous", "cauliflorous", "solitary", "clustered", "paired"} | {"Other", "Notes", "World", "Australia", "Rainforest", "Also", "Previously", "Occurs"}


HABIT_WORDS = {"tree", "iree", "thee", "lree", "shrub", "shrubs", "herb", "herbs", "palm", "vine", "small", "large", "pandanus", "padanus", "lily",
               "lily-like", "cycad", "banana", "scrambling", "scrambler", "mistletoe", "sedge", "scandent", "grass", "mangrove", "climber",
               "climbing", "erect", "prostrate", "epiphyte", "epiphytic", "fern", "strangler", "hemi-epiphyte", "parasitic", "leafless", "aquatic",
               "saprophyte", "orchid", "bamboo", "rattan", "liana", "twiner", "succulent", "robust", "slender", "creeping", "stemless"}


def is_habit(t):
    w = re.sub(r"[^A-Za-z-]", "", t.split()[0]).lower() if t.split() else ""
    return bool(re.match(r"^['*]?[A-Z']", t)) and bool(re.search(r"[a-z]", t)) and (w in HABIT_WORDS or bool(re.match(r"^[A-Z][a-z-]+(\s+[a-z-]+){0,4},?\s+(to|up to|about)\s+[0-9.]+\s?(m|mm|cm)\b", t)))


FIELD_ORDER = ["Leaves", "Inflorescence", "Flower", "Male flowers", "Female flowers", "Male cones", "Female cones", "Fruit", "Notes", "Distribution"]


def field_rank(lab):
    lab = {"Leaf": "Leaves", "Flowers": "Flower", "Fruits": "Fruit", "Inflorescences": "Inflorescence", "Figs": "Fruit"}.get(lab, lab)
    return FIELD_ORDER.index(lab) if lab in FIELD_ORDER else -1


def last_rank(e):
    rk = -1
    for ln in e["body"]:
        m = LABEL_RE.match(ln)
        if m:
            rk = max(rk, field_rank(m.group(1)))
    return rk


def next_over(i):
    """the next line below stream[i] on its page and column that overlaps it horizontally (within the next 4 lines)"""
    r = stream[i]
    for k in range(i + 1, min(i + 5, len(stream))):
        q = stream[k]
        if q["page"] != r["page"]:
            return k
        if min(q["x1"], r["x1"]) - max(q["x0"], r["x0"]) > 0.3 * min(q["x1"] - q["x0"], r["x1"] - r["x0"]) and q["y0"] >= r["y1"] - 0.004:
            return k
    return i + 1 if i + 1 < len(stream) else None


def similar(a, b):
    a, b = a.lower(), b.lower()
    if a == b:
        return True
    if len(a) < 4 or len(b) < 4:
        return False
    # edit distance <= 2 (OCR slips: Elacocarpus, Syzygiurn)
    d = list(range(len(b) + 1))
    for i, ca in enumerate(a, 1):
        p, d[0] = d[0], i
        for j, cb in enumerate(b, 1):
            p, d[j] = d[j], min(d[j] + 1, d[j - 1] + 1, p + (ca != cb))
    return d[-1] <= 2


# an authority ("(L.) Roxb.", "L.", "Hook. ex Hook.f.", "C.T.White & W.D.Francis", "Domin")
_AUTH_RE = re.compile(r"^\s*(\(|[A-Z][a-z]*\.|[A-Z]\.[A-Z]|[A-Z][a-z]+ ex |[A-Z][a-z]+\s*&|[A-Z][a-z]+$)")
_AUTH_LINK = {"ex", "et", "al.", "f.", "fil.", "&", "de", "van", "von", "la", "le", "du", "der", "in"}


class _Auth:
    """an authority line: "(L.) Roxb.", "Domin", "Michael J. Parsons", "Peter G. Wilson", "C.T.White & W.D.Francis" (every word a capitalised
    name or initial, or a link word; not a common name in capitals)"""
    def match(self, t):
        t = t.strip()
        if not t or not re.search(r"[a-z]", t) or len(t) > 45:
            return None
        # (an infraspecific name after the species authority: "Lam. subsp. integrifolia", "(Harms) Pedley var. glabra")
        t = re.sub(r"\s+(?:subsp\.|var\.|f\.|forma)\s+[a-z-]+.*$", "", t)
        toks = re.findall(r"[^\s]+", re.sub(r"[()]", " ", t))
        if toks and all(re.match(r"^[A-Z]", w) or w in _AUTH_LINK or re.match(r"^[A-Z]?[a-z]*\.$", w) for w in toks):
            return True
        return None


AUTH_RE = _Auth()
# names with a proper heading (an authority on the line or the next): their other, bare occurrences are figure captions
HEADED = set()
for k, r in enumerate(stream):
    mm = NAME_RE.match(r["text"])
    if not mm or mm.group(2) in NOT_GENUS or mm.group(4) in NOT_EPITHET:
        continue
    nx = stream[k + 1] if k + 1 < len(stream) else None
    if AUTH_RE.match(mm.group(5)) or (not mm.group(5).strip() and nx and nx["gap_above"] < 0.012 and AUTH_RE.match(nx["text"]) and not LABEL_RE.match(nx["text"]) and len(nx["text"]) < 45):
        HEADED.add(f"{mm.group(2)} {mm.group(4)}")
HEADED_GENERA = {h.split()[0] for h in HEADED}
# genus words of proper headings, counted: a word on 2+ headings is a real genus, not an OCR slip of the current one
GENUS_N = Counter(h.split()[0] for h in HEADED)
entries, family, genus, genus_auth, genus_text = [], None, None, "", []
cur = None
i = 0
while i < len(stream):
    r = stream[i]
    t = r["text"]
    tn = t.replace(" ", "")
    if FAM_RE.match(tn):
        # (a running head repeating the current family is not a heading: it must not end the entry continued below it)
        if not r.get("running") and tn.capitalize() != family:
            family = tn.capitalize()
            cur = None
            L(f"p{r['page']} family {family}")
        i += 1
        continue
    m = GENUS_RE.match(t)
    if m and not FAM_RE.match(m.group(1)) and m.group(1).capitalize() not in NOT_GENUS:
        rest = m.group(2)
        nxt = stream[i + 1]["text"] if i + 1 < len(stream) else ""
        g = m.group(1).capitalize()
        # a genus heading: the genus in capitals, with its authority (mixed case) on the line or the next, or followed by the etymology "Genus from / after ..."
        em = [mm for k in range(i + 1, min(i + 4, len(stream))) for mm in [re.match(rf"^({g[:4]}\w*)\s+(from|after|possibly|probably|named|the|an?)\b", stream[k]["text"])] if mm]
        # (the start of the heading lost: "YGIUM Gaertn." / "Syzygium from syzygos ...", "HIUM Lam." / "Canthium from ...")
        if not em and len(g) >= 3:
            em = [mm for k in range(i + 1, min(i + 4, len(stream))) for mm in [re.match(r"^([A-Z][a-z]+)\s+(from|after|possibly|probably|named)\b", stream[k]["text"])]
                  if mm and mm.group(1).lower().endswith(g.lower()) and mm.group(1) != g]
            if em:
                g = em[0].group(1)
        etym_next = bool(em)
        # (the genus as spelt in its etymology line when the capitals are misread: "ACACI" / "Acacia from ...")
        if em and em[0].group(1) != g and similar(em[0].group(1), g):
            g = em[0].group(1)
        auth_like = bool(rest) and len(rest) < 45 and not re.search(r"\d|,|\baltitude\b", rest) and bool(re.match(r"^[(A-Z]", rest)) and bool(re.search(r"[a-z.]", rest)) and not re.search(r"[A-Z]{3,}", rest)
        if r["gap_above"] > 0.012 and (auth_like or (etym_next and (cur is None or cur["stage"] != "head"))):
            genus, genus_auth, genus_text = g, rest, []
            cur = None
            L(f"p{r['page']} genus {genus} {rest}")
            i += 1
            continue
    # figure captions: a plain name (no authority) with space below it ("Annona squamosa" under the plate)
    cm = re.fullmatch(r"\*?\s*([A-Z][a-z]+)\s+(x\s+)?([a-z][a-z-]+|sp\.(\s*\([^)]*\)?)?)(\s+(subsp|var)\.\s+[a-z-]+)?\s*\.?", t)
    # (not body lines that look like a name: "Fruit unknown.", "Any month.", "Leaves compound")
    if cm and (cm.group(1) in NOT_GENUS or cm.group(1) in {"Any", "Leaves", "Leaf", "Fruit", "Flower", "Flowers", "Inflorescence", "Season", "Seeds", "Calyx", "Petals", "Notes"}
               or cm.group(3) in NOT_EPITHET or not (genus and similar(cm.group(1), genus) or cm.group(1) in HEADED_GENERA)):
        cm = None
    if cm:
        nx = stream[i + 1] if i + 1 < len(stream) else None
        if r["gap_below"] > 0.02:
            L(f"p{r['page']} caption dropped: {t}")
            i += 1
            continue
        if nx and re.fullmatch(r"(subsp|var|f)\.\s+[a-z-]+|\([^)]*\)", nx["text"].strip()) and nx["gap_below"] > 0.02:
            L(f"p{r['page']} caption dropped: {t} / {nx['text']}")
            i += 2
            continue
    m = NAME_RE.match(t)
    if m and (m.group(2) in NOT_GENUS or m.group(4) in NOT_EPITHET
              or not (genus and similar(m.group(2), genus) or m.group(2) in HEADED_GENERA)):
        m = None
    if m and not m.group(5).strip(" ."):
        nm = f"{m.group(2)} {m.group(4)}"
        k1 = next_over(i)
        nx = stream[k1]["text"] if k1 is not None else ""
        block_next = (not re.search(r"[a-z]", nx) and len(nx) > 3) or re.match(r"^Other names?\s*:|^(subsp|var)\.", nx) or is_habit(nx) or re.match(rf"^{re.escape(m.group(4))}\s+(from|after)\b", nx) \
            or (AUTH_RE.match(nx) and not LABEL_RE.match(nx) and len(nx) < 45)
        auth_next = k1 is not None and stream[k1]["gap_above"] < 0.012 and ((AUTH_RE.match(nx) and not LABEL_RE.match(nx) and len(nx) < 45)
                                                                              or re.match(r"^(subsp\.|var\.|f\.|forma)\s+[a-z-]+", nx))
        if (nm in HEADED and m.group(4) != "sp." and not auth_next) or (m.group(4) != "sp." and not block_next) or (m.group(4) == "sp." and not (block_next or re.match(r"^\(", nx))):
            L(f"p{r['page']} caption dropped: {t}")
            i += 1
            continue
    if m and genus and m.group(2) != genus and not similar(m.group(2), genus) and GENUS_N[m.group(2)] >= 2 and m.group(4) not in NOT_EPITHET and r["gap_above"] > 0.012:
        # (a misread genus heading - "DIOSPY.", "YGIUM", "HIUM": its text is kept)
        keep = m.group(2).startswith(genus[:4]) or (len(genus) >= 3 and m.group(2).lower().endswith(genus.lower()))
        L(f"p{r['page']} genus {m.group(2)} from a species heading (current genus {genus}{', misread' if keep else ''}): {t}")
        genus, genus_auth = m.group(2), ""
        if not keep:
            genus_text = []
    if m and m.group(2) not in NOT_GENUS and m.group(4) not in NOT_EPITHET and genus and not similar(m.group(2), genus) and r["gap_above"] > 0.012 \
            and m.group(4) != "sp." and re.match(r"^\s*(\(|[A-Z][a-z]*\.|[A-Z]\.[A-Z]|[A-Z][a-z]+ ex |L\.?$)", m.group(5)) \
            and any(re.match(rf"^{re.escape(m.group(4))}\s+(from|after)\b", stream[k]["text"]) or (not re.search(r"[a-z]", stream[k]["text"]) and len(stream[k]["text"]) > 3)
                    for k in range(i + 1, min(i + 4, len(stream)))):
        L(f"p{r['page']} genus {m.group(2)} from a species heading (no genus heading read): {t}")
        genus, genus_auth, genus_text = m.group(2), "", []
    if m and m.group(2) not in NOT_GENUS and m.group(4) not in NOT_EPITHET and genus and similar(m.group(2), genus) and r["gap_above"] > 0.012 and not (r["gap_below"] > 0.03 and not m.group(5).strip()):
        name = f"{genus} " + (f"x " if m.group(3) else "") + m.group(4)
        auth = m.group(5).strip()
        # phrase names: "Garcinia sp. (Davies Creek)", "Crudia sp. (Archer River)" (the bracket is part of the name; it may run onto the next line)
        if m.group(4) == "sp.":
            mm = re.match(r"^(\([^)]*\)?)\s*(.*)$", auth)
            if mm:
                name += " " + mm.group(1); auth = mm.group(2)
                while not name.endswith(")") and i + 1 < len(stream) and stream[i + 1]["gap_above"] < 0.012:
                    i += 1; name += " " + stream[i]["text"].strip()
        j = i + 1
        k1 = next_over(i)
        if k1 is not None and k1 > j:
            # (side-by-side lines - key answers - between the heading and its next line are moved after the heading block)
            side = stream[j:k1]
            del stream[j:k1]
            stream[j + 2:j + 2] = side
        # name continued on the next lines: infraspecific rank, phrase-name brackets or the authority
        while j < len(stream) and stream[j]["gap_above"] < 0.012:
            u = stream[j]["text"]
            if re.match(r"^(subsp\.|var\.|f\.|forma)\s", u):
                mm = re.match(r"^((?:subsp\.|var\.|f\.|forma)\s+[a-z-]+)(.*)$", u)
                name += " " + mm.group(1); auth = mm.group(2).strip() or auth
            elif m.group(4) == "sp." and re.match(r"^\(", u) and not name.endswith(")"):
                name += " " + u.strip()
            elif AUTH_RE.match(u) and not LABEL_RE.match(u):
                auth = (auth + " " + u).strip()
            else:
                break
            j += 1
        # an infraspecific name written after the species authority: "Atractocarpus fitzalanii (F. Muell.) Puttock subsp. fitzalanii",
        # "Ailanthus integrifolia" / "Lam. subsp. integrifolia" (the authority kept is the infraspecific one when given, else the species one)
        mm = re.search(r"\s*\b((?:subsp\.|var\.|f\.|forma)\s+(?!ex\b)[a-z-]+)\s*(.*)$", auth)
        if mm and not re.search(r"\b(?:subsp\.|var\.|f\.|forma)\s", name):
            name += " " + mm.group(1)
            auth = mm.group(2).strip() or auth[:mm.start()].strip()
        if cur is not None and (cur["taxon_name"] == name or (cur["taxon_name"].startswith(name + " ") and not auth)):
            L(f"p{r['page']} caption of the current entry dropped: {t}")
            i = j
            continue
        # (an infraspecific phrase name in brackets after the authority: "Rapanea subsessilis (F. Muell.) Mez (subsp. Gordonvale)")
        mm = re.search(r"\s*(\((?:subsp|var)\.\s+[A-Z][^)]*\))\s*$", auth)
        if mm:
            name += " " + mm.group(1); auth = auth[:mm.start()].strip()
        cur = dict(page=r["page"], family=family, genus=genus, name_line=t, taxon_name=name, authority=auth, introduced=bool(m.group(1)),
                   common=[], etym=[], other=[], body=[], stage="head", genus_text=" ".join(genus_text))
        entries.append(cur)
        i = j
        continue
    # genus-only line ("Erythrophleum" / "chlorostachys" / "(F. Muell.) Baill.")
    if genus and re.fullmatch(r"\*?\s*[A-Z][a-z]+", t) and similar(t.lstrip("* "), genus) and i + 1 < len(stream) and re.match(r"^[a-z][a-z-]+\b", stream[i + 1]["text"]) and r["gap_above"] > 0.012:
        stream[i + 1]["text"] = t + " " + stream[i + 1]["text"]
        stream[i + 1]["gap_above"] = r["gap_above"]
        i += 1
        continue
    if cur is not None and cur["stage"] != "key" and re.match(r"^\d{1,2}\.\s+[A-Z]", t):
        L(f"p{r['page']} key starts after {cur['taxon_name']} ({cur['stage']}): {t}")
        cur["resume"] = cur["stage"]; cur["stage"] = "key"
    if cur is not None and cur["stage"] == "key":
        # (resume only below the key block - a gap above the line - and not on a second couplet lead, "Fruit receptacle conspicuous"
        # / "...C. iripa", nor before another couplet)
        nxt2 = [stream[k]["text"] for k in range(i + 1, min(i + 3, len(stream)))]
        keyish = bool(re.search(r"\.{2,}|\u2026|\s\*?[A-Z]\.\s?[a-z-]+$|\s\d{1,2}$", t)) or any(re.match(r"^\d{1,2}\.\s", x) for x in nxt2) \
            or any(re.fullmatch(r"\.*\s*\*?\s*[A-Z]\.\s?[a-z-]+.*|\.*\s*\d{1,2}", x.strip()) for x in nxt2[:1])
        # (and only with a field the entry has not reached yet: a finished entry - Distribution read - never resumes)
        lm = LABEL_RE.match(t)
        ahead = (is_habit(t) and not cur["body"]) or (lm and not t.startswith("Leaf ") and field_rank(lm.group(1)) > last_rank(cur))
        if ahead and r["gap_above"] > 0.009 and not keyish:
            L(f"p{r['page']} {cur['taxon_name']} resumes after the key: {t}")
            cur["stage"] = "body"
        else:
            genus_text.append(t)
            i += 1
            continue
    if cur is None:
        genus_text.append(t)
        i += 1
        continue
    if cur["stage"] == "head":
        # heading block: common names (capitals), etymology, "Other names: ..."; the description starts at the habit sentence
        # ("Tree to 6 m, ...", "Climbing palm ...", "Lily to 1 m ...") or, without one, at the first field label
        if (LABEL_RE.match(t) and not t.startswith("Leaf ")) or is_habit(t):
            cur["stage"] = "body"
            continue
        if re.match(r"^Other names?\s*:", t) or cur["other"]:
            cur["other"].append(t)
        elif not re.search(r"[a-z]", t) and not cur["etym"]:
            cur["common"].append(t)
        else:
            cur["etym"].append(t)
        i += 1
        continue
    cur["body"].append(t)
    i += 1


# words seen whole inside lines, to tell a line-break hyphen ("natu-" "ralised", "Cas-" "sowaries") from a compound split at its
# hyphen ("Rat-" "kangaroos", "fruit-" "doves", "Flying-" "foxes")
WORDS = Counter()
for r in stream:
    WORDS.update(w.lower() for w in re.findall(r"[A-Za-z]+(?:-[A-Za-z]+)*", r["text"])[:-1])


def join_lines(lines):
    out = ""
    for ln in lines:
        ln = ln.strip()
        if not out:
            out = ln
        elif out.endswith("-") and not re.search(r"\d-$", out) and re.match(r"^[a-z]", ln):
            a = re.search(r"([A-Za-z]+(?:-[A-Za-z]+)*)-$", out)
            b = re.match(r"[a-z]+(?:-[a-z]+)*", ln)
            a, b = (a.group(1) if a else ""), b.group(0)
            if WORDS[(a + "-" + b).lower()] > WORDS[(a + b).lower()]:
                out = out + ln  # a compound split at its hyphen
            else:
                out = out[:-1] + ln  # line-break hyphen
        else:
            out += " " + ln
    return out


# an entry split in two by a misread caption ("Rapanea subsessilis (subsp. Gordonvale)" twice on p. 337): the second part's lines go to the first
merged = []
def clipped(a, b):
    # (a name clipped at a column edge: "Bridelia leichhardt" beside "Bridelia leichhardtii", "Wrightia salig" / "Wrightia saligna")
    pa, pb = a.split(), b.split()
    return len(pa) == len(pb) == 2 and pa[0] == pb[0] and pa[1] != pb[1] and len(min(pa[1], pb[1], key=len)) >= 4 and \
        (pa[1].startswith(pb[1]) or pb[1].startswith(pa[1]))
for e in entries:
    if merged and clipped(e["taxon_name"], merged[-1]["taxon_name"]):
        full = max(e["taxon_name"], merged[-1]["taxon_name"], key=len)
        L(f"p{e['page']} clipped name: {e['taxon_name']} / {merged[-1]['taxon_name']} merged as {full}")
        merged[-1]["taxon_name"] = full
        merged[-1]["body"] += e["common"] + e["etym"] + e["other"] + e["body"]
        continue
    if merged and e["taxon_name"] == merged[-1]["taxon_name"]:
        merged[-1]["body"] += e["common"] + e["etym"] + e["other"] + e["body"]
        L(f"p{e['page']} second part of {e['taxon_name']} merged into the first")
        continue
    merged.append(e)
entries = merged
names = Counter(e["taxon_name"] for e in entries)
kept = []
for e in entries:
    if not e["body"] and not e["etym"] and not e["common"]:
        if names[e["taxon_name"]] > 1:
            L(f"p{e['page']} empty entry dropped (name has another entry): {e['taxon_name']}")
            names[e["taxon_name"]] -= 1
            continue
        L(f"p{e['page']} empty entry kept: {e['taxon_name']}")
    kept.append(e)
entries = kept
rows = []
for e in entries:
    habit, fields, curf = [], defaultdict(list), None
    for ln in e["body"]:
        m = LABEL_RE.match(ln)
        if m and not (curf and re.match(r"^Leaf\b", ln)):
            lab = m.group(1)
            lab = {"Leaf": "Leaves", "Flowers": "Flower", "Fruits": "Fruit", "Inflorescences": "Inflorescence", "Figs": "Fruit"}.get(lab, lab)
            curf = lab
            fields[curf].append(("figs " if m.group(1) == "Figs" else "") + m.group(2))
        elif curf:
            fields[curf].append(ln)
        else:
            habit.append(ln)
    f = {k: join_lines(v) for k, v in fields.items()}
    if not habit and not f:
        L(f"p{e['page']} entry with no description dropped: {e['taxon_name']} | {e['name_line']} | {join_lines(e['common'] + e['etym'])[:80]}")
        continue
    rows.append(dict(book_page=e["page"], family=e["family"], genus=e["genus"], taxon_name=e["taxon_name"], authority=e["authority"],
                     introduced="introduced" if e["introduced"] else "", common_name=join_lines(e["common"]), etymology=join_lines(e["etym"]),
                     other_names=re.sub(r"^Other names?\s*:\s*", "", join_lines(e["other"])), habit=join_lines(habit),
                     leaves=f.get("Leaves", ""), inflorescence=f.get("Inflorescence", ""), flower=f.get("Flower", ""),
                     male_cones=f.get("Male cones", ""), female_cones=f.get("Female cones", ""), male_flowers=f.get("Male flowers", ""),
                     female_flowers=f.get("Female flowers", ""), fruit=f.get("Fruit", ""), notes=f.get("Notes", ""), distribution=f.get("Distribution", ""),
                     genus_text=e["genus_text"], name_line=e["name_line"]))

# a name with two entries (a key answer read as a heading inside a family key - "Dischidia littoralis" p. 31, "Tetrasynandra sp. (Mt Lewis)"
# p. 316): the entry with the most fields filled is kept
FIELDS = ["habit", "leaves", "inflorescence", "flower", "fruit", "notes", "distribution"]
best = {}
for k, r in enumerate(rows):
    sc = sum(bool(r[f]) for f in FIELDS)
    if r["taxon_name"] not in best or sc > best[r["taxon_name"]][0]:
        best[r["taxon_name"]] = (sc, k)
for k, r in enumerate(rows):
    if best[r["taxon_name"]][1] != k:
        L(f"p{r['book_page']} duplicate entry dropped (fewer fields than p{rows[best[r['taxon_name']][1]]['book_page']}): {r['taxon_name']}")
rows = [r for k, r in enumerate(rows) if best[r["taxon_name"]][1] == k]
# QA: entries per genus against the count in the genus text
gcount = Counter(r["genus"] for r in rows)
gtext = {}
for r in rows:
    gtext.setdefault(r["genus"], r["genus_text"])
with open(f"{OUT}/genus_counts.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["genus", "entries", "book_count", "match"])
    for g in sorted(gcount):
        mm = re.search(r"Rainforest tropical (?:Queensland|Qld|Old)\s*:\s*(?:about\s+)?(\d+)\s+species", gtext[g] or "")
        bc = int(mm.group(1)) if mm else ""
        w.writerow([g, gcount[g], bc, "" if bc == "" else gcount[g] == bc])
cols = list(rows[0].keys())
with open(f"{OUT}/cooper2004_profiles.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=cols)
    w.writeheader()
    w.writerows(rows)
L(f"{len(rows)} taxa")
print(f"{len(rows)} taxa; log in {OUT}/assemble_log.txt")
