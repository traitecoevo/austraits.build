"""Eremophila (R.J. Chinnock 2007, Eremophila and allied genera), the taxon accounts: OCR'd lines (lines/<tag>_<page>.tsv from page.py)
-> one row per taxon.
Book pages: Eremophile 093-300.pdf (tag a) is book p. 93 + (page - 1) to file p. 52 (book p. 144), then + 93 (book p. 145 is missing from
every scan); 301-500 (b) + 300; 501-650 (c) + 500. Eremophila 2.pdf (r) holds rescans of 10 pages (by their printed number, or by word overlap
for the unnumbered one, r 5 = book p. 346): the scan with more text is used.
Dropped: map axis labels ("120°"), map captions ("Map 161. Distribution of" / "Eremophila metallicorum"), photo captions ("Eremophila
metallicorum. Flowering branches"), figure plates (pages of 15 lines or fewer). Pieces of one visual line are joined; line-break hyphens are
resolved by the words seen whole elsewhere in the book.
An entry starts at its heading - "86. Eremophila metallicorum S. Moore, J. Linn. Soc., Bot. 34: 213 (1899)" (genus names below), or an
infraspecific "3b. Subsp. australe Chinnock, ..." of the species above - and runs to the next. Its parts: the citations; Type; the
description (from the first habit sentence after the last "Type:", "Low intricate shrub 0.4-0.8 m tall. Branches ..."); and the labelled
sections DISTRIBUTION AND ECOLOGY, CONSERVATION STATUS, DERIVATION OF EPITHET / NAME, NOTES, SELECTED SPECIMENS / SPECIMENS EXAMINED.
usage: entries.py  ->  ../../chinnock2007_profiles.csv"""
import csv, difflib, glob, os, re
from collections import Counter
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GENERA = "Myoporum|Bontia|Pentacoelium|Diocirea|Glycocystis|Calamphoreus|Sentis|Eremophila"


def ov(a, b):
    return min(a[3], b[3]) - max(a[1], b[1]) > 0.5 * min(a[3] - a[1], b[3] - b[1])


def load(f):
    rows, pn = [], None
    nz = lambda t: re.sub(r"[^a-z0-9]", "", t.lower())
    for ln in open(f, encoding="utf-8"):
        p = ln.rstrip("\n").split("\t")
        if p[1] == "pageno":
            pn = int(p[7]) if p[7].strip().isdigit() else None
            continue
        r = [p[1], float(p[2]), float(p[3]), float(p[4]), float(p[5]), p[7]]
        # (Vision sometimes returns a line twice, once whole and once from a column crop: the copy is dropped)
        if any(q[0] == r[0] and ov(q[1:5], r[1:5]) and (nz(q[5]) == nz(r[5]) or (len(nz(r[5])) > 10 and difflib.SequenceMatcher(None, nz(q[5]), nz(r[5])).ratio() > 0.9)) for q in rows):
            continue
        rows.append(r)
    return rows, pn


def merge_scans(base, other):
    nz = lambda t: re.sub(r"[^a-z0-9]", "", t.lower())
    bn = [nz(r[5]) for r in base]
    def match(t):
        t = nz(t)
        if len(t) < 4: return -1
        for i, b in enumerate(bn):
            if b == t or (len(b) > 8 and difflib.SequenceMatcher(None, b, t).quick_ratio() > 0.85 and difflib.SequenceMatcher(None, b, t).ratio() > 0.8):
                return i
        return -1
    after = {}      # base index -> lines to insert after it, in the other scan's order
    last = -1
    for r in other:
        i = match(r[5])
        if i >= 0:
            last = i
        elif len(nz(r[5])) >= 12 and last >= 0:
            after.setdefault(last, []).append([base[last][0]] + r[1:])      # (with the anchor line's column)
    out = []
    for i, r in enumerate(base):
        out.append(r); out += after.get(i, [])
    return out


# book page -> lines
book = {}
for f in sorted(glob.glob(f"{ROOT}/lines/[abc]_*.tsv")):
    tag, pg = os.path.basename(f)[:-4].split("_"); pg = int(pg)
    bp = {"a": pg + (92 if pg <= 52 else 93), "b": pg + 300, "c": pg + 500}[tag]
    book[bp] = (f, load(f)[0])
W = lambda rows: set(re.findall(r"[a-z]{6,}", " ".join(r[5] for r in rows).lower()))
for f in sorted(glob.glob(f"{ROOT}/lines/r_*.tsv")):
    rows, pn = load(f)
    if pn is None or pn not in book:
        w = W(rows)
        pn = max(book, key=lambda b: len(w & W(book[b][1])) / max(1, len(w)))
    # (each scan drops a few lines the other has: the one with more text is the base, and the other's lines it lacks are inserted after the
    # line they follow there)
    base, other = (rows, book[pn][1]) if sum(len(r[5]) for r in rows) > sum(len(r[5]) for r in book[pn][1]) else (book[pn][1], rows)
    book[pn] = (f if base is rows else book[pn][0], merge_scans(base, other))
pages_used = {b: os.path.basename(book[b][0]) for b in book}

def reorder(rows):
    # reading order again without the figure legends (dropped below anyway): a full-width legend at the foot of a page is not a break between
    # column bands - the left column's last lines ("SELECTED SPECIMENS / WESTERN AUSTRALIA. Three Springs ...", p. 267) come before the right column
    leg = lambda r: r[0] == "full" and re.match(r"^\s*F[Ii][Gg]\.?\s*\d|.*\bS[Cc][Aa][Ll][Ee]\s?:|.*\b[A-Z](?:-[A-Z])?,\s(?:side|front|outer|inner|enlarged|branching|cross-section)", r[5])
    rows = [r for r in rows if not leg(r)]
    out, band = [], []
    def flush():
        out.extend(sorted([r for r in band if r[0] == "left"], key=lambda r: r[2]) + sorted([r for r in band if r[0] != "left"], key=lambda r: r[2]))
    for r in sorted(rows, key=lambda r: r[2]):
        if r[0] == "full":
            flush(); band.clear(); out.append(r)
        else:
            band.append(r)
    flush()
    return out


def drop_keys(rows):
    # keys are not read, and their right-hand answers ("E. dichroantha", ".10") would run into the taxon text. A key block: the vertical stretch
    # holding 3+ couplet numbers ("8.", "10", "11.", or "8. Leaves ..."), with gaps under 0.06; dropped across the page if its lines cross the
    # gutter, else in its column only.
    nums = sorted((r for r in rows if re.fullmatch(r"\d{1,3}\.?", r[5].strip()) or re.match(r"^\d{1,3}\.\s+(?:Leaves|Branches|Sepals|Corolla|Flowers|Plants?|Stamens|Fruit|Pedicels?|Outer|Inner|Shrubs?|Vegetative|In)\b", r[5])), key=lambda r: r[2])
    blocks, cur = [], []
    for r in nums:
        if cur and r[2] - cur[-1][2] > 0.06:
            blocks.append(cur); cur = []
        cur.append(r)
    if cur: blocks.append(cur)
    for b in blocks:
        if len(b) < 3: continue
        # (map axis labels, "120", "130", or "10", "20", also look like numbers: a key's numbers line up vertically and go up by 0 or 1)
        ints = [int(re.match(r"\d+", r[5]).group()) for r in b]
        if max(r[1] for r in b) - min(r[1] for r in b) > 0.03 or sum(1 for u, v in zip(ints, ints[1:]) if 0 <= v - u <= 1) < 2:
            continue
        y0, y1 = b[0][2] - 0.005, b[-1][4] + 0.03
        band = [r for r in rows if y0 <= (r[2] + r[4]) / 2 <= y1]
        lead_x = sum(r[1] for r in b) / len(b)
        cross = any(r[1] < 0.3 and r[3] > 0.56 for r in band) or any(r[0] == "full" for r in band)
        side = "left" if lead_x < 0.45 else "right"
        rows = [r for r in rows if not (y0 <= (r[2] + r[4]) / 2 <= y1 and (cross or r[0] == side))]
    return rows


# a section's (or genus's) opening above the first entry on its page - heading, Latin and English diagnoses, map, key to species - is set full
# width but cut by the column crops where a word gap falls at the gutter, and its right-hand pieces were read after the left column, inside the
# first entry's description ("... cream tubular flowers. The glabrous relationship to Sect. Crustacea. or 3 prominent teeth near apex ... .1. E.
# compressa", p. 187): read from its start to the entry's heading line by line, both columns, as full-width text (cut off from the entry before
# it later, by SECT_START)
SECT_LINE = re.compile(r"^(?:[IVXLl1]+\.\s+)?(?:" + GENERA + r")\s+sect\.\s+[A-Z][a-z]+(?:\s+(?:Chinnock|Benth|Kraenzlin|F\.\s?Muell|R\.\s?Br|\(|A\.\s?DC|DC\.|L\.\s?S\.)|\s*$)|"
                       r"^(?:Lecto)?[Tt]ype species:|^\d\.\s+(?:" + GENERA + r")\s+(?:L\.|Sol\.|R\.\s?Br|Chinnock,\s+gen|Sieb)")
HEAD_LINE = re.compile(r"^\d{1,3}[a-z]?\.\s?(?:(?:" + GENERA + r")\s+[a-z]|Subsp\.)")


def sect_block(rows):
    # (not a running head read in the body, "XV. Eremophila sect. Decussatae" at the top margin, left or right: the section's own title is set
    # lower, centred)
    ss = [r[2] for r in rows if SECT_LINE.match(r[5].strip()) and not (re.match(r"^[IVXLl1]+\.\s+\S+\s+sect\.\s+\S+\s*$", r[5].strip()) and (r[1] < 0.15 or r[2] < 0.04))]
    if not ss: return rows
    hs = [r[2] for r in rows if HEAD_LINE.match(r[5].strip()) and r[2] > min(ss)]
    if not hs: return rows
    for r in rows:
        # (a right-hand line level with the heading's top is the column's text: "faces, slightly verrucose, 5.5-6 x", p. 187)
        if min(ss) - 0.002 <= r[2] < min(hs) - 0.012: r[0] = "full"
    return rows


CAP_MAP = re.compile(r"^Map\s+\d+[a-z]?\.")
CAP_PHOTO = re.compile(r"^(?:" + GENERA + r")\s+(?:sp\.\s+)?[a-z][a-z-]+(?:\s+(?:subsp|var)\.\s+[a-z][a-z-]+)?\.\s+[A-Z][^.]{0,70}\.?(?:\s+[A-Z][^.]{0,70}\.?)?$")
AXIS = re.compile(r"^[\d\s°'′.,|-]{1,12}$")
stream = []   # (book page, text)
for bp in sorted(book):
    rows = book[bp][1]
    if len(rows) <= 15:      # (a figure plate: captions only)
        continue
    # (a full-page figure: its legend, "FIG. 100 ...", and scale labels, with few text lines - its legend would run into the text around it)
    # (text lines of 25+ characters: a column beside a figure has short lines, p. 266)
    if any(re.match(r"^F[Ii][Gg]\.?\s*\d", r[5]) for r in rows) and sum(1 for r in rows if len(r[5]) >= 25 and not re.match(r"^F[Ii][Gg]", r[5])) <= 8:
        continue
    rows = reorder(drop_keys(sect_block(rows)))
    out, pcs = [], []
    skip_next = skip_cap = False
    for r in rows:
        # (map axis labels at a column's edge were joined to text lines as key answers: " ...... 120°", " ...... -40", " ...... 14C")
        r[5] = re.sub(r"\s*\.{6}\s+(?=\S*\d)[-\d°'′*.,|CcO]{1,6}(?=\s|$)", " ", r[5])
        r[5] = re.sub(r"\s*\.{6}\s*", " ", r[5])
        # (a photo caption joined to the end of a text line: "... lanate in upper half, Eremophila nivea. Flowering branc")
        r[5] = re.sub(r"\s+(?:" + GENERA + r")\s+[a-z][a-z-]+(?:\s+(?:subsp|var)\.\s+[a-z][a-z-]+)?\.\s+(?:Flowering|Flowers?|Habit|Fruits?|Branches|Branch|Leaves|Plants?|Close-up|Habitat)\b[^.]*\.?$", "", r[5])
        # (a map caption joined to a text line: "valleys. Map 67 Distribution of")
        r[5] = re.sub(r"\s*\bMap\s+\d+[a-z]?\.?\s*(?:Distribution(?:\s+of)?)?\s*$", "", r[5])
        t = r[5].strip()
        # (figure legends set beside or under a figure: "FIG. 97 Eremophila fallax. A, branching habit; B-E, ...", "... SCALE: A, 50 mm; B, 6
        # mm", "E, G-H, R.J. Chinnock 4526 (AD).")
        # (any line opening "Eremophila x. ..." is a caption; generic legend lettering, "D, adaxial view of leaf"; scale bars "m c 10")
        if len(re.findall(r"\b[A-Z](?:-[A-Z])?,\s\d[\d.]*\s?mm", t)) >= 2 or re.match(r"^\d[\d.]*\s?mm;\s[A-Z]", t) or \
           (len(t) < 120 and re.match(r"^(?:" + GENERA + r")\s+(?:sp\.\s+)?[a-z][a-z-]+(?:\s+(?:subsp|var)\.\s+[a-z][a-z-]+)?\.\s", t)) or re.search(r"\bScale units\b", t) or \
           re.search(r"(?:^|[;.]\s?)[A-Za-z](?:-[A-Z])?,\s(?:front|side|outer|inner|longitudinal|transverse|cross|enlarged|portion|branching|abaxial|adaxial|habit|leaf|flower|fruit|sepal|corolla|gynoecium|ovary|anther|stamen|seed|surface|view)\b", t) or \
           re.search(r"(?:^|[;.]\s)[A-Z](?:-[A-Z])?,\s[a-z]{3,}", t) or re.match(r"^m\s?c\s?\d", t) or \
           re.search(r"\b(?:cross-sections?|longitudinal sections?|front view|side view|inner surface|outer surface)\b[^.;]*\bshowing\b|\bshowing (?:two |the )?(?:variations?|leaf|hairs?)\b", t) or \
           (len(t) < 90 and re.search(r"\bPhoto:?\s", t)) or re.search(r"(?:^|;\s)[A-Z](?:-[A-Z])?(?:,\s[A-Z](?:-[A-Z])?)*,\s(?:[A-Z]\.){1,3}\s?[A-Z][a-z]+\s\d", t) or \
           re.search(r"\bS[Cc][Aa][Ll][Ee]\s?:", t) or re.search(r"(?:^|[;.]\s)[A-Z](?:-[A-Z])?(?:,\s[A-Z](?:-[A-Z])?)*,\s(?:side|front|outer|inner|cross-section|portion|"
                                                                     r"enlarged|branching|leaf|leaves|habit|branch|flower|fruit|corolla|sepal|gynoecium|ovary|"
                                                                     r"anther|seed|stamen|view|surface|hair)", t):
            continue
        if skip_next:
            skip_next = False
            if re.match(r"^(?:" + GENERA + r")\s+[a-z]", t) and len(t) < 60:
                continue
        # (and figure scale bars: "m 1c", "mm 3", "5 cm")
        if AXIS.match(t) or re.match(r"^(?:FIG|Fig)\.?\s*\d+\b", t) or re.match(r"^SCALE\b", t) or \
           (len(t) <= 8 and re.fullmatch(r"(?:\d{1,3}\s*)?[cm]?m(?:\s*\d{1,3})?(?:\s*[cm]{1,2})?", t)) or \
           re.fullmatch(r"[A-Z]{1,2}(?:[\s,]+[A-Z]{1,2}){0,7}", t) or re.fullmatch(r"\d{1,2}[,.]?\d?\s?[cm]?m\.?", t) or \
           (re.fullmatch(r"[A-Z]{3,5}", t) and t not in ("NOTES", "AND", "TYPE")):
            continue
        # (map captions, whole or in pieces: "Map 74." / "Distribution of" / "Eremophila ciliata")
        if CAP_MAP.match(t) or re.fullmatch(r"(?:Map\s+\d+[a-z]?\.\s*)?Distribution\s+of", t):
            skip_next = not re.search(r"(?:" + GENERA + r")\s+[a-z]", t)
            continue
        if CAP_PHOTO.match(t) and len(t) < 90:
            skip_cap = True
            continue
        # (a caption's second line, its place: "Diocirea ternata. Flowering branch." / "Balladonia, WA")
        if skip_cap:
            skip_cap = False
            if len(t) < 45 and re.search(r"(?:,\s*|\b)(?:WA|SA|NT|Qld|NSW|Vic|Tas|Queensland|Victoria|Tasmania|Hawaii|Photo:.*)\.?$", t):
                continue
        # (pieces of one visual line joined in x order, however many: "Leaves" / "prominently raised, decurrent," / "non-tuberculate.", p. 338)
        if out and out[-1][0] == r[0] and ov(out[-1][1:5], r[1:5]):
            pcs[-1] = sorted(pcs[-1] + [(r[1], r[5])])
            a = out[-1]
            out[-1] = [r[0], min(a[1], r[1]), min(a[2], r[2]), max(a[3], r[3]), max(a[4], r[4]), " ".join(t for _, t in pcs[-1])]
        else:
            out.append(r); pcs.append([(r[1], r[5])])
    stream += [(bp, r[5]) for r in out]

words = Counter(w.lower() for _, t in stream for w in re.findall(r"[A-Za-z]+(?:-[A-Za-z]+)*", t)[1:-1])


NUM = r"\d[\d.]*"
RNG = rf"{NUM}(?:\s*-\s*{NUM})?"
DIM = rf"(?:\(\s*{NUM}\s*-?\s*\)\s*)?{RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?\s*[×x]\s*(?:(?:ca|c)\.?\s*)?(?:\(\s*{NUM}\s*-?\s*\)\s*)?{RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?"


ORGANS = [("Leaves", True), ("Sepals", False), ("Ovary", False), ("Fruit", False), ("Seed", False)]


def fix_forms(x):
    # extremes and OCR forms, before anything reads the sizes
    x = re.sub(r"\bca\s+l\b", "ca 1", x)                                                             # "ca l mm" (l for 1)
    x = re.sub(r"(?<=[\s(])G(?=-\d)", "6", x)                                                         # "(4-) G-17"
    x = re.sub(r"\(\s*-\s*(\d[\d.]*)\s*(m|cm|mm)\s*\)", r"(-\1) \2", x)                             # "0.5-1.5 (-2.5 m) tall"
    x = re.sub(r"\(\s*(\d[\d.]*)\s*-\s*\)", r"(\1-)", x)                                           # "(2.5- )"
    x = re.sub(r"\(\s*-\s*(\d[\d.]*)\s*\)", r"(-\1)", x)                                           # "(- 8)"
    x = re.sub(rf"\((\d[\d.]*)\)\s+(?={RNG}\s*(?:\(|[×x]|mm|cm|m\b))", r"(\1-) ", x)               # "(6) 8-16" = (6-) 8-16
    x = re.sub(rf"(?<=\d)\s+\((\d[\d.]*)\)(?=\s*(?:[×x]|mm|cm|m\b))", r" (-\1)", x)                # "19.5 (23.5) ×" = (-23.5)
    x = re.sub(rf"(?<![(\d.-])(?<=\s)(\d[\d.]*)\)\s+(?={RNG}\s*(?:\(|[×x]))", r"(\1-) ", x)         # "18) 21-48" = (18-) 21-48
    x = re.sub(r"(?<=[\d)])\s*x\s*(?=[\d(c])", " × ", x)                                            # x between sizes
    # (a "×" read as "-" after "c.": "Seed ovoid, c. 2.5-0.7 mm", "c. 3.5- 1 mm" - the second number is smaller, so it is a width)
    x = re.sub(r"\b(ca?\.?)\s*(\d[\d.]*)\s*-\s*(\d[\d.]*)(?=\s*mm)", lambda m: f"{m.group(1)} {m.group(2)} × {m.group(3)}" if float(m.group(3)) < float(m.group(2)) else m.group(0), x)
    return x


# garbled passages checked against the page images (map and caption text read into a size), by taxon: (pattern, replacement)
FIX_TEXT = {
    "Eremophila duttonii": [(r"4\s*[x×]\s*-\s*n\s*(?:\S*\s*)?1\.5 mm", "4 × 1.5 mm")],
    "Myoporum parvifolium": [(r"margins entire\W+or sparsely serrate in distal half,[^(]*(\(10-\).*?3-6\.5)\s*Myoporum parvifolium\s*(\(-8\) mm)",
                              r"margins entire or sparsely serrate in distal half, \1 \2")],
    "Myoporum oppositifolium": [(r"leaf base broad, auriculate, spreading, (ovate to narrow-lanceolate)", r"spreading, \1; leaf base broad, auriculate")],
    # (p. 335: the photo's curled page edge hides the first letters of the left column's lines; "Erect" is the only reading)
    "Eremophila phyllopoda": [(r"\bect, round or flat-topped shrub", "Erect, round or flat-topped shrub")],
    # (the book prints "very widely ovate, 8.5-26 × 90-24 mm" for the posterior sepal, a misprint of 9.0-24: the other pairs are 4.5-16 and 2-11 mm wide)
    "Eremophila cuneifolia": [(r"8\.5-26 × 90-24 mm", "8.5-26 × 9.0-24 mm")],
    # (p. 492-494: the label "Leaves" closes p. 492 and the text resumes on p. 494 after a full-page figure; the habit's bracketed aside moved
    # after its noun, "Sparsely (with few main stems) to much-branched shrub")
    # (p. 571: the label and the map caption, "Distribution of Eremophila latrobei subsp. latrobei", read in among each other, "DISTRIBUTION latrobei
    # ECOLOGY Eremophila subsp. AND Eremophila latrobei subsp. latrobei is widespread", and the caption's words taken out with the subject's)
    # (p. 337: the right-hand half of a figure legend line read into the habit sentence)
    "Eremophila rigens": [(r"\s*and outer \(below\) surface of fruiting sepal: note branched hairs at right", "")],
    "Eremophila latrobei subsp. latrobei": [(r"Eremophila subsp\. AND latrobei is widespread", "AND Eremophila latrobei subsp. latrobei is widespread")],
    "Eremophila forrestii subsp. capensis": [(r"Sparsely \(with few main stems\) to much-branched shrub", "Sparsely to much-branched shrub (with few main stems)"),
                                             (r"\bLeave\b[^;]*?(?=broadly elliptic to orbicular)", "Leaves ")],
}


HAIRY = r"\b(?:glabrous|glabrescent|hairs?|hairy|tomentum|tomentose|pubescen\w*|puberul\w*|velutinous|villous|hirsute|lanate|indumentum|lepidote|scales|stellate|dendritic)\b"


def share_hairs(x):
    # one indumentum stated for the branches and leaves together is each organ's: the habit sentence's "... shrub 0.5-1.2 m tall with branches,
    # leaves, pedicels and sepals clothed in a silvery-grey simple tomentum ..." and a subspecies' "Branches and leaves velutinous." -> the phrase
    # closes the Branches and the Leaves sentence ("Branches non-tuberculate ...; clothed in a silvery-grey simple tomentum ..."), or is one of
    # its own when the organ has none; to its first ";" and only when it is about hairs ("branches and leaves with ... white salt crystals" is not)
    m = re.search(r"(?:^|(?<=\.\s))(?:Branches|Stems)\s+and\s+leaves\s+([^.;]*?)\.(?=\s|$)", x)
    if m and re.search(HAIRY, m.group(1)):
        return x[:m.start()] + f"Branches {m.group(1)}. Leaves {m.group(1)}." + x[m.end():]
    hs = re.match(r"[^.]*?(?:\.(?=\d)[^.]*?)*(?:\.|$)", x)
    if not hs: return x
    m = re.search(r"\b(?:with\s+)?(?:young\s+)?branches(?:,\s*|\s+and\s+)leaves(?:\s+surfaces)?(?:\s*\([^)]*\))?(?:,?\s*(?:and\s+)?(?:pedicels|flowers|(?:outer\s+(?:and\s+inner\s+)?)?surfaces?\s+of\s+(?:sepals|calyx)|sepals|calyx))*\s+([^;]*?)\.?$",
                  hs.group(0).rstrip("."))
    if not m or not re.search(HAIRY, m.group(1)): return x
    ph = m.group(1).strip(" ,")
    rest = x[hs.end():].lstrip()
    # (as the organ's indumentum part, "; indumentum clothed in a silvery-grey ...": the parser then reads its hairs only, not the hairs' colour,
    # "persistent" or "appressed" as the organ's; at the end of the organ's sentence, not in the clause the leaf's size and shapes are read from,
    # and before a petiole's clause, "; petiole 3-6 mm long", which would take it)
    for lab in ("Branches", "Leaves"):
        ml = re.search(rf"(?:^|(?<=\.\s)){lab}\b(?:[^.]|\.(?=\d))*", rest)
        if ml:
            sen = rest[ml.start():ml.end()].rstrip(" ;,")
            mp = re.search(r";\s*petioles?\b[^;]*$", sen)
            # (and before a branch's leaf-base clause, "..., leaf bases persistent, decurrent", which the parser takes as the subject after it)
            if lab == "Branches": mp = re.search(r"(?:[,;]|\s+but)\s*(?:leaf[- ](?:bases?|scars?|base scars|base attachments)|tubercles?)\b", sen) or mp
            at = ml.start() + (mp.start() if mp else len(sen))
            rest = rest[:at] + "; indumentum " + ph + rest[at:]
        else:
            ins = re.search(r"(?:^|(?<=\.\s))(?:Leaves|Flowers|Inflorescences?)\b", rest) if lab == "Branches" else re.search(r"(?:^|(?<=\.\s))(?:Flowers|Inflorescences?|Sepals)\b", rest)
            at = ins.start() if ins else 0
            rest = rest[:at] + f"{lab} with indumentum {ph}. " + rest[at:]
    return x[:hs.end()] + " " + rest.lstrip()


def norm_desc(x):
    # OCR and print forms of the description, into the forms the parser reads
    x = fix_forms(x)
    # (a petiole's length in the leaf sentence closes its own clause: "shortly petiolate, petioles 1-3 mm long, alternate, lanceolate ..." - the
    # shapes after it are the leaf's)
    # (and moved to the end of the leaf sentence: "shortly petiolate, petioles 1-3 mm long, alternate, lanceolate ..." -> "..., alternate,
    # lanceolate ...; petioles 1-3 mm long." - the shapes after it are the leaf's)
    def mv_pet(m):
        sen = m.group(0); pm = re.search(r"\b(petioles?\s[^,;.]*?\d\s?mm long)[,;]\s*", sen)
        if not pm or pm.end() >= len(sen) - 2: return sen
        return (sen[:pm.start()] + sen[pm.end():]).rstrip(". ") + "; " + pm.group(1) + ". "
    x = re.sub(r"\bLeaves\b(?:[^.]|\.(?=\d))*\.\s*", mv_pet, x)
    x = re.sub(r"\bca[;.,]?\s+(?=\d)", "c. ", x)                                                    # "× ca 1 mm" -> "c."
    x = re.sub(r"\b0,(?=\d)", "0.", x)                                                              # "0,25-0.4 m"
    x = re.sub(r"(?<=\d)\s+(?=tall\b)", " m ", x)                                                   # "0.4-0.6 tall"
    x = re.sub(r"\((-?\d[\d.]*)\.\)", r"(\1)", x)                                                    # "(-3.)"
    # the habit sentence: "shrub 0.2-0.4 × 0.2-1 m" = height × width; a size in m without "tall" is the height ("shrub 1-1.5 (-3) m.", "Shrub 1-3 m,
    # frequently root-suckering ..., or solitary tree 3-8 m tall"); not a diameter or spread
    hs = re.match(r"[^.]*?(?:\.(?=\d)[^.]*?)*(?:\.|$)", x)
    if hs and re.search(r"\b(?:[Ss]hrubs?|[Tt]rees?|[Ss]ubshrubs?|[Hh]erbs?|[Pp]lants?|[Mm]allees?)\b", hs.group(0)):
        h = hs.group(0)
        h = re.sub(rf"({RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?)\s*[×x]\s*({RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?)\s*m\b(?:\s*(?:tall|high))?", r"\1 m tall and \2 m wide", h)
        h = re.sub(rf"({RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?\s*m)\b(?!\s*(?:tall|high|wide|diam|across|in diam|×|x\b))", r"\1 tall", h)
        x = h + x[hs.end():]
    x = re.sub(r"(?<=[a-z0-9)])\s+(?=(?:Flowers|Sepals|Corolla|Stamens|Ovary|Fruit|Seed|Leaves)\s+[a-z(0-9])", ". ", x)  # "... mm long Sepals 5"
    x = share_hairs(x)
    # a pedicel's length closing its clause ("pedicel slightly flattened, tomentose, hairs simple, crisped, 7.5-13 mm long"): next to "pedicel"
    x = re.sub(rf"\b(pedicels?)\s+([^;.]*?),\s*((?:c\.\s*)?(?:\(\s*{NUM}\s*-?\s*\)\s*)?{RNG}(?:\s*\(\s*-?\s*{NUM}\s*\))?\s*(?:mm|cm)\s+long)(?=[.;,])", r"\1 \3, \2", x)
    x = re.sub(r"\(\s*-\s*(\d[\d.]*)\s*(m|cm|mm)\s*\)", r"(-\1) \2", x)                           # "0.5-1.5 (-2.5 m) tall"
    x = re.sub(r"\b(Leaves|Branches|Flowers|Sepals|Stamens)(?=[a-z])", r"\1 ", x)                  # "Leavessessile"
    x = re.sub(r"\(\s*(\d[\d.]*)\s*-\s*\)", r"(\1-)", x)                                         # "(2.5- )"
    x = re.sub(r"\(\s*-\s*(\d[\d.]*)\s*\)", r"(-\1)", x)                                         # "(- 8)"
    x = re.sub(rf"\((\d[\d.]*)\)\s+(?={RNG}\s*(?:\(|[×x]|mm|cm|m\b))", r"(\1-) ", x)             # "(6) 8-16" = (6-) 8-16
    x = re.sub(rf"(?<=\d)\s+\((\d[\d.]*)\)(?=\s*(?:[×x]|mm|cm|m\b))", r" (-\1)", x)              # "19.5 (23.5) ×" = (-23.5)
    x = re.sub(rf"(?<![(\d.-])(?<=\s)(\d[\d.]*)\)\s+(?={RNG}\s*(?:\(|[×x]))", r"(\1-) ", x)       # "18) 21-48" = (18-) 21-48
    x = re.sub(r"(?<=[\d)])\s*x\s*(?=[\d(c])", " × ", x)                                          # x between sizes
    # a size given without a unit ("(26-) 34-59 (-65) × (5.3-) 6.8-16 (-22);"): mm, as throughout the book
    x = re.sub(rf"({DIM})(?!\s*(?:mm|cm|m)\b)(?=\s*[;,.]\s|\s*$)", r"\1 mm", x)
    # "tapering to a short petiole-like base, 1-1.5 mm long" is the base's length, not the leaf's
    x = re.sub(r"(petiole-like base),\s*(?=(?:c\.\s*)?\d)", r"\1 ", x)
    # an organ's size given late in its sentence ("Fruit dry, ovoid, beaked; exocarp grey; endocarp faintly ribbed; 9-9.5 × 6-7.5 mm, glabrous", "Leaves
    # sessile ...; surfaces granulate, 12-45 × 0.8 mm") would be read as the last part's: moved to directly after the organ's label ("Fruit 9-9.5 ×
    # 6-7.5 mm, dry, ...")
    for lab, last in ORGANS:
        m = re.search(rf"\b{lab}\b(?=\s)(.*?)(?=\b(?:Branches|Leaves|Flowers|Sepals?|Corolla|Stamens?|Ovary|Fruit|Seeds?|Chromosome)\b(?=\s[a-z(0-9])|$)", x)
        if not m: continue
        body = m.group(1)
        dims = [d for d in re.finditer(rf"((?:c\.\s*)?{DIM})\s*(mm|cm|m)\b", body) if not re.search(r"than\s*$", body[:d.start()])]
        if not dims: continue
        d = dims[-1] if last else dims[0]
        # (a count stays first: "Sepals 5, imbricate, ..." -> "Sepals 5, 3-4 × 1-2 mm, imbricate, ...")
        mn = re.match(r"\s*(?:\(?\d+(?:-\d+)?\)?(?:\s*\(?-?\d*\)?)?(?:\s*\([^)]*\))?,\s*)", body)
        lead = mn.group(0).strip() + " " if mn and mn.end() <= d.start() else ""
        head_ = body[mn.end() if lead else 0:d.start()]
        if head_.strip(" ,;:") == "": continue
        rest = (head_.rstrip(" ,;:") + re.sub(r"^\s*[,;]?", ",", body[d.end():], count=1)).strip()
        rest = re.sub(r",\s*\.", ".", rest)
        sz = re.sub(r"^c\.\s*", "", d.group(0))      # (a leading "c." is not read by the parser: "Seed c. 2.2 × 1 mm")
        x = x[:m.start(1)] + " " + lead + sz + ", " + rest.lstrip(" ,") + " " + x[m.end(1):]
    # (a leading "c." before a size right after its label is not read: "Seed c. 2.2 × 1 mm")
    x = re.sub(r"\b(Seed|Fruit|Ovary|Leaves|Sepals \d+,)\s+c\.\s*(?=\d[\d.]*(?:\s*-\s*\d[\d.]*)?\s*×)", r"\1 ", x)
    # (a figure reference that lost its number, "Branches and leaves velutinous. Fig", and a section name left at the end, "... photo of fruit p. 41.
    # sect. Subscissocarpae")
    x = re.sub(r"(?:\s+Figs?\.?|\s+sect\.\s+[A-Z][a-z]+|\s+(?:mm|cm)(?:\s+[\d.•]+)+\.?)+\s*$", "", x)
    return re.sub(r"\s+", " ", x).strip()


def clean(x):
    # (small-caps labels read in mixed case: "ECoLoGY", "NoTES")
    x = re.sub(r"\b(?i:distribution|ecology|conservation|status|derivation|epithet|notes|selected|specimens|examined)\b",
               lambda m: m.group(0).upper() if sum(c.isupper() for c in m.group(0)) >= 2 else m.group(0), x)
    # (a figure legend's scale values read inside a line: "C, 3 mm; D, 7.5 mm; E-F, 7 mm; G, 8 mm", "mm; H, 3 mm.")
    x = re.sub(r"(?:\bmm;\s)?(?:\b[A-Za-z](?:-[A-Z])?,\s\d[\d.]*\s?mm[;.]?\s*)+", " ", x)
    # (a running head read in the body: "XVI. Eremophila sect. Hygrophanae")
    x = re.sub(r"\b[IVXL]+\.\s+(?:Eremophila|Myoporum)\s+sect\.\s+[A-Z][a-z]+\s*", "", x)
    # (scale bars and captions read inside a line: "m c 10", "1.2 c m", "... Scale units: 1 cm")
    x = re.sub(r"\bm\s?\d[\d.•]*\s?c\b(?:\s?\d[\d.•]*)?|\bm\s?c\s?\d[\d.•]*(?:\s?c(?:\s?m)?)?(?=\s|$)|\b[\d.•]+\s?c\s?m\b(?=\s|$)|\S*\.?\s*\bScale units:?\s*\d*\s*\w*", " ", x)
    # (map scraps: "Map Distribution of" without its name, the map's scale "- m", "-m g")
    x = re.sub(r"\bMap\s+Distribution\s+of\b|(?<!\S)-\s?m(?:\s+g)?(?!\S)", " ", x)
    # (map axis labels misread, "I5O*" for 150°, and map captions inside the text: "Ma 232. Distribution of Eremophila hygrophana")
    x = re.sub(r"(?<!\S)(?=\S*\d)[0-9IlOS]{2,3}[°*\"®](?!\S)", " ", x)
    x = re.sub(r"\bMap?\s+\d+[a-z]?\.\s*Distribution\s+of\s+(?:" + GENERA + r")\s+[a-z][a-z-]+(?:\s+subsp\.\s+[a-z][a-z-]+)?", " ", x)
    # (map axis labels left in the text: "120°", "-10°", "150*", "30-")
    return re.sub(r"\s+", " ", re.sub(r"(?<![\w.])-?\d{2,3}\s?[°\"*](?![\w])|(?<=\s)-\s?\d{1,2}°", " ", x)).strip()


# (a genus or section treatment - heading, Latin diagnosis, type species, description, key - follows the last entry before it: the entry ends at
# its heading, "IX. Eremophila sect. Carnosae Eremophila sect. Carnosae Chinnock, sect. nov.", "Ill. Myoporum sect. Disoon Type species: ...",
# "3. Bontia L., Sp. Pl. ...", "5. Calamphoreus Chinnock, gen. nov."; not at a running head read in the body, "XVI. Eremophila sect. Hygrophanae")
# Searched after the entry's first label (or its chromosome number), so that a species' own Latin diagnosis, "Frutex aromaticus ramis ...", in
# its citations is not taken; the reading order puts the section's Latin and English diagnoses before its heading or "Type species:" on some
# pages ("... (PERTH). Frutex intricatus virgatus, ... Type species: Myoporum cordifolium ..."). A key to the subspecies closes the species' entry.
AUTH = r"(?:Chinnock|Benth|Kraenzlin|F\.\s?Muell|R\.\s?Br|\((?:R\.\s?Br|F\.\s?Muell|A\.\s?DC|DC|Benth|Kraenzl)|A\.\s?(?:DC|Gray)|DC\.|L\.\s?S\.\s?Smith)"
SECT_START = re.compile(r"(?:\b[IVXLl1]+\.\s+)?\b(?:" + GENERA + r")\s+sect\.\s+[A-Z][a-z]+\s+(?:" + AUTH + r"|(?:" + GENERA + r")\s+sect\.)|\b(?:Lecto)?[Tt]ype species:|"
                        r"(?<![\w.])\d\.\s+(?:" + GENERA + r")\s+(?:L\.|Sol\.|R\.\s?Br|Chinnock,\s+gen|Sieb)|\b(?:Frutices|Frutex|Arbores|Arbor)\s+[a-z]|"
                        r"\bKey to (?:the )?(?:species|subspecies|sections|genera)\b|"
                        # (old genus names in a section's synonymy: "Duttonia F. Muell., Hook. J. Bot. 8 (1856)", "Stenochilus R. Br., Prod. 517")
                        r"\b(?:Duttonia|Pholidia|Stenochilus|Eremodendron|Pseudopholidia|Pholidiopsis|Sentis|Eremocosmos)\s+(?:sect\.\s+[A-Z][a-z]+\s+)?(?:R\.\s?Br|F\.\s?Muell|A\.\s?DC|DC\.|\()")


CUE = re.compile(r"(?<!See )\b(?:Map\s+(?:of\s+)?Distribution(?:\s+of)?|Distribution\s+Map(?:\s+of)?|(?:\bof\s+)?Distribution(?:\s+of)?|Map(?:\s+of)?(?:\s+\d+[lI]?\.?)?|"
                 r"Ma\s+\d+[lI]?\.)(?![A-Za-z])(?!\s+and\s+ecology)(?!\s+\d+\s*\(p)")
VERB = re.compile(r"\s+(?:is|are|was|were|occurs|grows|has|forms|extends|ranges|appears|favours|prefers)\b")


def strip_caps(x, name):
    # map captions read into the text lines beside the map, whole or in pieces, with the map's scale and axis scraps: "... clay Map 62
    # Distribution of Diocirea loams and usually dominates violacea the ground layer", "style obliquely Map Eremophila nivea attached",
    # "Eremophila rigida is known from Distribution of a few locations in the eastern part Eremophila rigida of the ..."; not "See Map 5 (page 64).".
    # The caption's cue ("Map 62", "Distribution of", "of Distribution") goes with the name words right after it; name words it lacks are the
    # caption's when they follow within 20 words, out of place: not "E. rigida", "subsp. australe", "Latin rigida", a synonym "Bontia clarkei
    # (Oldfield & F. Muell.)", nor a sentence's subject ("... is Eremophila longifolia widespread", "subsp. nutans is restricted")
    g, ep = name.split()[0], name.split()[1]
    inf = name.split()[3] if len(name.split()) > 3 else None
    own = [g, ep] + (["subsp.", inf] if inf else [])
    pat = lambda t: r"subsp\.?" if t == "subsp." else (re.escape(t[:-1]) + t[-1] + "?" if t.endswith("ii") else re.escape(t))   # ("youngi", "forresti")
    tok = re.compile(r"\s+(?:(?:" + GENERA + r")|sect\.\s+[A-Z][a-z]+|" + "|".join(pat(t) for t in own[1:]) + r")(?=\s|$|[.,;])")
    nb = r"(?<![A-Z]\. )(?<!subsp\. )(?<!Subsp\. )(?<!Latin )"
    def ok(a, b):
        # (an occurrence that is no sentence's subject and no synonym's epithet)
        return not VERB.match(x, b) and not re.match(r"\s*\(|\s+[A-Z][a-z]*\.\s", x[b:]) and not re.search(r"\b[A-Z][a-z]+\s+$", x[:a])
    pos = 0
    while True:
        m = CUE.search(x, pos)
        if not m: break
        end, got = m.end(), []
        if not re.search(r"\bLatin\s*$", x[:m.start()]):
            while True:
                mt = tok.match(x, end)
                if not mt: break
                got.append(mt.group(0).strip()); end = mt.end()
        if got and VERB.match(x, end):
            end, got = m.end(), []
        x = x[:m.start()] + " " + x[end:]
        pos = m.start() + 1
        have = [any(re.fullmatch(pat(t), w) for w in got) for t in own]
        if all(have) or (got and not any(have)): continue
        wend = pos + sum(len(w) + 1 for w in x[pos:].split()[:20])
        # (not past the next section: "... 1E. DERIVATION OF EPITHET ... NOTES Floral details of Eremophila cuneata are ...")
        ml = re.compile(r"\.\s+(?:DISTRIB|CONSERV|DERIV|NOTES|SELECTED|SPECIMENS)").search(x, pos, wend)
        if ml: wend = ml.start()
        # (the longest run of the name, "Eremophila interstans subsp." in "Unlike subsp. interstans Eremophila interstans subsp. it rarely ...", with
        # a lone genus beside it, "South paisleyi subsp. Eremophila Australian range", a repeat, and a species' "subsp. gilesii" after it)
        done = False
        for L in range(len(own), 1, -1):
            for st in range(len(own) - L + 1):
                if own[st] == "subsp." or any(have[st:st + L]): continue
                rp = re.compile(nb + r"\b" + r"\s+".join(pat(t) for t in own[st:st + L]) + r"(?=\s|$|[.,;])")
                mw = next((q for q in rp.finditer(x, pos, wend) if not VERB.match(x, q.end())), None)
                if mw and not VERB.match(x, mw.end()):
                    a, b = mw.start(), mw.end()
                    if st > 0:
                        mg = re.match(rf"\s+{g}(?=\s)", x[b:]); mg2 = re.search(rf"\b{g}\s+$", x[:a])
                        if mg: b += mg.end()
                        elif mg2: a = mg2.start()
                    mr = re.match(r"\s+" + r"\s+".join(pat(t) for t in own[st:st + L]) + r"(?=\s|$|[.,;])", x[b:]) or \
                         (re.match(r"\s+subsp\.?\s+[a-z]+\b(?!\s+(?:is|occurs|grows|was|has)\b)", x[b:]) if not inf else None)
                    if mr: b += mr.end()
                    x = x[:a] + " " + x[b:]; wend -= b - a - 1
                    for k in range(st, st + L): have[k] = True
                    done = True; break
            if done: break
        # (the last epithet on its own: "dominates violacea the ground layer", "it rarely occurs in woodland but virgata prefers")
        k = len(own) - 1
        if not have[k]:
            for mw in re.compile(rf"(?<!{g} )" + nb + r"\b" + pat(own[k]) + r"(?=\s|$|[.,;])").finditer(x, pos, wend):
                if ok(mw.start(), mw.end()):
                    x = x[:mw.start()] + " " + x[mw.end():]; break
    # (the map's scale and axis labels: "- km", "HO*", "-m*", "130™", "- im", "--*"; Cyrillic letters read in a map, "Ш", "вс.")
    x = re.sub(r"(?<!\S)(?:-*\s?(?:im|m|km|HO)?\*+|\d{2,3}™|-+\s?(?:km|im)|\.!|\"o|Г\d+°?|\S*[\u0400-\u04FF]\S*)(?!\S)|(?<![\d\s])\s+km\b(?!\s*\d)|(?<=[a-z.,])\s+km(?=\s+[A-Z])", " ", x)
    return re.sub(r"\s+", " ", x).strip()


def join(lines):
    out = ""
    for t in lines:
        t = t.strip()
        if not out:
            out = t; continue
        m = re.search(r"([A-Za-z]+)-$", out); n = re.match(r"([a-z]+)", t)
        if m and n:
            a, b = m.group(1), n.group(1)
            out = out[:-1] + t if words[(a + b).lower()] >= words[(a + "-" + b).lower()] and not (words[b.lower()] > 0 and words[(a + "-" + b).lower()] > 0) else out + t
        else:
            out = out + " " + t
    return re.sub(r"\s+", " ", out).strip()


# (a short piece joined in front of a heading on the same visual line: "(1861) 149. Eremophila bowmanii F. Muell, ...": split off)
st1 = []
for bp, t in stream:
    m = re.search(r"(?<=\s)\d{1,3}[a-z]?\.\s?(?:(?:" + GENERA + r")\s+[a-z]|Subsp\.)", t)
    if m and re.fullmatch(r"\(\d{4}\)\.?|\S{1,6}", t[:m.start()].strip()):
        st1 += [(bp, t[:m.start()].strip()), (bp, t[m.start():])]
    else:
        st1.append((bp, t))
stream = st1
# (heading numbers lost by the OCR: E. occidens's "52." is not on its scan)
LOST_NO = {"Eremophila occidens Chinnock, G. Paczkowska": "52."}
stream = [(bp, next((n + " " + t for k, n in LOST_NO.items() if t.startswith(k)), t)) for bp, t in stream]
# (a heading whose number was read as its own line: "52." / "Eremophila ...", "64a." / "Subsp. ..."; the number may also come after the name)
st2 = []
for i, (bp, t) in enumerate(stream):
    if st2 and re.fullmatch(r"\d{1,3}[a-z]?\.", st2[-1][1].strip()) and re.match(r"^(?:(?:" + GENERA + r")\s+(?:sp\.\s+)?[a-z]|(?:Subsp|Var|Forma?)\.?\s+[a-z])", t):
        st2[-1] = (st2[-1][0], st2[-1][1].strip() + " " + t)
    else:
        st2.append((bp, t))
stream = st2
# (book p. 145, missing from every scan, held the heading and the start of Myoporum degeneri: its text from p. 146 is its own entry)
for i, (bp, t) in enumerate(stream):
    if bp == 146:
        stream.insert(i, (146, "24. Myoporum degeneri (Webster) Degener & I. Degener [heading on p. 145, not scanned]"))
        break
SP_HEAD = re.compile(r"^(\d{1,3})\.\s?(" + GENERA + r")\s+((?:sp\.\s+)?[a-z][a-z-]+)\b(.*)$")
# (an epithet read with a capital: "197c. Subsp. Lanata Chinnock, subsp. nov.")
INF_HEAD = re.compile(r"^(\d{1,3})([a-z])\.\s?(Subsp|Var|Forma?)\.?\s+([A-Za-z][a-z-]+)\b(.*)$")
entries, cur, sp = [], None, None
for bp, t in stream:
    m = SP_HEAD.match(t)
    if m and not re.match(r"^\s*[A-Z][a-z]+\.?\s*$", m.group(4) or "x") or (m and m.group(4) == ""):
        sp = {"no": m.group(1), "genus": m.group(2), "epithet": m.group(3)}
        cur = {"no": m.group(1), "page": bp, "name": f"{m.group(2)} {m.group(3)}", "rank": "species", "head": t, "lines": []}
        entries.append(cur); continue
    m = INF_HEAD.match(t)
    if m and sp and m.group(1) == sp["no"]:
        rk = {"Subsp": "subsp.", "Var": "var.", "Form": "f.", "Forma": "f."}[m.group(3)]
        cur = {"no": m.group(1) + m.group(2), "page": bp, "name": f"{sp['genus']} {sp['epithet']} {rk} {m.group(4).lower()}",
               "rank": {"subsp.": "subspecies", "var.": "variety", "f.": "form"}[rk], "head": t, "lines": []}
        entries.append(cur); continue
    if cur is not None:
        cur["lines"].append(t)

# (OCR slips in headings)
FIX_NAME = {"bebriana": "behriana", "goodwini": "goodwinii", "youngi": "youngii", "bygrophana": "hygrophana", "forresti": "forrestii", "calorbabdos": "calorhabdos"}
for e in entries:
    e["name"] = " ".join(FIX_NAME.get(w, w) for w in e["name"].split())
LAB = [("DISTRIBUTION AND ECOLOGY", "distribution_ecology"), ("DISTRIBUTION ECOLOGY", "distribution_ecology"), ("DISTRIBUTION AND", "distribution_ecology"), ("CONSERVATION STATUS", "conservation"), ("DERIVATION OF EPITHET", "derivation"),
       ("DERIVATION OF NAME", "derivation"), ("SELECTED SPECIMENS", "specimens"), ("SPECIMENS EXAMINED", "specimens"), ("NOTES", "notes")]
# (the label is split over lines and sometimes loses its "AND": "DISTRIBUTION" / "AND" / "ECOLOGY", "DISTRIBUTION" / "ECOLOGY")
lab_re = re.compile(r"\b(DISTRIB[A-Z]{3,7}(?:\s+AND)?(?:\s+\S{1,15}){0,5}?\s+(?:AND\s+)?ECOLOGY(?:\s+AND\b)?|ECOLOGY(?=\s+[A-Z][a-z]|\s+This\b)|"
                    r"Distribution and ecology(?=\s+[A-Z])|"
                    + "|".join(re.escape(l).replace(r"\ ", r"\s+") for l, _ in sorted(LAB, key=lambda x: -len(x[0]))) + r")\b")
LABMAP = dict(LAB)
DESC_RE = re.compile(r"(?:^|(?<=[.)]\s))(?:[A-Z][a-z-]+,?\s+){0,4}?(?:[a-z-]+,?\s+){0,4}?(?:[Ss]hrubs?|[Ss]hrublets?|[Tt]rees?|[Tt]reelets?|[Ss]ubshrubs?|[Mm]allees?|[Hh]erbs?(?!\.\s?[A-Z]{2,})|[Pp]lants?)\b"
                     r"(?=[^.]{0,120}(?:\d\s?(?:cm|m)\b|tall|high|wide|across|\bBranches\b|\.))")
CITE = re.compile(r"\(\d{4}\)|\b\d+\s?:\s?\d|\b(?:[Hh]olotype|[Ll]ectotype|[Ii]sotypes?|[Ss]yntypes?|[Ii]solectotypes?|[Nn]eotype|sensu|auct|nom\.|Fl\.|Bot\.|Prodr|Gen\.|Pl\.|Herb\.|s\.n\.|s\.dat|photo)|"
                  r"^\s*\[|^\s*See Notes|\b[A-Z](?:-[A-Z])?,\s\d|\bTypes?\s?[:.]|\bType citation|"
                  r"\((?:[A-Z][A-Za-z-]{0,6}[\d ,;:.]*)+\)[.;]?\s*$|^\s*\S{1,4}\s*$")     # (herbaria closing a sentence; a stray "Br.")
FIELDS = ["citations", "type", "description", "distribution_ecology", "conservation", "derivation", "notes", "specimens"]
rows, CUT, CAPS = [], [], []
for e in entries:
    txt = join(e["lines"])
    mb = lab_re.search(txt) or re.search(r"\bChromosome numbers?\b", txt)
    ms = SECT_START.search(txt, mb.end()) if mb else None
    if ms:
        CUT.append((e["name"], txt[max(0, ms.start() - 60):ms.start()], txt[ms.start():ms.start() + 80], len(txt) - ms.start()))
        txt = txt[:ms.start()]
    txt0 = clean(txt)
    txt = strip_caps(txt0, e["name"])
    if txt != txt0:
        CAPS.append((e["name"], txt0, txt))
    for pat, rep in FIX_TEXT.get(e["name"], []):
        txt = re.sub(pat, rep, txt)
    r = {"entry_no": e["no"], "page": e["page"], "taxon_name": e["name"], "rank": e["rank"], "heading": e["head"], "common_name": ""}
    r.update({k: "" for k in FIELDS})
    ml = lab_re.search(txt)
    head, notes = (txt[:ml.start()], txt[ml.start():]) if ml else (txt, "")
    lost_dist = ""
    # (a Distribution and ecology label lost by the OCR: the description ends at its figure reference, "... Chromosome number unknown. Fig. 93.",
    # and what follows - "Myoporum tetrandrum is ...", "It occurs on rocky flats ..." - is the distribution and ecology)
    if not re.search(r"\bDISTRIB[A-Z]{3,7}\b|\bECOLOGY\b", notes):
        mf = re.search(r"\bChromosome numbers?\b[^.]*(?:\.\d[^.]*)*\.\s*(?:[^.]{0,120}\.\s*)?Figs?\.?\s[\d,\s and-]+(?:,?\s*(?:photos?|p\.)[^.]*)?\.\s+(?=\S)", head) or \
             re.search(r"\bChromosome numbers?\b[^.(]*(?:\([^)]*\))?[^.]*\.\s+(?=\S)", head)
        if mf and len(head) - mf.end() > 30:
            lost_dist = head[mf.end():].strip(); head = head[:mf.end()]
    types = [m.start() for m in re.finditer(r"\b(?:Type|Holotype|Lectotype|Neotype|Syntypes?)s?\s?[:.](?=\s)", head)]
    after = types[-1] if types else 0
    # (the description: the first organ label after the last type citation, "Branches ...", "Leaves ...", and the sentences before it back to the
    # last citation-like one - synonyms may follow the type: "Bontia adenotricha (F. Muell. ex Benth.) Kuntze, Revis. Gen. Pl. 2: 501 (1891), as
    # 'Bondtia'. Erect, aromatic shrubs 0.5-1.4 m tall. Branches ..."; a habit sentence elsewhere is only a fallback)
    md = None
    ml2 = re.compile(r"\b(?:Branches|Branchlets|Leaves|Stems?)\b(?=[\s,])").search(head, after)
    if ml2:
        start = ml2.start()
        for mb in reversed(list(re.finditer(r"(?:^|(?<=[.)]))\s*(?=[A-Z])", head[after:ml2.start()]))):
            sen = head[after + mb.end():start]
            if CITE.search(sen):
                # (a habit sentence run into the citation before it, its full stop lost: "... Revis. Gen. Pl. 2:501 (1891), as 'Bondtia' Divaricate or
                # tangled shrub 0.5-1.5 m tall.", "... Spin Multi-stemmed prostrate to erect ... shrub or small tree 0.2-6 m tall")
                hb = list(re.finditer(r"(?:(?<=\s)|^)[A-Z][a-z-]+(?:[ ,]+(?:to\s+|or\s+)?(?:\([^)]{1,30}\)|[a-z-]+)){0,9}?\s(?:shrubs?|trees?|subshrubs?)\b", sen))
                if hb:
                    start = after + mb.end() + hb[-1].start()
                break
            start = after + mb.end()
        md = re.compile(r"").search(head, start)
    if not md:
        for m in DESC_RE.finditer(head):
            if m.start() >= after:
                md = m; break
    if not md and types:
        md = re.compile(r"(?<=[.)]\s)[A-Z][a-z]+[ ,]").search(head, after)
    if not md and not types and (e["rank"] != "species" or "[heading on p. 145" in e["head"]):
        # (a typical subspecies has no citations or type: its text is its description; so has Myoporum degeneri from p. 146 on)
        md = re.compile(r"^").search(head)
    if md:
        pre, r["description"] = head[:md.start()].strip(), head[md.start():].strip()
    else:
        pre = head.strip()
    # (leading citation sentences left at the start: "Chinnock 7194 (holotype: AD, isotypes: MEL, PERTH). Outer sepals ...")
    while True:
        mc0 = re.match(r"^(.*?[.)\]])\s+(?=[A-Z\[(])", r["description"])
        if mc0 and CITE.search(mc0.group(1)):
            pre = (pre + " " + mc0.group(1)).strip(); r["description"] = r["description"][mc0.end():]
        else:
            break
    # (a fragment before the habit sentence: "Stoward 393 (BM). m c 10 1.5 c m Low spreading to erect shrub ...", "PERTH. Dense, rounded ... shrub")
    if e["rank"] == "species" or True:
        mh = re.search(r"(?:(?<=[.)\s])|^)(?:[A-Z][a-z-]+,?\s+(?:[a-z-]+,?\s+){0,6}?(?:shrubs?|trees?|subshrubs?)\b|Shrubs?\b)", r["description"][:150])
        if mh and 0 < mh.start() < 90 and not re.search(r"\b(?:Leaves|Flowers|Sepals|Corolla)\b", r["description"][:mh.start()]):
            pre = (pre + " " + r["description"][:mh.start()]).strip(); r["description"] = r["description"][mh.start():]
    # (common names follow the chromosome number: "Chromosome number unknown. Boobialla, blueberry tree. Fig. 56.")
    mc = re.search(r"(Chromosome number[^.]*(?:\.\d[^.]*)?\.)\s+((?:(?!\bFigs?\b)[A-Z][^.]{2,120}\.\s*)?)(Figs?\.?\s[\d, and-]+\.?)?\s*$", r["description"])
    if mc and mc.group(2).strip():
        r["common_name"] = mc.group(2).strip(" .")
        r["description"] = (r["description"][:mc.start(2)] + (mc.group(3) or "")).strip()
    r["description"] = norm_desc(r["description"])
    if types:
        r["citations"], r["type"] = pre[:types[-1]].strip(), pre[types[-1]:].strip()
    else:
        r["citations"] = pre
    if lost_dist and not r["distribution_ecology"]:
        r["distribution_ecology"] = lost_dist
    if notes:
        parts = lab_re.split(notes)
        for lab, val in zip(parts[1::2], parts[2::2]):
            k = "distribution_ecology" if lab.startswith(("DISTRIB", "ECOLOGY", "Distribution")) else LABMAP[re.sub(r"\s+", " ", lab)]
            r[k] = (r[k] + " " + val.strip()).strip()
    rows.append(r)

import pickle
pickle.dump((CUT, CAPS), open(f"{ROOT}/debug_cut_caps.pkl", "wb"))
with open(f"{ROOT}/../chinnock2007_profiles.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["entry_no", "page", "taxon_name", "rank", "heading", "common_name"] + FIELDS)
    w.writeheader(); w.writerows(rows)
with open(f"{ROOT}/pages_used.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["book_page", "file_page"]); w.writerows(sorted(pages_used.items()))
print(len(rows), "entries;", Counter(r["rank"] for r in rows), Counter(r["taxon_name"].split()[0] for r in rows if r["rank"] == "species"))
print("empty:", {k: sum(1 for r in rows if not r[k]) for k in FIELDS})
print("replacement scans used:", sorted(b for b, f in pages_used.items() if f.startswith("r_")))
