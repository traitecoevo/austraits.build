"""Eremophila (R.J. Chinnock 2007, Eremophila and allied genera) page photographs -> text lines in reading order. Adapted from
Verticordia_ocr/scripts/page.py (the same method): each page is rendered at the scans' native 240 dpi and OCR'd with Apple Vision (vocrw: one row per word, with its
box and Vision's line number): the whole page, and the left and right text columns cropped on their own. Vision runs some full-page lines
across the two columns ("This typical variety is ... irregular RELATED SPECIES V. acerosa has ...") and its word boxes inside a line are
too rough to split them, so a wide full-page line is replaced by the column crops' lines when both crops have a line at its height and
the right one starts at the gutter. Lines wider than 0.6 of the page are full-width text (species description, key, headings); the
others are left- or right-column text, a far-right piece on a key line ("var. angustifolia") is joined to the line it ends. The page is
read in bands: full-width text, then the left column, then the right column, down to the next full-width line. Map labels (Perth,
Albany ...), the page number and the running foot "VERTICORDIA" are dropped.
The running head (top of the page) is dropped and the printed page number (foot) kept on a "pageno" row.
usage: page.py <pdf> <tag> <page> [--force]  ->  ../lines/<tag>_<page>.tsv: order, kind (full / left / right / pageno), x0, y0, x1, y1, src, text"""
import os, re, subprocess, sys
HERE = os.path.dirname(os.path.abspath(__file__)); ROOT = os.path.dirname(HERE)
pdf, tag, page = sys.argv[1], sys.argv[2], int(sys.argv[3])
out = f"{ROOT}/lines/{tag}_{page:03d}.tsv"
os.makedirs(f"{ROOT}/lines", exist_ok=True)
if os.path.exists(out) and "--force" not in sys.argv: sys.exit(0)
img = f"{ROOT}/lines/tmp_{tag}_{page:03d}.jpg"
subprocess.run(["pdftoppm", "-f", str(page), "-l", str(page), "-r", "240", "-jpeg", "-jpegopt", "quality=92", "-singlefile", pdf, img[:-4]], check=True)

def ocr(x0=0.0, x1=1.0, y0=0.0, y1=1.0, keep=None):
    # (keep: (lo, hi) bounds on a word's centre - a column crop reaches into its neighbour, so that skew does not cut the first letters off its
    # own lines, "ect, round or flat-topped shrub" for "Erect", p. 455; the neighbour's words are dropped)
    res = subprocess.run([f"{HERE}/vocrw", img, f"{x0:.4f}", f"{y0:.4f}", f"{x1:.4f}", f"{y1:.4f}"], capture_output=True, text=True, check=True)
    vl = {}
    for ln in res.stdout.splitlines():
        p = ln.split("\t")
        if len(p) < 7: continue
        w = (float(p[1]), float(p[2]), float(p[3]), float(p[4]), "\t".join(p[6:]))
        if keep and not (keep[0] <= (w[0] + w[2]) / 2 < keep[1]): continue
        vl.setdefault(int(p[0]), []).append(w)
    return [[min(w[0] for w in ws), min(w[1] for w in ws), max(w[2] for w in ws), max(w[3] for w in ws), " ".join(w[4] for w in sorted(ws)).strip()]
            for ws in vl.values()]

full = ocr()
# the column gutter: the vertical white space between the columns, the x in the middle of the page that the fewest narrow lines cross; the
# right column starts just right of it (text wrapped beside a figure or map starts further right)
narrow = [l for l in full if l[2] - l[0] < 0.6 and len(l[4]) >= 10]
xs = [0.38 + 0.004 * i for i in range(66)]
cross = [sum(1 for l in narrow if l[0] < x < l[2]) for x in xs]
low = min(cross) if cross else 0
runs, cur = [], []
for x, c in zip(xs, cross):
    if c <= low + 1: cur.append(x)
    elif cur: runs.append(cur); cur = []
if cur: runs.append(cur)
white = (lambda r: (r[0] + r[-1]) / 2)(max(runs, key=len)) if runs else 0.5
st = sorted(l[0] for l in narrow if white <= l[0] < white + 0.06)
gut = (st[len(st) // 2 if len(st) < 3 else 0] + 0.005) if st else white + 0.012
L, R = ocr(0.0, gut + 0.03, keep=(0.0, gut - 0.004)), ocr(gut - 0.05, 1.0, keep=(gut - 0.004, 1.0))
# (and three overlapping full-width bands: Vision drops lines of the full-width description too, "Sepals ... Petals ... Stamens" under
# V. citrella, p. 3)
B = [l for y0, y1 in ((0.0, 0.4), (0.3, 0.7), (0.6, 1.0)) for l in ocr(0.0, 1.0, y0, y1) if l[1] > y0 + 0.003 and l[3] < y1 - 0.003]
os.remove(img)
ov = lambda a, b: min(a[3], b[3]) - max(a[1], b[1]) > 0.5 * min(a[3] - a[1], b[3] - b[1])
hx = lambda a, b: min(a[2], b[2]) - max(a[0], b[0]) > 0
MAP = re.compile(r"^(?:[^A-Za-z]{0,3}|[A-Za-z]\s)(?:Meekatharra|Geraldton|Laverton|Kalgoorlie|PERTH|Perth|Esperance|Albany|Carnarvon|Kununurra|Broome|Port Hedland|Darwin|Eucla)$")
# the running head ("I. Myoporum sect. Myoporum", "Eremophila sect. ...") and the page number at the foot (kept apart, below)
pageno = [l for l in full if l[1] > 0.93 and re.fullmatch(r"\d{1,3}", l[4].strip())]
junk = lambda l: bool(l[3] < 0.045 or (l[1] > 0.93 and re.fullmatch(r"\d{1,3}", l[4].strip())))
# (a full-page "line" much taller than a text line is Vision's garbled read of several lines, "eave newhat pedit bout lie tuber and with os
# mal tom, 45 branched" over three description lines, p. 352: dropped, so that the band and column passes read those lines)
# (compared with lines of its own kind: the full-width text is set larger than the column text)
def med(xs): xs = sorted(xs); return xs[len(xs) // 2] if xs else 0.03
med_w = med([l[3] - l[1] for l in full if len(l[4]) >= 20 and l[2] - l[0] > 0.6])
med_n = med([l[3] - l[1] for l in full if len(l[4]) >= 20 and l[2] - l[0] <= 0.6])
# (not the entry headings, set larger: "6. Myoporum acuminatum R. Br., Prodr. 515 (1810)")
full = [l for l in full if l[3] - l[1] <= 1.8 * (med_w if l[2] - l[0] > 0.6 else med_n) or re.match(r"^\d{1,3}[a-z]?\.\s?[A-Z][a-z]+ [a-z]", l[4])]
rows = []
for l in full:
    if junk(l): continue
    if l[2] - l[0] > 0.6:
        # (a true two-column line: the left crop's text ends short of the gutter, and the right crop's starts at it, not at the crop edge;
        # a full-width line cut by the crops runs to the edges)
        rr = [r for r in R if ov(l, r) and gut - 0.011 < r[0] < gut + 0.03 and len(r[4]) >= 8]
        ll = [x for x in L if ov(l, x) and x[2] < gut - 0.008 and len(x[4]) >= 8]
        # (Eremophila: the pages are two-column text, and a wide line among column lines - narrow lines just above and below it on both
        # sides - is a merged one even when skew puts the left column's line end near the gutter, "Branches erect, flattened DISTRIBUTION AND
        # ECOLOGY Eremophila dichroantha ...", p. 289; captions, keys and genus descriptions set full width sit among wide lines)
        merged_by_nbrs = False
        if not (rr and ll):
            near = [n for n in full if n is not l and n[2] - n[0] <= 0.6 and abs((n[1] + n[3]) / 2 - (l[1] + l[3]) / 2) < 0.05]
            # (a section diagnosis set full width beside its map, p. 404, is split here too; entries.py reads a section's opening line by line)
            if any((n[0] + n[2]) / 2 < gut for n in near) and any((n[0] + n[2]) / 2 >= gut for n in near):
                merged_by_nbrs = True
                rr = [r for r in R if ov(l, r) and gut - 0.025 < r[0] < gut + 0.04 and len(r[4]) >= 3]
                ll = [x for x in L if ov(l, x) and x[2] < gut + 0.004 and len(x[4]) >= 3]
        if rr and ll:
            rows.append(ll[0] + ["crop", "left"]); rows.append(rr[0] + ["crop", "right"]); continue
        # (a merged line by its neighbours whose other half the crops read differently: the full line's own words are split where the left
        # crop's line ends, by its word count; "found in other species, ... E. forrestii | tuberculate, dendritic-tomentose ...", p. 523)
        if merged_by_nbrs and (ll or rr):
            fw = l[4].split()
            if ll:
                n = len(ll[0][4].split())
            else:
                n = len(fw) - len(rr[0][4].split())
            if 0 < n < len(fw):
                rows.append([l[0], l[1], gut - 0.01, l[3], " ".join(fw[:n]), "split", "left"])
                rows.append([gut, l[1], l[2], l[3], " ".join(fw[n:]), "split", "right"]); continue
        rows.append(l + ["full", "full"]); continue
    # (a line's column is where its middle lies; right-column text wrapped beside a map starts far right: "and north-western" / "Victoria. In
    # South", p. 132. No key-answer joining here: the keys are not read)
    rows.append(l + ["full", "right" if (l[0] + l[2]) / 2 >= gut else "left"])
# full-width band lines the full-page pass missed (before the column crops: a band line is whole where a crop has half of a full-width line,
# "1m wide, with or without lignotuber, ...", V. chrysanthella p. 10)
for x in B:
    if len(x[4]) >= 3 and not junk(x) and not any(ov(x, r) and hx(x, r) for r in rows):
        w = x[2] - x[0]
        if w > 0.6:
            # (a band line wider than a column is full-width only if it does not cross the gutter between two column lines)
            if any(ov(x, r) for r in rows if r[6] in ("left", "right") and r[5] == "full"): continue
            rows.append(x + ["band", "full"])
        else:
            rows.append(x + ["band", "right" if (x[0] + x[2]) / 2 >= gut else "left"])
# column-crop lines the full-page pass missed (Vision drops some lines on a whole page: "shrub 10-70 cm x 10-80 cm. Floral leaves are
# broader than", p. 124): a left-crop line ending short of the gutter, or a right-crop line starting at or right of it, that no line
# already read overlaps
for x in L:
    if x[2] < gut - 0.015 and len(x[4]) >= 3 and not any(ov(x, r) and hx(x, r) for r in rows) and not junk(x):
        rows.append(x + ["crop", "left"])
for x in R:
    if x[0] > gut - 0.011 and len(x[4]) >= 3 and not any(ov(x, r) and hx(x, r) for r in rows) and not junk(x):
        rows.append(x + ["crop", "right"])
rows = sorted([r for r in rows if r[6] != "drop"], key=lambda r: r[1])
order, band = [], []
def flush():
    order.extend(sorted([r for r in band if r[6] == "left"], key=lambda r: r[1]) + sorted([r for r in band if r[6] == "right"], key=lambda r: r[1]))
for r in rows:
    if r[6] == "full": flush(); band = []; order.append(r)
    else: band.append(r)
flush()
if pageno: order.append(pageno[0] + ["full", "pageno"])
with open(out, "w", encoding="utf-8") as f:
    for i, r in enumerate(order):
        f.write(f"{i}\t{r[6]}\t{r[0]:.4f}\t{r[1]:.4f}\t{r[2]:.4f}\t{r[3]:.4f}\t{r[5]}\t{r[4]}\n")
print(page, len(order), "lines; gutter", round(gut, 3), "; merged lines split", sum(1 for r in order if r[5] == "crop") // 2)
