"""Verticordia (E.A. George 2002) page photographs -> text lines in reading order.
Each page is rendered at 250 dpi from the full-resolution PDF and OCR'd with Apple Vision three times (vocrw: one row per word, with its
box and Vision's line number): the whole page, and the left and right text columns cropped on their own. Vision runs some full-page lines
across the two columns ("This typical variety is ... irregular RELATED SPECIES V. acerosa has ...") and its word boxes inside a line are
too rough to split them, so a wide full-page line is replaced by the column crops' lines when both crops have a line at its height and
the right one starts at the gutter. Lines wider than 0.6 of the page are full-width text (species description, key, headings); the
others are left- or right-column text, a far-right piece on a key line ("var. angustifolia") is joined to the line it ends. The page is
read in bands: full-width text, then the left column, then the right column, down to the next full-width line. Map labels (Perth,
Albany ...), the page number and the running foot "VERTICORDIA" are dropped.
usage: page.py <pdf> <page> [--force]  ->  ../lines/p<page>.tsv: order, kind (full / left / right), x0, y0, x1, y1, src, text"""
import os, re, subprocess, sys
HERE = os.path.dirname(os.path.abspath(__file__)); ROOT = os.path.dirname(HERE)
pdf, page = sys.argv[1], int(sys.argv[2])
out = f"{ROOT}/lines/p{page:03d}.tsv"
os.makedirs(f"{ROOT}/lines", exist_ok=True)
if os.path.exists(out) and "--force" not in sys.argv: sys.exit(0)
img = f"{ROOT}/lines/tmp_{page:03d}.jpg"
subprocess.run(["pdftoppm", "-f", str(page), "-l", str(page), "-r", "250", "-jpeg", "-jpegopt", "quality=92", "-singlefile", pdf, img[:-4]], check=True)

def ocr(x0=0.0, x1=1.0, y0=0.0, y1=1.0):
    res = subprocess.run([f"{HERE}/vocrw", img, f"{x0:.4f}", f"{y0:.4f}", f"{x1:.4f}", f"{y1:.4f}"], capture_output=True, text=True, check=True)
    vl = {}
    for ln in res.stdout.splitlines():
        p = ln.split("\t")
        if len(p) >= 7: vl.setdefault(int(p[0]), []).append((float(p[1]), float(p[2]), float(p[3]), float(p[4]), "\t".join(p[6:])))
    return [[min(w[0] for w in ws), min(w[1] for w in ws), max(w[2] for w in ws), max(w[3] for w in ws), " ".join(w[4] for w in sorted(ws)).strip()]
            for ws in vl.values()]

full = ocr()
# the column gutter: where full-page lines starting right of the middle most often begin
starts = [int(l[0] * 100) / 100 for l in full if 0.45 < l[0] < 0.65 and len(l[4]) >= 15]
gut = (max(set(starts), key=starts.count) + 0.005) if starts else 0.53
L, R = ocr(0.0, gut + 0.005), ocr(gut - 0.015, 1.0)
# (and three overlapping full-width bands: Vision drops lines of the full-width description too, "Sepals ... Petals ... Stamens" under
# V. citrella, p. 3)
B = [l for y0, y1 in ((0.0, 0.4), (0.3, 0.7), (0.6, 1.0)) for l in ocr(0.0, 1.0, y0, y1) if l[1] > y0 + 0.003 and l[3] < y1 - 0.003]
os.remove(img)
ov = lambda a, b: min(a[3], b[3]) - max(a[1], b[1]) > 0.5 * min(a[3] - a[1], b[3] - b[1])
hx = lambda a, b: min(a[2], b[2]) - max(a[0], b[0]) > 0
MAP = re.compile(r"^(?:[^A-Za-z]{0,3}|[A-Za-z]\s)(?:Meekatharra|Geraldton|Laverton|Kalgoorlie|PERTH|Perth|Esperance|Albany|Carnarvon|Kununurra|Broome|Port Hedland|Darwin|Eucla)$")
# map labels, page numbers and the running foot
junk = lambda l: bool(MAP.match(l[4]) or (l[1] > 0.93 and re.fullmatch(r"\d{1,3}|V\s*ERTICORDIA|\d{1,3}\s+V\s*ERTICORDIA|V\s*ERTICORDIA\s+\d{1,3}", l[4], re.I)))
# (a full-page "line" much taller than a text line is Vision's garbled read of several lines, "eave newhat pedit bout lie tuber and with os
# mal tom, 45 branched" over three description lines, p. 352: dropped, so that the band and column passes read those lines)
# (compared with lines of its own kind: the full-width text is set larger than the column text)
def med(xs): xs = sorted(xs); return xs[len(xs) // 2] if xs else 0.03
med_w = med([l[3] - l[1] for l in full if len(l[4]) >= 20 and l[2] - l[0] > 0.6])
med_n = med([l[3] - l[1] for l in full if len(l[4]) >= 20 and l[2] - l[0] <= 0.6])
# (not the entry headings, set larger: "88. Verticordia chrysostachys Meisner (1857)")
full = [l for l in full if l[3] - l[1] <= 1.8 * (med_w if l[2] - l[0] > 0.6 else med_n) or re.match(r"^\d{1,3}[a-z]?\.\s?Verticordia\b", l[4])]
rows = []
for l in full:
    if junk(l): continue
    if l[2] - l[0] > 0.6:
        # (a true two-column line: the left crop's text ends short of the gutter, and the right crop's starts at it, not at the crop edge;
        # a full-width line cut by the crops runs to the edges)
        rr = [r for r in R if ov(l, r) and gut - 0.011 < r[0] < gut + 0.03 and len(r[4]) >= 8]
        ll = [x for x in L if ov(l, x) and x[2] < gut - 0.015 and len(x[4]) >= 8]
        if rr and ll:
            rows.append(ll[0] + ["crop", "left"]); rows.append(rr[0] + ["crop", "right"]); continue
        rows.append(l + ["full", "full"]); continue
    xc = (l[0] + l[2]) / 2
    kind = "right" if abs(l[0] - gut) < 0.04 else ("left" if xc < gut else "tail")
    rows.append(l + ["full", kind])
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
            rows.append(x + ["band", "right" if abs(x[0] - gut) < 0.04 else ("left" if (x[0] + x[2]) / 2 < gut else "tail")])
# column-crop lines the full-page pass missed (Vision drops some lines on a whole page: "shrub 10-70 cm x 10-80 cm. Floral leaves are
# broader than", p. 124): a left-crop line ending short of the gutter, or a right-crop line starting at or right of it, that no line
# already read overlaps
for x in L:
    if x[2] < gut - 0.015 and len(x[4]) >= 3 and not any(ov(x, r) and hx(x, r) for r in rows) and not junk(x):
        rows.append(x + ["crop", "left"])
for x in R:
    if x[0] > gut - 0.011 and len(x[4]) >= 3 and not any(ov(x, r) and hx(x, r) for r in rows) and not junk(x):
        rows.append(x + ["crop", "right" if abs(x[0] - gut) < 0.04 else "tail"])
# a far-right tail (key answer) joins the line whose vertical span it shares
for t in [r for r in rows if r[6] == "tail"]:
    host = [r for r in rows if r is not t and r[6] != "tail" and ov(r, t) and r[2] < t[0]]
    if host:
        h = max(host, key=lambda r: r[2]); h[4] = h[4] + " ...... " + t[4]; h[2] = t[2]; t[6] = "drop"
    else:
        t[6] = "right"
rows = sorted([r for r in rows if r[6] != "drop"], key=lambda r: r[1])
order, band = [], []
def flush():
    order.extend(sorted([r for r in band if r[6] == "left"], key=lambda r: r[1]) + sorted([r for r in band if r[6] == "right"], key=lambda r: r[1]))
for r in rows:
    if r[6] == "full": flush(); band = []; order.append(r)
    else: band.append(r)
flush()
with open(out, "w", encoding="utf-8") as f:
    for i, r in enumerate(order):
        f.write(f"{i}\t{r[6]}\t{r[0]:.4f}\t{r[1]:.4f}\t{r[2]:.4f}\t{r[3]:.4f}\t{r[5]}\t{r[4]}\n")
print(page, len(order), "lines; gutter", round(gut, 3), "; merged lines split", sum(1 for r in order if r[5] == "crop") // 2)
