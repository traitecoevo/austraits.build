"""Second OCR pass for one page: find the text columns from the full-page pass, OCR each column strip on its own
(Vision drops some lines on a full page, e.g. "long; calyx green. Sep.-Dec." under Annona glabra), and write the union.

usage: colpass.py <pdf> <tag> <page>
reads  ocr/<tag>_<page>.tsv (x0 y0 x1 y1 conf text, page fractions)
writes ocr2/<tag>_<page>.tsv (col x0 y0 x1 y1 conf src text), src = crop / full
"""
import os, re, subprocess, sys

SP = "/private/tmp/claude-502/-Users-z3524079-GitHub-traits-build-inst-support/fb9a03f2-b5ee-4e53-a6f9-9631bd56b1ab/scratchpad/cooper"
pdf, tag, page = sys.argv[1], sys.argv[2], int(sys.argv[3])
base = f"{tag}_{page:03d}"
out = f"{SP}/ocr2/{base}.tsv"
if os.path.exists(out) and os.path.getsize(out) > 0 and "--force" not in sys.argv:
    sys.exit(0)


def read(path):
    rows = []
    for ln in open(path, encoding="utf-8"):
        p = ln.rstrip("\n").split("\t")
        if len(p) < 6:
            continue
        rows.append([float(p[0]), float(p[1]), float(p[2]), float(p[3]), float(p[4]), "\t".join(p[5:])])
    return rows


full = read(f"{SP}/ocr/{base}.tsv")
# body lines (long enough to be column text, not headings / captions / key leaders)
# (not the running heads at the top of the page, "FRUITS OF THE AUSTRALIAN TROPICAL RAINFOREST", which start mid-page)
# (lines of 20+ characters: a column holding a genus heading and its key has few long lines - Darlingia, p. 409 - and merged with the
# column beside it when only long lines counted)
# (and not lines Vision ran across two columns: wider than a column, 0.36 of the page)
body = sorted([r for r in full if len(r[5]) >= 20 and 0.10 < r[2] - r[0] < 0.36 and r[1] > 0.07 and r[3] - r[1] < 0.025], key=lambda r: r[0])
cols = []  # [left, right]
for r in body:
    if cols and r[0] - cols[-1][2] < 0.06:
        c = cols[-1]
        c[0] = min(c[0], r[0]); c[1] = max(c[1], r[2]); c[2] = max(c[2], r[0]); c[3] += 1
    else:
        cols.append([r[0], r[2], r[0], 1])  # left, right, last x0, n
cols = [c for c in cols if c[3] >= 3]
# (a cluster starting inside the previous column's width is that column's indented key lines or right-aligned key answers, not a column)
merged = []
for c in cols:
    if merged and c[0] < merged[-1][1] - 0.03:
        m = merged[-1]
        m[1] = max(m[1], c[1]); m[3] += c[3]
    else:
        merged.append(list(c))
cols = merged
if not cols:
    cols = [[0.0, 1.0, 0, 0]]
# column strips (2026-11-08, third version): pages are photographed with some skew, so a column's left edge moves down the page and
# fixed strips cut the start or end off lines ("ensely clothed ...", "YGIUM Gaertn."). Each column has a core between the mid-gutters on
# either side; it is OCR'd with a 0.04 margin into its neighbours, lines cut at a strip edge are dropped (they are whole in the next strip),
# and a line belongs to the column whose core holds its centre.
bounds = [0.0] + [(cols[i - 1][1] + cols[i][0]) / 2 for i in range(1, len(cols))] + [1.0]
cores = [(bounds[i], bounds[i + 1]) for i in range(len(cols))]
strips = [(max(0.0, l - 0.04), min(1.0, r + 0.04)) for l, r in cores]
img = f"{SP}/ocr2/{base}"
subprocess.run(["pdftoppm", "-f", str(page), "-l", str(page), "-r", "240", "-jpeg", "-singlefile", pdf, img], check=True)
# lines running across columns (a genus introduction set the full page width: "World: about 500 species ...", Syzygium p. 356) are
# taken whole from the full-page pass, and the column crops' words inside them are dropped
span = [r for r in full if r[2] - r[0] > 0.40 and r[1] > 0.07 and r[3] - r[1] < 0.025]
crop = []
for ci, (l, r) in enumerate(strips):
    res = subprocess.run([f"{SP}/scripts/vocrw", img + ".jpg", f"{l:.4f}", "0", f"{r:.4f}", "1"], capture_output=True, text=True, check=True)
    lines = {}
    for ln in res.stdout.splitlines():
        p = ln.split("\t")
        if len(p) < 7:
            continue
        lid, x0, y0, x1, y1, cf, w = int(p[0]), float(p[1]), float(p[2]), float(p[3]), float(p[4]), float(p[5]), "\t".join(p[6:])
        xc, yc = (x0 + x1) / 2, (y0 + y1) / 2
        # (words of the column's own core only: a line Vision ran into the margin keeps its own words, the neighbour's are dropped)
        if not (cores[ci][0] <= xc < cores[ci][1]):
            continue
        if any(s0 <= xc <= s1 and t0 - 0.002 <= yc <= t1 + 0.002 for s0, t0, s1, t1, _, _ in span):
            continue
        lines.setdefault(lid, []).append((x0, y0, x1, y1, cf, w))
    for lid, ws in lines.items():
        ws.sort(key=lambda q: q[0])
        crop.append([ci, min(q[0] for q in ws), min(q[1] for q in ws), max(q[2] for q in ws), max(q[3] for q in ws), min(q[4] for q in ws), "crop", " ".join(q[5] for q in ws)])
os.remove(img + ".jpg")


def col_of(x0, x1):
    xc = (x0 + x1) / 2
    for ci, (l, r) in enumerate(cores):
        if l <= xc < r:
            return ci
    return -1


def norm(s):
    return re.sub(r"[^a-z0-9]", "", s.lower())


# full-page lines the crops missed (no crop line overlapping it vertically with similar text) are kept too
outrows = list(crop)
for x0, y0, x1, y1, cf, t in span:
    outrows.append([col_of(x0, x0 + 0.01), x0, y0, x1, y1, cf, "span", t])
for x0, y0, x1, y1, cf, t in full:
    if any(f == (x0, y0, x1, y1, cf, t) for f in span):
        continue
    ci = col_of(x0, x1)
    if ci >= 0 and (x0 < strips[ci][0] or x1 > strips[ci][1]):
        continue
    hit = False
    for c in crop:
        if c[0] == ci and min(y1, c[4]) - max(y0, c[2]) > 0.4 * (y1 - y0):
            a, b = norm(t), norm(c[7])
            if a and b and (a in b or b in a or len(set(a) & set(b)) > 0.6 * len(set(a))):
                hit = True
                break
    if not hit:
        outrows.append([ci, x0, y0, x1, y1, cf, "full", t])
outrows.sort(key=lambda r: (r[0] if r[0] >= 0 else 99, r[2]))
with open(out + ".tmp", "w", encoding="utf-8") as f:
    for r in outrows:
        f.write(f"{r[0]}\t{r[1]:.4f}\t{r[2]:.4f}\t{r[3]:.4f}\t{r[4]:.4f}\t{r[5]:.2f}\t{r[6]}\t{r[7]}\n")
os.replace(out + ".tmp", out)
