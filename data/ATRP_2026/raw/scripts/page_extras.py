"""ATRP (Australian Tropical Rainforest Plants, Lucid online edition) pages saved in ATRP_html/: what atrp_fact_sheets.csv does not hold.
  - habit_icons: the page's type icons, in page order ("Tree; Shrub (woody or herbaceous, 1-6 m tall)"), the site's own growth-form scoring
  - germination: epigeal / hypogeal, from the photo captions ("Cotyledon stage, epigeal germination. (c) CSIRO"), with the caption
usage: page_extras.py  ->  ../atrp_page_extras.csv (one row per page, keyed by file as in atrp_fact_sheets.csv)"""
import csv, glob, html, os, re
HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
rows = []
for f in sorted(glob.glob(f"{HERE}/ATRP_html/*.htm")):
    t = open(f, encoding="utf-8", errors="replace").read()
    icons = re.findall(r'class="type-icon" alt="([^"]*)"', t)
    caps = sorted({html.unescape(c).strip() for c in re.findall(r'data-footer="([^"]*?(?:epigeal|hypogeal)[^"]*)"', t, re.I)})
    germ = sorted({g.lower() for c in caps for g in re.findall(r"\b(epigeal|hypogeal)\b", c, re.I)})
    rows.append({"file": os.path.basename(f), "habit_icons": "; ".join(icons), "germination": " ".join(germ), "germination_caption": " | ".join(caps)})
with open(f"{HERE}/atrp_page_extras.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["file", "habit_icons", "germination", "germination_caption"]); w.writeheader(); w.writerows(rows)
print(len(rows), "pages;", sum(bool(r["habit_icons"]) for r in rows), "with icons;", sum(bool(r["germination"]) for r in rows), "with germination")
