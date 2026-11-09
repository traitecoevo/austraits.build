"""Brophy, Craven & Doran (2013) Melaleucas (ACIAR Monograph 156), species accounts: the PDFs' own text layer in reading order.
pdftotext -bbox-layout gives text blocks with positions; per page, blocks are read left column then right column, top to bottom.
Dropped: the rotated running heads (narrow, tall blocks), page numbers, map / photo captions above the text (handled in profiles.py).
usage: blocks.py <pdf> <tag>  ->  <out>/<tag>_blocks.tsv: page, book_page, col, x0, y0, x1, y1, text"""
import re, subprocess, sys, html, os
pdf, tag = sys.argv[1], sys.argv[2]
OUT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "blocks")
os.makedirs(OUT, exist_ok=True)
h = subprocess.run(["pdftotext", "-bbox-layout", pdf, "-"], capture_output=True, text=True).stdout
rows = []
for pi, pg in enumerate(re.findall(r"<page width=\"([\d.]+)\".*?</page>", h, re.S) and re.findall(r"<page.*?</page>", h, re.S), start=1):
    W = float(re.search(r'width="([\d.]+)"', pg).group(1))
    blocks, bp = [], ""
    for b in re.finditer(r'<block xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</block>', pg, re.S):
        x0, y0, x1, y1 = map(float, b.groups()[:4])
        lines = [" ".join(html.unescape(w) for w in re.findall(r">([^<]*)</word>", ln)) for ln in re.findall(r"<line.*?</line>", b.group(5), re.S)]
        t = "\n".join(lines)
        if x1 - x0 < 25 and y1 - y0 > 100: continue                      # rotated running head
        if y0 > 800 and re.fullmatch(r"\d{1,3}", t.strip()): bp = t.strip(); continue   # page number
        blocks.append((0 if x0 < W / 2 - 10 else 1, y0, x0, x1, y1, t))
    for col, y0, x0, x1, y1, t in sorted(blocks):
        rows.append((pi, bp, col, x0, y0, x1, y1, t.replace("\t", " ").replace("\n", "\\n")))
with open(f"{OUT}/{tag}_blocks.tsv", "w", encoding="utf-8") as fh:
    for r in rows: fh.write("\t".join(str(v) for v in r) + "\n")
print(tag, len(rows), "blocks")
