"""Recall check: words (5+ letters, seen 3+ times in the Vision OCR) that the scan's own Adobe text layer has on a page and the Vision OCR
of that page lacks. usage: recall.py <pdf>  ->  ../recall_pages.csv"""
import csv, glob, os, re, subprocess, sys
from collections import Counter
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
W = lambda s: re.findall(r"[a-z]{5,}", s.lower())
adobe = subprocess.run(["pdftotext", sys.argv[1], "-"], capture_output=True, text=True).stdout.split("\f")
vis, allw = {}, Counter()
for f in sorted(glob.glob(f"{ROOT}/lines/p*.tsv")):
    pg = int(f[-7:-4]); t = " ".join(l.rstrip("\n").split("\t")[-1] for l in open(f, encoding="utf-8"))
    vis[pg] = set(W(t)); allw.update(W(t))
out = []
for pg in sorted(vis):
    miss = sorted({w for w in W(adobe[pg - 1]) if allw[w] >= 3 and w not in vis[pg]})
    out.append((pg, len(miss), " ".join(miss)))
out.sort(key=lambda o: -o[1])
with open(f"{ROOT}/recall_pages.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["page", "n_missing", "missing_words"]); w.writerows(out)
print("total missing words", sum(o[1] for o in out), "; pages with >= 4:", sum(1 for o in out if o[1] >= 4))
for o in out[:12]: print(o[0], o[1], o[2][:150])
