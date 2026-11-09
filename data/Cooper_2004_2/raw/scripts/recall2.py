"""Recall check: words (5+ letters, seen 3+ times in the Vision text) that the scan's own Adobe text layer has on a page and the Vision
OCR of that page lacks. usage: recall2.py <out_dir with pages.csv> <ocr dir name, e.g. ocr2>  -> <out_dir>/recall_pages.csv"""
import csv, re, sys
from collections import Counter
SP = "/private/tmp/claude-502/-Users-z3524079-GitHub-traits-build-inst-support/fb9a03f2-b5ee-4e53-a6f9-9631bd56b1ab/scratchpad/cooper"
OUT, OCR = sys.argv[1], sys.argv[2]
chosen = [r for r in csv.DictReader(open(f"{OUT}/pages.csv")) if r["chosen"] == "True"]
W = lambda s: re.findall(r"[a-z]{5,}", s.lower())
vis, allw = {}, Counter()
for r in chosen:
    t = " ".join(l.rstrip("\n").split("\t")[-1] for l in open(f"{SP}/{OCR}/{r['scan']}_{int(r['scan_page']):03d}.tsv", encoding="utf-8"))
    vis[r["book_page"]] = set(W(t)); allw.update(W(t))
tot, out = 0, []
for r in chosen:
    pg = open(f"{SP}/txt/Cooper_2004_{r['scan']}.txt", errors="replace").read().split("\f")[int(r["scan_page"]) - 1]
    miss = sorted({w for w in W(pg) if allw[w] >= 3 and w not in vis[r["book_page"]]})
    tot += len(miss)
    out.append((r["book_page"], r["scan"], r["scan_page"], len(miss), " ".join(miss)))
out.sort(key=lambda o: -o[3])
with open(f"{OUT}/recall_pages.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["book_page", "scan", "scan_page", "n_missing", "missing_words"]); w.writerows(out)
print("total missing words", tot, "; pages with >= 4:", sum(1 for o in out if o[3] >= 4))
for o in out[:15]:
    print(o[0], o[1], o[2], o[3], o[4][:120])
