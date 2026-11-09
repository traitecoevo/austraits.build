"""Brophy, Craven & Doran (2013) species accounts -> one row per species (blocks/*_blocks.tsv from blocks.py).
A species starts at its heading block ("Melaleuca acacioides F.Muell.", near the top of a page); its text runs to the next heading.
Fields are split at the book's labels (Publication:, Derivation:, Description:, Natural occurrence:, Ecology:, Flowering time:, ...).
Line ends: a soft hyphen joins the word; a hard hyphen is resolved by word frequency in the book ("rap-idly" -> "rapidly", "grey-green" kept).
usage: profiles.py  ->  ../brophy2013_profiles.csv"""
import csv, os, re
from collections import Counter
HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = ["species-a-c", "species-d-k", "species-l-o", "species-p-r", "species-s-z"]
LABELS = {"Taxonomy": "taxonomy", "Synonym": "synonyms", "Synonyms": "synonyms", "Publication": "publication", "Publications": "publication",
          "Derivation": "derivation", "Description": "description", "Natural occurrence": "natural_occurrence", "Ecology": "ecology",
          "Flowering time": "flowering_time", "Essential oils": "essential_oils", "Essential oil": "essential_oils", "Oil yield": "oil_yield",
          "Essential oil yield": "oil_yield", "Reference on essential oils": "oil_references", "References on essential oils": "oil_references",
          "Notes": "notes", "Note": "notes",
          # (the book's own variants: M. depauperata "Natural Occurrence:", M. nesophila "Natural Distribution:")
          "Natural Occurrence": "natural_occurrence", "Natural Distribution": "natural_occurrence"}
FIELDS = ["taxonomy", "synonyms", "publication", "derivation", "description", "natural_occurrence", "ecology", "flowering_time",
          "essential_oils", "oil_yield", "oil_references", "notes"]
lab_re = re.compile(r"(?:^|(?<=\n)|(?<=[.;)\]] ))(" + "|".join(sorted(map(re.escape, LABELS), key=len, reverse=True)) + r")\s?:\s*")
# letter-spaced lines in the text layer (three, all in species-a-c), as printed
SPACED = {"( s o m e t i m e s m o re o r l e s s s o ) o r s c at t e re d .": "(sometimes more or less so) or scattered.",
          "w i d e . Pe tal s d e c i d u o u s , 2 . 5 – 3 . 3 m m l o n g .": "wide. Petals deciduous, 2.5–3.3 mm long.",
          "De s c ri p t ion : S hr ub or tre e 0 . 5 – 4 m t a l l .": "Description: Shrub or tree 0.5–4 m tall."}
HEAD_RE = re.compile(r"^Melaleuca ([a-z][a-z-]+)\s+(.+)$")


def join_lines(t):
    t = re.sub("­\\s*\n\\s*", "", t)           # soft hyphen at a line end
    t = t.replace("­", "")
    # a hard hyphen at a line end ("rap-\nidly", "grey-\ngreen"): joined when the joined word is commoner in the book than the hyphenated one;
    # when neither is found elsewhere, joined unless the second piece is a word of its own ("incor-rectly" -> joined, "peg-fruited" kept)
    def hy(m):
        a, b = m.group(1), m.group(2)
        nj, nh = WORDS[(a + b).lower()], WORDS[(a + "-" + b).lower()]
        join = nj > nh or (nj == nh == 0 and WORDS[b.lower()] == 0)
        if b in ("and", "or", "to"): return a + "- " + b                  # "mono- and sesquiterpenes", "red- or purple-flowered"
        return a + ("" if join else "-") + b
    t = re.sub(r"([A-Za-z]+)-\n([a-z]+)", hy, t)
    t = re.sub(r"\b(paper|hair) y\b", r"\1y", t)        # kerning gaps in the text layer ("bark paper y", "glabrescent to hair y")
    return re.sub(r"\s*\n\s*", " ", t).strip()


out, sp = [], None
for tag in FILES:
    f = f"{HERE}/blocks/{tag}_blocks.tsv"
    if not os.path.exists(f):
        continue
    for ln in open(f, encoding="utf-8"):
        pg, bp, col, x0, y0, x1, y1, t = ln.rstrip("\n").split("\t")
        t = "\n".join(SPACED.get(l, l) for l in t.split("\\n")).strip()
        m = HEAD_RE.match(t.replace("\n", " ")) if float(y0) < 120 else None
        if m:
            sp = {"file": tag, "pdf_page": pg, "book_page": bp, "taxon_name": "Melaleuca " + m.group(1), "authority": m.group(2).strip(), "text": []}
            out.append(sp)
        elif sp is not None:
            sp["text"].append(t)

# word counts over the whole book, line-internal words only (a line's first and last words may be pieces of a broken word)
WORDS = Counter(w.lower() for sp in out for t in sp["text"] for ln in t.replace("\u00ad", "").split("\n")
                for w in re.findall(r"[A-Za-z]+(?:-[A-Za-z]+)*", ln)[1:-1])
rows = []
for sp in out:
    # (labels are matched before the lines are joined, so a label opening a line is found too)
    parts = lab_re.split("\n".join(sp["text"]))
    r = {k: sp[k] for k in ("file", "pdf_page", "book_page", "taxon_name", "authority")}
    r.update({k: "" for k in FIELDS})
    r["pre_text"] = join_lines(parts[0])
    for lab, val in zip(parts[1::2], parts[2::2]):
        k = LABELS[lab]
        r[k] = (r[k] + " " + join_lines(val)).strip()
    # (M. nesophila: the book prints its publication under a "Description:" label, and the description has no label)
    m = re.match(r"^((?:in [^.]*?|[A-Z][^.]*?)\(1[89]\d\d\))\s+(?=[A-Z][a-z]+ (?:or |to |\d))", r["description"])
    if m and not r["publication"]:
        r["publication"], r["description"] = m.group(1), r["description"][m.end():]
    rows.append(r)

with open(f"{HERE}/../brophy2013_profiles.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["file", "pdf_page", "book_page", "taxon_name", "authority"] + FIELDS + ["pre_text"])
    w.writeheader()
    w.writerows(rows)
print(len(rows), "species; empty:", {k: sum(1 for r in rows if not r[k]) for k in FIELDS + ["pre_text"]})
