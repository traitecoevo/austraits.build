"""Verticordia (E.A. George 2002), species accounts: OCR'd lines (lines/p*.tsv from page.py) -> one row per taxon.
Pieces of one visual line are joined; an entry starts at its heading ("4. Verticordia endlicheriana Schauer (1846)", "4a. Verticordia
endlicheriana Schauer var. endlicheriana") and runs to the next. A species entry holds the etymology and type paragraphs, the description
("A shrub to 1 m tall ... Ovules 2.") and, for a species with infraspecific taxa, a key to them ("There are five varieties: 1 Petals ...
...... var. major"); the notes of a taxon (a species without infraspecific taxa, or each variety / subspecies) are an opening paragraph and
labelled sections (DISTRIBUTION, HABITAT, FLOWERING PERIOD, DISTINGUISHING CHARACTERISTICS, RELATED (AND CONFUSING) SPECIES, OTHER NAME(S)
USED, CONSERVATION STATUS, CULTIVATION, PLATE caption). Line-break hyphens are resolved by the words seen whole elsewhere in the book.
usage: entries.py  ->  ../../verticordia_profiles.csv"""
import csv, difflib, glob, os, re
from collections import Counter
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MAPW = re.compile(r"^[^A-Za-z]*(?:[A-Za-z]\s?)?(?:Meekatharra|Geraldton|Laverton|Kalgoorlie|PERTH|Perth|Esperance|Albany|Carnarvon|Kununurra|Broome|"
                  r"Port Hedland|Darwin|Eucla|WA|JE|NT|SA)[^A-Za-z]*$")


def ov(a, b):
    return min(a[3], b[3]) - max(a[1], b[1]) > 0.5 * min(a[3] - a[1], b[3] - b[1])


stream = []   # (page, kind, text)
for f in sorted(glob.glob(f"{ROOT}/lines/p*.tsv")):
    pg = int(f[-7:-4])
    rows = []
    for ln in open(f, encoding="utf-8"):
        p = ln.rstrip("\n").split("\t")
        rows.append([p[1], float(p[2]), float(p[3]), float(p[4]), float(p[5]), p[7]])
    # pieces of one visual line in the same column (or full width): joined left to right
    out = []
    for r in rows:
        if MAPW.match(r[5]):
            continue
        if out and out[-1][0] == r[0] and ov(out[-1][1:5], r[1:5]):
            a, b = (out[-1], r) if out[-1][1] <= r[1] else (r, out[-1])
            out[-1] = [r[0], min(a[1], b[1]), min(a[2], b[2]), max(a[3], b[3]), max(a[4], b[4]), a[5] + " " + b[5]]
        else:
            out.append(r)
    stream += [(pg, r[0], r[5]) for r in out]

# line-break hyphens: "Aug-" + "ust" -> "August", "granitic-" + "loam" kept when the hyphenated word is seen elsewhere
words = Counter(w.lower() for _, _, t in stream for w in re.findall(r"[A-Za-z]+(?:-[A-Za-z]+)*", t)[1:-1])


def join(lines):
    out = ""
    for t in lines:
        t = t.strip()
        if not out:
            out = t
            continue
        m = re.search(r"([A-Za-z]+)-$", out)
        n = re.match(r"([a-z]+)", t)
        if m and n:
            a, b = m.group(1), n.group(1)
            if words[(a + b).lower()] >= words[(a + "-" + b).lower()] and not (words[b.lower()] > 0 and words[(a + "-" + b).lower()] > 0):
                out = out[:-1] + t
            else:
                out = out + t
        else:
            out = out + " " + t
    return re.sub(r"\s+", " ", out).strip()


HEAD = re.compile(r"^(\d{1,3})([a-z]?)\.\s?Verticordia\s+((?:[x×]\s?)?[a-z][a-z-]+)\b(.*)$")
entries, cur = [], None
for pg, kind, t in stream:
    m = HEAD.match(t)
    if m:
        cur = {"no": m.group(1) + m.group(2), "page": pg, "epithet": m.group(3), "rest": m.group(4).strip(), "lines": []}
        entries.append(cur)
        continue
    if cur is not None:
        cur["lines"].append(t)
# headings wrapped onto the next line ("49c. Verticordia insignis subsp" / "compta ...")
# (an infraspecific heading's species epithet is its species entry's: "15a. Verticordia servata var. serrata", OCR for serrata)
sp_ep = {re.match(r"\d+", e["no"]).group(): e["epithet"] for e in entries if not re.search(r"[a-z]$", e["no"])}
for e in entries:
    n = re.match(r"\d+", e["no"]).group()
    if re.search(r"[a-z]$", e["no"]) and n in sp_ep: e["epithet"] = sp_ep[n]
# (and "21b. Verticordia staminosa subsp. cylindracea A.S. George" / "var. cylindracea (1991)")
for e in entries:
    if (re.search(r"\b(?:subsp|var)\.?$", e["rest"]) or (e["lines"] and re.match(r"^(?:subsp|var)\.\s?[a-z]", e["lines"][0]))) and e["lines"]:
        e["rest"] += " " + e["lines"].pop(0)

LAB = [("DISTINGUISHING CHARACTERISTICS", "distinguishing"), ("RELATED AND CONFUSING SPECIES", "related_species"), ("RELATED SPECIES", "related_species"),
       ("OTHER NAMES USED", "other_names"), ("OTHER NAME USED", "other_names"), ("CONSERVATION STATUS", "conservation"), ("FLOWERING PERIOD", "flowering_period"),
       ("DISTRIBUTION", "distribution"), ("HABITAT", "habitat"), ("CULTIVATION", "cultivation"), ("PLATE", "plate")]
lab_re = re.compile(r"\b(" + "|".join(re.escape(l).replace(r"\ ", r"\s+") for l, _ in LAB) + r")\b")
FIELDS = ["etymology_type", "description", "key", "intro", "distribution", "habitat", "flowering_period", "distinguishing", "related_species",
          "other_names", "conservation", "cultivation", "plate"]
DESC_RE = r"(?:(?:Usually|Mostly|Often|Generally|Typically) )?(?:an?|one)\b[^.]{0,80}?\b(?:shrubs?|shrublets?|trees?|treelets?|plants?|subshrubs?|mallees?)\b"
KEY_RE = re.compile(r"(?:There are|The species has|This species has|Two|Three|Four|Five|Six)\b[^.:]{0,80}?\b(?:varieties|subspecies)\b[^.:]{0,60}[:.]?")
ETY_RE = re.compile(r"\b(?:This (?:species|specific|subspecies|varietal|variety|hybrid)|The (?:specific|subspecific|varietal) (?:name|epithet))\b")
rows = []
for e in entries:
    rest = e["rest"]
    # (every rank of the name: "Verticordia staminosa subsp. cylindracea var. erecta"; the rank is the last)
    infras = re.findall(r"\b(subsp|var)\.?\s+([a-z][a-z-]+)", rest)
    infra = infras[-1] if infras else None
    name = "Verticordia " + re.sub(r"^[x×]\s?", "x ", e["epithet"]) + "".join(f" {a}. {b}" for a, b in infras)
    auth = re.sub(r"\s*\b(?:subsp|var)\.?\s+[a-z][a-z-]+", "", rest).strip()
    r = {"entry_no": e["no"], "page": e["page"], "taxon_name": name, "rank": ("subspecies" if infra[0] == "subsp" else "variety") if infra else "species",
         "authority": auth, "common_name": ""}
    r.update({k: "" for k in FIELDS})
    txt = join(e["lines"])
    # the labelled notes: from the first DISTRIBUTION / HABITAT label
    ml = re.search(r"\b(?:DISTRIBUTION|HABITAT)\b", txt)
    head, notes = (txt[:ml.start()], txt[ml.start():]) if ml else (txt, "")
    # (a common name may open the entry, before "This species is named ...")
    me = ETY_RE.search(head)
    if me and 0 < me.start() < 60 and not infra:
        r["common_name"], head = head[:me.start()].strip(" .,;"), head[me.start():]
    mk = KEY_RE.search(head)
    md = re.search(r"(?:^|(?<=[.)]\s))" + DESC_RE, head, re.I) if not infra else None
    # (a description following a sentence that lost its full stop: "... by William Vincent Fitzgerald A shrub to more than 2m tall ...", V. picta)
    if not md and not infra:
        md = re.search(r"(?<=[a-z] )An? (?:[a-z-]+ ){0,4}?(?:shrub|shrublet|tree|subshrub)\b", head)
    if md:
        r["etymology_type"] = head[:md.start()].strip()
        desc = head[md.start():mk.start() if mk and mk.start() > md.start() else len(head)].strip()
        mo = re.search(r"\bOvules\s[^.]*\.", desc)
        if mo and not mk:
            r["description"], r["intro"] = desc[:mo.end()], desc[mo.end():].strip()   # (an unlabelled opening note may follow "Ovules N.")
        else:
            r["description"] = desc
        if mk and mk.start() > md.start():
            r["key"] = head[mk.start():].strip()
    else:
        # (a subspecies / variety: common name, etymology and type collection, then its description from "This (typical) variety is a
        # small, usually compact shrub 15-60 cm tall ..." or a habit sentence; V. endlicheriana var. compacta)
        me = ETY_RE.search(head)
        if me and 0 < me.start() < 60:
            r["common_name"], head = head[:me.start()].strip(" .,;"), head[me.start():]
        # (the sentences on the name and the collections are the etymology / type; the rest is the description, "This is a slender, upright
        # shrub to ...", "The typical subspecies is a small, multi-stemmed shrub ...")
        sents = re.split(r"(?<=[a-z0-9)]\.)\s+(?=[A-Z])", head.strip())
        ety = [bool(re.search(r"\b(?:name|epithet|named|commemorates|honours?|derived from|type collection|collections? (?:was|were)|collected|gathered|"
                               r"Drummond|Latin|Greek)\b", x)) for x in sents]
        r["etymology_type"] = " ".join(x for x, e in zip(sents, ety) if e)
        r["description"] = " ".join(x for x, e in zip(sents, ety) if not e)
    if notes:
        parts = lab_re.split(notes)
        for lab, val in zip(parts[1::2], parts[2::2]):
            k = dict(LAB)[re.sub(r"\s+", " ", lab)]
            r[k] = (r[k] + " " + val.strip()).strip()
    rows.append(r)

# keys to the infraspecific taxa: each taxon gets the leads on its path through its species' key ("1: Sepals 1.5-4mm long ... 2: Sepals 2.5-4
# mm long ... 4 Leaves 7-14mm long ... 5 Bushy shrub to 80 cm ... var. plumosa"), used only when the key reads cleanly (its terminal
# names are the species' infraspecific taxa, each once, and every couplet has its two leads)
LEAD = re.compile(r"(?:^|(?<=\s))(\d{1,2})(:?)\s+(?=[A-Z])")
ANS = re.compile(r"\.{3,}[\s.•·,]*((?:subsp|var)\.\s*[a-z][a-z-]+)")
key_log = []
for sp in [r for r in rows if r["key"]]:
    no = re.match(r"\d+", sp["entry_no"]).group()
    kids = {r["taxon_name"].split()[-1]: r for r in rows if r["rank"] != "species" and re.match(r"\d+", r["entry_no"]).group() == no}
    k = sp["key"]
    ms = list(LEAD.finditer(k))
    leads = []
    if ms:
        # (from the first numbered lead: the opening sentence, "There are seven varieties, but many intermediate forms also occur", is not a lead)
        for i, m in enumerate(ms):
            t = k[m.end():ms[i + 1].start() if i + 1 < len(ms) else len(k)].strip()
            a = ANS.search(t)
            leads.append((int(m.group(1)), m.group(2) == ":", re.sub(r"[\s.;,]+$", "", t[:a.start()] if a else t).strip(), a.group(1).split()[-1] if a else None))
    else:
        # (a two-taxon key has no numbers: "There are two varieties: Staminodes fimbriate; ... ...... var. acerosa Staminodes dentate; ... ...... var. preissii")
        k = re.sub(r"^[^:]*:\s*", "", k)
        prev = 0
        for a in ANS.finditer(k):
            leads.append((1, prev > 0, re.sub(r"[\s.;,]+$", "", k[prev:a.start()]).strip(), a.group(1).split()[-1])); prev = a.end()
    # (answers misread by the OCR, "subsp. roet" for roei: matched to the nearest infraspecific name)
    leads = [(n, alt, t, (difflib.get_close_matches(ans, list(kids), n=1, cutoff=0.7) or [ans])[0] if ans else None) for n, alt, t, ans in leads]
    stack, paths, ok = [], {}, True
    for n, alt, t, ans in leads:
        if not alt:
            stack.append((n, alt, t))
        else:
            while stack and not (stack[-1][0] == n and not stack[-1][1]): stack.pop()
            if not stack: ok = False; break
            stack[-1] = (n, alt, t)
        if ans: paths[ans] = [x[2] for x in stack]
    cnt = Counter((n, alt) for n, alt, _, _ in leads)
    ok = ok and set(paths) == set(kids) and all(cnt[(n, True)] == 1 and cnt[(n, False)] == 1 for n, _ in cnt)
    key_log.append((sp["taxon_name"], ok, len(paths), len(kids)))
    if ok:
        for nm, p in paths.items():
            kids[nm]["key_path"] = ". ".join(x.rstrip(";,") for x in p) + "."
for r in rows: r.setdefault("key_path", "")
print("keys read:", sum(1 for x in key_log if x[1]), "of", len(key_log), "; not read:", [x for x in key_log if not x[1]])

with open(f"{ROOT}/../verticordia_profiles.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["entry_no", "page", "taxon_name", "rank", "authority", "common_name"] + FIELDS + ["key_path"])
    w.writeheader(); w.writerows(rows)
print(len(rows), "entries;", Counter(r["rank"] for r in rows), "; empty:", {k: sum(1 for r in rows if not r[k]) for k in FIELDS})
