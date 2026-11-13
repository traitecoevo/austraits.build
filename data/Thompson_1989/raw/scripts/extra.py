"""Scripted extraction of the regular characters of Thompson (1989) Leptospermum descriptions:
stems (branching angle, constitution), bark, leaf scent, flowers, hypanthium, sepals, petals, stamens, ovary,
fruit, seeds, flowering period, distribution and habitat. Every row is reviewed in review_extra.txt."""
import json, re
from pathlib import Path

here = Path(__file__).parent
WORDS = {w.strip().lower() for w in open("/usr/share/dict/words")} | {
    "glabrescent", "pubescent", "pubescence", "hypanthium", "bracteoles", "loculus", "locular", "placenta", "ovules", "anther",
    "filaments", "stigma", "inset", "exserted", "deciduous", "persistent", "diameter", "flowering", "specimens", "aromatic",
    "elliptical", "oblanceolate", "obovate", "acuminate", "umbo", "cuneiform", "reticulate", "striate", "scarious", "ciliate",
    "glabrous", "stamens", "sepals", "petals", "flange", "exfoliating", "flaking", "more", "mostly", "much", "smooth", "almost",
    "between", "without", "within", "maturity", "midrib", "valves", "seeds", "mature", "rather", "numerous", "prominent", "minute",
    "remaining", "ultimately", "intermediate", "multistemmed", "compact", "tasmania", "queensland", "granite", "sandstone", "lateritic"}
SPACED = [("Distribution:", r"D\s*i\s*s\s*t\s*r\s*i\s*b\s*u\s*t\s*i\s*o\s*n\s*:"),
          ("Selected specimens:", r"S\s*e\s*l\s*e\s*c\s*t\s*e\s*d\s+s\s*p\s*e\s*c\s*i\s*m\s*e\s*n\s*s\s*:"),
          ("Specimens examined:", r"S\s*p\s*e\s*c\s*i\s*m\s*e\s*n\s*s\s+e\s*x\s*a\s*m\s*i\s*n\s*e\s*d\s*:"),
          ("Main flowering period:", r"M\s*ain\s+flowering\s+period\s*:")]


def dejoin(s):
    s = re.sub(r"[ \t]+", " ", s)
    for good, pat in SPACED:
        s = re.sub(pat, good, s)
    s = re.sub(r"\b\d{3} \S{2,12} \S{2,6} 3\(3\): 1989\b ?", "", s)   # garbled running headers ("406 7W(%x%z Pb/. 3(3): 1989")
    s = re.sub(r"\bI\.(\d)", r"1.\1", s)                 # OCR "I.5" for 1.5
    s = re.sub(r"\bDee\.", "Dec.", s).replace("M ay ", "May ")
    s = re.sub(r"\bM (ar|ay)\b", r"M\1", s)
    s = re.sub(r"(?<=\d-)(\d) (\d)\b", r"\1\2", s)       # "6-1 0" -> "6-10"
    s = re.sub(r"(\d) \.(\d)", r"\1.\2", s)               # "3 .5" -> "3.5"
    s = re.sub(r"(\d)mm\b", r"\1 mm", s)
    for w in ["Stamens", "Hypanthium", "diameter", "flowering", "persistent", "deciduous", "Mature", "Main", "Sepals", "Petals",
              "ovules", "loculus", "aromatic", "glabrous", "pubescent", "numerous", "Rather", "Much", "Robust", "Compact", "Multistemmed",
              "more", "mostly", "much", "without", "almost"]:
        s = re.sub(r"\b" + r"\s?".join(w) + r"\b", w, s)
    s = re.sub(r"(\d)\s*m\s+m\b", r"\1 mm", s)
    s = re.sub(r"\bm\s+m\b", "mm", s)
    toks = s.split(" ")
    out, i = [], 0
    while i < len(toks):
        a = toks[i]
        if i + 1 < len(toks):
            b = toks[i + 1]
            ca, cb = re.sub(r"\W", "", a), re.sub(r"\W", "", b)
            j = (a + b)
            jc = re.sub(r"[^A-Za-z]", "", j).lower()
            if ca.isalpha() and cb.isalpha() and re.match(r"^[A-Za-z]+$", a) and jc in WORDS and \
                    (ca.lower() not in WORDS or cb.lower() not in WORDS or len(ca) == 1 or ca.lower() in {"m", "w", "o", "um"}):
                out.append(j); i += 2; continue
        out.append(a); i += 1
    s = " ".join(out)
    s = re.sub(r"(\d)\s*[-—–]\s*(\d)", r"\1-\2", s)
    s = re.sub(r"\(\s*(\d[\d.]*)\s*[-—–]\s*\)", r"(\1-)", s)
    s = re.sub(r"\(\s*[-—–]\s*(\d[\d.]*)\s*\)", r"(-\1)", s)
    s = re.sub(r"(\d)\s*°", r"\1°", s)
    s = re.sub(r"(?<=\d-)(\d) (\d)\b", r"\1\2", s)
    s = re.sub(r"\(\s*M\s*aps?\s", "(Map ", s)
    s = re.sub(r"\blong-?\s?persist", "long-persist", s)
    s = re.sub(r"(\d)-1 1-locular", r"\1-11-locular", s)
    s = re.sub(r"(\d)\s?locular", r"\1-locular", s)
    s = re.sub(r"(\d)\s*-\s*\(-\s*(\d)\)\s*locular", r"\1(-\2)-locular", s)   # "5 - (-6) locular"
    s = re.sub(r"(\))-\s+locular", r"\1-locular", s)                                  # "3-4(-5)- locular"
    s = re.sub(r"(\d)-celled", r"\1-locular", s)
    return re.sub(r"\s+", " ", s)


ORGANS = ["Flowers", "Bracts", "Hypanthium", "Sepals", "Petals", "Stamens", "Style", "Ovary", "Fruits", "Fruit",
          "Mature seeds", "Seeds", "Main flowering period:", "Distribution:", "Selected specimens:", "Specimens examined:"]


def segments(text):
    """organ -> sentence(s), from 'Organ' to the next organ heading."""
    pos = []
    for o in ORGANS:
        for m in re.finditer(r"(?:(?<=[\.;\)°] )|(?<=\. ))" + re.escape(o) + (r"\b" if o[-1].isalpha() else ""), text):
            pos.append((m.start(), o))
    pos.sort()
    seg = {}
    for k, (p, o) in enumerate(pos):
        e = pos[k + 1][0] if k + 1 < len(pos) else len(text)
        key = {"Fruits": "Fruit", "Seeds": "Mature seeds"}.get(o, o)
        if key not in seg:
            seg[key] = text[p:e].strip()
    return seg


MONTHS = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]


def months(s):
    """'Nov.-Jan.', 'Mar.-May and Sept.-Oct.', 'Jan.' -> 12-character y/n code"""
    s = re.sub(r"\bDee\b", "Dec", s)
    flags = ["n"] * 12
    any_m = False
    for part in re.split(r"\band\b|;|,", s):
        found = re.findall(r"\b(jan|feb|mar|apr|may|june?|july?|aug|sept?|oct|nov|dec)[a-z]*\.?", part.lower())
        idx = [MONTHS.index(m[:3]) for m in found]
        if not idx:
            continue
        any_m = True
        a, b = idx[0], idx[-1]
        k = a
        while True:
            flags[k] = "y"
            if k == b: break
            k = (k + 1) % 12
    return "".join(flags) if any_m else None


def nums(s):
    """'2-3 (-7)', '(5-) 10-15 (-18)', 'c. 3', 'to c. 4', '1.5-c. 3' -> rows of (value, value_type, qualifier, note)"""
    s = s.replace("c. ", "c.").strip()
    rows = []
    m = re.match(r"^(?:\((\d[\d.]*)-\)\s*)?(?:to )?(c\.)?(\d[\d.]*)(?:\s*-\s*(?:c\.)?(\d[\d.]*))?(?:\s*\(-(\d[\d.]*)\))?", s)
    if not m:
        return rows
    lo_x, c, a, b, hi_x = m.groups()
    to = s.startswith("to ")
    ext = bool(lo_x or hi_x)
    q = "usually" if ext else ""
    if b:
        rows += [(a, "minimum", q), (b, "maximum", q)]
    elif to:
        rows.append((a, "maximum", q))
    else:
        rows.append((a, "mean" if c else "mode", q))
    if lo_x: rows.append((lo_x, "minimum", "extreme_range"))
    if hi_x: rows.append((hi_x, "maximum", "extreme_range"))
    return rows


NUM = r"(?:\(\d[\d.]*-\)\s*)?(?:to )?(?:c\. ?)?\d[\d.]*(?:\s*-\s*(?:c\. ?)?\d[\d.]*)?(?:\s*\(-\d[\d.]*\))?"


def angle_rows(phr):
    """branching angle phrase -> (value, value_type, qualifier, note)"""
    p = phr.replace("an angle of ", "").replace("a variable angle of ", "").replace("c. ", "c.").strip()
    q = "usually" if p.startswith("usually") else ("often" if p.startswith("often") else "")
    p = re.sub(r"^(?:usually|often|mostly) (?:at )?", "", p)
    p = re.sub(r"^from ", "", re.sub(r"\s*\(flush with the stem\)", "", p))
    n = [int(x) for x in re.findall(r"(\d+)°", p)]
    out = []
    m = re.match(r"^\(?(\d+)°-\)\s*(\d+)°\s*\(-(\d+)°\)", p)
    if m:   # (30°-) 45°(-60°)
        return [(m.group(2), "mode", "usually", ""), (m.group(1), "minimum", "extreme_range", ""), (m.group(3), "maximum", "extreme_range", "")]
    m = re.match(r"^c\.\s*\((\d+)°-\)\s*(\d+)°", p)
    if m:   # c. (40°-)30°
        return [(m.group(2), "mean", "usually", ""), (m.group(1), "maximum", "extreme_range", "printed as c. (40°-)30°")]
    m = re.match(r"^(c\.)?\s*(\d+)°\s*\(-(\d+)°\)", p)
    if m:   # c. 45° (-30°)
        lo, hi = sorted([int(m.group(2)), int(m.group(3))])
        return [(m.group(2), "mean" if m.group(1) else "mode", "usually", ""),
                (m.group(3), "minimum" if int(m.group(3)) < int(m.group(2)) else "maximum", "extreme_range", "")]
    if "or even" in p and len(n) == 2:
        return [(str(n[0]), "mode", "usually", ""), (str(n[1]), "maximum", "extreme_range", "or even " + str(n[1]) + "°")]
    if re.search(r"or (?:even )?less", p) and len(n) == 1:
        return [(str(n[0]), "maximum", q, p)]
    if re.search(r"or more", p) and len(n) == 1:
        return [(str(n[0]), "minimum", q, p)]
    if len(n) >= 2:
        lo, hi = min(n[:2]), max(n[:2])
        note = p if re.search(r"less than|more than|flush|or more|but", p) else ""
        return [(str(lo), "minimum", q, note), (str(hi), "maximum", q, note)]
    if len(n) == 1:
        return [(str(n[0]), "mean" if "c." in p else "mode", q, "")]
    return out


def size_rows(seg, tail):
    """(rows, raw) for 'from less than 5 to more than 12 mm', 'often more than 20 mm', 'to more than 30 mm' or a plain NUM"""
    m = re.search(r"usually (\d[\d.]*)-(\d[\d.]*), (occasionally|rarely|often) (?:more than |up to |to )?(\d[\d.]*)\s*mm " + tail, seg)
    if m:
        return [(m.group(1), "minimum", "usually", ""), (m.group(2), "maximum", "usually", ""),
                (m.group(4), "minimum" if "more than" in m.group(0) else "maximum", m.group(3), m.group(0).split(", ")[1].split(" mm")[0])], m.group(0)
    m = re.search(r"from less than (\d[\d.]*) to more than (\d[\d.]*)\s*mm " + tail, seg)
    if m:
        return [(m.group(1), "minimum", "", "from less than " + m.group(1)), (m.group(2), "maximum", "", "to more than " + m.group(2))], m.group(0)
    m = re.search(r"(often |usually )?(to )?more than (\d[\d.]*)\s*mm " + tail, seg)
    if m:
        q = (m.group(1) or "").strip()
        return [(m.group(3), "maximum" if m.group(2) else "minimum", q, m.group(0).split(" mm")[0])], m.group(0)
    m = re.search(r"(" + NUM + r")\s*mm (?:or more )?" + tail, seg)
    if m:
        return [(v, vt, q, "or more" if "or more" in m.group(0) else "") for v, vt, q in nums(m.group(1))], m.group(0)
    return [], None


def first_size(seg, word="long"):
    m = re.search(r"(" + NUM + r")\s*mm (?:" + word + r")", seg)
    return (m.group(1), m.group(0)) if m else (None, None)


soil_words = ["sand", "sandy", "sandy loam", "loam", "clay", "clayey", "gravel", "gravelly", "lateritic gravel", "laterite", "peat", "peaty",
              "alluvium", "alluvial", "rocky", "skeletal", "shallow", "deep", "basaltic soils", "podzolic", "swampy", "moist", "wet", "damp"]
geology_words = ["granite", "granitic", "sandstone", "basalt", "basaltic", "quartzite", "serpentinite", "serpentine", "laterite", "lateritic",
                 "limestone", "dolerite", "rhyolite", "trachyte", "shale", "conglomerate", "quartz", "ironstone", "slate", "schist", "volcanic"]
habitat_words = ["heath", "heaths", "heathland", "forests", "woodlands", "scrubs", "rainforest", "wet sclerophyll forest", "dry sclerophyll forest", "sclerophyll forest", "forest", "woodland",
                 "scrub", "mallee", "swamp", "swamps", "bog", "bogs", "creek", "creeks", "creek-banks", "stream", "streams", "stream-beds",
                 "river", "rivers", "watercourses", "gully", "gullies", "escarpment", "escarpments", "cliffs", "outcrops", "rock outcrops",
                 "boulders", "rocks", "ridges", "slopes", "plateau", "coastal", "dunes", "headlands", "mountain", "mountains", "subalpine", "alpine",
                 "flats", "coastal flats", "plains", "riparian", "rainforest margins", "forest margins", "grassland", "moorland", "sedgeland"]


def word_hits(text, words):
    hits = []
    for w in sorted(words, key=len, reverse=True):
        if re.search(r"\b" + re.escape(w) + r"\b", text, re.I) and not any(w in h for h in hits):
            hits.append(w)
    order = sorted(hits, key=lambda w: re.search(r"\b" + re.escape(w) + r"\b", text, re.I).start())
    return " ".join(x.replace(" ", "_") for x in order)


def colour_map(words):
    m = {"white": "white_cream", "cream": "white_cream", "greenish": "", "pink": "pink", "pinkish": "pink", "red": "red_brown",
         "reddish": "red_brown", "crimson": "red_brown", "mauve": "blue_purple", "purple": "blue_purple"}
    m.update({"creamy": "white_cream", "greenish": ""})
    out = []
    words = re.sub(r"purplish pink", "pink", words)
    for w in re.findall(r"[a-z]+", words.lower()):
        v = m.get(w, "")
        if v and v not in out: out.append(v)
    return " ".join(out)


def extract(name, text, page, is_subsp):
    rows = []
    sec = ("Subspecies description" if is_subsp else "Description") + f", p.{page}"

    def add(trait, raw, val, clean, vtype="mode", unit="", q="", note="", ent="", inyml=True, section=None):
        rows.append(dict(taxon_name=name, entity_type="species", rank_scored_from="", commonness_qualifier=q, population_region="",
                         entity_measured=ent, trait=trait, **{"in_traits.yml": "x" if inyml else ""}, trait_value_raw=raw,
                         trait_value=val, trait_value_clean=clean, value_type=vtype, unit=unit, source_section=section or sec,
                         notes=(note if inyml else ("Candidate trait, not in config/traits.yml (checked 2026-11). " + note).strip())))

    def addnum(trait, raw, s, unit, note="", ent="", inyml=True):
        for v, vt, q in nums(s):
            add(trait, raw, v, v, vt, unit, q, note, ent, inyml)

    seg = segments(text)
    habit = re.split(r";|, the younger|, with the younger|\. Leaves", text)[0]
    # --- stems
    m = re.search(r"younger (?:stems?|branches) ([^,;]*?)(?:,|;| with)", text)
    if m:
        w = m.group(1)
        mm = re.search(r"\b(very slender|slender|very stout|rather stout|stout|relatively stout|thin|often slender|usually thin|usually rather stout|often rather stout|rather slender|short and stout|stout and tapering|relatively stout)\b", w)
        if mm:
            v = mm.group(1)
            clean = "slender" if re.search(r"slender|thin", v) else "robust"
            q = "often" if v.startswith("often") else ("usually" if v.startswith("usually") else "")
            add("stem_constitution", "the younger stems " + w, v, clean, ent="younger stems", q=q,
                note="stout mapped to robust (ABRS_2026 stem_constitution dictionary)" if clean == "robust" else "")
    hw = []
    for w, clean in [("straggling", "straggly"), ("rigid", "rigid"), ("robust", "robust"), ("slender", "slender")]:
        hm = re.search(r"\b" + w + r"\b", habit.split(" with ")[0].split(" from ")[0], re.I)
        if hm and not re.search(r"(?:younger|young) stems", habit[:hm.start()]):
            pre = habit[:hm.start()]
            q = "often" if re.search(r"\boften(?: very)? $", pre) else ("usually" if re.search(r"\busually\b[^,;]*$", pre) else "")
            hw.append((hm.start(), w, clean, q))
    if hw:   # one row, in text order ("Straggling slender shrub")
        hw.sort()
        add("stem_constitution", habit, " ".join(w for _, w, _, _ in hw), " ".join(c for _, _, c, _ in hw), q=hw[0][3], note="from the habit clause")
    ang = re.search(r"(?:branch(?:ing|es)|spreading widely from each other)(?:[^;.°]|c\.)*?\d+°", text)
    if ang:
        st0 = ang.start()
        # the phrase runs from the first number (or "c." / "(" / "from") to the end of the clause
        head = re.search(r"(?:from |c\. ?)?\(?(?:less than )?\d+°", text[st0:]).start() + st0
        tail = re.split(r";|\. (?=[A-Z])| but |, (?!\d)", text[head:], maxsplit=1)[0]
        lead = text[max(0, st0 - 12):head]
        q = "usually" if re.search(r"\b(usually|mostly)\b", lead) else ("often" if re.search(r"\boften\b", lead) else "")
        raw = text[st0:head] + tail
        for v, vt, q2, note in angle_rows(tail):
            add("stem_branching_angle", raw, v, v, vt, "degrees", q2 or q, note, "younger stems", inyml=False)
    # --- bark
    bi = re.search(r"\bbark\b", habit)
    if bi:
        st = max(habit.rfind(" with", 0, bi.start()), habit.rfind(" the ", 0, bi.start()), habit.rfind(", the", 0, bi.start()))
        b = re.sub(r"^[\s,]*(?:with,?\s*)?", "", habit[st:] if st >= 0 else habit[bi.start():]).strip()
        b = re.sub(r"^(?:[a-z ]*branches and |a horizontal trunk and )", "", b)
        tex = []
        for pat, lev in [(r"smooth", "smooth"), (r"rough", "rough"), (r"furrowed|corrugated", "furrowed"), (r"fibrous|fibres|fibre-like|thread-like", "fibrous"),
                         (r"stringy", "stringy"), (r"flak(?:y|ing|es)", "flaky"), (r"papery", "papery"), (r"corky", "corky")]:
            mm = re.search(pat, b)
            if mm: tex.append((mm.start(), lev))
        add("bark_description", b, b, "", inyml=False)
        if tex:
            add("bark_texture", b, " ".join(l for _, l in sorted(tex)), " ".join(l for _, l in sorted(tex)),
                note="corrugated mapped to furrowed" if "corrugated" in b else "")
    # --- leaf scent
    lm = re.search(r"Leaves ((?:strongly |somewhat |rather |sometimes )?(?:aromatic|lemon-scented)(?:, but not strongly so)?)", text)
    if lm:
        add("leaf_scent", "Leaves " + lm.group(1), lm.group(1), "aromatic" if "aromatic" in lm.group(1) else "lemon-scented",
            q="sometimes" if lm.group(1).startswith("sometimes") else "", inyml=False)
    sm = re.search(r"((?:often )?(?:strongly )?lemon-scented|spice-like odour[^;.]*)", text)
    if sm and not lm and is_subsp:
        v = sm.group(0)
        add("leaf_scent", v, v, "lemon-scented" if v.startswith(("often", "strongly", "lemon")) else "aromatic",
            q="often" if v.startswith("often") else "", inyml=False,
            note="aromatic with a spice-like odour, scarcely if at all lemon-scented" if "spice" in v else "")
    # --- flowers
    f = seg.get("Flowers", "")
    if f:
        cpart = re.split(r",?\s*(?=(?:to \d|to more|c\. |\(|\d|usually|most|the |from |occurring|single|1 or|in |not |assumed))", f[len("Flowers "):], maxsplit=1)[0]
        if not re.search(r"white|cream|pink|red", cpart):
            mm2 = re.search(r"in diameter, ((?:white|cream|pink|red)[a-z ]*?),", f)
            cpart = mm2.group(1) if mm2 else ""
        cpart = cpart.strip(" ,")
        if re.search(r"white|cream|pink|red", cpart):
            parts = re.split(r",?\s*\b(rarely|often|occasionally|sometimes)\b,?\s*", cpart)
            base = re.sub(r",?\s*or$", "", parts[0].strip(" ,"))
            alts = [(parts[k], re.sub(r",?\s*or$", "", parts[k + 1].strip(" ,"))) for k in range(1, len(parts) - 1, 2)]
            add("flower_colour", "Flowers " + cpart, base, colour_map(base), q="usually" if alts else "",
                note="'probably always white'" if "probably" in base else "")
            for q, v in alts:
                add("flower_colour", "Flowers " + cpart, v, colour_map(v), q=q, note="flushed with colour" if "flushed" in v else "")
        rws, raw = size_rows(f, "in diameter")
        for v, vt, q, note in rws:
            add("flower_diameter", raw, v, v, vt, "mm", q, note)
        nm = re.search(r"\b((?:singly|single)(?:,? or,?|,)?(?:\s*(?:occasionally|rarely|sometimes),?)?(?:\s*(?:\d(?:\s*-\s*\d)?(?:\(-\d\))?|2 or more))?(?: together)?|several(?: \(up to \d+\))?(?: together)?|\d(?:\s*(?:-|or)\s*\d)?(?:\s*\(-\d\))? (?:together|in bract-axils))", f)
        if nm:
            phr = nm.group(1).strip(" ,")
            q = (re.search(r"occasionally|rarely|sometimes", phr) or [""])[0]
            single = bool(re.match(r"singl", phr))
            ns = [int(x) for x in re.findall(r"\b(\d)\b", re.sub(r"\(-\d\)|\(up to \d+\)", "", phr))]
            ext = re.search(r"\(-(\d)\)", phr)
            upto = re.search(r"\(up to (\d+)\)", phr)
            vals = []
            if single and ns:
                vals = [("1", "mode", "usually"), (str(max(ns)), "maximum", q)] if q else [("1", "minimum", "usually" if ext else ""), (str(max(ns)), "maximum", "usually" if ext else "")]
            elif single:
                vals = [("1", "mode", "")]
            elif upto:
                vals = [(upto.group(1), "maximum", "")]
            elif len(ns) >= 2:
                vals = [(str(min(ns)), "minimum", "usually" if ext else ""), (str(max(ns)), "maximum", "usually" if ext else "")]
            elif len(ns) == 1:
                vals = [(str(ns[0]), "mode", "")]
            if ext:
                vals.append((ext.group(1), "maximum", "extreme_range"))
            for v, vt, qq in vals:
                add("flowers_per_inflorescence", "Flowers ... " + phr, v, v, vt, "count", qq,
                    note="flowers single (solitary) or a few together in bract-axils" + ("; 'or more'" if "or more" in phr else ""))
    # --- hypanthium
    h = seg.get("Hypanthium", "")
    v, raw = first_size(h, r"long|in length")
    if v:
        addnum("flower_hypanthium_length", raw, v, "mm")
    # --- sepals
    s = seg.get("Sepals", "")
    if s:
        pm = re.search(r"Sepals[^;.]*?\b((?:(?:usually|mostly|readily|rather|tardily|often|very|as seen on the fruit),? ){0,3}(?:persistent|deciduous)(?: (?:or|but|though) [^,;]*)?)", s)
        if pm:
            v = pm.group(1)
            clean = " ".join(dict.fromkeys(re.findall(r"persistent|deciduous", v)))
            q = "usually" if re.search(r"\b(usually|mostly)\b", pm.group(0)) else ""
            add("sepal_persistence", pm.group(0), v, clean, q=q, inyml=False)
        v, raw = first_size(s, r"long|in length")
        if v:
            addnum("flower_sepal_length", raw, v, "mm")
    # --- petals
    p = seg.get("Petals", "")
    v, raw = first_size(p, r"long|in length")
    if v:
        addnum("flower_petal_length", raw, v, "mm")
    # --- stamens
    st = seg.get("Stamens", "")
    if st:
        bm2 = re.search(r"in bundles (?:probably )?of (?:c\. )?(" + NUM + r")|(" + NUM + r") per bundle", st)
        if bm2:
            addnum("stamens_per_bundle", bm2.group(0), bm2.group(1) or bm2.group(2), "count", inyml=False,
                   note="stamens in bundles opposite the petals (Leptospermum); the flower's total is not given")
        v, raw = first_size(st.split("anther")[0], r"long|in length")
        if v:
            ent = "filaments" if re.search(r"filaments[^.;]*" + re.escape(raw), st) else ""
            addnum("flower_filament_length" if ent else "flower_stamen_length", raw, v, "mm")
    # --- ovary
    o = seg.get("Ovary", "")
    if o:
        om = re.search(r"Ovary (?:usually |\(\? always\) )?(\(\d\s*-\)\s*)?(\d+(?:\s*-\s*\d+)?)(?:\s*\(-\s*(\d+)\))?(?:\s*\((?:rarely|occasionally) (\d)\))?-locular", o)
        mo = re.search(r"Ovary (\d)-, or occasionally (\d) - or (\d) -, locular", o)
        if mo:
            add("ovary_locule_count", mo.group(0), mo.group(1), mo.group(1), "mode", "count", "usually", inyml=False)
            add("ovary_locule_count", mo.group(0), mo.group(2), mo.group(2), "minimum", "count", "occasionally", inyml=False)
            add("ovary_locule_count", mo.group(0), mo.group(3), mo.group(3), "maximum", "count", "occasionally", inyml=False)
        mt = re.search(r"Ovary (\d+)\s*- to more than (\d+)-locular", o)
        if mt:
            add("ovary_locule_count", mt.group(0), mt.group(1), mt.group(1), "minimum", "count", inyml=False)
            add("ovary_locule_count", mt.group(0), mt.group(2), mt.group(2), "maximum", "count", inyml=False, note="to more than " + mt.group(2))
        if om:
            lo_x, core, hi_x, rare = om.groups()
            ext = bool(lo_x or hi_x or rare)
            q = "usually" if ext else ""
            core = core.replace(" ", "")
            if "-" in core:
                a, b = core.split("-"); add("ovary_locule_count", om.group(0), a, a, "minimum", "count", q, inyml=False); add("ovary_locule_count", om.group(0), b, b, "maximum", "count", q, inyml=False)
            else:
                add("ovary_locule_count", om.group(0), core, core, "mode", "count", q, inyml=False)
            if lo_x: x = lo_x.strip("(-) "); add("ovary_locule_count", om.group(0), x, x, "minimum", "count", "extreme_range", inyml=False)
            if hi_x: add("ovary_locule_count", om.group(0), hi_x, hi_x, "maximum", "count", "extreme_range", inyml=False)
            if rare: add("ovary_locule_count", om.group(0), rare, rare, "mode", "count", "rarely", inyml=False)
        ov = re.search(r"each loculus with (?:c\. ?)?(" + NUM.replace("(?:c\\. ?)?", "") + r"|\d+)(?: or more)? ovules", o)
        if ov:
            raw = ov.group(0)
            addnum("flower_ovules_per_functional_carpel_count", raw, ("c." + ov.group(1)) if "c." in raw else ov.group(1), "count")
    # --- fruit
    fr = seg.get("Fruit", "")
    if fr:
        clause = re.split(r",\s*(?=(?:c\.\s*|to |up to |usually |\(?\d))", re.sub(r"^Fruits?\s*(?:\([^)]*\)\s*)?", "", fr))[0]
        clause = re.split(r", (?:the |widest|ultimately|variable|often variable|very variable|from |mostly|rarely|often \d|up to|not seen|pubescent|glabr)", clause)[0].strip(" ,")
        pv = None
        if re.search(r"\bprobably\b", clause):
            pv = None
        elif re.search(r"readily shed|readily deciduous|soon shed|not persisting|^(?:usually |tardily |readily |soon )?deciduous|^shed", clause):
            pv = "deciduous"
        elif re.search(r"persist", clause):
            pv = "persistent"
        if pv:
            add("fruit_persistence", fr[:len(clause) + 40].split(" mm")[0], clause, pv, inyml=False,
                note="the capsule stays on the plant (persistent) or falls soon after the seeds are shed (deciduous)")
        rws, raw = size_rows(fr, "in diameter")
        for v, vt, q, note in rws:
            add("fruit_width", raw, v, v, vt, "mm", q, ("fruit diameter; " + note).strip("; "))
    # --- seeds
    sd = seg.get("Mature seeds", "")
    if sd and not re.search(r"not seen(?! mature\)? ?,? ?c)", sd[:40]) or re.search(r"not seen mature\)?,? c\.", sd):
        v, raw = first_size(sd, r"long|in length")
        if v:
            addnum("seed_length", raw, v, "mm", note="seeds described as not seen mature" if "not seen" in sd else "")
        shm = re.search(r"\b((?:irregularly |broadly |narrowly |linear-|very )*(?:cuneiform|obovoid|ovoid|oblong|ellipsoid|linear|clavate|fusiform))\b", sd)
        if shm:
            w = shm.group(1)
            clean = "cuneate" if "cuneiform" in w else ("ovoid" if "ovoid" in w else ("ellipsoid" if "ellipsoid" in w else ("fusiform" if "fusiform" in w else "")))
            add("seed_shape", raw or sd[:80], w, clean)
    # --- flowering period, distribution and habitat (the "Distribution:" label is sometimes printed mid-sentence)
    spec_re = (r"S\s*e\s*l\s*e\s*c\s*t\s*e\s*d\b|S\s*p\s*e\s*c\s*i\s*m\s*e\s*n\s*s?\b|O\s*t\s*h\s*e\s*r\s+S\s*p\s*e\s*c|"
               r"\b(?:Central|North|South|Far South) Coast:|Northern Tablelands:|\.{4,}|\b1\* |\ba\. subsp\.")
    tail = re.split(spec_re, text)[0]
    fm = re.search(r"Main flowering(?: periods?)?:\s*", tail)
    seg["Main flowering period:"] = ""
    seg["Distribution:"] = ""
    if fm:
        rest = tail[fm.end():]
        pm = re.match(r"(.*?\.)(?=\s+[A-Z(])", rest)
        per = pm.group(1) if pm else rest
        if "Distribution:" in rest[:160]:
            per = rest.split("Distribution:")[0].strip()
        seg["Main flowering period:"] = "Main flowering period: " + per
        seg["Distribution:"] = "Distribution: " + rest[len(per):].replace("Distribution:", " ").strip()
    elif "Distribution:" in tail:
        seg["Distribution:"] = "Distribution: " + tail.split("Distribution:", 1)[1].strip()
    seg["Distribution:"] = re.sub(r"\s+", " ", seg["Distribution:"])
    mf = seg.get("Main flowering period:", "")
    if mf:
        mt = re.match(r"Main flowering period:\s*([^.]*(?:\.[^A-Z]*?)*?)(?=\s*(?:Distribution:|$))", mf)
        period = mf.replace("Main flowering period:", "").strip().rstrip(".")
        code = None if re.match(r"(?i)uncertain|not known", period) else months(period.replace("M ay", "May"))
        if code:
            hedge = re.search(r"probably|perhaps|\?", period)
            add("flowering_time", mf.strip(), period, code,
                note="the main flowering period" + ("; hedged in the source" if hedge else ""))
    # --- distribution and habitat
    d = seg.get("Distribution:", "")
    if d:
        d = d.replace("Distribution:", "").strip()
        dm = re.search(r"^(.*?\((?:see )?Maps? [^)]*\)\.?)\s*(.*)$", d)
        if dm:
            dist, hab = dm.group(1).strip(), dm.group(2).strip()
        else:
            dist, hab = d, ""
        # habitat: from the first sentence that reads as habitat, while the sentences keep doing so
        hab_start = r"(?:In|On|Among|Along|Usually|Often|Mostly|Especially|Associated|Found|Endemic in|Locally in|Mainly)\b"
        sents = re.split(r"(?<=\.)\s+(?=[A-Z])", hab.strip(" /"))
        keep, on = [], False
        for sn in sents:
            if re.match(hab_start, sn):
                on = True; keep.append(sn)
            elif on:
                break
        if not keep and re.search(r"\b(In|On) [a-z]", dist):     # habitat run into the distribution sentence
            pass
        hab = " ".join(keep).strip(" /")
        add("distribution_description", dist, dist, "", inyml=False)
        if hab:
            add("habitat_description", hab, hab, "", inyml=False)
            for t, ws in [("soil_terms", soil_words), ("geology_terms", geology_words), ("habitat_terms", habitat_words)]:
                hits = word_hits(hab, ws)
                if hits:
                    add(t, hab, hits, "", inyml=False)
            if re.search(r"soil|sand|clay|loam|gravel|granite|sandstone|basalt|rock|outcrop|ridge|slope|heath|forest|woodland|creek|bank|swamp", hab, re.I):
                add("plant_growth_substrate", hab, "terrestrial", "terrestrial",
                    note="inferred: soils, rocks or vegetation named in the habitat; no aquatic or epiphytic habit named")
    return rows
