"""check_synonyms.py FILE.yml : synonym problems that matter now traits.build applies
`(Synonyms, ...)` automatically (word-bounded, lower-cased): a synonym that is also a level of the
same trait (that level can never survive), a synonym listed under two levels, a synonym equal to its
own level, and multi-word synonyms written with spaces. Works on traits.yml or the review file."""
import re, sys, os, yaml
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
def entries(f):
    y = yaml.safe_load(open(f)) if 'traits.yml' in f else None
    if y: return y['traits']['elements']
    from compare import load
    return {t: v for s in load(f).values() for t, v in s.items()}
n = 0
# duplicate trait keys: YAML loaders silently keep the last copy, so count headers in the text
import collections
heads = collections.Counter(re.findall(r"(?m)^    \^?([A-Za-z0-9_]+):\s*$", open(sys.argv[1]).read()))
for t, c in heads.items():
    if c > 1: n += 1; print(f"{t}: trait entry appears {c} times")
for t, v in entries(sys.argv[1]).items():
    lv = v.get('allowed_values_levels') or {}
    seen = {}
    for lev, d in lv.items():
        m = re.search(r'(?<=\(Synonyms,)[^)]+', str(d))
        for s in ([x.strip() for x in re.split('[,;]', m.group(0))] if m else []):
            if not s: continue
            p = []
            if s.lower() in lv and s.lower() != lev: p.append(f"is also a level -> data '{s}' always becomes '{lev}'")
            if s.lower() == lev: p.append("same as its own level")
            if s.lower() in seen and seen[s.lower()] != lev: p.append(f"also listed under '{seen[s.lower()]}'")
            if ' ' in s: p.append("contains a space (use _)")
            seen.setdefault(s.lower(), lev)
            for x in p: n += 1; print(f"{t}.{lev}: synonym '{s}' {x}")
print(f"{n} synonym/duplicate problems")
