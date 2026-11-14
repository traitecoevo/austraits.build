"""check.py FILE [--fix-groups]: check structure_measured / keywords / trait_group /
characteristic_measured against APD vocab and develop's structure_measured terms."""
import csv, re, sys, yaml, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import blocks, develop_text
APD = '/Users/z3524079/GitHub/APD/data/'
rd = lambda f: list(csv.DictReader(open(APD+f, encoding='utf-8-sig')))
PC = {r['label'].strip(): r['identifier'] for r in rd('published_classes.csv')}
GL = {r['label'].strip(): r['identifier'] for r in rd('APD_glossary.csv')}
H = {r['label'].strip(): r for r in rd('APD_trait_hierarchy.csv')}
HID = {r['identifier']: l for l, r in H.items()}
def chain(term):
    out = [term]
    while True:
        p = H[out[-1]]['Parent'].rsplit('/', 1)[-1]
        if p not in HID or HID[p] == 'plant trait': return out
        out.append(HID[p])
dev = yaml.safe_load(develop_text())['traits']['elements']
STRUCT = {x.strip() for v in dev.values() for x in str(v.get('structure_measured') or '').split('; ') if x.strip()}
MULTI = sorted([l for l in list(PC) + list(GL) + list(H) + list(STRUCT) if '; ' in l], key=len, reverse=True)

def terms(s):
    s = ' '.join(str(s or '').split())
    out = []
    while s:
        for m in MULTI:
            if s == m or s.startswith(m + '; '):
                out.append(m); s = s[len(m) + 2:]; break
        else:
            t, _, s = s.partition('; '); out.append(t.strip())
    return [t for t in out if t]

def fix_groups(ts):
    known = [t for t in ts if t in H]
    leaves = [t for t in known if not any(t in chain(o)[1:] for o in known)]
    seq = [x for l in leaves for x in chain(l)]
    return [x for i, x in enumerate(seq) if x not in seq[i+1:]]  # keep last -> shared ancestors after descendants

FIELDS = ['structure_measured', 'keywords', 'trait_group', 'characteristic_measured']
def check(name, e):
    probs = []; seen = {}
    for f in FIELDS:
        ts = terms(e.get(f))
        if not ts: probs.append(f"{f}: missing"); continue
        for t in ts:
            if t in seen: probs.append(f"{f}: '{t}' duplicated (also in {seen[t]})")
            seen.setdefault(t, f)
        if f == 'structure_measured':
            probs += [f"{f}: '{t}' not a develop structure term" for t in ts if t not in STRUCT]
        elif f == 'trait_group':
            probs += [f"{f}: '{t}' not in APD hierarchy" for t in ts if t not in H]
            want = fix_groups(ts)
            if [t for t in ts if t in H] != want: probs.append(f"{f}: should be '{'; '.join(want)}'")
        else:
            probs += [f"{f}: '{t}' not in published classes/glossary/hierarchy" for t in ts if t not in PC and t not in GL and t not in H]
    return probs

if __name__ == '__main__':
    lines = open(sys.argv[1]).read().splitlines(keepends=True)
    n = 0
    for t, (s, e) in blocks(lines).items():
        txt = ''.join(lines[s:e])
        body = txt.splitlines(); body[0] = t + ':'
        try: entry = yaml.safe_load('\n'.join(body[:1] + [l[4:] for l in body[1:]]))[t]
        except Exception as ex: print(f"{t} (line {s+1}): YAML error {ex}"); continue
        p = check(t, entry)
        if p: n += 1; print(f"\n{t} (line {s+1})"); [print("   ", x) for x in p]
    print(f"\n{n} traits with problems")
