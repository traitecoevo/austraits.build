"""compare.py ORIG EDITED: per-branch trait renames, removed/added traits, and level changes."""
import sys, re, yaml, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import blocks
def load(f):
    out = {}; br = None; sec = {}
    lines = open(f).read().splitlines(keepends=True)
    for l in lines:
        if re.match(r"^[A-Za-z]", l) and l.rstrip().endswith(':') and not l.startswith('#'):
            br = l.rstrip()[:-1]; sec[br] = []; continue
        if br: sec[br].append(l)
    for br, ls in sec.items():
        out[br] = {}
        for t, (s, e) in blocks(ls).items():
            body = ls[s:e]; body[0] = t + ':\n'
            out[br][t] = yaml.safe_load(''.join(body[:1] + [l[4:] for l in body[1:]]))[t]
    return out
if __name__ == "__main__":
    O, E = load(sys.argv[1]), load(sys.argv[2])
    for br in O:
        o, e = O[br], E.get(br, {})
        print(f"\n===== {br}")
        olab = {v.get('label'): k for k, v in o.items()}
        pairs = {}
        for k, v in e.items():
            if k in o: pairs[k] = k
            elif v.get('label') in olab: pairs[k] = olab[v['label']]
        for k in e:
            if k not in pairs: print(f"NEW/UNMATCHED in edited: {k}")
        for k in o:
            if k not in pairs.values(): print(f"REMOVED from edited: {k}")
        for k, ok in pairs.items():
            if k != ok: print(f"RENAMED: {ok} -> {k}")
            ol = o[ok].get('allowed_values_levels') or {}; nl = e[k].get('allowed_values_levels') or {}
            if o[ok].get('type') != e[k].get('type'): print(f"  {k}: type {o[ok].get('type')} -> {e[k].get('type')}")
            if ol or nl:
                gone = [x for x in ol if x not in nl]; add = [x for x in nl if x not in ol]
                if gone or add: print(f"  {k}: levels removed {gone}; added {add}")
