"""impact2.py BRANCH EDITED.yml BRANCH_TRAITS_YML [REF] [--data-root DIR]
Mimics traits.build develop on a branch's datasets: value substitutions (exact) -> `match: word`
substitutions -> traits.yml `(Synonyms, ...)` word replacement -> unique tokens. Compares BEFORE (the
branch's traits.yml) with AFTER (branch traits.yml overlaid with the edited entries; renamed traits
mapped). Datasets are read with `git show REF:...` unless --data-root points at a checkout.
Reports trait_name renames, and for each trait/token values newly invalid AFTER (were OK BEFORE) and
values invalid both BEFORE and AFTER."""
import sys, io, csv, re, subprocess, yaml, collections, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from compare import load
REPO = "/Users/z3524079/GitHub/austraits.build"
RENAMED = {'stipule_length': 'leaf_stipule_length', 'stipule_width': 'leaf_stipule_width', 'stipule_presence': 'leaf_stipule_presence',
  'petiole_presence': 'leaf_petiole_presence', 'bracteole_length': 'flower_bracteole_length', 'extrafloral_nectary_count': 'extrafloral_nectary_count_per_leaf',
  'involucre_length': 'flower_involucre_height', 'leaf_main_vein_count': 'leaf_primary_vein_count', 'leaf_starch_per_mass': 'leaf_starch_per_dry_mass',
  'leaf_glucose_per_mass': 'leaf_glucose_per_dry_mass', 'leaf_fructose_per_mass': 'leaf_fructose_per_dry_mass', 'leaf_sucrose_per_mass': 'leaf_sucrose_per_dry_mass',
  'stem_starch_per_mass': 'stem_starch_per_dry_mass', 'root_starch_per_mass': 'root_starch_per_dry_mass', 'recruitment_intensity': 'recruitment_pattern'}
args = [a for a in sys.argv[1:] if not a.startswith('--')]
br, edited, btraits = args[:3]; ref = args[3] if len(args) > 3 else f"origin/{br}"
root = sys.argv[sys.argv.index('--data-root') + 1] if '--data-root' in sys.argv else None
if root: args = args  # noqa
def show(p):
    if root:
        f = os.path.join(root, p); return open(f).read() if os.path.exists(f) else None
    r = subprocess.run(["git", "-C", REPO, "show", f"{ref}:{p}"], capture_output=True, text=True); return r.stdout if r.returncode == 0 else None
def lsdata():
    if root: return [os.path.relpath(os.path.join(dp, f), root) for dp, _, fs in os.walk(os.path.join(root, 'data')) for f in fs if f == 'metadata.yml']
    return subprocess.run(["git","-C",REPO,"ls-tree","-r","--name-only",ref,"data/"],capture_output=True,text=True).stdout.split()
BEFORE = yaml.safe_load(open(btraits))['traits']['elements']
E = {t: v for s in load(edited).values() for t, v in s.items()}
AFTER = dict(BEFORE); [AFTER.pop(o, None) for o in RENAMED]; AFTER.update(E)
def syns(defs, tn):
    out = []
    for lev, d in (defs.get(tn, {}).get('allowed_values_levels') or {}).items():
        m = re.search(r'(?<=\(Synonyms,)[^)]+', str(d))
        if m: out += [(s.strip().lower(), lev) for s in re.split('[,;]', m.group(0)) if s.strip()]
    return out
def wordrep(v, rules):
    for f, r in rules:
        if f and f != r: v = re.sub(r'\b' + re.escape(f) + r'\b', r, v)
    return ' '.join(dict.fromkeys(v.split()))
def proc(v, vsubs, wsubs, defs, tn):
    v = v.lower(); v = vsubs.get(v, v); v = wordrep(v, wsubs); v = wordrep(v, syns(defs, tn)); return v
ren, newbad, oldbad = [], collections.defaultdict(collections.Counter), collections.defaultdict(collections.Counter)
for mf in sorted(f for f in lsdata() if f.endswith("metadata.yml")):
    d = mf.split("/")[1]; md = yaml.safe_load(show(mf))
    trs = md.get('traits') if isinstance(md.get('traits'), list) else []
    trs = [t for t in trs if t.get('trait_name') in E or t.get('trait_name') in RENAMED]
    if not trs: continue
    S = md.get('substitutions'); S = S if isinstance(S, list) else []
    ds = md.get('dataset', {}); longfmt = ds.get('data_is_long_format')
    txt = show(f"data/{d}/data.csv"); rows = list(csv.DictReader(io.StringIO(txt))) if txt else []
    for t in trs:
        tb = t['trait_name']; ta = RENAMED.get(tb, tb)
        if ta != tb: ren.append((d, tb, ta))
        la = AFTER[ta].get('allowed_values_levels') or {}; lb = (BEFORE.get(tb) or {}).get('allowed_values_levels') or {}
        if not la: continue
        vs = {str(s['find']).lower(): str(s['replace']).lower() for s in S if s['trait_name'] == tb and s.get('match', 'value') != 'word'}
        ws = [(str(s['find']).lower(), str(s['replace']).lower()) for s in S if s['trait_name'] == tb and s.get('match') == 'word']
        vi = t.get('var_in')
        vals = [r.get(ds.get('value','value')) for r in rows if r.get(ds.get('trait_name','trait_name')) == vi] if longfmt else [r.get(vi) for r in rows]
        for raw in vals:
            if raw in (None, '', 'NA'): continue
            a = proc(raw, vs, ws, AFTER, ta); b = proc(raw, vs, ws, BEFORE, tb)
            bok = all(x in lb for x in b.split())
            for tok in a.split():
                if tok in la: continue
                (newbad if bok else oldbad)[(ta, tok)][d] += 1
fmt = lambda c: ', '.join(f'{k} {n}' for k, n in sorted(c.items()))
print(f"### {br} ({root or ref})\n\n#### trait_name renames needed in metadata.yml")
for d, a, b in ren: print(f"- {d}: {a} -> {b}")
print("\n#### NEWLY invalid after the edits (valid before)")
for (tn, tok), c in sorted(newbad.items()): print(f"- {tn}: `{tok}`  ({fmt(c)})")
print("\n#### invalid before and after (pre-existing)")
for (tn, tok), c in sorted(oldbad.items()): print(f"- {tn}: `{tok}`  ({fmt(c)})")
