"""insert_into_develop.py EDITED.yml TARGET_TRAITS_YML SCRATCH_DIR
Insert every trait in EDITED.yml (all branch sections) into TARGET (a develop traits.yml), each placed
where it sits on its source branch: after the nearest preceding trait on that branch that is in TARGET.
Renamed traits are located on the branch by their old name (RENAMES). SCRATCH_DIR holds
traits_<branch>.yml snapshots (git show origin/<branch>:config/traits.yml)."""
import sys, re, os, yaml
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import blocks
RENAMES = {'leaf_stipule_length': 'stipule_length', 'leaf_stipule_width': 'stipule_width',
  'leaf_stipule_presence': 'stipule_presence', 'leaf_petiole_presence': 'petiole_presence',
  'flower_bracteole_length': 'bracteole_length', 'extrafloral_nectary_count_per_leaf': 'extrafloral_nectary_count',
  'flower_involucre_height': 'involucre_length', 'leaf_primary_vein_count': 'leaf_main_vein_count',
  'leaf_starch_per_dry_mass': 'leaf_starch_per_mass', 'leaf_glucose_per_dry_mass': 'leaf_glucose_per_mass',
  'leaf_fructose_per_dry_mass': 'leaf_fructose_per_mass', 'leaf_sucrose_per_dry_mass': 'leaf_sucrose_per_mass',
  'stem_starch_per_dry_mass': 'stem_starch_per_mass', 'root_starch_per_dry_mass': 'root_starch_per_mass'}
edited, target, scratch = sys.argv[1:4]
secs, br = {}, None
for l in open(edited).read().splitlines(keepends=True):
    if re.match(r"^[A-Za-z][A-Za-z0-9_.-]*:\s*$", l): br = l.rstrip()[:-1]; secs[br] = []; continue
    if br: secs[br].append(l)
lines = open(target).read().splitlines(keepends=True)
plan = []  # (anchor, branch_pos, body)
for br, sec in secs.items():
    bl = open(f"{scratch}/traits_{br}.yml").read().splitlines(keepends=True)
    bb = blocks(bl); order = sorted(bb, key=lambda t: bb[t][0])
    tb = blocks(lines)
    for t, (s, e) in blocks(sec).items():
        body = sec[s:e]
        while body and (not body[-1].strip() or body[-1].lstrip().startswith('#')): body.pop()
        body[0] = f"    {t}:\n"
        if t in tb: sys.exit(f"{t} already in target")
        old = RENAMES.get(t, t)
        if old not in bb: sys.exit(f"{t} ({old}) not on branch {br}")
        i = order.index(old); anchor = None
        for prev in reversed(order[:i]):
            if prev in tb: anchor = prev; break
        plan.append((anchor, i, br, t, body))
# insert, grouped by anchor, in branch order, bottom-up so line numbers stay valid
tb = blocks(lines)
from collections import defaultdict
g = defaultdict(list)
for a, i, br, t, body in plan: g[a].append((br, i, t, body))
for a in sorted(g, key=lambda a: tb[a][1], reverse=True):
    s, e = tb[a]
    while e > s and not lines[e-1].strip(): e -= 1  # insert right after anchor's last content line
    ins = [l for _, _, _, body in sorted(g[a]) for l in body]
    lines[e:e] = ins
    print(f"after {a}: {', '.join(t for _, _, t, _ in sorted(g[a]))}")
open(target, 'w').write(''.join(lines))
y = yaml.safe_load(open(target))
print("YAML ok;", len(y['traits']['elements']), "traits")
