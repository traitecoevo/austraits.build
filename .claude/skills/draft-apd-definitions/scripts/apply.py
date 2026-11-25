"""apply.py (renamed traits, listed in impact2_renames.py, are replaced in place)
 EDITED.yml BRANCH TARGET_TRAITS_YML [--dry]
Write BRANCH's edited entries back into TARGET_TRAITS_YML in place (same position); the ^ marker
is stripped. Entries deleted from EDITED are removed from the target; entries new in EDITED are
appended after the last of that branch's new traits. Trait renames show up as removed + added."""
import os, re, sys, yaml
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import blocks, develop_traits
edited, branch, target = sys.argv[1:4]; dry = "--dry" in sys.argv
sec, on = [], False
for l in open(edited).read().splitlines(keepends=True):
    if re.match(r"^\S", l) and not l.startswith("#"):
        on = l.rstrip() == f"{branch}:"; continue
    if on: sec.append(l)
new_blocks = {}
for t, (s, e) in blocks(sec).items():
    body = sec[s:e]
    while body and (not body[-1].strip() or body[-1].startswith("#")): body.pop()
    body[0] = f"    {t}:\n"
    new_blocks[t] = body
lines = open(target).read().splitlines(keepends=True)
# renamed traits: rename the header in place first so the edited entry replaces it where it sits
from impact2_renames import RENAMED
for i, l in enumerate(lines):
    m = re.match(r"^    ([A-Za-z0-9_]+):\s*$", l)
    if m and m.group(1) in RENAMED and RENAMED[m.group(1)] in new_blocks: lines[i] = f"    {RENAMED[m.group(1)]}:\n"
tb = blocks(lines); dev = develop_traits()
branch_new = [t for t in tb if t not in dev or t in new_blocks]
changed, removed = [], []
for t in sorted(branch_new, key=lambda t: tb[t][0], reverse=True):
    s, e = tb[t]; old = lines[s:e]; trail = []
    while old and not old[-1].strip(): trail.insert(0, old.pop())
    if t not in new_blocks: lines[s:e] = []; removed.append(t)
    elif new_blocks[t] != old: lines[s:e] = new_blocks[t] + trail; changed.append(t)
added = [t for t in new_blocks if t not in tb]
if added:
    tb2 = blocks(lines)
    anchor = max((tb2[t][1] for t in branch_new if t in tb2), default=None)
    if anchor is None: sys.exit(f"no anchor to insert {added}")
    lines[anchor:anchor] = [l for t in added for l in new_blocks[t]]
print(f"{branch}: changed={len(changed)} removed={len(removed)} added={len(added)}")
for k, v in (("changed", changed), ("removed", removed), ("added", added), ("ALSO IN DEVELOP", [t for t in added if t in dev])):
    if v: print(f"  {k}: {', '.join(sorted(v))}")
if not dry:
    open(target, "w").write("".join(lines)); yaml.safe_load(open(target))
