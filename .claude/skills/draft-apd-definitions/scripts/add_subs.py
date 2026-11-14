"""add_subs.py DATA_ROOT RULES.tsv : add `match: word` substitutions to data/<id>/metadata.yml.
RULES.tsv columns: dataset_id, trait_name, find, replace. Each entry goes after the last existing
substitution for the same trait (else at the end of the substitutions block). Skips duplicates."""
import sys, re, csv, yaml
root, rules = sys.argv[1:3]
by = {}
for r in csv.reader(open(rules), delimiter='\t'):
    if r and not r[0].startswith('#'): by.setdefault(r[0], []).append(r[1:])
for d, rs in by.items():
    f = f"{root}/data/{d}/metadata.yml"; L = open(f).read().splitlines(keepends=True)
    md = yaml.safe_load(''.join(L)); S = md.get('substitutions') if isinstance(md.get('substitutions'), list) else []
    have = {(s['trait_name'], str(s['find'])) for s in S}
    si = next(i for i, l in enumerate(L) if l.startswith('substitutions:'))
    if L[si].strip() != 'substitutions:':  # e.g. `substitutions: .na`
        L[si] = 'substitutions:\n'
    end = next((i for i in range(si + 1, len(L)) if re.match(r'^[A-Za-z_]+:', L[i])), len(L))
    added = 0
    for tn, find, rep in rs:
        if (tn, find) in have: continue
        ent = [f"- trait_name: {tn}\n", f"  find: {find}\n", f"  replace: {rep}\n", "  match: word\n"]
        idx = [i for i in range(si + 1, end) if L[i].rstrip() == f"- trait_name: {tn}"]
        if idx:
            at = idx[-1] + 1
            while at < end and not L[at].startswith('- '): at += 1
        else:
            at = end
        L[at:at] = ent; end += len(ent); added += 1
    open(f, 'w').write(''.join(L)); yaml.safe_load(open(f))
    print(f"{d}: {added} added")
