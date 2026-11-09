# Independent check of Chinnock_2007_2: sizes read straight from each description by organ sentence, against data.csv.
# Reports taxa where a trait the text gives is missing from data.csv or differs. usage: qa.py  ->  prints a summary, writes qa_report.csv
import csv, re
from collections import Counter, defaultdict
csv.field_size_limit(10**9)
P = list(csv.DictReader(open('/Users/z3524079/GitHub/austraits.build-data.scraping.scripts/data_extra/chinnock2007_profiles.csv')))
rows = list(csv.DictReader(open('data/Chinnock_2007_2/data.csv')))
num = defaultdict(list)
for r in rows:
    if r.get('measurement_range') == 'extreme' or r['trait_scoring_method'] == 'inferred_from_species': continue
    if r['value_type'] in ('minimum', 'maximum', 'mean', 'mode'):
        try: num[(r['taxon_name'], r['trait_name'])].append(float(r['value']))
        except ValueError: pass
N = r'\d[\d.]*'
RNG = rf'({N})(?:\s*-\s*({N}))?'
EXT1 = rf'(?:\(\s*{N}\s*-?\s*\)\s*)?'
EXT2 = rf'(?:\s*\(\s*-?\s*{N}\s*\))?'
DIM = re.compile(rf'{EXT1}{RNG}{EXT2}\s*[×x]\s*(?:ca\.?\s*)?{EXT1}{RNG}{EXT2}\s*(mm|cm|m)\b')
LONG = re.compile(rf'{EXT1}(?:ca\.?\s*)?{RNG}{EXT2}\s*(mm|cm)\s+long')
MULT = {'mm': 1, 'cm': 10, 'm': 1000}


def sent(d, lab):
    # the organ's sentence: from its label to the next organ label
    m = re.search(rf'\b{lab}\b(.*?)(?=\b(?:Branches|Leaves|Flowers?|Sepals?|Corolla|Stamens?|Ovary|Fruit|Seeds?|Chromosome)\b(?=\s[a-z(0-9])|$)', d)
    return m.group(1) if m else ''


def vals(m, g1, g2, mult): return sorted(float(x) * mult for x in (m.group(g1), m.group(g2)) if x)


def close(a, b): return all(any(abs(x - y) < 1e-6 for y in b) for x in a)


out = []
for p in P:
    t, d = p['taxon_name'], p['description']
    if not d: continue
    chk = []
    # leaf: the L x W of the Leaves sentence (the last one, after any petiole)
    ls = sent(d, 'Leaves'); ms = list(DIM.finditer(ls))
    if ms:
        m = ms[-1]; u = MULT[m.group(5)]
        chk += [('leaf_length', vals(m, 1, 2, u), m.group(0)), ('leaf_width', vals(m, 3, 4, u), m.group(0))]
    for lab, trl, trw in [('Sepals', 'flower_sepal_length', 'flower_sepal_width'), ('Fruit', 'fruit_length', 'fruit_width'), ('Seed', 'seed_length', 'seed_width')]:
        s = sent(d, lab); m = DIM.search(s)
        if m:
            u = MULT[m.group(5)]
            chk += [(trl, vals(m, 1, 2, u), m.group(0)), (trw, vals(m, 3, 4, u), m.group(0))]
    s = sent(d, 'Ovary'); m = DIM.search(s)
    if m:
        u = MULT[m.group(5)]; chk += [('ovary_length', vals(m, 1, 2, u), m.group(0))]
    s = sent(d, 'Corolla'); m = LONG.search(s)
    if m and not re.search(r'lobes?\s[^;]{0,30}$', s[:m.start()]):
        u = MULT[m.group(3)]; chk += [('flower_length', vals(m, 1, 2, u), m.group(0))]
    m = re.search(rf'pedicel[^;.]*?{EXT1}{RNG}{EXT2}\s*(mm|cm)\s+long', sent(d, 'Flowers?'))
    if m: chk += [('pedicel_length', vals(m, 1, 2, MULT[m.group(3)]), m.group(0))]
    # height: the habit sentence
    hs = re.match(r'[^.]*?(?:\.(?=\d)[^.]*?)*\.', d)
    hs = hs.group(0) if hs else ''
    m = re.search(rf'(?:to\s+)?(?:ca\.?\s*)?{EXT1}{RNG}{EXT2}\s*(m|cm)\s+(?:tall|high)', hs)
    if m:
        u = 1 if m.group(3) == 'm' else 0.01
        chk += [('plant_height', vals(m, 1, 2, u), m.group(0))]
    elif 'tall' in hs or 'high' in hs:
        chk += [('plant_height?', [], hs[:80])]
    m = re.search(r'\bStamens?\s+(\d+)', d)
    if m: chk += [('flower_fertile_stamens_count', [float(m.group(1))], m.group(0))]
    for tr, v, src in chk:
        got = num.get((t, tr), [])
        if tr.endswith('?'):
            if not num.get((t, 'plant_height')): out.append((t, tr, 'not read', '', '', src))
            continue
        if not got: out.append((t, tr, 'missing', v, '', src))
        elif not close(v, got): out.append((t, tr, 'differs', v, sorted(set(got)), src))
with open('ignore/jobs/erem1/qa_report.csv', 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow(['taxon_name', 'trait', 'status', 'text_values', 'data_values', 'text']); w.writerows(out)
print(len(P), 'taxa,', len(out), 'problems'); print(Counter((x[1], x[2]) for x in out).most_common())
