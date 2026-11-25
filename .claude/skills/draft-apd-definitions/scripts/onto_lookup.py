"""onto_lookup.py TERMFILE OUT.tsv : exact (case-insensitive) label/synonym matches across local ontologies."""
import re, sys, html
O = '/Users/z3524079/Library/CloudStorage/OneDrive-UNSW/Documents/data_for_GitHub_repos/APD/ontologies/'
terms = {l.strip().lower() for l in open(sys.argv[1]) if l.strip()}
out = open(sys.argv[2], 'w')
def emit(src, iri, label, how, defn):
    out.write('\t'.join([src, iri, label, how, (defn or '')[:300].replace('\t',' ').replace('\n',' ')]) + '\n'); out.flush()
def check(src, iri, label, syns, defn):
    for how, s in [('label', label)] + syns:
        if s and html.unescape(s).strip().lower() in terms:
            emit(src, iri, label, f"{how}={s}", defn)
def obo(f):
    cur = None
    for l in open(O+f, encoding='utf-8', errors='replace'):
        l = l.rstrip('\n')
        if l.startswith('['):
            if cur and cur.get('id'): check(f, cur['id'], cur.get('name'), cur['syn'], cur.get('def'))
            cur = {'syn': []} if l == '[Term]' else None
        elif cur is not None:
            if l.startswith('id: '): cur['id'] = l[4:]
            elif l.startswith('name: '): cur['name'] = l[6:]
            elif l.startswith('def: '): cur['def'] = l[5:]
            elif l.startswith('synonym: '):
                m = re.match(r'synonym: "(.*?)" (\w+)', l)
                if m: cur['syn'].append((m.group(2), m.group(1)))
            elif l.startswith('is_obsolete: true'): cur = None
    if cur and cur.get('id'): check(f, cur['id'], cur.get('name'), cur['syn'], cur.get('def'))
LAB = re.compile(r'<(rdfs:label|oboInOwl:has(?:Exact|Related|Narrow|Broad)Synonym|P108|P90|skos:prefLabel|skos:altLabel|obo:IAO_0000115|P97|owl:deprecated)\b[^>]*>(.*?)</\1>')
def owl(f):
    iri, lab, syns, defn, dep, depth = None, None, [], None, False, 0
    for l in open(O+f, encoding='utf-8', errors='replace'):
        m = re.search(r'<owl:Class rdf:about="([^"]+)"', l)
        if m:
            if iri and not dep: check(f, iri, lab, syns, defn)
            iri, lab, syns, defn, dep = m.group(1), None, [], None, False
            continue
        if iri is None: continue
        for k, v in LAB.findall(l):
            v = re.sub(r'<[^>]+>', '', v)
            if k in ('rdfs:label', 'P108', 'skos:prefLabel') and lab is None: lab = v
            elif k in ('obo:IAO_0000115', 'P97'): defn = defn or v
            elif k == 'owl:deprecated': dep = v == 'true'
            else: syns.append((k.split(':')[-1], v))
        if '</owl:Class>' in l:
            if not dep: check(f, iri, lab, syns, defn)
            iri = None
    if iri and not dep: check(f, iri, lab, syns, defn)
def ttl(f):
    txt = open(O+f, encoding='utf-8').read()
    for blk in re.split(r'\n\s*\n', txt):
        m = re.match(r'\s*(<[^>]+>|\S+:\S+)', blk)
        if not m: continue
        labs = re.findall(r'skos:(prefLabel|altLabel)\s+"([^"]+)"@en', blk)
        pref = next((v for k, v in labs if k == 'prefLabel'), None)
        d = re.search(r'skos:definition\s+"([^"]+)"', blk)
        check(f, m.group(1), pref, [(k, v) for k, v in labs if k == 'altLabel'], d.group(1) if d else None)
for f in sys.argv[3:]:
    print('scanning', f, flush=True)
    (obo if f.endswith('.obo') else ttl if f.endswith('.ttl') else owl)(f)
