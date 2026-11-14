import csv, io, re
P = '/Users/z3524079/GitHub/APD/data/published_classes.csv'
OBO = 'http://purl.obolibrary.org/obo/'
def obo(iri_id, label, desc, abbr, vocab):
    return [OBO + iri_id, label, desc, iri_id, OBO, 'obo', abbr, vocab]
new = [
  obo('PO_0020041', 'stipule', 'A cardinal organ part that is an appendage [NCIT:C137974] at the base of a vascular leaf [PO:0009025].', 'PO', 'Plant Ontology'),
  obo('PO_0009100', 'involucre', 'A collective phyllome structure composed of two or more involucral bracts at the base of a cluster of flowers, as found in Asteraceae or Compositae.', 'PO', 'Plant Ontology'),
  obo('PO_0009065', 'hypanthium', 'A cup- or tube-shaped cardinal organ part, bearing perianth and stamens on its rim, so these arise above the base, and often above the top, of the ovary.', 'PO', 'Plant Ontology'),
  obo('PO_0009043', 'bracteole', 'A prophyll [PO:0009042] which is borne singly or in pairs on a pedicel [PO:0030112].', 'PO', 'Plant Ontology'),
  obo('PO_0009057', 'extrafloral nectary', 'A nectary [PO:0009035], not occurring on a floral organ [PO:0025395] or other part of a flower [PO:0009046].', 'PO', 'Plant Ontology'),
  obo('FLOPO_0986006', 'corolla lobe', '', 'FLOPO', 'Flora Phenotype Ontology'),
  obo('PATO_0002006', '2-D shape', 'A shape that inheres in a 2 dimensional entity, such as a cross section or projection of a 3 dimensional entity.', 'PATO', 'Phenotypic Quality Ontology'),
  obo('NCIT_C137974', 'appendage', 'An appendage or accessory structure of an adjacent or related organ.', 'NCIT', 'National Cancer Institute Thesaurus'),
  obo('NCIT_C79654', 'pistil', '', 'NCIT', 'National Cancer Institute Thesaurus'),
  obo('GO_0022611', 'dormancy process', 'A developmental process in which dormancy (sometimes called a dormant state) is induced, maintained or broken. Dormancy is a suspension of most physiological activity and growth that can be reactivated.', 'GO', 'Gene Ontology'),
  ['http://vocabs.lter-europe.net/EnvThes/21485', 'growing season', '', 'EnvThes:21485', 'http://vocabs.lter-europe.net/EnvThes/', 'EnvThes', 'EnvThes', 'Environmental Thesaurus'],
]
lines = open(P, newline='').read().splitlines(keepends=True)
ents = [next(csv.reader([l]))[0] for l in lines]
for row in new:
    if row[0] in ents: print('already present', row[3]); continue
    stem = re.sub(r'[0-9]+$', '', row[0])
    same = [i for i, e in enumerate(ents) if re.sub(r'[0-9]+$', '', e) == stem]
    below = [i for i in same if e_num(ents[i]) < e_num(row[0])] if False else [i for i in same if ents[i] < row[0]]
    at = (max(below) if below else (min(same) - 1 if same else len(ents) - 1)) + 1
    buf = io.StringIO(); csv.writer(buf, lineterminator='\n').writerow(row)
    lines.insert(at, buf.getvalue()); ents.insert(at, row[0])
    print(f'added {row[3]} {row[1]!r} at line {at+1}')
open(P, 'w', newline='').write(''.join(lines))
