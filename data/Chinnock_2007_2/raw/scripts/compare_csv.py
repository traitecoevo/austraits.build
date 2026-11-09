# compare_numeric.csv + compare_categorical.csv (compare2.R) -> ~/Downloads/Chinnock_2007_vs_Chinnock_2007_2.csv. Run from the repository root.
import csv, os
n = list(csv.DictReader(open('ignore/jobs/erem1/compare_numeric.csv')))
c = list(csv.DictReader(open('ignore/jobs/erem1/compare_categorical.csv')))
out = os.path.expanduser('~/Downloads/Chinnock_2007_vs_Chinnock_2007_2.csv')
f = lambda a, b: '' if a in ('', 'NA') and b in ('', 'NA') else f"{a if a not in ('', 'NA') else '?'}-{b if b not in ('', 'NA') else '?'}"
with open(out, 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow(['taxon_name', 'trait_name', 'Chinnock_2007', 'Chinnock_2007_2', 'status'])
    for x in sorted(n, key=lambda x: (x['trait_name'], x['taxon_name'])):
        w.writerow([x['taxon_name'], x['trait_name'], f(x['a_min'], x['a_max']), f(x['b_min'], x['b_max']), x['status']])
    for x in sorted(c, key=lambda x: (x['trait_name'], x['taxon_name'])):
        w.writerow([x['taxon_name'], x['trait_name'], x['a'], x['b'], x['status']])
print(out)
