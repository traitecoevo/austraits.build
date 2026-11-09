# writes ignore/jobs/nt1/make_meta.py from the SAH_2026 one (ignore/jobs/sa1/make_meta.py), with the NT_2026 source, text and paths
s=open("ignore/jobs/sa1/make_meta.py").read()
def rep(a, b):
    global s
    assert s.count(a) == 1, a[:80]; s = s.replace(a, b)
rep('''# SAH_2026/metadata.yml, first creation (SA Flora profiles, read by the ABRS parser in SA Flora mode; user 2026-11-05). Afterwards the
# file is only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks reused by var_in from the other 2026 flora datasets (their
# cross-name mappings included); contexts (with their value lists) and substitutions from those datasets for the traits mapped here.
# Written without reference to SAH_2014 / SAH_2022 / SAH_2023.''','''# NT_2026/metadata.yml, first creation (NT Flora online factsheets, read by the ABRS parser in NT Flora mode; user 2026-11-07). Afterwards
# the file is only updated add-only (ignore/jobs/cp1/meta_update.py). Trait blocks reused by var_in from the other 2026 flora datasets
# (their cross-name mappings included); contexts (with their value lists) and substitutions from those datasets for the traits mapped here.
# Written without reference to NTH_2014 / NTH_2022 / NTH_2023 / NTH_2025 (the comparison cases afterwards).''')
rep('"data/SAH_2026/metadata.yml"','"data/NT_2026/metadata.yml"'); rep('open("data/SAH_2026/data.csv")','open("data/NT_2026/data.csv")')
rep('"WAH_2026_4", "RBGV_2026"]','"WAH_2026_4", "RBGV_2026", "SAH_2026"]')
rep('''cal = ("Months of {w} from the Phenology field of the profile (\\"Flowers: Aug.-Oct.\\", \\"Flowers mainly July-Sept.\\"; verbatim in "
       "value_description), as a 12-character y/n string starting in January; parenthetical extreme months and qualified alternatives are on "
       "commonness_qualifier rows.")''','''cal = ("Months of {w} from the {f} field of the factsheet's Ecological Attributes (month lists, \\"Mar, Apr, May, Jun, Aug, Sep\\", or "
       "ranges, \\"Feb - July\\", \\"throughout the year, mainly Oct-Dec\\"; verbatim in value_description), as a 12-character y/n string starting "
       "in January; qualified alternatives are on commonness_qualifier rows.")''')
rep('for t, w in [("flowering_time", "flowering"), ("fruiting_time", "fruiting")]:\n    if t in blocks: blocks[t] = dict(blocks[t], methods=cal.format(w=w))',
    'for t, w, f in [("flowering_time", "flowering", "Flowering"), ("fruiting_time", "fruiting", "Fruiting")]:\n    if t in blocks: blocks[t] = dict(blocks[t], methods=cal.format(w=w, f=f))')
rep('''    key: SAH_2026
    bibtype: Online
    author: '{State Herbarium of South Australia}'
    year: '2026'
    title: 'Flora of South Australia online'
    publisher: Board of the Botanic Gardens and State Herbarium, Adelaide
    url: https://flora.sa.gov.au
    note: accessed 5 November 2026''','''    key: NT_2026
    bibtype: Online
    author: '{Northern Territory Herbarium}'
    year: '2026'
    title: 'FloraNT: Northern Territory Flora Online'
    publisher: Northern Territory Government
    url: https://eflora.nt.gov.au
    note: accessed 7 November 2026''')
rep("Extracted the trait data from the Flora of South Australia online profiles","Extracted the trait data from the FloraNT factsheets")
rep('os.path.exists("data/SAH_2026/raw/taxon_urls.csv")','os.path.exists("data/NT_2026/raw/taxon_urls.csv")'); rep("in the saflora_url column","in the ntflora_url column")
i=s.index('  description: To compile'); j=s.index("  notes: '{url_note}")
s=s[:i]+open("ignore/jobs/nt1/meta_mid.txt").read()+s[j:]
rep('''A few description fragments that the SA Flora web pages file under another heading
    (Common Names, Phenology, Published Illustrations) were read as part of the description.''','''The common names are those of the factsheet without the genus name that opens every
    list; biostatus introduced is from Introduced Status ("Introduced to NT").''')
bad=[l for l in s.splitlines() if ("SAH" in l or "South Aus" in l) and "RBGV_2026" not in l]
assert not bad, bad
open("ignore/jobs/nt1/make_meta.py","w").write(s); print("written")
