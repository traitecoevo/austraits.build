# Drops genus / family copies from data/ABRS_AtoM_2026/raw/ecology_vetted.csv whose group statement is hedged (user 2026-11-05: copy a
# group value only when the genus / family text states one value that clearly applies to every member; never when any word suggests
# ambiguity - "most", "usually", "mainly", "primarily", "sometimes", "often", "common", "reported to be"; a member whose own text differs
# still overrides an unhedged copy). The 63 distinct group statements with such words were read on 2026-11-05; these 12 hedge the copied
# value itself (the others hedge something else - "bird- (and possibly mammal-) pollinated" is certain for birds - or name the species).
import csv
p = "data/ABRS_AtoM_2026/raw/ecology_vetted.csv"
DROP = [  # (evidence substring, traits or None for every trait)
    ("Most species of Chiloglottis use this system of deception", None),
    ("Usually pollinated by flies and beetles, occasionally bees", None),                       # Zieria
    ("Pollination is primarily by native bees, but", None),                                      # Euphrasia
    ("Seeds usually have an obvious aril and are dispersed by ants", None),                      # Euryomyrtus
    ("Dispersal mainly by birds attracted to the conspicuous", None),                            # Brackenridgea
    ("Cleistogamous flowers are common.", None),                                                 # Viola
    ("Cleistogenes sometimes present in basal sheaths", None),                                   # Nassella
    ("(sometimes cleistogamous with reduced paleas", ("breeding_system",)),                      # Microlaena
    ("The fruits are often glandular and sticky", None),                                         # Pisonia
    ("When an insect pollinator, usually a bee", None),                                          # Sarracenia
    ("Freycinetia species are reported to be vertebrate-pollinated", ("pollination_syndrome",)),
    ("are sometimes produced in large flushes", ("flowering_predictability",)),                 # Trachoma
]
rows = list(csv.DictReader(open(p))); fields = list(rows[0].keys())
def hit(r):
    if r["trait_scoring_method"] not in ("inferred_from_genus", "inferred_from_family"): return False
    return any(s in r["evidence"] and (tr is None or r["trait_name"] in tr) for s, tr in DROP)
keep = [r for r in rows if not hit(r)]
print(len(rows) - len(keep), "group-copy rows dropped;", len(keep), "left")
w = csv.DictWriter(open(p, "w", newline=""), fieldnames=fields, lineterminator="\n"); w.writeheader(); w.writerows(keep)
