#!/usr/bin/env python3
"""Bean_2018: writes raw/Bean_2018_vouchers.csv from the hand transcription below (checked on the page images).

One line per collection: status | institutions | collector | record number | date | locality | state | country | section | notes.
Several institutions give one row each. The paper cites no accession numbers.
"""
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
ANI = "Stemodia anisata"
MC = "The two NT specimens bear the phrase name Stemodia sp. Manners Creek (Notes, p.243)."

V = {
    ANI: """
holotype|BRI|R. Booth & D. Kelman|3572|2004-07-25|Burke District: Ardmore Station, S of Mount Isa|Qld|Australia|Type, p.242|Holotype on 1 sheet. Also Figs 1 and 2.
isotype|CANB,DNA,NSW,NY|R. Booth & D. Kelman|3572|2004-07-25|Burke District: Ardmore Station, S of Mount Isa|Qld|Australia|Type, p.242|Isotypes listed as distribuendi (to be distributed).
specimen examined|DNA|Risler & Duguid|986|2001-08|Georgina Downs Station, 5 km NNW of no. 8 bore|NT|Australia|Additional specimens examined, p.243|""" + MC + """
specimen examined|DNA|Albrecht & Latz|6319|1995-03|7 km WSW of no. 3 bore, Manners Creek Station|NT|Australia|Additional specimens examined, p.243|""" + MC + """
specimen examined|BRI|Thomas & Turpin|3421|2007-04|Gregory North District: 2 km NNW of Cravens Peak homestead, on road to Sand Hill bore|Qld|Australia|Additional specimens examined, p.243|
specimen examined|BRI|Nicholson|cjnEthabuka12|2011-08|Ethabuka, Simpson Desert interdune swale near Field River|Qld|Australia|Additional specimens examined, p.243|
specimen examined|AD,BRI|Gillen|JSG7b|2011-06|Ethabuka Station, 7.8 km N of homestead|Qld|Australia|Additional specimens examined, p.243|
specimen examined|BRI|Kemp & Radford|JEK11756|2010-08|Ethabuka Station, c. 375 km SSW of Mt Isa, near border with Northern Territory|Qld|Australia|Additional specimens examined, p.243|
specimen examined|BRI,DNA,MEL|Halford & Forster|QM64|2010-06|Ethabuka campground, c. 3 km S of homestead, 112 km NW of Bedourie|Qld|Australia|Additional specimens examined, p.243|
specimen examined|BRI|Silcock|JLS961|2011-08|25 km W of Ethabuka homestead, 10 km S of Field River Road|Qld|Australia|Additional specimens examined, p.243|
""",
    "Stemodia sp. Manners Creek (T.S.Henshall 1779)": """
phrase name voucher|AD,NT|T.S. Henshall|1779|||NT|Australia|Notes, p.243|Phrase-name voucher. W.R. Barker identified the AD duplicate as Elacholoma prostrata (AVH 2018); the NT specimen is also thought to be Elacholoma (P. Jobson pers. comm.).
""",
    "Stemodia sp. Tanami (P.K. Latz 8218)": """
phrase name voucher||P.K. Latz|8218|||NT|Australia|Notes, p.243|Phrase-name voucher; the author saw only an image on the Flora NT website. Herbarium not stated.
""",
}

COLS = ["taxon_name", "specimen_status", "institution_code", "catalog_number", "collector", "record_number", "collection_date", "locality",
        "state", "country", "source_section", "notes"]
out = []
for taxon, block in V.items():
    for line in block.strip().splitlines():
        p = [x.strip() for x in line.split("|")]
        if len(p) != 10:
            raise SystemExit(f"{taxon}: {len(p)} fields: {line}")
        status, insts, coll, rec, date, loc, state, country, sec, notes = p
        for inst in insts.split(",") if insts else [""]:
            out.append(dict(taxon_name=taxon, specimen_status=status, institution_code=inst, catalog_number="", collector=coll,
                            record_number=rec, collection_date=date, locality=loc, state=state, country=country, source_section=sec, notes=notes))
with open(ROOT / "data" / "Bean_2018" / "raw" / "Bean_2018_vouchers.csv", "w", newline="") as f:
    w = csv.DictWriter(f, COLS)
    w.writeheader()
    w.writerows(out)
print(len(out), "voucher rows")
