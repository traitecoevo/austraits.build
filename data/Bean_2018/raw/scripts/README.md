# Bean_2018 scripts

Written for this dataset, so there is no other copy. Run from the repository root, in this order. The build never runs them.

| File | Writes | Origin |
|---|---|---|
| `make_trait_data.py` | `raw/trait_data.csv`, `raw/Bean_2018_taxa.csv`, `data.csv` | Hand transcription of every clause of the paper, written in this folder (2026-11-10), following `data/Bean_1992/raw/scripts/make_trait_data.py` |
| `make_vouchers.py` | `raw/Bean_2018_vouchers.csv` | Hand transcription of the type and specimen citations, written in this folder (2026-11-10) |
| `make_meta.py` | `metadata.yml` | Generator written in this folder (2026-11-10), adapted from `data/Bean_1992/raw/scripts/make_meta.py`. It reads `SUBS` from `make_trait_data.py` for the substitutions |

Source: `taxon_descriptions/resources-journal_articles/Plantaginaceae Stemodia anisata new_species Bean_2018 Austrobaileya part299905.pdf`
(BHL copy of Austrobaileya 10(2): 242--246). The text layer garbles some digits ("37-4.5 mm" for 3.7--4.5), so every clause was
checked on page images made with `pdftoppm -r 120 -gray -png`.
