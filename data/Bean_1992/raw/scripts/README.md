# Bean_1992 scripts

Written for this dataset, so there is no other copy. Run from the repository root, in this order. The build never runs them.

| File | Writes | Origin |
|---|---|---|
| `make_trait_data.py` | `raw/trait_data.csv`, `raw/Bean_1992_taxa.csv`, `data.csv` | Hand transcription of every clause of the paper, written in this folder (2026-11-09); per-taxon treatment type and first-description year added 2026-11-10 |
| `make_vouchers.py` | `raw/Bean_1992_vouchers.csv` | Hand transcription of the type and specimen citations, written in this folder (2026-11-09) |
| `make_meta.py` | `metadata.yml` | Generator written in this folder (2026-11-09). It reads `SUBS` from `make_trait_data.py` for the substitutions |

Source: `taxon_descriptions/resources-journal_articles/Myrtaceae Leptospermum partial_revision Bean_1992 Austrobaileya part366003.pdf`
(BHL scan of Austrobaileya 3(4): 643--659). Its text layer is poor, so the clauses were read from page images made with
`pdftoppm -r 130 -gray -png`, with the text layer (`pdftotext`) used only to check the transcription afterwards.
