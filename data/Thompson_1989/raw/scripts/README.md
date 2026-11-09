# Thompson_1989 scripts

Written for this dataset, so there is no other copy. Run in this order, from any directory. The build never runs them.

| File | Writes | Origin |
|---|---|---|
| `extract_text.py` | `raw/text/` (`t.txt`, `blocks.json`, `desc.txt`) | Text of the PDF with `pdftotext -layout`, split into one block per taxon (2026-11-09) |
| `spec.txt` | (read by `make_trait_data.py`) | Hand transcription of each taxon's habit and leaf sentences: growth form, habit, branching, height, leaf shape and size (2026-11-09) |
| `extra.py` | (imported by `make_trait_data.py`) | Scripted reading of the regular sentences: stems, bark, flowers, hypanthium, sepals, petals, stamens, ovary, fruit, seeds, flowering period, distribution and habitat. Every trait's values were reviewed taxon by taxon (2026-11-09) |
| `make_trait_data.py` | `raw/trait_data.csv`, `data.csv` | Combines the above, adds the ecology stated in the Introduction and the discussion of groups and the ovule numbers in the key to subspecies, finds each clause's page, copies species values down to subspecies silent on a character (2026-11-09/10) |
| `make_meta.py` | `metadata.yml` | Generator modelled on `data/Bean_1992/raw/scripts/make_meta.py` (2026-11-10) |

Source: `taxon_descriptions/resources-journal_articles/Myrtaceae Leptospermum revision Thompson_1989 Telopea telopea19894902.pdf`
(Telopea 3(3): 301--449). The text layer is good apart from split words ("Stam ens", "diam eter", "m ore") and the odd misread
("Dee" for Dec., "I.5" for 1.5, "6-1 0"), which `extra.py`'s `dejoin()` repairs. Two values were checked on page images:
L. incanum height (1--2 m, read as "12 m") and the L. argenteum leaf width ("about 1/2 as wide").

`raw/text/` is the full text of the paper and can be regenerated with `extract_text.py`.
