# George_2002: scripts used to build this dataset

Copies made 2026-11-09 for tracing. The working copies are at the paths below (`ignore/jobs/...` is in the austraits.build flora-extractions worktree; `~/GitHub/austraits.build-data.scraping.scripts/...` is the scraping repo). Paths inside the scripts point to those locations, not to this folder.

- `page.py` <- `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Verticordia_ocr/scripts/page.py`
- `entries.py` <- `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Verticordia_ocr/scripts/entries.py`
- `recall.py` <- `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Verticordia_ocr/scripts/recall.py`
- `vocr.swift` <- `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Verticordia_ocr/scripts/vocr.swift`
- `vocrw.swift` <- `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Verticordia_ocr/scripts/vocrw.swift`
- `make_meta.py` <- `ignore/jobs/vert1/make_meta.py`
- `proc_vert.R` <- `ignore/jobs/vert1/proc_vert.R`
- `excl.R` <- `ignore/jobs/vert1/excl.R`

The trait text is read by the shared flora parser, `data/ABRS_AtoM_2026/raw/build_data_csv.R`.
