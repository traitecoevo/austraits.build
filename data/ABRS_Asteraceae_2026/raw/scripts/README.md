# ABRS_Asteraceae_2026: scripts used to build this dataset

Copies made 2026-11-09 for tracing. The working copies are at the paths below (`ignore/jobs/...` is in the austraits.build flora-extractions worktree; `~/GitHub/austraits.build-data.scraping.scripts/...` is the scraping repo). Paths inside the scripts point to those locations, not to this folder.

- `make_meta.py` <- `ignore/jobs/as1/make_meta.py`
- `final_qa_ast.R` <- `ignore/jobs/as2/final_qa_ast.R`

The trait text is read by the shared flora parser, `data/ABRS_AtoM_2026/raw/build_data_csv.R`.

make_meta.py is superseded by ../../ABRS_AtoM_2026/raw/scripts/meta_update.py (add-only); kept for the record.
