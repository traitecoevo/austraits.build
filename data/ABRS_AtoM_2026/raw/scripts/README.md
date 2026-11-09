# ABRS_AtoM_2026: scripts used to build this dataset

Copies made 2026-11-09 for tracing. The working copies are at the paths below (`ignore/jobs/...` is in the austraits.build flora-extractions worktree; `~/GitHub/austraits.build-data.scraping.scripts/...` is the scraping repo). Paths inside the scripts point to those locations, not to this folder.

- `make_meta.py` <- `ignore/jobs/am1/make_meta.py`
- `check_long.py` <- `ignore/jobs/am1/check_long.py`
- `final_qa_atom.R` <- `ignore/jobs/am1/final_qa_atom.R`
- `meta_update.py` <- `ignore/jobs/cp1/meta_update.py`
- `cand_all.py` <- `ignore/jobs/cp1/cand_all.py`

The shared parser for all the 2026 flora datasets is ../build_data_csv.R. meta_update.py (add-only metadata updates) and cand_all.py (candidate-trait register) serve every flora dataset; they live here with the parser.
