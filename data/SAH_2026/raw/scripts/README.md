# SAH_2026: scripts used to build this dataset

Copies made 2026-11-09 for tracing. The working copies are at the paths below (`ignore/jobs/...` is in the austraits.build flora-extractions worktree; `~/GitHub/austraits.build-data.scraping.scripts/...` is the scraping repo). Paths inside the scripts point to those locations, not to this folder.

- `make_meta.py` <- `ignore/jobs/sa1/make_meta.py`
- `add_questions.py` <- `ignore/jobs/sa1/add_questions.py`
- `vet.py` <- `ignore/jobs/sa1/vet.py`
- `eco_cand.py` <- `ignore/jobs/sa1/eco_cand.py`
- `phen_over.py` <- `ignore/jobs/sa1/phen_over.py`
- `final_qa_sah.R` <- `ignore/jobs/sa1/final_qa_sah.R`
- `proc_sah.R` <- `ignore/jobs/sa2/proc_sah.R`

The trait text is read by the shared flora parser, `data/ABRS_AtoM_2026/raw/build_data_csv.R`.
