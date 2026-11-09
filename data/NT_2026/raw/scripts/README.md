# NT_2026: scripts used to build this dataset

Copies made 2026-11-09 for tracing. The working copies are at the paths below (`ignore/jobs/...` is in the austraits.build flora-extractions worktree; `~/GitHub/austraits.build-data.scraping.scripts/...` is the scraping repo). Paths inside the scripts point to those locations, not to this folder.

- `crawl.py` <- `ignore/jobs/nt1/crawl.py`
- `scrape.py` <- `ignore/jobs/nt1/scrape.py`
- `names.py` <- `ignore/jobs/nt1/names.py`
- `gen_make_meta.py` <- `ignore/jobs/nt1/gen_make_meta.py`
- `make_meta.py` <- `ignore/jobs/nt1/make_meta.py`
- `vet.py` <- `ignore/jobs/nt1/vet.py`
- `eco_cand.py` <- `ignore/jobs/nt1/eco_cand.py`
- `proc_nt.R` <- `ignore/jobs/nt1/proc_nt.R`
- `excl_nt.R` <- `ignore/jobs/nt1/excl_nt.R`
- `final_qa_nt.R` <- `ignore/jobs/nt1/final_qa_nt.R`

The trait text is read by the shared flora parser, `data/ABRS_AtoM_2026/raw/build_data_csv.R`.
