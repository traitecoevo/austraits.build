# ABRS QA helpers (Flora of Australia extraction)

The FoA extraction is split by family across `data/ABRS_AtoM_2026`, `ABRS_NtoZ_2026`, `ABRS_Myrtaceae_2026` (and later
`ABRS_Asteraceae_2026`, `ABRS_Fabaceae_2026`, `ABRS_Proteaceae_2026`). One parser, `data/ABRS_AtoM_2026/raw/build_data_csv.R`
(flora-extractions-2026 worktree), writes all of them; `own_dataset` / `dataset_of()` in the script route families.
All helpers expect env `S` = a scratch folder; run from the worktree root.

Chunk run (whole families via FOA_FAMILIES, plus single genera of a very large family via FOA_GENERA; writes $S/chunk.csv
and the QA object $S/qa.rds; ~20 s on 10 cores):
    FOA_FAMILIES="Fam1,Fam2" FOA_GENERA="Gen1,Gen2" FOA_OUT=$S/chunk.csv FOA_QA=$S/qa.rds Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R
Full run (rewrites every ABRS_*_2026/data.csv; ~10 min, parallel via FOA_CORES, default 10). Snapshot first, and run a
frozen copy of the script (Rscript reads the file as it goes, so editing it mid-run corrupts the run):
    mkdir -p $S/data_old; for f in data/ABRS_*_2026/data.csv; do cp $f $S/data_old/$(basename $(dirname $f)).csv; done
    cp data/ABRS_AtoM_2026/raw/build_data_csv.R $S/build_run.R; FOA_QA=$S/qa_full.rds Rscript $S/build_run.R

| script | what it prints |
|---|---|
| abrs_qa_tabs.R | every categorical description -> mapped value table ($S/cat_tab.txt) + numeric ranges / min>max |
| abrs_qa2.R / abrs_qa2b.R | genus/family copy-down values (universal list, rejected list, copied counts) + numeric outliers |
| abrs_qa3.R | height misses + growth-form / habit / substrate rows with source text |
| abrs_qa4.R | flowering / fruiting calendars next to source Phenology; all commonness / region rows |
| abrs_qa5.R | measurement misses (leaf / fruit / seed text with numbers but no value) |
| abrs_diff.R | value-level diff of all ABRS_*_2026/data.csv vs $S/data_old/ (or $S/data_old.csv) for previously checked families (regression check); saves $S/diff.rds |
| abrs_final_qa.R | duplicate keys, digit-digit hyphens, 12-char calendars, columns not in traits.yml over all ABRS_*_2026 files; reads $S/fams.txt |
| abrs_grp.R | print genus/family description sentences matching env P for groups in env G |
