# ABRS_2026 QA helpers (Flora of Australia extraction)

Used after each family chunk of `data/ABRS_2026/raw/build_data_csv.R` (flora-extractions-2026 worktree).
All expect env `S` = a scratch folder; run from the worktree root.

Chunk run (writes $S/chunk.csv and the QA object $S/qa.rds):
    FOA_FAMILIES="Fam1,Fam2" FOA_OUT=$S/chunk.csv FOA_QA=$S/qa.rds Rscript data/ABRS_2026/raw/build_data_csv.R
Full run (rewrites data.csv; QA object for diffing):
    cp data/ABRS_2026/data.csv $S/data_old.csv; FOA_QA=$S/qa_full.rds Rscript data/ABRS_2026/raw/build_data_csv.R

| script | what it prints |
|---|---|
| abrs_qa_tabs.R | every categorical description -> mapped value table ($S/cat_tab.txt) + numeric ranges / min>max |
| abrs_qa2.R / abrs_qa2b.R | genus/family copy-down values (universal list, rejected list, copied counts) + numeric outliers |
| abrs_qa3.R | height misses + growth-form / habit / substrate rows with source text |
| abrs_qa4.R | flowering / fruiting calendars next to source Phenology; all commonness / region rows |
| abrs_qa5.R | measurement misses (leaf / fruit / seed text with numbers but no value) |
| abrs_diff.R | value-level diff of data.csv vs $S/data_old.csv for previously checked families (regression check) |
| abrs_final_qa.R | duplicate keys, digit-digit hyphens, 12-char calendars, columns not in traits.yml; reads $S/fams.txt |
| abrs_grp.R | print genus/family description sentences matching env P for groups in env G |
