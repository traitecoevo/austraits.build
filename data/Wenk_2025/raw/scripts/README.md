# Wenk_2025 raw/scripts

## add_source_id.R

Rebuilds the `source_id` column of `data/Wenk_2025/data.csv` from the reference
columns already in the file (`reference`, `author`, `year`, `short_title`, `title`,
`journal`, `doi`, ...) and writes the matching references to `data/Wenk_2025/sources.yml`
(named by `source: sources.yml` in `metadata.yml`). Run from the repo root:

```
Rscript data/Wenk_2025/raw/scripts/add_source_id.R
```

It changes only the `source_id` column of `data.csv`. Each distinct `reference` gets one
`source_id`:

- **Online floras, databases and institution-authored works** get a flora or agency code
  plus the year. Online floras use the codes AusTraits uses for them elsewhere:
  `ABRS_2025` (Flora of Australia online), `NHNSW_2025` (PlantNET), `RBGV_2025` (VicFlora),
  `WAH_2026` (Florabase), `NTH_2026` (FloraNT), `SAH_2026` (eFlora SA), `CPBR_2026`
  (EUCLID), `TMAG_2026`, `ATRP_2026`, `Efloras_2026`, `PPD_2026` (Flora of Pakistan),
  `PROTA_2026`, `AVH_2026`. Their entries copy the flora dataset's own reference
  (e.g. `WAH_2023_1`), with the year changed and without its access date. Other sites
  keep a compact name (`ATH_2026`, `WFO_2026`, `POWO_2026`, `Jepson_2026`, ...).
  Printed Flora of Australia volumes become `ABRS_<year>`.
- **Works with a personal author** get the first author's surname plus the year
  (e.g. `Brophy_2013`). Surnames are transliterated to ASCII and spaces, apostrophes and
  hyphens are removed, so "de Vogel" becomes `deVogel` and "Airy-Shaw" `AiryShaw`.
- **Works with no author and no code** get a compact form of the title plus the year
  (e.g. `FloraKimberleyRegion_1992`, `FloraPerthRegion1_1987`).
- Undated online sources take 2026, the year they were consulted (`iNaturalist_2026`).
- Distinct references that share an ID get `_1`, `_2`, … in reference order.
- **Checked against AusTraits** (`scripts/source_references.R`): a work already in another
  dataset (same DOI, or same title and year) takes that key and copies its entry exactly
  (e.g. `Hill_1995` from Stephens_2021, `Crisp_2017` from the Crisp_2017 dataset;
  `same_as` in the script lists seven matched by eye, such as `Chinnock_2007`); a key
  that names a different work elsewhere takes the next free suffix (e.g. `Brown_2019` →
  `Brown_2019_2`). The lookup's `action` column records which.
- Rows with no matched reference (`reference` is `NA NA. NA. NA NA. NA.`) are linked from
  the scorer's note in `reference_OG` where it names a work (Flora of South Australia
  Part II, Hayata 1920, Shtein & Smith 2021); the rest (236 → 214 rows) keep a blank
  `source_id`, so they cite the dataset, and `custom_R_code` copies the note into
  `measurement_remarks`.

`source_id_lookup.csv` lists every reference, the `source_id` it was given and, for
keys checked against AusTraits, what was done (`copy` and the dataset copied from,
`rename` and why, or `new`).

Edits by hand live in tables near the top of the script: `codes`, `recode`,
`unmatched`, `book_authors`, `authors`, `fixes`, `manual`, `keep` and `same_as`.

Fitzgerald_1918 and Johnston_1997 each merge two reference strings for the same paper.
The volumes of Flora of the South West (1 and 2) both cite `Wheeler_2002`, the dataset
entered from that work.
