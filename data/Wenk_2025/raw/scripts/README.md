# Wenk_2025 raw/scripts

## add_source_id.R

Rebuilds the `source_id` column of `data/Wenk_2025/data.csv` from the reference columns
already in the file (`reference`, `author`, `year`, `short_title`, `title`). Run from the
repo root:

```
Rscript data/Wenk_2025/raw/scripts/add_source_id.R
```

It changes only the `source_id` column. Each distinct `reference` gets one `source_id`:

- **Online floras, databases and institution-authored works** get a flora or agency code
  plus the year: `ABRS_2025` (Flora of Australia online), `NHNSW_2025` (PlantNET),
  `RBGV_2025` (VicFlora), `WAH_2026` (Florabase), `ATRP_2026` (Australian Tropical
  Rainforest Plants), `ATH_2026` (Australian Tropical Ferns and Lycophytes), `WFO_2026`,
  `POWO_2026`, `eFloras_2026`, `Jepson_2026`, and so on. Printed Flora of Australia
  volumes become `ABRS_<year>`. The codes are listed in the `codes` table in the script.
- **Works with a personal author** get the first author's surname plus the year
  (e.g. `Brophy_2013`). Surnames are transliterated to ASCII and spaces and apostrophes
  are removed, so "de Vogel" becomes `deVogel` and "Kükenthal" becomes `Kukenthal`.
- **Works with no author and no code** get a compact form of the title plus the year
  (e.g. `FloraKimberleyRegion_1992`, `FloraPerthRegion1_1987`).
- If the year is missing, the ID has no year (e.g. `AVH`, `Mueller`).
- Distinct references that share an ID get `_1`, `_2`, … in reference order.
- Rows with no matched reference (`reference` is `NA NA. NA. NA NA. NA.`) get a blank
  `source_id`.

`source_id_lookup.csv` lists every reference and the `source_id` it was given.

The references are not yet mapped as sources in `metadata.yml`.

Known duplicates: `Fitzgerald_1918_1`/`_2` and `Johnston_1997_1`/`_2` are each the same
paper under two slightly different reference strings.
