---
name: convert-raw-data-file-into-data-csv
description: Pivot a dataset's long-format `trait_data.csv` (one row per taxon/trait/value, as produced by `paper-trait-location-extraction`) into a compact wide-format `data.csv` in the same folder -- one row per taxon (or per taxon x experimental-context combination), traits along the columns. Trigger phrase: the user typing "convert raw data file into data.csv" (ask which dataset's `raw/` folder if it isn't obvious from context). Any repeated non-trait dimension that distinguishes multiple measurements of the same trait for the same taxon (a pollination treatment, a survey year, a habitat, a size class, ...) becomes its own explicit column -- never a suffix baked into the trait's column name.
---

# Convert raw data file into data.csv

Takes the long, fully-annotated `trait_data.csv` a `paper-trait-location-extraction`-style
extraction produces (every row carrying `taxon_name`, `entity_type`,
`entity_context`, `location_name`, `trait`, `trait_value_raw`, `trait_value`,
`trait_value_clean`, `value_type`, `unit`, `error_type`, `error_value`, `n`,
`context`, `source_section`, `notes`) and reshapes it into a `data.csv` a
curator can actually scan or drop into a spreadsheet: traits as columns,
one row per taxon (or per taxon x context combination, see Step 3). This is
a *derived convenience view* -- `trait_data.csv` stays the source of truth
with its full quotes and provenance; `data.csv` sits alongside it in the
same `raw/` folder and carries only clean, mappable values.

## Step 1 -- find every place the same (taxon, location, trait) has more than one row

Before designing any columns, group the long file and see what's actually
repeating:

```python
import csv
from collections import defaultdict

rows = list(csv.DictReader(open("data/<id>/raw/trait_data.csv")))
groups = defaultdict(list)
for r in rows:
    groups[(r["taxon_name"], r["location_name"], r["trait"])].append(r)

for key, rs in groups.items():
    if len(rs) > 1:
        for r in rs:
            print(key, "| context=", r["context"], "| value_type=", r["value_type"],
                  "| entity_context=", r["entity_context"])
```

Every group with more than one row is repeating for exactly one of two
reasons, and they get handled completely differently:

1. **A genuine second dimension** -- the rows describe the *same trait
   measured under different conditions*: a pollination treatment (Open/
   Autogamy/Self/Outcross/Supplementary), a survey year, a time of day, a
   germination cohort, a habitat, a size class. This is visible as a
   recurring pattern in the `context` column across many traits, not just
   one. **This becomes its own explicit column** (`pollination treatment`,
   `survey year`, ...) with short controlled tag values -- never suffixed
   onto the trait's own column name (`seeds_per_fruit [Open]` is wrong;
   a `pollination treatment` column with value `Open` is right). A dataset
   can have more than one such dimension at once (e.g. a seedling-survival
   table keyed by germination cohort *and* habitat *and* survival year all
   together) -- give each its own column rather than collapsing them into
   one compound string.
2. **Multiple raw, not-yet-mapped identities for the same conceptual
   measurement** -- most commonly `flower_visitor`/`pollination_vector_*`
   rows where `entity_context` carries the actual species name (e.g.
   `Lasioglossum castor`, `Amegilla chlorocyanea`) behind a generic mapped
   `trait_value_clean` (`bee`). When several such rows share the same
   taxon, location and trait **and nothing else distinguishes them**, don't
   give them separate rows or separate columns -- merge them into **one**
   row, comma-delimiting the identities into a companion `<trait> identity`
   column, and comma-delimit `trait_value_clean` in the same order in the
   main column if the mapped values differ from each other (e.g.
   `trait = "bee, bee, fly"`, `... identity = "Lasioglossum castor,
   Lasioglossum chapmani, Bombyliidae"`). If a genuine context dimension
   *also* applies (e.g. different visitors recorded in different years),
   handle that with Step 1's column first, then comma-delimit within each
   resulting context-row only.

A third, easier case: rows differing only in `value_type` (`minimum` vs
`maximum`, or `mean` with an `error_type`/`error_value`) are not a context
dimension at all -- see Step 2.

## Step 2 -- turn value_type into column pairs, not row differences

For a single trait column, `value_type` determines how many companion
columns it needs, independent of any context dimension from Step 1:

- `mode` / `raw` -> one column (`trait (unit)`), value = `trait_value_clean`.
- `mean` (or `median`) with an `error_type` -> two adjacent columns:
  `trait (unit)` and `trait SE` (name the second column after whatever
  `error_type` actually says -- usually SE, occasionally CI_95 -- don't
  assume).
- `minimum` and `maximum` rows for the same trait+context -> two adjacent
  columns: `trait (minimum) (unit)` and `trait (maximum) (unit)`.
- If the *same* trait+context genuinely has both a raw observed range
  (min/max) *and* a separately reported mean+SE (e.g. an observed range of
  seeds per fruit alongside a model-estimated mean number of seeds
  produced) -- that is not a new context dimension, it's just two
  statistics about the same cell. Give it both column pairs on the same
  row (`trait (minimum)`, `trait (maximum)`, `trait mean`, `trait SE`)
  rather than inventing a "statistic type" column.

## Step 3 -- decide the row grain

Default to **one row per taxon** (per taxon x `location_name`, if a taxon
was studied at more than one site -- those are genuinely separate rows,
never merged). If Step 1 found a real context dimension that applies to
*some* traits (e.g. a pollination-treatment experiment) while other traits
are simple whole-species characteristics (flower colour, growth form,
life history, ...), you have two reasonable options -- pick whichever
reads more cleanly for the dataset at hand, there's no single right answer:

- Emit **one row per (taxon, context-value)** combination that actually has
  data (never fabricate a row for a treatment/context that wasn't measured
  for that taxon just to keep a rectangular grid), and fold the
  context-independent, whole-species columns onto the **first** such row
  for each taxon (leave them blank on the rest) -- this is usually the more
  compact choice and was preferred in the worked Eakin-Busher_2020 example.
- Or give whole-species traits their **own dedicated row** per taxon (with
  the context column blank/`NA` on that row) and keep every context-value
  row purely for the context-dependent traits. Slightly more rows, but
  keeps "this row is a treatment cell" and "this row is a species
  characteristic" visually separate.

Either way: don't silently drop rows with no `taxon_name` at all (a rare,
genuinely pooled/community-level measurement not attributable to one
species) into this file -- they don't fit a per-taxon wide row, so leave
them out of `data.csv` and say so in the report back; they're still in
`trait_data.csv`.

An aggregate context value that summarises several of the others (a
"pooled across all treatments" row, a merged "insect-simulated" category
combining three raw treatments) is fine to live as just another tag value
in the same context column -- flag what it aggregates in the chat report
rather than bloating the cell text with an explanation.

## Step 4 -- build and validate

Write `data.csv` into the same `raw/` folder as `trait_data.csv`. Column
order: `taxon_name`, `location_name`, then any context dimension column(s)
from Step 1, then trait columns in roughly the order they first appear in
`trait_data.csv`. After writing:

- Re-open with `csv.DictReader` and confirm every row has the same column
  count as the header (catches unescaped commas).
- Confirm every `location_name` value used in `data.csv` also appears in
  `location_data.csv`.
- Confirm every `location_name` maps to exactly **one** coordinate pair. If a
  raw source's own location column bundles a place name together with
  coordinates (e.g. `"Melaleuca, 31°40′28″S 115°53′44″E"`) and, once split,
  the same name recurs with a *different* coordinate pair elsewhere in the
  file, that's two distinct sites sharing a name (or a transcription slip in
  the source) -- either way, don't let both coordinate pairs sit under one
  `location_name`. Disambiguate by appending `_1`, `_2`, ... to the name, in
  order of first appearance in the file (so the more common/first-seen
  coordinate pair gets `_1`, e.g. `Melaleuca_1` / `Melaleuca_2`), and flag
  the split names plus their coordinates in the Step 5 report so the curator
  can check it against the source rather than silently trusting the guess.
  **Never collapse the repeats into one "representative" coordinate (a mean
  or median) instead of splitting** -- this is tempting at scale (a dataset
  with hundreds of named places, many showing small cross-record coordinate
  disagreement, feels like it's just measurement/rounding noise not worth
  fragmenting into dozens of `_1`/`_2`/`_3`... variants), but averaging
  silently discards whichever reading was actually correct for any single
  record, exactly the "silently trusting the guess" this rule exists to
  avoid. Every coordinate that parses as a plausible, in-range geographic
  point is retained under its own suffixed name, however many there are and
  however close together they sit -- the only reading that gets dropped
  entirely (mapped to `location_name: unknown` for that row alone, not
  merged into a neighbour) is one that isn't a real interpretable coordinate
  at all (out of range for the study region, or unparseable, e.g. a stray
  non-numeric character or an impossible degree value from a transcription
  slip) -- and that never removes the *other* valid coordinate(s) sharing
  the name.
- Spot-check a handful of numeric cells directly against the numbers
  already sitting in `trait_data.csv`'s `trait_value_raw`/`notes` for the
  same taxon+trait+context -- this is a pivot of data already extracted and
  reviewed, not a fresh read of the source PDF, so any mismatch is a bug in
  the pivot code, not a transcription question.

## Step 5 -- report back

Column count and row count; which column(s) got introduced as explicit
context dimensions and why (name the recurring pattern you found in
`context`); which traits, if any, got comma-delimited multi-identity
treatment (Step 1, case 2); which rows (if any) were left out of `data.csv`
because they had no `taxon_name`; the row-grain choice made in Step 3; and
any `location_name` values that had to be split into `_1`/`_2`... variants
(Step 4) because the same name carried more than one coordinate pair --
name the variants and their coordinates so the curator can check them
against the source.
