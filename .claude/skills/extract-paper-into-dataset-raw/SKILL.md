---
name: extract-paper-into-dataset-raw
description: Read a source PDF for a dataset that already has (or is about to get) a `data/<Surname_Year>/` folder, and write its trait data straight into `data/<Surname_Year>/raw/trait_data.csv` (plus `raw/location_data.csv` when the source names field sites) -- the same audit-able long-format CSV(s) already sitting in `data/EakinBusher_2020/raw/`, `data/Houston_2002/raw/`, `data/Yates_2004/raw/`, `data/Ladd_2019/raw/` and similar. Trigger phrase: the user typing "extract paper into data/<id>/raw" or "extract trait data from <paper/PDF> into data/<id>/raw", or handing over a PDF plus a dataset id and asking for its raw data table(s). Differs from `paper-trait-location-extraction`, which deliberately writes to a scratch folder *outside* `data/` for a paper that hasn't been decided on yet -- use this skill instead once the curator already treats the paper as a specific dataset (an existing, even if still-empty, `data/<id>/` folder; or an explicit "call it <Surname_Year>" instruction), and wants the CSV(s) to land where a real dataset's raw files live.
---

# Paper PDF -> `data/<Surname_Year>/raw/` trait table (+ location table)

This writes directly into a dataset's own folder, not a pre-review scratch
folder. Use `paper-trait-location-extraction` instead when the curator is
still deciding whether a paper is worth adding at all -- this skill is for
the point after that decision, where the paper already **is** a dataset (an
empty or partial `data/<id>/` folder already exists, e.g. a stub someone
created ahead of time) or the curator names the `<Surname_Year>` id directly.

The extraction rules below (what counts as a trait, how to read tables vs.
prose vs. figures, the column semantics) are identical to
`paper-trait-location-extraction` -- read that skill's Step 0/1/1a/2 for the
full reasoning if anything here is ambiguous. What's different is **only**
the output location and one extra column this repo's real raw files carry
that the reconnaissance skill's own doc doesn't mention.

## Step 0 -- identify the dataset id and read the whole paper

- Confirm the target folder: `data/<Surname_Year>/` (create it if it doesn't
  exist yet). Check whether `raw/trait_data.csv` already exists there --
  if it does, this is likely a re-run or an extension, not a first pass;
  read the existing file first so new rows are consistent with it (same
  candidate trait names, same species-name spelling) rather than duplicating
  under a slightly different name.
- Get the page count, then read the whole PDF (the `Read` tool caps at 20
  pages/call). Don't jump straight to "Results" -- trait values worth
  recording turn up in the Abstract, Methods (e.g. a species-comparison
  table), Results tables, Results prose, figure captions, and Discussion
  (papers often restate a key number when interpreting it).
- Note the full citation (authors, year, title, journal, volume, pages, DOI)
  for the chat report, and check whether this paper is already cited as a
  `secondary_NN` source in another dataset's `metadata.yml` (e.g.
  `data/Ladd_2026/metadata.yml` cites `Ladd_2023`) -- if so, say so, but
  extract anyway: a compilation typically only pulled a handful of traits
  out of a much richer source.

## Step 1 -- what goes in `raw/trait_data.csv`

Any measured or described property of the study organism(s) or their
interactions -- morphology, phenology, reproductive biology, breeding-system
outcomes, visitor/pollinator interactions, counts, proportions, rates --
whether it comes from a data table (transcribe every cell), from results
stated in prose ("ranging from 57-79%" is two rows, `minimum`/`maximum`,
never collapsed to a midpoint), or from a figure caption/axis label that
states a number directly. Skip pure statistical-method bookkeeping (GLMM
family, R package, model diagnostics) -- that belongs in the add-dataset
skill's `methods` field, not here.

**A number that exists only as a bar height or point position on a chart,
with no printed label and no value repeated in the text, is not a data
point** -- do not estimate it by eye. Leave it out (or add a row with
`trait_value_raw` blank and a `notes` entry naming the figure), and name it
in the Step 3 report as a figure-only number not extracted.

### Columns -- the 16-column format this repo's raw files actually use

```
taxon_name,entity_type,entity_context,location_name,trait,trait_value_raw,trait_value,trait_value_clean,value_type,unit,error_type,error_value,n,context,source_section,notes
```

This is `paper-trait-location-extraction`'s column set plus one: `location_name`
sits where that skill sometimes calls it `location_code` (rename it
`location_code` here too, following the same rule, if the source keys its own
results by a short site code rather than a spelled-out name), and
**`trait_value_clean`** is new. Every other column's meaning is exactly as
that skill documents (`entity_type`, `entity_context`, `trait_value`,
`value_type`, `unit`, `error_type`/`error_value`, `n`, `context`,
`source_section`, `notes` -- read it before writing your first row if
unsure). The one worth restating because it's easy to get backwards:

- **`trait_value_raw`**: the value exactly as the source states it, never
  adjusted or unit-converted.
- **`trait_value`**: populated **only** when `trait` is a real
  `config/traits.yml` trait *and* it's categorical -- the matched
  `allowed_values_levels` term(s), space-delimited if more than one applies
  simultaneously. Blank for every numeric row and for any unmapped candidate
  trait, mapped or not.
- **`trait_value_clean`**: the actually-usable cleaned value, populated on
  **every** row regardless of mapping status -- the parsed number for a
  numeric row (`23.5`, `0.5`, with any `~`/units/prose stripped), or the
  cleaned term for a categorical row, whether or not that term is a
  confirmed `allowed_values_levels` match (so it equals `trait_value` when
  there is one, and carries the best-effort clean term when there isn't).
  This is what lets a spreadsheet user sort/filter the file without having
  to parse `trait_value_raw` prose themselves.

Check `config/traits.yml` for a real trait name before assuming there isn't
one (`grep` it directly -- 500+ concepts, too big to skim). When nothing
fits, use a short plain-language candidate name (`pollen_tackiness`,
`ovary_locule_number`) and say so in `notes` --
`"Candidate trait, not in config/traits.yml (checked <today's date, e.g. 2026-09>)."`
-- exactly the phrasing already used across `data/Houston_2002/raw/trait_data.csv`.
A candidate name plus an unmapped value is exactly as valuable a row as a
mapped one; never leave a real measurement out just because it won't map.

**Split a compound source phrase into separate rows per concept.** "6 equal,
straight stamens" is a count and a form -- two rows, two `trait` names, not
one merged trait; splitting is what lets one half map to a real trait even
when the other doesn't.

**A trait whose real-world values are far more specific than its AusTraits
allowed values** (e.g. `flower_visitor`'s generic `bee`/`beetle` vs. a source
naming an actual genus) still gets its best-fit generic `trait_value`, plus
the raw specific name(s) in `entity_context`.

**When a value the source reports for one species is explicitly sourced
from a *different* paper** (a table footnote like "values from Smith &
Jones 2010"), still record it -- but flag this clearly in `notes` on every
affected row, since it means the number is not this paper's own measurement
and a curator deciding `metadata.yml`'s `source`/`secondary_NN` references
later needs to know that.

**When a species the source's own table logic would lead you to expect a
value for has no value given** (e.g. one species present in a floral-traits
table but silently missing from a companion counts/ratios table), don't just
skip past it -- add a row with blank `trait_value_raw` and a `notes`
explanation (quoting the source's own explanation if it gives one, e.g. "not
determined because X was unavailable", or saying plainly that the source
gives no explanation if it doesn't). This is the same principle as the
figure-only-number case: a **named, explicit gap** is worth recording, a
**silent skip** is not.

## Step 2 -- `raw/location_data.csv`, only if the source has sites

Look for a location table, a Methods section listing sites, or a map figure
with named points -- in that priority order. **If the source has no field
sites at all** (e.g. specimens examined "in the field or from herbaria" with
no site named or geocoded, a purely morphological/lab study), do not create
this file -- say so explicitly in the Step 3 report instead of writing an
empty or placeholder CSV.

When sites do exist, follow `paper-trait-location-extraction`'s Step 2 in
full (same column set, same join-key rule that `location_name` values must
be identical strings between the two files, same do-not-geocode-from-a-map-
pixel rule, same steer toward reusing a property name already used in
another dataset's `metadata.yml` `locations:` block -- `data/Funk_2016` is a
good worked example -- over inventing a new one).

## Step 3 -- report back in chat

- The full citation, and exactly which file(s) were written
  (`data/<id>/raw/trait_data.csv`, plus `raw/location_data.csv` if written).
- Row counts, and species/site counts, for a sanity check.
- Figure-only numbers not extracted (name the figure).
- Any named, explicit gaps recorded per the rule above (a species missing a
  value the paper's own table structure implies it should have).
- Values flagged as sourced from a *different* paper via a footnote, if any.
- Whether no `location_data.csv` was warranted, and why.
- A short list of the candidate (non-`config/traits.yml`) trait names used,
  so the curator can decide whether any are worth proposing as real traits.

This is reconnaissance output sitting in a real dataset folder, not a
finished dataset -- it does not touch `metadata.yml`, does not decide
`unit_in` conversions or `substitutions`, and does not align taxonomy.
Turning it into a build-ready dataset is `traits-build-add-dataset`'s job
from here (`fill-metadata-yml`, `fix-location-metadata`,
`update-substitutions-file` and `convert-raw-data-file-into-data-csv` cover
its later phases).

## Character handling

PDF text extraction mangles a small, predictable set of characters --
sanitise as you transcribe, not after. The one to never forget: degree
symbol is U+00B0 (`°`), not U+00BA (`º`).
