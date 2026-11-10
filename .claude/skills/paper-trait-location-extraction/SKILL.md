---
name: paper-trait-location-extraction
description: Read a source PDF (a journal article, report, or thesis) for a candidate new dataset and pull out two audit-able CSVs -- a trait_data.csv of every measured/described trait value (from data tables AND from results described in prose), each row carrying both its raw value and a best-effort match against config/traits.yml's controlled trait/entity_type/value_type vocabulary, and a location_data.csv of every site/location mentioned (coordinates, soil, geology, climate, description). Use this whenever a curator hands over a paper PDF and wants to know "what's actually in this paper" before deciding to add it, or wants a head start on the traits-build-add-dataset skill's Phase 1 (read the source) and Phase 3 (locations) -- even if they just say "pull the trait and location data out of this PDF" or "make me a table of what's in this paper" without naming either skill.
---

# Paper PDF -> trait table + location table

This is the **reconnaissance step**, not the dataset-building step. It answers
"what quantitative/categorical information does this paper actually contain,
and where was it measured" by producing two plain CSVs a curator can skim in
a spreadsheet -- going as far as proposing a `trait_name`/`entity_type`/
`value_type` match against `config/traits.yml` for each row, but never
silently discarding a row just because no match exists. It does not create
`data/<dataset_id>/` and does not write `metadata.yml` -- turning a reviewed
`trait_data.csv` into an actual dataset (deciding `unit_in` conversions,
`substitutions`, taxonomic alignment, and everything else Phase 1/3 onward
covers) is `traits.build`'s own `traits-build-add-dataset` skill; read that
skill's `references/metadata-fields.md` before the two CSVs here become a
real dataset.

Two files, always, in a **new folder named `<Surname_Year>`** (the paper's
first author and publication year -- e.g. `Ladd_2023`), created *outside*
`data/` (next to the source PDF, e.g. `export/<Surname_Year>/`, or wherever
the curator's working files live). Placing it outside `data/` matters: this
folder is pre-review scratch, not a finished dataset, and must not be
mistaken for one.

- `trait_data.csv`
- `location_data.csv`

## Step 0 -- read the whole paper first

Get the page count, then read every page (the `Read` tool caps at 20
pages/call -- most journal articles fit in one call). Do not jump straight to
a "Results" heading: numeric trait values worth recording turn up in the
Abstract, Methods (e.g. a species-comparison table), Results tables, Results
prose, figure captions, and even Discussion (papers often restate a key
number when interpreting it). Skim the References/Supplementary note at the
end too -- "Data available on request" or a Supplementary Table reference
tells you *why* some numbers are thin and gives the curator a concrete next
step rather than a silent gap.

Note the full citation (authors, year, title, journal, volume, pages, DOI) as
you go -- you'll want it for the report back to the curator, and it's what
determines `<Surname_Year>`. Check `data/` for a same-named folder first (or
a compilation that already cites this paper as a `secondary_NN` source, the
way `data/Ladd_2026/metadata.yml` cites `Ladd_2023`) -- if the paper is
already partly captured elsewhere, say so; it doesn't mean skip this
extraction, since a compilation typically only pulled a handful of traits
out of a much richer source.

## Step 1 -- trait_data.csv

**What counts as a trait worth a row**: any measured or described property of
the study organism(s) or their interactions -- morphology, phenology,
reproductive biology, breeding-system/mating-trial outcomes, visitor/
predator/pollinator interactions, counts, proportions, rates. Comes from
three kinds of source, and all three belong in the same file:

1. **A real data table** (e.g. a "Table 1" of species characteristics) --
   transcribe every cell.
2. **Results stated in prose** -- "ranging from 57-79%", "around five seeds
   per fruit", "significantly less than... (0.16)" -- these are exact
   numbers even though there's no table row for them; don't skip them just
   because they're sentence-shaped. A stated range becomes two rows (`min`/
   `max`), matching how `species-profile-traits` handles ranges -- never
   collapse to a midpoint.
3. **Figure captions/axis labels that state a number directly** -- e.g. "the
   maximum number of beetles per flower... was 0.41 per flower in 2021" is a
   number from prose *about* a figure, which is fine; the distinction that
   matters is Step 1a below.

Skip pure statistical-method bookkeeping (which GLMM family, which R
package, model diagnostics) -- that's methods, not trait data, and belongs in
the `traits-build-add-dataset` skill's `methods`/`sampling_strategy` fields
later, not in this table.

### Step 1a -- a bar/line chart is not a data table

If a number exists **only** as a bar height or a point position on a chart
-- no printed data label, no value repeated anywhere in the text -- do not
estimate it by eye and write it down as if it were stated. Eyeballing a
pixel position is exactly the kind of invented-number risk the rest of the
`traits.build` pipeline explicitly forbids for methods text, and it applies
here too. Instead:

- Leave the row out of `trait_data.csv` entirely, or add it with
  `trait_value_raw` blank and a `notes` entry naming the figure (e.g.
  `"value only shown graphically in Fig. 5A, not stated numerically in
  text"`).
- Say so in the chat report at the end, under "figure-only numbers not
  extracted" -- this is genuinely useful information (it tells the curator
  whether it's worth emailing the authors for the underlying data, which
  many papers' "Data available on request" line explicitly invites).

A number stated in a caption or in prose *that happens to describe* a figure
is fine to record (Step 1, point 3) -- the line is "is there a printed
value anywhere" vs "would I have to measure pixels".

### `trait_data.csv` columns

```
taxon_name,entity_type,entity_context,location_code,trait,trait_value_raw,trait_value,value_type,unit,error_type,error_value,n,context,source_section,notes
```

- **`taxon_name`**: the scientific name as given. Blank only for a trait that
  genuinely isn't species-specific (rare -- almost everything in a trait
  table is about a taxon).
- **`entity_type`**: `species` / `population` / `individual`, same three
  values AusTraits itself uses. A fixed, undifferentiated characteristic in
  a species-comparison table (growth form, anther dehiscence, a breeding-
  system conclusion) is `species`. A number that came from measuring a
  sample -- it has an `n`, a site, a year, a treatment -- is `population`,
  even when the source doesn't use that word. Use `individual` only when the
  source is explicit that one value belongs to one named/tagged plant, not a
  sample of them (rare; most per-plant numbers in a paper are still
  presented as a population-level mean/proportion/count).
- **`location_code`**: see Step 2 for why this is often `location_code`
  rather than `location_name`, and why the values here have to exactly match
  the other file's. Blank for a general species-level trait not broken out
  by site.
- **`trait`**: the real AusTraits `trait_name` when you can confidently
  identify one that matches the concept being measured -- check
  `config/traits.yml` (`grep`/`yaml.safe_load` it directly; it's 500+
  concepts, too big to skim by eye) before assuming there's no match. When
  nothing fits, use a short, plain-language candidate name instead
  (`beetles_per_flower`, `proportion_fruit_with_larva`) -- **don't leave a
  trait undocumented just because it won't map**; a candidate name plus an
  unmapped `trait_value` is exactly as valuable a row as a mapped one, since
  it's a real thing the source measured and a future trait proposal has to
  start somewhere. Reuse the same string across rows measuring the same
  thing so they group when sorted.
- **A single source phrase describing two distinct concepts is two rows,
  two `trait` names, not one compound trait.** ("6 equal, straight" stamens
  is a *count* and a *form* -- split into a `flower_fertile_stamens_count`
  row and a `stamen_form` row, not one `stamen_number_and_form` row.) Look
  for whether one of the two halves matches a real AusTraits trait even when
  the other doesn't; splitting is what makes that possible; a merged
  compound name can't map to anything.
- **`trait_value_raw`**: the value exactly as the source states it --
  **never adjusted, corrected, or unit-converted**, even when you're
  confident the source is wrong or the database would want a different
  unit. This is the permanent, checkable record of what the paper actually
  said.
- **`trait_value`**: **categorical traits only.** The `trait`'s own
  `allowed_values_levels` term(s) that `trait_value_raw` maps onto -- fill
  it in whenever a defensible match exists (even a medium-confidence one --
  say so in `notes`), leave it **blank** when no real trait exists for this
  row, or a real trait exists but no allowed value fits. Never populate this
  for a numeric trait -- a numeric value doesn't get "matched" to anything,
  it's just retained (see `unit`, below). When more than one value applies
  simultaneously (a growth-form table cell reading "geophyte, climber",
  both terms real values of `plant_growth_form`), space-delimit them in
  `trait_value` -- `geophyte climber` -- while `trait_value_raw` keeps the
  source's own punctuation (`geophyte, climber`) untouched. This is the
  same simultaneous-value convention `scraped-species-traits` uses.
- **A trait whose real-world values are far more specific than its
  AusTraits allowed values** (`flower_visitor`'s allowed values are generic
  -- `bee`, `beetle`, `honeybee` -- while the source names an actual genus
  or species) still gets its best-fit generic `trait_value`, plus an
  **`entity_context`** column carrying the raw name(s) the source actually
  gave (`"Notobrachypterus sp."`, `"Leioproctus sp. (2 species); Lasioglossum
  sp."`) -- so the specific identity survives the mapping instead of being
  silently flattened to "beetle"/"bee".
- **`value_type`**: for a categorical row (mapped or not), this is always
  `mode` -- AusTraits' own term for a categorical value, whether or not this
  row actually found a `trait_value` match. For a numeric row, use
  AusTraits' real `value_type` vocabulary as seen across this repo's
  `metadata.yml`s: `raw` (a single stated number), `mean`, `median`,
  `minimum`, `maximum`, `expert_score` -- not ad hoc labels like
  `proportion`/`count`/`descriptive`. A stated range's two rows are
  `minimum`/`maximum`, never a `range` value_type. A row with no value at
  all (Step 1a) leaves `value_type` blank too.
- **`unit`**: as given in the source (`cm`, `flowers/m2`, `beetles per 1000
  flowers`...), **never converted** -- numeric traits are always retained in
  their original units here; unit conversion is the add-dataset skill's
  `unit_in`/`unit_conversions.csv` concern, not this one's. Blank for
  categorical rows.
- **`error_type` / `error_value`**: when the source reports a mean alongside
  an uncertainty measure (`± 0.04 (CI)`, `±10.0 (CI)`, an SD, an SE), keep it
  on the *same row* as the mean rather than inventing a fake `value_type`
  for it -- `error_type: CI_95`, `error_value: 0.04`. AusTraits' own
  `value_type` vocabulary has no slot for a confidence interval, so don't
  try to force one in there.
- **`n`**: sample size, if the source states one for that specific value.
  Blank if not given -- don't infer it from a nearby, differently-scoped `n`.
- **`context`**: whatever qualifies the value beyond taxon/location/trait --
  a treatment name (`autogamy treatment`, `pollen supplementation`), a year,
  a life stage, "pooled across all sites" -- anything that would otherwise
  make two genuinely different values look like duplicates of each other.
- **`source_section`**: where it came from (`Table 1, p.3`, `Results p.5`,
  `Fig. 9 caption, p.8`) -- makes every row checkable against the PDF without
  re-reading the whole thing.
- **`notes`**: verbatim short quote for anything non-obvious, a match-
  confidence caveat for a `trait_value` that's a judgment call, the
  figure-only caveat from Step 1a, or a flag like "n unclear whether per
  species or combined -- ask contributor".

## Step 2 -- location_data.csv

Four places to look, same priority order as the add-dataset skill's Phase 3:
a location table in the paper, a Methods section listing sites, a map figure
with named points, or (last resort) nothing -- in which case say so rather
than inventing a site.

**One row per site**, columns starting with the ones that (almost) every
dataset has, then whatever else the source actually reports as its own
column -- don't force every dataset into the same fixed property list, but
do reuse a property name already used by other datasets in this repo's
`data/*/metadata.yml` `locations:` blocks where one genuinely fits (check a
few `metadata.yml`s' `locations:` block for the closest analog before
inventing a new header -- `data/Funk_2016/metadata.yml` is a good worked
example of the shape this file's columns should end up in) rather than
inventing a synonym for a concept that already has a name in this repo:

```
location_name,location_code,latitude (deg),longitude (deg),description,locality,<...other reported properties>
```

- **`location_name` is the join key back to `trait_data.csv`, and the two
  files' values for it must be identical strings** -- if a trait row is
  tagged with a site, that exact string has to appear as a `location_name`
  row in `location_data.csv`, or the two tables can't be joined. This is the
  single most important rule in this file: check it explicitly before
  calling either file done, not just when writing them.
- **A source that uses two identifiers per site** (a spelled-out name in
  Methods, e.g. "Nicholson Road", plus a short code used in every figure and
  results sentence, e.g. "NR") gets **both** captured, as two separate
  columns -- `location_name` for the full name, `location_code` for the
  short form. Whichever one the source's own results actually key on
  (almost always the short code, since that's what tables/figures/prose
  cite results by) is what goes in `trait_data.csv`'s location column too --
  rename that column `location_code` rather than `location_name` when this
  happens, so the shared column name states which identifier is actually
  doing the joining, and the values still line up exactly per the rule
  above.
- **Coordinates actually printed in the source** (decimal degrees or DMS) --
  transcribe, converting DMS to decimal degrees and keeping the original
  string in `notes` so the conversion is checkable (see the add-dataset
  skill's `references/curator-recipes.md` for the exact DMS idiom).
- **Coordinates shown only as a point on a map with no printed lat/long**
  (a UTM/easting-northing grid, a scale bar, a dot on a regional map) --
  this is the location equivalent of Step 1a: do not estimate degrees from a
  pixel position. Leave `latitude (deg)`/`longitude (deg)` blank, and if the
  site is a named, identifiable real-world place (a named reserve, a named
  hill), say so in `notes` as something the curator could geocode
  themselves -- clearly labelled as *not from the source*, distinct from a
  value the source actually stated.
- **`description` is a short phrase, vegetation type or the single most
  defining site characteristic** -- `"urban bushland reserve"`, `"Banksia
  woodland"`, `"granite outcrop"` (see `data/Funk_2016/metadata.yml`'s
  `locations:` block for the length/register this should match). It is
  **not** the place for positional detail, reserve/site-number names, or
  distances -- that belongs in `locality` (e.g. `"Bush Forever site 456,
  Perth, Western Australia"`, `"~5 km southwest of Nicholson Road (NR)"`).
  Splitting these two apart matters: `description` is what a reader
  compares across sites/datasets at a glance, `locality` is what lets them
  find the place.
- **A named geological formation or land-system name** (Bassendean,
  Spearwood, or any other named unit) goes under
  `geology (stratigraphic map unit)` -- the property name already used for
  this across the repo (`data/Funk_2016`, `data/Grootemaat_2017_2`, ...) --
  rather than a bespoke `land_system` column. Where the source names a
  system but not a rock/sediment type outright, use the closest one-or-two-
  word rendering it does give (e.g. "younger soils over limestone" ->
  `Spearwood limestone`).
- **This is a separate column from `soil_type`, not a replacement for it.**
  `geology (stratigraphic map unit)` names the formation; `soil_type` keeps
  the source's own description of the soil itself (texture, depth, parent
  material as described in prose) -- e.g. `geology (stratigraphic map
  unit): Bassendean sand` alongside `soil_type: leached siliceous sands`,
  or `geology: granite` alongside `soil_type: granite outcrop, very shallow
  soil`. Transcribe both when the source gives both; don't fold one into
  the other's `notes` just because they overlap in wording.
- **A numeric property (rainfall, temperature, elevation, ...) is the bare
  number, nothing else** -- exactly the same rule as `trait_data.csv`'s
  `trait_value_raw`/`unit` split (Step 1), applied here:
  `precipitation, MAP (mm): 816`,
  never `816 mm (near the coast)`. Any qualifier, caveat, or scope note
  ("this is a single shared value for three sites, not measured per-site",
  "source says 'approximately'") goes in that row's `notes`, not glued into
  the number.
- **Every other reported property** (elevation, distance from another site,
  vegetation type not already captured in `description`) becomes its own
  column, transcribed as given. A genuinely new property name is fine --
  flag it in the chat report as new vocabulary, the same as the add-dataset
  skill's Phase 3 asks for.

## Step 3 -- report back in chat

After writing both files, tell the curator:

- the full citation and where the two CSVs landed;
- row/site counts for a sanity check;
- **figure-only numbers not extracted** (Step 1a) -- name the figure and
  what it would take to get the real value (author request, supplementary
  data);
- **sites without source-stated coordinates** (Step 2) -- named but
  ungeocoded;
- anything the source explicitly says is available elsewhere and wasn't
  accessible (a "Data available on request" line, a Supplementary Table
  referenced but not attached) -- this is exactly the kind of gap the
  add-dataset skill's `questions:` block is for later, so surface it now
  rather than losing it.

## Character handling

PDF text extraction mangles a small, predictable set of characters --
sanitise as you transcribe, not after. Full table and reasoning in
`traits-build-add-dataset`'s `references/troubleshooting.md`; the one to
never forget: degree symbol is U+00B0 (`°`), not U+00BA (`º`).
