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
unsure).

**Specimen identifiers get their own column.** When the source gives
herbarium voucher / collection numbers (or tree tags, accession numbers),
add a `record_number` column right after `location_name` holding only the
identifiers (semicolon-separated if several) -- never bury them in `notes` or
`entity_context`. They will later map to the metadata.yml `identifiers:`
block (DarwinCore `recordNumber` for collectors' numbers), one identifier
column per holding herbarium, split in `raw/extra_R_code_<id>.R`. Also keep the source's
own collection/provenance cell verbatim in its own column (e.g.
`herb_coll_nos`): its garden/field/glasshouse codes drive a per-record
`basis_of_record` later. Seedlot numbers are not vouchers (Hallam_1970).

**Site/treatment information read from the source gets its own column, not a
`context` string.** A site's fire severity, a time-since-fire, whether a
percentage covers all sites or burnt sites only: add a named column (e.g.
`fire_severity`, `sites_included`) right after `location_name`, so data.csv can
simply be a copy of trait_data.csv. Don't write `"fire severity: high"` into
`context` and then parse it back out in `raw/extra_R_code_<id>.R`. That
script is only for genuinely derived logic (Tasker_2026).

**`entity_type` and `value_type` for numbers:** a measured value (nutrient
concentrations and other chemistry included) is never `species`, even a range
compiled across many studies in a review -- use `population` with no location,
or `metapopulation` for a named region. `value_type: raw` is only for a value
the source explicitly gives for one individual or one bulked sample; a lone
value printed in a min-max range column is `mean` (Judd_1996).

The one worth restating because it's easy to get backwards:

- **`trait_value_raw`**: the value exactly as the source states it, never
  adjusted or unit-converted.
- **`trait_value`**: the source's own descriptor term(s) in snake_case
  (`dark_red_purplish`, `cylindrical`, `outcrossing_mating_system`), for every
  categorical row including unmapped candidates; the parsed number for a
  numeric row. metadata.yml points `value:` here and maps these terms with
  `substitutions:`, so the rawer terms stay easy to remap later.
- **`trait_value_clean`**: the AusTraits `allowed_values_levels` value(s) the
  term maps to (`red_brown`, `terete`, `allogamy`), space-delimited if several;
  the parsed number for a numeric row. It is a curator's reference column, not
  the build column. Write CSVs with LF line endings.

Check `config/traits.yml` for a real trait name before assuming there isn't
one (`grep` it directly -- 500+ concepts, too big to skim). When nothing
fits, use a short plain-language candidate name (`pollen_tackiness`,
`ovary_locule_number`) and say so in `notes` --
`"Candidate trait, not in config/traits.yml (checked <today's date, e.g. 2026-09>)."`
-- exactly the phrasing already used across `data/Houston_2002/raw/trait_data.csv`.
A candidate name plus an unmapped value is exactly as valuable a row as a
mapped one; never leave a real measurement out just because it won't map.

**Name candidates the way traits.yml already names things (Dadswell_1972).**
- A statistic of an existing trait is never a new trait. "Mean of the 10 widest
  vessels" is `stem_vessel_diameter` with `value_type: maximum`, not
  `stem_vessel_diameter_largest`.
- No suffixes like `_abundance`, `_percentage`, `_height_cells`, `_test` or
  `_distinctness`. Proportions are `_fraction` (`wood_ray_biseriate_fraction`).
  A height counted in cells is `wood_ray_height`, with the unit carrying "cells".
  `wood_ray_cell_height` would mean the height of a cell. A categorical version
  of a numeric trait takes `_categorical` (`wood_axial_parenchyma_fraction_categorical`,
  like `wood_fibre_wall_thickness_categorical`).
- Otherwise use the plain IAWA/structure noun: `wood_vessel_tyloses`, `wood_fibre_pits`,
  `wood_crystals`, `wood_deposits` (with a tissue column), `wood_splinter_burning`,
  `wood_apotracheal_parenchyma` (paralleling `wood_paratracheal_parenchyma`).

**Prose species accounts need a sentence-by-sentence pass against the whole
of `config/traits.yml`**, not just the traits the curator mentioned. For
example, "dense and somewhat fleshy foliage appeared to protect the species
from low intensity fires ... Seedling recruitment ... was seen in both burnt
and unburnt locations" holds `plant_tolerance_fire` and
`post_fire_recruitment`. Grep broadly (fire, recruitment, seedbank, clonal,
storage organ, time-after-fire, tolerance) before deciding a sentence has no
trait. Include literature statements the account adopts when the study
itself has no contrary observation. When a clear mechanism has no allowed
value, propose a new value rather than a candidate trait (Saunders_2024).

**Branch/twig/stemwood tissue chemistry is a `wood_*` trait, not a `stem_*`
or new organ trait.** Nutrient concentrations in branches, twigs or stemwood
go under the matching `wood_<element>_per_dry_mass` (e.g.
`wood_N_per_dry_mass`), with `entity_measured: branch` / `twig` / `stem wood`
added to `context` (plus any size class, e.g. `twig diameter: <2 cm`) --
never `stem_*_per_dry_mass`, and never invented `branch_*`/`twig_*` traits
(Judd_1996). When a source splits woody organs into **wood vs bark**
(including root crown / tap root / lateral root), map them to `wood_*` /
`bark_*` with the organ (`root crown`, `tap root`, `primary branch`, ...) as
the `entity_measured` context -- not `root_*` traits. Dead-stem tissue goes to
`wood_dead_*` / `bark_dead_*` (Westman_1977).

**Stratum and habitat descriptors are context columns, not candidate traits.**
"Overstorey trees", "understorey shrubs", "secondary canopy species", "species
of more open habitats" go in their own columns (e.g. `canopy_position`,
`habitat_openness`) on every row of that taxon, later mapped as `contexts:`.
A germination response to a hormone or chemical that is not a germination
treatment (e.g. GA3 *inhibiting* germination) is a new value of the candidate
trait `germination_cues`, not a trait of its own (Bell_1995).

**Geophytes, cultivation and species descriptions (Reiter_2025).**
- For a geophyte, check every deciduousness trait: `leaf_phenology` (deciduous /
  summer_dormancy / withered_at_flowering), `foliage_time`, and `bud_bank_location`
  (`fleshy_underground_organ`) as well as `storage_organ`.
- A single-inflorescence geophyte's spike or scape height is also `plant_height_reproductive`.
- `reproductive_maturity` is never the time from transplanting or in vitro sowing to first
  flowering in cultivation. Nursery emergence %, flowering-plant counts and survival are
  study-specific: raw only, not candidate traits.
- Numbers from a cited species description are `entity_type: species`; keep both ends of a
  range. Use the flora colour rules: head colour only, markings dropped.
- Map a pollination record only to the most specific trait that fits
  (`pollination_vector_known` > `pollination_vector_possible` > `flower_visitor`), never two
  of them; a few older datasets break this rule.
- When the authors run an experiment and state a conclusion (e.g. pollinator exclusion,
  no seed set, so outcrossing), map that conclusion (`breeding_system: allogamy`). Peer
  review accepted it, so don't second-guess it over untested alternatives.

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
