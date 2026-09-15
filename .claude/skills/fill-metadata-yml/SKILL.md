---
name: fill-metadata-yml
description: Fill in the `unknown`/placeholder fields of a dataset's `data/<id>/metadata.yml` -- the `source` (primary/secondary_01/secondary_02... references), `contributors`, `dataset` description fields, and each entry's `entity_type`/`value_type`/`basis_of_value`/`methods` in the `traits:` block -- by matching the conventions already used across this repo's other `metadata.yml` files. Trigger phrase: the user typing "fill metadata.yml" or "fill in metadata" (ask which dataset if it isn't obvious from context), typically alongside a screenshot or pasted text of the paper's reference list. Does not touch `locations:` (see `fix-location-metadata`) or `substitutions:` (see `update-substitutions-file`) -- those are separate skills for separate sections of the same file.
---

# Fill metadata.yml

A dataset's `metadata.yml` usually starts life as a skeleton -- produced by
hand or by an earlier extraction step -- with real `data.csv` column names
already wired into `dataset:` (`taxon_name:`, `trait_name:`, `value:`,
`location_name:`) and a `traits:` entry per distinct trait already present
(one `var_in:` per unique value in the trait-name column), but everything
else still says `unknown`. This skill fills those fields in by pattern-
matching against the hundreds of already-curated `metadata.yml` files
already in `data/*/`, rather than inventing a schema from scratch.

## Step 0 -- find the file and confirm the trait list

**If `data/<id>/metadata.yml` doesn't exist yet, generate the skeleton with
`traits.build::metadata_create_template()` -- don't hand-author one by
copying another dataset's file.** That function is the schema's own source
of truth for field presence and order (it reads `get_schema()$metadata` and
writes fields in a fixed sequence), so it can't reproduce the kind of bug
hand-copying risks -- e.g. `read_metadata()` extracts `custom_R_code:` by
scanning raw lines from `  custom_R_code:` up to the next `  collection_date:`
line rather than through normal YAML parsing, so `collection_date:` *must*
immediately follow `custom_R_code:` with nothing in between, or that scan
swallows whatever field sits between them into the R code string and it
fails to `parse()`. Call it non-interactively from the repo root:

```r
traits.build::metadata_create_template(
  "<id>", skip_manual = FALSE,
  user_responses = list(
    data_is_long_format = TRUE/FALSE,
    taxon_name = "<data.csv column>", trait_name = "...", value = "...",  # long format only
    location_name = "<data.csv column>" (or NA), individual_id = NA,
    collection_date = "<a real range if known, else leave for Step 3>"
  )
)
```
(requires `data/<id>/data.csv` to already exist; `skip_manual = TRUE` skips
even this column-mapping step and leaves those fields `unknown` too, for
when you'd rather fill them by hand in Step 3.) The rest of this skill's
steps then fill in that skeleton's placeholder values -- they don't change
its field set or order.

Read the target `data/<id>/metadata.yml` and its sibling `data.csv`. Check
that every `var_in:` in the existing `traits:` block matches a distinct
value actually present in the trait-name column named in `dataset.trait_name`
-- if the skeleton predates a later edit to `data.csv` (a renamed/merged
trait column, an added trait), reconcile the list first.

## Step 1 -- source (primary / secondary_NN)

The user will usually hand you the reference list directly (pasted text, a
screenshot, or a paper PDF) rather than expect you to look it up. Map it to
`source:` entries:

- `primary:` is whichever reference the user names as primary (ask if not
  said explicitly and more than one reference exists) -- typically the main
  paper the dataset is drawn from, or the most recent/complete one when a
  paper supersedes an earlier thesis.
- Every other reference becomes `secondary_01:`, `secondary_02:`, ... in
  `data.csv` row-count order (whichever reference the most rows in the
  dataset actually cite gets the lower number) when the user hasn't said
  otherwise -- not necessarily chronological order.
- `bibtype: Article` fields: `key` (Lastname_Year, matching the dataset's own
  naming pattern -- check what other entries in this repo use for the same
  first-author-surname if the paper has been split/reused before), `year`,
  `author` (`Initial. Initial. Lastname` chained with `and`, matching the
  style already used across this repo -- grep a few existing `metadata.yml`
  `author:` fields for the exact convention rather than guessing), `title`,
  `journal`, `volume`, `number`, `pages` (`start--end`, double-dash), `doi`.
- `bibtype: Thesis` fields instead: `author`, `year`, `title`, `institution`,
  `type:` (`PhD`/`Honours`/`Masters` -- infer from wording like "Doctor of
  Philosophy"). Grep `bibtype: Thesis` in `data/*/metadata.yml` for the exact
  field set before writing one from scratch.
- If a `source_id`/equivalent column already exists in `data.csv` mapping
  each row to one of these keys (e.g. built while converting the raw file),
  cross-check it names the same keys you're about to define here.

## Step 2 -- contributors

`data_collectors:` is one entry per distinct author across the references
you just added (dedupe reused authors across primary/secondary), each with
`last_name`, `given_name`, `ORCID:` and `affiliation:`. When the source only
gave you initials (`AL Ritchie`, `KM Frick`), expanding to a full given name
from general knowledge of the field is a guess, not a fact -- verify it (see
below) before writing it in, and say so plainly in your report back and put
a `questions:` entry (Step 6) if you're still not confident, rather than
presenting it as verified. Don't guess at "Kingsley" for an initial like `KM`
without checking -- an initial can expand to a name you wouldn't predict
(e.g. `KM Frick` is Karen, not Kingsley).

**Before writing `unknown`/`.na.character` for an ORCID or affiliation,
actually check two places, in this order:**

1. **This repo first.** `grep -rn "last_name: <Surname>" data/*/metadata.yml`
   -- if this exact person already has a `metadata.yml` entry elsewhere (a
   frequent collaborator will), reuse their `ORCID:` and `affiliation:`
   verbatim rather than re-deriving them. This is the highest-confidence
   source since it's already been vetted once.
2. **The open web**, if step 1 finds nothing: the paper's own byline (often
   lists ORCID iDs and affiliations directly -- try fetching an open-access
   copy, e.g. a PMC/PubMed Central mirror, since a Wiley/Elsevier publisher
   page will usually 403 on a plain fetch), the public ORCID registry
   (`https://pub.orcid.org/v3.0/<orcid-id>/person` and `.../works` are plain
   JSON and don't need auth -- cross-check the `works` list actually includes
   the paper in question before trusting a name match, since common names
   collide), or an institutional profile page (a university research
   repository listing usually states current ORCID/affiliation directly).

Only fall back to `.na.character` (the dominant convention in this repo --
grep `ORCID:` across `data/*/metadata.yml` to confirm before using a
different placeholder) for ORCID, or `unknown` for affiliation, once both of
those come up empty -- and say in your report that you checked and found
nothing, not just that you defaulted to unknown. Leave `assistants:` and
`dataset_curators:` as the skeleton already has them, unless the user says
otherwise.

## Step 3 -- dataset section

Fill `description:`, `basis_of_record:`, `life_stage:`, `sampling_strategy:`,
`original_file:`, `notes:`. Grep a handful of `data/*/metadata.yml` files for
these exact field names first -- `basis_of_record:` and similar fields draw
from a small controlled set of strings (`field`, `lab`, `literature`,
`preserved_specimen`, `captive_cultivated`, `field_experiment`, ...) that's
cheaper to grep than to recall.

**`description:` is a one-sentence statement of the study's motivation --
what the researchers were trying to learn -- in your own words, not
verbatim, and not a summary of what data/traits happen to be included.**
"We measured leaf delta13C and delta18O in parasites and their hosts..." is
wrong -- that's what they *did* and *have data for*, not what they were
*trying to find out*. Pull the actual research question from the paper's
Introduction/Abstract (often stated near the end of the Introduction, e.g.
"we used X to make inferences about Y") and phrase it as a `to <verb> ...`
or `whether ...` sentence naming the actual question, not the dataset's
contents.

**`sampling_strategy:` is a verbatim quote from the primary manuscript's own
Methods section, not a paraphrase -- and should cover the full field-collection
protocol, not just the site/species-selection paragraph.** Fetch the paper
(an open-access mirror -- PMC is usually fetchable even when the publisher's
own page 403s) and pull every paragraph describing how the data were
collected in the field (site/species selection rationale, collection
dates, what counted as one sample/observation, per-group collection
protocol when it differs, storage/timing details like "leaf water sampling
took place between 1100 and 1600 hours") word for word, quoted, concatenated
in the order the manuscript presents them -- rather than summarising it in
your own words, and rather than stopping after the first relevant paragraph
when a later one (e.g. covering the second of two collection trips, or a
sample-storage step) is just as much a sampling protocol as the first. A
paraphrase, or a partial quote that leaves out a still-relevant protocol
paragraph, doesn't meet the bar. If the primary reference doesn't describe
the specific data in `data.csv` (e.g. a dataset merges records that actually
come from a secondary reference's methodology instead), say so explicitly
and quote from whichever reference actually describes it, naming which one
in the text (`Verbatim from <key>, section N.N <heading>: "..."`). If every
relevant reference is paywalled and unreachable, don't paraphrase from memory
or guess -- write what's known plus a `questions:` entry (Step 6) asking the
curator to supply the verbatim text if they have access. `description:` can
still be your own summary; only `sampling_strategy:` (and, by the same logic,
each trait's `methods:` in Step 4 below) needs to be verbatim.

**If `custom_R_code:` is present, `collection_date:` must be the very next
`dataset:` field, with nothing between them.** `traits.build:::read_metadata()`
extracts `custom_R_code` (which can span many lines, including lines that
look like `key: value`) with a hardcoded line-range hack: it finds the line
matching `  custom_R_code:` and the line matching `  collection_date:` and
takes everything between as the code text -- it does not use the YAML
parser's own value for this field. Reordering `dataset:` so anything
(`taxon_name:`, `location_name:`, ...) sits between them makes that scan
swallow those lines into the R code string, which then fails to `parse()`
with a confusing error pointing at unrelated trailing text -- not a hint
about which field actually moved. Keep the order `custom_R_code:` (if
present) immediately followed by `collection_date:`, then the rest.

**`custom_R_code:` can never contain an R `#` comment, and needs its apostrophes
doubled.** Two separate gotchas, both easy to hit when writing or editing this
field:
- `traits.build:::process_custom_code()` collapses *all* whitespace in the
  extracted code -- including real newlines -- down to single spaces before
  calling `parse()`. That means the multi-line code you wrote ends up as one
  long line at build time, so an R `#` comment silently swallows every mutate
  argument that followed it on the lines below, and the code fails to parse
  with an error that looks unrelated to the actual cause (e.g. "unexpected end
  of input" pointing at the last line). Put any explanation in that trait's
  `methods:` or the dataset's `notes:` instead -- never as a `#` comment inside
  `custom_R_code:`.
- The field is a single-quoted YAML scalar, where the *only* escape is
  doubling a literal `'` (`''`) -- a stray apostrophe (e.g. writing "the
  paper's units" in explanatory text you left inside the block) closes the
  string early and breaks the YAML parse with a confusing "did not find
  expected key" error several lines later. Easiest fix: don't put prose with
  apostrophes inside `custom_R_code:` at all.

`collection_date:` -- fill with the actual date/date-range stated in the
paper's Methods rather than leaving it `unknown`, formatted `yyyy-mm/yyyy-mm`
(single slash, no dashes) for a range spanning the full period of study --
this is the convention the vast majority of existing `metadata.yml` files in
this repo already use (confirmed by grepping `collection_date:` across
`data/*/metadata.yml`), e.g. a paper stating "sampling took place ... December
2001 ... and again ... February 2002" becomes `2001-12/2002-02`. A single
date can stay `yyyy-mm-dd`, or just `yyyy` if that's all the paper gives. Only
leave it `unknown` when the paper genuinely doesn't state a date -- don't
guess one. When `data.csv` has a column giving each record's own date (varies
row to row, e.g. four seasonal gas-exchange visits, or per-specimen collection
dates), set `collection_date:` to that column's name instead of a fixed range
-- `traits.build` resolves it as a column reference when it matches one (see
e.g. `Arnold_2021`'s `collection_date: Date`, `ANBG_2019`'s `collection_date:
gathering_date`).

**A per-record date is `collection_date:`, not a `temporal_context`.** Only
model something as a `category: temporal_context` in `contexts:` when it adds
meaning *beyond* the date itself -- e.g. the paper stratifies sampling into
"dry season" vs "wet season" and that classification, not the specific
date, is what the analysis actually uses. A raw column that's just a season
label standing in for a specific date/period ("September 1995", "January
1996", ...) is a date, however it's spelled -- convert it to a real
`yyyy-mm`-ish value in `data.csv` and point `collection_date:` at that column,
rather than registering it as a context.

Use `notes:` to record anything a later curator should know about how
`data.csv` was derived (disambiguated location names, derived/matched
columns, judgement calls) -- especially anything already flagged in a prior
step's chat report when converting the raw file.

**Never annotate a field's own content with what kind of field it is or where
it came from** -- e.g. don't append `(Materials and methods, verbatim)` or
`(Abstract, verbatim)` to a `sampling_strategy:`/`methods:`/`description:`
value. Whether a field is verbatim-quoted or your own summary is defined by
this skill and the database schema, not by text inside the field itself; if
provenance needs recording for a curator's benefit, say it in the chat report
or a `questions:` entry (Step 6), never inline in the value.

## Step 4 -- per-trait entity_type / value_type / basis_of_value / methods

For each `traits:` entry, fill:

- `unit_in: .na` for a categorical trait (no unit).
- `trait_name:` -- matches `var_in:` exactly for a trait that needs no
  renaming.
- `entity_type:` -- `species` for a literature-synthesis-style dataset with
  no location tie per record; `population` when each record is tied to a
  specific site/location (the common case for field observation data);
  `individual` when the record is of one physically identified plant
  (typically photograph-based visitor/vector records). Grep a few
  `metadata.yml` files that already use the same `var_in:` trait for
  precedent before picking.
- `value_type: mode` for a categorical trait recorded as a single label per
  entity (the overwhelming majority case for `flower_visitor`/
  `pollination_vector_known`/`pollination_vector_possible`-style traits).
- `basis_of_value:` -- `expert_score` for observational/scored categorical
  traits (the norm for the three pollination traits above across this repo);
  `measurement` for a directly measured quantitative trait; check existing
  usage for the same trait before deviating.
- `methods:` -- one paragraph *per trait*, describing what evidence
  distinguishes this trait's records from a sibling trait's (e.g. what
  separates a `pollination_vector_known` record from a
  `pollination_vector_possible` or `flower_visitor` record for the same raw
  dataset). Same verbatim-from-the-manuscript rule as `sampling_strategy:` in
  Step 3 applies here when the distinction is actually described in a
  reference's Methods text -- quote it rather than paraphrasing, and name
  which reference (primary or secondary_NN) it came from if it isn't the
  primary one. Only fall back to your own words, grounded in whatever
  method/evidence columns the raw data actually carries (e.g. a `METHOD` and
  `POLLINATION EVIDENCE` column pair -- group the distinct (method, evidence)
  combinations per trait first, `groupby`-style, same idea as Step 1 of
  `convert-raw-data-file-into-data-csv`), when no reference you can access
  actually describes the distinction in its own words -- and say so plainly
  (chat report + a `questions:` entry) rather than presenting the paraphrase
  as sourced.

**Trait-value matching against `allowed_values_levels` is case-insensitive
at build time** (`traits.build`'s actual value-matching, not just the
substitutions-file curation check in `update-substitutions-file`) -- don't
add a `substitutions:` entry just to fix letter-casing (e.g. raw `C3`/`C4`
mapping to allowed values `c3`/`c4` needs no substitution at all; the raw
value already matches as-is). Only add a substitution for a genuine
spelling, synonym, or compound-value difference -- check whether the raw
value already matches case-insensitively before writing one.

**Replicate observations at the same (taxon, location) sharing no other
distinguishing column** (a real, user-confirmed pattern -- e.g. several rows
recording different flower visitors seen at the same site for the same
species) need a counter before they can pivot or before duplicate rows would
otherwise look like meaningless repeats. Only add this when the user
explicitly asks for it and names the grouping columns -- never proactively,
since a repeat can just as easily mean "the same value was read in twice" as
"a genuine second observation," and that's a judgement call for the curator,
not a default to assume. When asked, use the repo's own helper rather than
hand-rolling it:
```r
custom_R_code: '
  data |>
    generate_observation_numbers(<grouping column(s), e.g. species, location_name>)
'
```
(`generate_observation_numbers()`, defined in `R/custom_R_code.R`, adds a
`observation_number` column via `dplyr::row_number()` within the grouping
columns given). Wire the resulting column in as its own context, matching
the pattern used across `Ritchie_2021`/`Gosper_1999`/`Davis_2026`/
`Cornwell_2018`:
```yaml
contexts:
- context_property: replicate observations
  category: temporal_context
  var_in: observation_number
```

**A raw source that records the same categorical identity across several
parallel columns of differing taxonomic/conceptual precision** (e.g. an
animal-visitor table with a species binomial, genus, family, order and class
all as separate columns, only some of which will resolve to a trait's
controlled vocabulary) needs two things, not just a single best-guess
`var_in:` column:
1. **Two derived columns in `data.csv`** documenting, per row, which of the
   source columns supplied the most specific value that actually resolves to
   the trait's `allowed_values_levels` (by name, e.g. `Pollinator match
   column`) and what that raw value was (e.g. `Pollinator match value`) --
   `trait_name:` maps in from the second column, and the first exists purely
   so a future curator can trace any mapped value straight back to which
   source field produced it, without re-deriving the matching logic. Only
   fall back to a coarser column (family, then order, then class) when
   nothing at a more specific level resolves -- check each candidate value
   against the trait's actual `allowed_values_levels`/synonyms
   programmatically (see `config/traits.yml`'s `(Synonyms, ...)` parenthetical
   on each value) rather than assuming a column's typical rank order always
   wins; a real taxonomic fact (e.g. a genus belonging to a named tribe, or a
   family that's entirely one vocabulary term) may need a new synonym added to
   `config/traits.yml` rather than a per-dataset substitution -- see
   `update-substitutions-file` for that distinction.
2. **A separate `<subject> taxon name` entity_context** (matching
   `flower visitor taxon name` in `Ritchie_2021`/`Gosper_1999`/`Davis_2026`)
   recording the single most specific name documented for that record
   *regardless* of whether it resolves to a controlled value -- so a species
   identified all the way down (e.g. a woodswallow species with no
   family-specific pollinator-vector vocabulary term, mapped to the coarser
   `bird`) still has its real identity preserved for anyone who wants it,
   rather than being silently flattened to whatever generic value the trait
   itself needed.

## Step 5 -- validate

Parse the finished file with `yaml.safe_load` as a first pass, but don't
stop there -- `yaml.safe_load` will happily "succeed" on a file that's
actually broken from `traits.build`'s own perspective, because
`read_metadata()` extracts some fields (`custom_R_code:`, see above) by
scanning raw lines rather than through normal YAML parsing. When R and the
`traits.build` package are available, validate with the real function
instead/as well:
```r
m <- traits.build:::read_metadata("data/<id>/metadata.yml")
```
and confirm `m$source$primary$key`, `length(m$traits)`, `length(m$contexts)`
and `length(m$locations[[1]])` (if already filled) look right. If the
dataset has a `custom_R_code:`, also run it through
`traits.build:::process_custom_code()` against the real `data.csv` (after
`source("R/custom_R_code.R")` so helpers like `generate_observation_numbers`/
`replace_duplicates_with_NA` are in scope) to confirm it actually parses and
produces the expected columns -- this is the check that has caught every
real bug in this skill's own worked examples; `yaml.safe_load` alone missed
all of them.

## Step 6 -- report back / flag uncertainty

Don't silently present a guess as a fact. Anything inferred rather than
sourced directly from what the user gave you -- an expanded author given
name, a taxonomically-uncertain `entity_type` choice, an assumed
`institution` for a thesis -- goes in the chat report *and* as a
`questions:` entry (`question1:`, `question2:`, ... -- grep existing
`questions:` blocks for the exact style) in the file itself, so a future
curator reviewing the dataset sees it even without reading the chat log.
