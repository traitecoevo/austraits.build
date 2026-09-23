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
- **For a dataset compiled from many original sources (a literature/database
  compilation, not one paper), use the real `dataset.source_id:` schema
  field** -- don't invent your own per-row citation-tracking column/scheme
  instead (e.g. don't label rows `secondary_01`/`secondary_02`; that's an
  internal YAML key, not a citation format, and traits.build won't recognise
  it as one). See Cornwell_2018, Kew_2019_3/_4, Davis_2026, Choat_2012 for
  the working pattern: `dataset: source_id: source_id` points at a data.csv
  column (typically built in `custom_R_code`, e.g. via a `dplyr::recode()`/
  named-vector lookup or `paste0(ref_author, "_", ref_year)`) holding an
  `author_year`-style value per row -- disambiguated with a trailing `_1`/
  `_2`/... where the same first author has more than one compiled source in
  the same year (see Kew_2019_3's worked `ifelse(stringr::str_detect(...))`
  chain, or build the disambiguation as a lookup table if there are too many
  collisions to hand-write). For sources promoted to a formal `secondary_NN`
  entry (see the >~50-observations-worth-a-citation judgement call above),
  `source_id` must exactly equal that reference's `key:` (e.g. `Bernhardt_1987`)
  so traits.build can cross-reference them; for every other source it should
  still be a real `author_year` value if derivable, not the raw internal
  Dataset_ID -- that keeps every record traceable back to its original source
  even without a formal citation entry.

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

**`custom_R_code:` can never contain an R `#` comment, needs its apostrophes
doubled, and needs an explicit `;` between top-level statements written on
separate lines.** Three separate gotchas, all stemming from the same
mechanism and all easy to hit when writing or editing this field:
- `traits.build:::process_custom_code()` collapses *all* whitespace in the
  extracted code -- including real newlines -- down to single spaces before
  calling `parse()`. That means the multi-line code you wrote ends up as one
  long line at build time, so an R `#` comment silently swallows every mutate
  argument that followed it on the lines below, and the code fails to parse
  with an error that looks unrelated to the actual cause (e.g. "unexpected end
  of input" pointing at the last line). Put any explanation in that trait's
  `methods:` or the dataset's `notes:` instead -- never as a `#` comment inside
  `custom_R_code:`.
- The same newline-collapsing means that **two separate top-level statements
  written on their own lines** (e.g. defining a lookup vector/helper on one
  line, then piping `data |> ...` on the next) **silently merge onto one line
  with only a space between them once collapsed** -- which is invalid R (you
  can't write `x <- 1 y <- 2` without a `;` or real newline between them).
  The parse error this produces ("unexpected symbol") points at a position
  deep in the flattened one-line text with no obvious connection to the
  actual cause. If `custom_R_code:` needs to define something before the
  `data |> ...` pipe (e.g. a named lookup vector for a `dplyr::recode()`/
  `case_when()`-style join), end that definition with an explicit `;` before
  the pipe starts.
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

**This means no "Verbatim from ... Methods > 'Section heading':" preamble, no
section-heading labels stitched into the quote, and no inline caveat sentence
appended after it.** A `methods:`/`sampling_strategy:` value that needs to be
verbatim gets the manuscript's own sentences and nothing else -- paste the
text, don't narrate that you pasted it. Concretely, this:

```
methods: 'Verbatim from Smith et al. (2023), Methods > Data collection >
  "Gas exchange": "During the first two harvests, we measured Aarea,
  Amass, and gs..." NB this data-file description labels Jmax.mass as a
  carboxylation rate, which is conventionally Vcmax -- possibly a
  labelling swap; flagged, not corrected, in questions below.'
```

is wrong twice over: the `Verbatim from ... "Gas exchange":` preamble
announces the field's own provenance inline (forbidden above), and the `NB
...` sentence is a curator-facing caveat sitting in a field a database
consumer will read as the measurement method. The fix moves the caveat to a
`questions:` entry and leaves the field as just the manuscript's sentences:

```
methods: During the first two harvests, we measured Aarea, Amass, and gs
  until they stabilized on one fully expanded leaf on each focal plant
  using an LI-6400XT system (LI-COR, Lincoln, NE)...
questions:
  question4: The source data file's trait.description for Jmax.mass/
    Vcmax.mass appear swapped (Jmax.mass is described as a carboxylation
    rate, conventionally Vcmax) -- worth confirming which data.csv column
    is really Jmax vs Vcmax before trusting it.
```

`notes:` and `questions:` are the *only* places curator-facing commentary
belongs -- not `description:`, not `sampling_strategy:`, not any trait's
`methods:`. This repo's `metadata.yml` is a professional, published data
artifact, not a running commentary on how it was assembled; a field a
database consumer reads as scientific content should contain only that
content.

## Step 4 -- per-trait entity_type / value_type / basis_of_value / methods

For each `traits:` entry, fill:

- `unit_in:` -- for a numeric trait, **the raw/source unit, as the source
  reports it -- never a unit you've already converted the values to.** Map
  `var_in:` straight at the raw data.csv column and let `traits.build`'s own
  conversion pipeline (`config/unit_conversions.csv`, a lookup table of
  `unit_from,unit_to,function` rows -- `function` can be a reciprocal, e.g.
  `cm2/g,g/m2,10000/x`, not just a linear scale) do the conversion, rather
  than pre-computing the target-unit value yourself in `custom_R_code` and
  then declaring `unit_in:` as the *already-converted* unit. **If the build
  fails with "Missing unit conversion," the fix is almost always to add the
  missing row to `config/unit_conversions.csv`** (it's a shared, repo-wide
  resource -- adding `um/mm2,mm/mm2,x*0.001` there benefits every future
  dataset reporting vein density in um/mm2, not just this one), not to work
  around it privately in this dataset's `custom_R_code`. Reserve
  `custom_R_code` value transforms for things unit conversion genuinely can't
  express (e.g. undoing an analysis-scale transform the source itself applied,
  like a log/sqrt back-transform) -- a straight unit conversion, including a
  reciprocal, belongs in the shared table. `unit_in: .na` for a categorical
  trait (no unit).
- `trait_name:` -- matches `var_in:` exactly for a trait that needs no
  renaming.
- `entity_type:` -- **`species` is only correct for a value drawn from a
  species-level taxonomic description (a flora, an expert score) or a
  categorical trait that never varies within a species (a fixed
  characteristic).** Any numeric trait value from an experiment or field
  sampling -- a measurement on a real plant at a real time/place, even after
  averaging replicate individuals into a taxon mean -- is `population` (the
  common case: each record ties to a specific site/accession/common-garden,
  even a single-site study) or `individual` (the record is of one physically
  identified plant, typically photograph-based visitor/vector records).
  Calling population-level field data `species` just because there's only
  one site, or because the sampling design feels species-centric, is a real
  mistake with a **silent, non-cosmetic build consequence**: `traits.build`'s
  `dataset_process()` sets `location_id` to `NA` for every row with
  `entity_type == "species"`, by design (species-level values are treated as
  location-independent). The `locations:` block can be completely correct
  and the build can succeed with zero errors or warnings while every trait
  value's location link is silently dropped -- this has happened twice in
  this skill's own worked examples, caught only by explicitly checking
  `unique(result$traits$location_id)` after a build, never by the build
  itself. Grep a few `metadata.yml` files that already use the same `var_in:`
  trait for precedent, but if in doubt, ask "did a real measurement happen
  here" -- if yes, it's `population`/`individual`, not `species`.
- `value_type: mode` for a categorical trait recorded as a single label per
  entity (the overwhelming majority case for `flower_visitor`/
  `pollination_vector_known`/`pollination_vector_possible`-style traits).
- `basis_of_value:` -- `expert_score` for observational/scored categorical
  traits (the norm for the three pollination traits above across this repo);
  `measurement` for a directly measured quantitative trait; check existing
  usage for the same trait before deviating.
- `replicates:` -- set this **per trait entry**, not as a dataset-level
  column mapping, unless the dataset genuinely has one comprehensive,
  populated column of real per-row sample sizes backing every trait (rare
  for a small manually-extracted single-paper dataset). Every categorical
  trait (`value_type: mode`) gets `replicates: .na`, even when the raw data
  does carry a real per-row count (e.g. how many individuals were examined
  for one `flower_visitor` sighting) -- that number belongs in
  `measurement_remarks` (e.g. an `n=X` piece of a `custom_R_code`
  concatenation), not in `replicates`, which is for how many samples
  underlie one reported numeric *value*, not how many individuals were
  involved in an observation.
  **The correct value follows directly from `entity_type:` above, per the
  real schema text (`schema$austraits$elements$traits$elements$replicates`):**
  `entity_type: species` (a taxonomic-description/invariant-categorical fact)
  gets `replicates: .na`. `entity_type: individual` gets `replicates: 1` (one
  organism, one measurement) -- not a stated sample size, since there's only
  ever one. `entity_type: population` must **never** be `.na` -- it's a
  number (`replicates: 20`), a range (`replicates: 2-3`), a column reference
  when a real per-row count backs every value (`replicates: n_column`), or
  the literal string `replicates: unknown` when the source genuinely doesn't
  state a count. Calling something `species` specifically to justify a lazy
  `.na` is the same mistake as above, just approached from the `replicates:`
  side instead of the `entity_type:` side.
  **Never write an unquoted `replicates: n` (or any bare single-letter/
  `yes`/`no`-like value) as a column-reference** -- R's `yaml` package
  (what `read_metadata()` uses) parses bare `n`/`no`/`y`/`yes` (any case) as
  a *boolean*, so `replicates: n` silently becomes `replicates: FALSE` for
  every row, not a lookup of a column named `n`. This passes `dataset_test()`
  and the build without error; it just produces wrong data. Quoting it
  (`replicates: 'n'`) works, but the sturdier fix is to **rename the raw
  data.csv column itself** away from the bare `n`/`y`/`yes`/`no` (e.g. `n` ->
  `replicates`) so the reference (`replicates: replicates`) is never a
  landmine in the first place -- quoting only works if you remember to do it
  at every single reference to that column, everywhere in the file.
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

**Never add a new synonym to a shared `config/traits.yml` value as a
shortcut for resolving one dataset's raw spelling -- that belongs in a
per-dataset substitution, almost always.** A real mistake made and corrected
in this repo's history: several raw family/genus names from one dataset's
animal-visitor table (`Apidae`, `Muridae`, `Pteropodidae`, `Amegilla`,
`Braunsapis`/`Exoneura`) got added as new `(Synonyms, ...)` entries on
`bee`/`rodent`/`bat`/`anthophorini`/`allodapini` directly in `config/traits.yml`,
reasoning "this taxon genuinely belongs to that broader group" -- but that
reasoning proves too much: nearly *any* family is a child of *some* allowed
value's broader clade, and admitting every one would make the shared
vocabulary balloon with taxon names that are really just one dataset's
substitution. The correct test for a **new** global synonym is much
narrower than "is this taxonomically true" -- only add one when:
- it's a genuine **exact synonym**, the Latin name *for the same concept*
  the English value already names (`marsupial` / `Marsupialia`,
  `rodent` / `Rodentia` -- not `Muridae`, which is one family *within*
  Rodentia, a different, narrower concept even though every member is
  technically a rodent), or
- the value's own `config/traits.yml` description **already explicitly
  names that family/genus** (`carpenter_bee`'s description already says
  "genus *Xylocopa*", `stingless_bee`'s already says "genera *Tetragonula*
  and *Austroplebeia*" -- so listing those as synonyms documents what the
  definition already commits to, it doesn't extend it).

Anything else -- a raw value that's merely *a kind of* the broader group,
however confidently identified -- is a per-dataset substitution (this
dataset's `substitutions:` block or `substitutions.csv`), never a
`config/traits.yml` edit. This matters more for the pollination-interaction
traits (`pollination_vector_known`/`_possible`, `flower_visitor`) than most:
their definitions are still under active development ahead of being
contributed to APD, where AusTraits' trait vocabulary is formally
versioned and cited -- once merged there, a value's allowed synonyms are
far harder to change, so an addition that seemed harmless as a shortcut
today is a much bigger deal once it's upstream. When genuinely unsure
whether something clears the bar above, default to the substitution and
flag the judgment call to the curator rather than editing the shared file.

**Replicate observations at the same (taxon, location) sharing no other
distinguishing column** (a real, user-confirmed pattern -- e.g. several rows
recording different flower visitors seen at the same site for the same
species) need a counter before they can pivot or before duplicate rows would
otherwise look like meaningless repeats. **Never add `generate_observation_numbers`
proactively -- only when the user explicitly asks for it and names the
grouping columns.** This is not a minor style preference: reaching for it as
a default, pre-emptive step is a *known, repeated mistake* that papers over
other, more serious errors instead of surfacing them. Two concrete ways it
does this:
- **It masks a missing or wrong `location_name` mapping.** What looks like
  "the same taxon recorded twice at the same site" is often actually two
  *different* sites that got collapsed to one because the location column
  wasn't identified/cleaned correctly. Silently adding an observation counter
  makes the rows pivot cleanly, but hides the real bug -- the fix is to find
  and fix the location mapping, not to launder the duplication away.
- **It masks an `entity_type` that should be `individual`, not
  `population`.** If every row genuinely is a distinct individual-level
  observation (e.g. one visitor-insect sighting, one tagged plant), the
  "repeats" aren't repeats at all -- they're real data at a finer grain than
  `population`. Adding an observation counter to force a population-level
  pivot is choosing the wrong entity model instead of reconsidering it.
A repeat can just as easily mean "the same value was read in twice" as "a
genuine second observation, correctly modelled at population level" -- and
that distinction is a judgement call for the curator, not a default to
assume. So: when you notice rows that look like duplicates, stop and flag it
to the user (mention the possible causes above) instead of reaching for this
helper on your own initiative. When the user does ask for it, confirm the
grouping columns with them, then use the repo's own helper rather than
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
1. **Two derived columns, created via `custom_R_code` -- never by hand-editing
   `data.csv`** (`data.csv` should stay a byte-identical, traceable copy of
   the raw/source file; see the `custom_R_code` gotchas above) documenting,
   per row, which of the source columns supplied the most specific value that
   actually resolves to the trait's `allowed_values_levels` (by name, e.g.
   `Pollinator match column`) and what that raw value was (e.g. `Pollinator
   match value`) -- `trait_name:` maps in from the second column, and the
   first exists purely so a future curator can trace any mapped value
   straight back to which source field produced it, without re-deriving the
   matching logic. A `dplyr::case_when()` keyed on genus/family/order (most
   specific first, falling back to coarser ranks) inside `custom_R_code` is
   the way to build this -- remember `custom_R_code` reads `data.csv` via
   `read_csv_char()`, so every column is character type even for what look
   like 0/1 flag columns (compare `pol == "1"`, not `pol == 1`). Only fall
   back to a coarser column (family, then order, then class) when nothing at
   a more specific level resolves -- check each candidate value against the
   trait's actual `allowed_values_levels`/synonyms programmatically (see
   `config/traits.yml`'s `(Synonyms, ...)` parenthetical on each value) rather
   than assuming a column's typical rank order always wins; a real taxonomic
   fact (e.g. a genus belonging to a named tribe, or a family that's entirely
   one vocabulary term) may need a new synonym added to `config/traits.yml`
   rather than a per-dataset substitution -- see `update-substitutions-file`
   for that distinction. (An older dataset already in this repo may have
   these two columns baked into a checked-in `data.csv` instead -- that
   reflects an earlier convention, not the pattern to follow now.)
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

If `locations:` is non-trivial (more than one named location, or any location
beyond a single site with no per-record variation), run the real
`dataset_build()` (not just `read_metadata()`) and check
`unique(result$traits$location_id)` explicitly. If it comes back all-`NA`
while `entity_type:` includes `species` anywhere, that's the `species`-nulls-
`location_id` gotcha from Step 4, not a genuine "no location data" situation
-- the build gives no error or warning either way, so this check is the only
way to catch it.

## Step 6 -- report back / flag uncertainty

Don't silently present a guess as a fact. Anything inferred rather than
sourced directly from what the user gave you -- an expanded author given
name, a taxonomically-uncertain `entity_type` choice, an assumed
`institution` for a thesis -- goes in the chat report *and* as a
`questions:` entry (`question1:`, `question2:`, ... -- grep existing
`questions:` blocks for the exact style) in the file itself, so a future
curator reviewing the dataset sees it even without reading the chat log.
