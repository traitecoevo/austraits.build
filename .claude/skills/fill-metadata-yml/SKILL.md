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
`original_file:`, `notes:` (leave `collection_date:` as `unknown` unless a
real date is available -- don't guess a date). Grep a handful of
`data/*/metadata.yml` files for these exact field names first --
`basis_of_record:` and similar fields draw from a small controlled set of
strings (`field`, `lab`, `literature`, `preserved_specimen`,
`captive_cultivated`, `field_experiment`, ...) that's cheaper to grep than to
recall.

**`sampling_strategy:` is a verbatim quote from the primary manuscript's own
Methods section, not a paraphrase.** Fetch the paper (an open-access mirror --
PMC is usually fetchable even when the publisher's own page 403s) and pull
the actual paragraph(s) describing how the data were collected in the field
(survey design, timing, what counted as an observation/measurement) word for
word, quoted, rather than summarising it in your own words -- a paraphrase
here, however accurate, doesn't meet the bar. If the primary reference
doesn't describe the specific data in `data.csv` (e.g. a dataset merges
records that actually come from a secondary reference's methodology instead),
say so explicitly and quote from whichever reference actually describes it,
naming which one in the text (`Verbatim from <key>, section N.N <heading>:
"..."`). If every relevant reference is paywalled and unreachable, don't
paraphrase from memory or guess -- write what's known plus a `questions:`
entry (Step 6) asking the curator to supply the verbatim text if they have
access. `description:` can still be your own summary; only
`sampling_strategy:` (and, by the same logic, each trait's `methods:` in
Step 4 below) needs to be verbatim.

Use `notes:` to record anything a later curator should know about how
`data.csv` was derived (disambiguated location names, derived/matched
columns, judgement calls) -- especially anything already flagged in a prior
step's chat report when converting the raw file.

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

## Step 5 -- validate

Parse the finished file with `yaml.safe_load` and confirm it loads without
error and that `source.primary.key`, `len(traits)` and `len(locations)`
(if already filled) look right -- a stray indentation or an unescaped colon
inside a `title:`/`methods:` string is the most common breakage.

## Step 6 -- report back / flag uncertainty

Don't silently present a guess as a fact. Anything inferred rather than
sourced directly from what the user gave you -- an expanded author given
name, a taxonomically-uncertain `entity_type` choice, an assumed
`institution` for a thesis -- goes in the chat report *and* as a
`questions:` entry (`question1:`, `question2:`, ... -- grep existing
`questions:` blocks for the exact style) in the file itself, so a future
curator reviewing the dataset sees it even without reading the chat log.
