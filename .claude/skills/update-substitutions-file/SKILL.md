---
name: update-substitutions-file
description: Fill in the `replace` column of a dataset's `substitutions.csv` (typically `data/<id>/raw/substitutions.csv`) by matching each raw `find` value against the `allowed_values_levels` of its `trait_name` in `config/traits.yml`. Trigger phrase: the user typing "update substitutions file" (ask which dataset if it isn't obvious from context). Part of the same metadata.yml-filling skill suite as `fix-location-metadata`, but a different section -- this one is about mapping a source's own categorical spellings onto AusTraits' controlled vocabulary, not locations.
---

# Update substitutions file

`substitutions.csv` is one of the four GATE 1 review files
`traits-build-add-dataset` scaffolds (`trait_name`, `find` pre-filled from
the source; `replace` left for a curator) -- this skill is specifically
about doing that curator step: proposing a `replace` value for every row,
never inventing a controlled-vocabulary term that doesn't exist.

## Step 1 -- load the trait dictionary for exactly the traits in this file

```python
import yaml, csv
traits = yaml.safe_load(open("config/traits.yml"))["traits"]["elements"]

rows = list(csv.DictReader(open("data/<id>/raw/substitutions.csv")))
needed = {r["trait_name"] for r in rows}
vocab = {t: traits[t]["allowed_values_levels"] for t in needed}  # {trait: {value: description}}
```

Only pull the vocabulary for the trait(s) actually present in this file --
`config/traits.yml` is 500+ concepts, no need to hold all of it in mind at
once.

## Step 2 -- split `find` into atomic terms

A source's raw categorical value is very often already a compound --
`"orange+pink"`, `"white/pink/orange/red"`, `"pink/purple"` -- because the
contributor recorded a mix, not because it's one indivisible category.
Split on whatever delimiter the source used (`+`, `/`, `,`, `&`,
`" and "`) into individual words before matching each one separately. A
`find` value with no delimiter is already atomic.

## Step 3 -- match each atomic term against the trait's allowed values

Check, in this order, before concluding there's no match:

1. **Exact match** (case-insensitive) against an `allowed_values_levels` key
   -- `traits.build` matches trait values against `allowed_values_levels`
   case-insensitively at build time, so a raw value differing from the
   allowed value only in letter-casing (`C3` vs `c3`) already validates with
   no substitution at all. Filling `replace` with the canonically-cased form
   here is still fine (harmless, and clearer for a reader of the file), but
   don't treat a case difference as a reason to invent a substitution
   *outside* this scaffolded file -- e.g. never add a one-off
   `substitutions:` block directly in `metadata.yml` (see
   `fill-metadata-yml`) purely to fix casing.
2. **A listed synonym.** Every value's description in `config/traits.yml`
   ends `"(Synonyms, x, y)"` when it has any -- e.g. `honeybee`'s
   description lists `apis_mellifera`. Check this before assuming a term
   has no match; the dictionary's own synonym list is exactly what it's
   for.
3. **A merged/compound allowed value whose description already covers the
   term.** AusTraits often merges two everyday categories into one value
   -- `blue_purple` ("Flower colour is blue or purple"), `yellow_orange`,
   `red_brown`, `white_cream` are the recurring flower-colour case. A source
   term matching *either* half of the description's "X or Y" is a match on
   its own -- it does **not** need a second value alongside it. (`"blue"` ->
   `blue_purple`, not `blue_purple purple`.)

A term that doesn't clear any of these three isn't a match -- don't stretch
a fourth, weaker kind of inference to force one.

## Step 4 -- de-duplicate before joining

When a compound `find` splits into terms that map to the **same** allowed
value (`"yellow/orange"` -> `yellow_orange` + `yellow_orange`), the
`replace` cell gets that value **once**, not repeated. Preserve the order
terms first appeared in `find` otherwise.

## Step 5 -- space-delimit multiple distinct matches

When a compound `find` maps to more than one distinct allowed value, join
them with a single space in `replace` -- `"orange+pink"` ->
`yellow_orange pink` -- the same simultaneous-value convention used
throughout this repo's own worked examples (see
`paper-trait-location-extraction`'s `trait_value` rule for the same
mechanism applied earlier in the pipeline). Never use `+`, `/`, or a comma
in `replace` itself -- those are the source's punctuation, not AusTraits'.

## Step 6 -- leave genuinely unsure rows as `NA`, and say so

If a term is a real colour/category but doesn't clearly belong to one
allowed value over another -- `"peach"` sitting ambiguously between `pink`
and `yellow_orange`, with no dictionary description or synonym settling it
either way -- leave `replace` as `NA` (matching the file's existing
placeholder, not blank) rather than guessing. **List every row left `NA` in
the chat report, with the one-line reason it didn't resolve** -- that's the
whole point of flagging rather than silently guessing: it hands the
curator exactly the judgment calls that need a human, instead of either a
wrong answer or a silent gap they'd have to re-discover by re-reading the
file.

## Step 7 -- report back

For the dataset just processed: how many rows resolved outright, how many
resolved via a synonym or merged-category match (worth a quick sanity spot-
check even though confident), and the full list of rows left `NA` with the
reason. Do not touch `trait_name` or `find` -- only `replace` is this
skill's to fill in, matching `traits-build-add-dataset`'s "decision cells
stay empty for the curator, the skill fills what the source says" principle
applied to the one column that *is* the curator's decision here.
