---
name: add-sources
description: Give every `source_id` in a compilation dataset (`data/<id>/`) a full reference -- map the citations, website URLs or `author_yyyy` codes a dataset's rows carry onto keys, write those references to `data/<id>/sources.yml` (named by `source: sources.yml` in `metadata.yml`), and check every key against the references already in AusTraits so the same work keeps one key and one key names one work. Use when the user asks to "add sources", "map source_ids", "add references for source_id", "make the source_id values resolve", or points at a dataset failing `dataset_test()`'s `source_id` check (traitecoevo/austraits.build#888). Trigger phrase: "add sources".
---

# Add sources

A compilation dataset (one assembled from many floras, papers and websites)
records, per row, where each value came from. AusTraits keeps that as
`source_id`: **every `source_id` value must be the `dataset_id` or the `key`
of a reference in the dataset's `source` list**, and `dataset_test()` fails
anything else (traits.build #296/#298). A blank `source_id` cites the
dataset itself.

Keys are shared across the whole database: the build pools every dataset's
references, so **one key must name one work everywhere, and one work should
carry one key**. Most of the effort is in keeping to that.

Worked examples: `data/Wenk_2025/raw/scripts/add_source_id.R` (structured
reference columns, 1,487 references) and
`data/eFLOWER_Dun_2022/raw/add_source_id.R` with
`raw/source_lookup.csv` (free-text citations and website URLs). Older
examples kept in `custom_R_code`: `eFLOWER_2021`, `Kew_2019_1`/`_3`/`_4`/`_5`,
`Nano_2011`, `Prendergast_2026_2`, `Stephens_2021`/`_2022`.

## Shared helpers

`ignore/source_references.R` (source it from the repo root; a local file -- `ignore/` is git-ignored):

| function | does |
|---|---|
| `source_reference_index(exclude = "<id>")` | every reference in `data/*/metadata.yml` and the `sources.yml` files they name, one row each (`dataset_id`, `type`, `key`, `year`, `title`, `doi`, the entry itself in `entry`) |
| `source_resolve_keys(cand, index, keep = , same_as = )` | decides each candidate key: `copy` (same work already in AusTraits, by DOI or by title + year, or listed in `same_as`: take its key and copy its entry), `rename` (key names a different work: next free suffix), `new` |
| `source_suggest_same_as(resolved, cand, index)` | near misses the DOI/title test cannot see -- the same book with a subtitle, a wrong year in someone's entry -- to check by eye and add to `same_as` |
| `source_copy_entry(index, key, dataset_id)` | the entry as written in that dataset |
| `source_authors_from_semicolons()` | "Surname, Given; Surname, Given" (Zotero/CSL) to "Given Surname and Given Surname", particles moved (`Mueller, Ferdinand Von` to `Ferdinand von Mueller`), institutions braced |
| `source_clean_title()`, `source_clean_pages()`, `source_clean_text()` | strip `<i>` tags, `1-20` to `1--20`, swap characters AusTraits disallows (U+2010 hyphen, no-break space, dagger) |
| `source_check_entries(entries)` | every problem a build or `dataset_test()` would hit -- see Step 5 |
| `source_write_yml(entries, file)` | writes `sources.yml` as traits.build writes it, fields in the usual order |

## Step 0 -- see what the rows carry

Load the data **after `custom_R_code`** (that is what `dataset_test()` checks):

```r
library(traits.build); library(tidyverse)
m <- read_metadata("data/<id>/metadata.yml")
d <- read_csv("data/<id>/data.csv", col_types = cols(.default = "c"))
d2 <- traits.build:::process_custom_code(m$dataset$custom_R_code)(d)
```

Count distinct reference values and how many rows have none. Typical shapes:

- **Structured reference columns** (`author`, `year`, `title`, `journal`,
  `doi`, from a reference manager) -- build entries from the columns (Wenk_2025).
- **Free-text citations** (`FullRef`, `refdesc`) -- CrossRef, then parse,
  then hand-fix the rest (eFLOWER_Dun_2022, Kew_2019_*).
- **Website URLs only** -- one key per site and year (eFLOWER_Dun_2022's
  `Reference`, 79% of its rows).
- **`author_yyyy` codes with no reference** -- find each work (issue #888 §1).

Ask the user before starting if it isn't clear (a) whether website-only rows
get site keys or cite the dataset, (b) whether `source_id` should live in
`data.csv` (written by a `raw/` script -- preferred for anything over a few
dozen references) or be derived in `custom_R_code`, (c) what
`measurement_remarks` should hold once references are linked.

## Step 1 -- keys

- **Personal author**: first author's surname + year, ASCII, with spaces,
  apostrophes and hyphens removed: `deVogel_2026`, `AiryShaw_1980`,
  `vanSteenis_1984`. Two works by the same stem get `_1`, `_2` (the existing
  convention: `Hislop_2012_1`, `Hislop_2012_2`).
- **Online floras and databases** use the codes AusTraits already uses, with
  the year the record was scored (not the site's founding year):

  | site | code | | site | code |
  |---|---|---|---|---|
  | Flora of Australia (online, FoA volumes) | `ABRS` | | eFloras (any flora_id) | `Efloras` |
  | PlantNET | `NHNSW` | | APSA pollen atlas | `APSA` |
  | VicFlora | `RBGV` | | DELTA families/grass genera | `Dallwitz` |
  | Florabase | `WAH` | | Flora of Pakistan (Tropicos) | `PPD` |
  | FloraNT | `NTH` | | Australian Tropical Rainforest Plants (incl. RFK7) | `ATRP` |
  | eFlora SA | `SAH` | | EUCLID | `CPBR` |
  | Flora of Tasmania online | `TMAG` | | Australasian Virtual Herbarium | `AVH` |

  A key that is a `dataset_id` (`NHNSW_2022`, `ABRS_2022`, `AVH_2026`)
  copies that dataset's own reference exactly. Where the dataset ids carry a
  suffix, use the one the convention picked: Florabase 2022 is `WAH_2022_2`,
  2023 `WAH_2023_1`. Other years copy the flora dataset's reference with the
  year changed and its `note:` (access date) dropped. Before inventing a
  code for any other site, search the index for its URL
  (`index %>% filter(str_detect(map_chr(entry, ~ .x$url %||% ""), "domain"))`)
  and reuse the code another dataset gave it.
- **Undated sources** take the year they were consulted; never leave a key
  without a year.
- **Printed Flora of Australia volumes** are `ABRS_<year>` too, which can
  clash with `ABRS_<year>` meaning the online flora in eFLOWER_2021 -- the
  resolver renames the printed one (`ABRS_2013_2`).

### Free-text citations

Work through them in this order, recording the outcome per citation in a
`raw/source_lookup.csv` (one row per distinct citation/URL: `reference`,
then `site`, or `key` + fields, or `same_as`, plus `origin`):

1. **Already in another dataset.** Compilations often share a source export
   (eFLOWER_2021 and eFLOWER_Dun_2022 are both PROTEUS exports). Run the
   other dataset's `custom_R_code` and join on the citation text to get its
   key: `same_as`.
2. **CrossRef**, one query per citation:
   `https://api.crossref.org/works?rows=3&query.bibliographic=<citation>`.
   Query **sequentially, about one per second**, and back off on HTTP 429
   (four parallel threads got rate-limited within minutes); save results as
   you go so a rerun resumes. Accept a hit only if its title is contained in
   the citation (normalised to lower-case letters and digits), the years agree
   within one, and the first author's family name is in the citation.
3. **Parse** the rest. PROTEUS-style citations ("Surname AB, Surname CD.
   2001. Title. Journal 12(3): 1-20.") parse reliably if the title/journal
   split is taken at the **last** ". " before the volume -- titles are full
   of abbreviations ("gen. nov.", "Muell.", "L."). Print journals that are
   long or contain ". " to catch bad splits.
4. **By hand**: books, chapters, floras, websites given as citations, and
   anything above that failed. Search (IPNI for the taxon's protologue, the
   web for titles) before giving up. A citation that cannot be identified is
   recorded as `unidentified` with the reason in `note`, and its rows cite the
   dataset -- report these to the user.

Several citations of one work (different formatting, a typo) share one key;
different works on one stem get `_1`, `_2`. Citations can contain stray
line breaks or carriage returns: join citations to the lookup on
`str_squish()`ed text.

## Step 2 -- entries

Field names in use: `key bibtype year author title journal volume number
pages publisher place isbn doi url note`, plus `booktitle editor` for
`InBook` (eFLOWER_2021 uses them; `dataset_test()` accepts them).

- `Article` needs `author`, `title`, `journal`, `year`. Keep `doi`; add `url`
  only when there is no DOI.
- `Book` needs `author` (or a braced institution) and `title`; give
  `publisher` and `place` when known.
- A chapter in a flora or edited volume is `InBook` with `booktitle` and
  `editor`.
- `Online` needs `author` **and** `url`: a site with no person behind it gets
  its name braced as author, `'{Wikipedia}'`, `'{eFloras}'`.
- `Unpublished` for personal communications and unpublished data
  (`title: 'Unpublished data: ...'`) when they are to be cited as a source.
- **Commas in `author`**: traits.build turns every comma into "and", so
  "Gregory T. Chandler, Michael D. Crisp" is fine (older entries are written
  that way) but a comma inside a braced institution splits it --
  `'{Royal Botanic Gardens Kew}'`, not `'{Royal Botanic Gardens, Kew}'`.
- Strip HTML (`<i>Acacia</i>`) from titles; `pages: 1--20`.
- Fill gaps the data leaves when the work is known -- editors of authorless
  floras, the journal of an article with none, the year of an undated paper
  from its protologue (IPNI's API: `https://www.ipni.org/api/1/search?q=<name>`
  gives publication and year for the taxon scored from it). Never invent a
  title: write a bracketed description and say so in `note:`
  (`title: '[Account of Oreobolus R.Br.]'`, `note: Title not verified ...`).

## Step 3 -- check keys against AusTraits

```r
index <- source_reference_index(exclude = "<id>")
cand <- tibble(key = ..., year = ..., title = ..., doi = ...)
resolved <- source_resolve_keys(cand, index, keep = <keys not to match by title>)
```

Read every `copy` and `rename` before accepting them:

- `copy` with a **different key** usually means the data had the author order
  or year wrong (Wenk's `Johnson_1995` is Stephens_2021's `Hill_1995`,
  Hill & Johnson) -- take the existing key. But if the existing key is itself
  misspelt (eFLOWER_2021 `Airy_1980`, SPRAT_2025_2 `Crayzer_2004`), put the
  correct key in `keep` and tell the user about the typo.
- Keys built by convention (online floras) go in `keep`, so they are not
  matched to another dataset's off-convention key for the same site
  (SPRAT_2025_2 wrote PlantNET as `PlantNET_2025`).
- A key that is an existing site key (`Efloras_2013`, `NHNSW_2022`) is the
  same site: force `copy`, do not let a title mismatch rename it.
- Two of the dataset's references matching one entry is a warning: fine
  when they really are one work (the two volumes of Flora of the South West
  are the `Wheeler_2002` dataset), otherwise fix the match.
- `rename` gives the next free suffix (`Brown_2019` to `Brown_2019_2`); check
  the clash really is a different work. In Wenk_2025 six of the first 22
  renames were the same book under a subtitle or a wrong year elsewhere
  (`Chinnock_2007`, `Cooper_2013`, `Dunlop_1995`, `Stanley_1986`,
  `Bailey_1899`, `Orchard_1989`) -- always run `source_suggest_same_as()` and
  put real matches in `same_as`.
- A DOI can name a whole series (every volume of Mueller's *Fragmenta*
  shares 10.5962/bhl.title.287), so the resolver only accepts a DOI match
  when volume and year (within one) agree too -- pass `volume` in `cand`.
- **Reruns must be stable.** Once another dataset has copied one of this
  dataset's entries, a rerun finds that key "held elsewhere"; the resolver
  treats a key held by the same work (same title or DOI) as no clash, and
  reuses a suffixed key the same work already holds. After any change to the
  helpers, rerun every dataset done so far and diff its `sources.yml` against
  the previous one -- it should be unchanged.
- A whole printed flora cited through one of its chapters ("Marchant et al.
  1987. Tremandraceae. Flora of the Perth Region 1") is `same_as` the volume
  when the chapter has no author of its own; a chapter with its own author
  (Forster 1996, Apocynaceae, Flora of Australia 28) is an `InBook`.

## Step 4 -- rows with no reference

Read the scorer's notes for them. Link any that name a work (a DOI, a short
title, "Icones Plantarum Formosanarum 9 - Hayata - 1920"). The rest keep a
blank `source_id`, so they cite the dataset; put the note into
`measurement_remarks` in `custom_R_code` (Prendergast_2026_2 does the same for
unreleased surveys and others' unpublished data). Do not store a mostly
empty text column in a large `data.csv`: readr guesses it `logical` from the
first 100,000 rows and `dataset_test()` fails with parsing errors -- derive it
in `custom_R_code` instead.

**Unpublished records are not sources.** A person's "unpublished
observations", a private collection, slides, manuscript notes or a student
project has too little to cite: its records keep a blank `source_id` (the
dataset), and the record is named in a column mapped to
`measurement_remarks` (Brown_1997's `unpublished_source`; Prendergast_2026_2
does the same). Theses, consultancy reports and society bulletins are
sources. A personal communication that adds "also see <paper>" cites the paper,
with the personal communication in the remarks column.

**Map a source column to `measurement_remarks` only where it links a record
to information about its source that is cited nowhere else** -- an
unpublished source, an author's reply to queries. Not for lists of published
works offered as evidence for the dataset's own scoring (Osborne_2014,
Metcalfe_2020_1/_2: the records cite the dataset, no remark), and not when
the remark would repeat the
record's `source_id` (derive a remark column that is blank wherever
`source_id` is set), repeat references the trait's `methods` already cite
(Meers_2007), or point to the dataset's own `secondary` papers (Ottaviani_2015
keeps its papers as `secondary`; only the author's replies are remarks). A
`#` comment, or an apostrophe, inside `custom_R_code` breaks it: the code is a
single-quoted YAML string read as one line.

For a compilation whose references are numbered or abbreviated, also keep in
`data.csv` (not mapped into AusTraits) a `reference_full` column with each
record's reference written out, and a `source_notes` column recording how the
reference was read -- an OCR guess, a number that contradicts the record (a
bee paper cited for a bird), a missing reference. Write the records that need
checking to a table in `raw/` (Brown_1997's `Brown_1997_references_to_check.csv`)
that someone can take to the print source.

## Step 5 -- write and test

1. `problems <- source_check_entries(final)` must be empty: every entry
   prints (RefManageR is strict about required fields), no comma inside a
   braced author, no disallowed character.
2. `source_write_yml(final, "data/<id>/sources.yml")`, and in `metadata.yml`
   add `  source: sources.yml` under `source:` after the `dataset:` entry, and
   `source_id: source_id` under `dataset:`.
3. Write the `source_id` column (and only that) back to `data.csv`; confirm
   every other column is unchanged by diffing against `git show HEAD:...`.
4. Keep a lookup in `raw/` (`source_id`, the original reference string, and
   for checked keys `action`/`copy_from`/`note`) so the mapping can be
   audited, and document the script in `raw/README.md` or its header.
5. `traits.build::dataset_test("<id>")` -- it checks that every `source_id`
   resolves and that every `source` reference is used by some row (an unused
   one is a typo or belongs in `secondary`). Write only references cited by
   rows that survive `custom_R_code`: eFLOWER_Dun_2022 drops rows duplicated
   in eFLOWER_2021, and site keys used only by those rows failed this check.
6. Check every key the dataset shares with another dataset names the same
   work there (same title and year in the index) -- the build pools all
   datasets' references by key.

## Report to the user

Counts (references written; copied / renamed / new; rows still citing the
dataset and why), the keys renamed because they clash, typos found in
**other** datasets' keys (offer a follow-up rather than editing those
datasets), and any reference whose fields were inferred rather than read.
