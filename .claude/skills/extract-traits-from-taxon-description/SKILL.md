---
name: extract-traits-from-taxon-description
description: Read a taxonomic paper (new species description, revision, re-circumscription -- e.g. Australian Journal of Taxonomy, Muelleria, Nuytsia, Telopea articles) and write every morphological and ecological character of every taxon it describes into a long-format `data/<Surname_Year>/raw/trait_data.csv`, plus a `raw/<Surname_Year>_vouchers.csv` listing the type and other specimens examined. Hand-transcribed (no parser), following the floras conventions -- commonness rows, text-order space-delimited values, every character even without a traits.yml trait. Use when the user hands over a taxon-description PDF and asks for "a table of all trait data from this taxon description", "extract traits from this species description", or similar. Trigger phrase: "extract traits from taxon description".
---

# Taxon description PDF -> `raw/trait_data.csv` + `raw/<id>_vouchers.csv`

A middle path between two other skills:

- `extract-paper-into-dataset-raw`: same long-format, hand-transcribed `raw/trait_data.csv` for one
  PDF. Read its column semantics (`trait_value_raw` / `trait_value` / `trait_value_clean`) and
  candidate-naming rules; they all apply.
- `extract-traits-from-floras`: same **character rules** (commonness rows, text order, extremes,
  every character, colour/growth-form/calendar conventions). Its "Principles" and "Mapping rules
  learned" sections apply here verbatim. The difference is only that there is no parser: the
  source is one paper with a few taxa, so transcribe each sentence by hand.

Worked example: `data/Thiele_2025/` on the `flora-extractions-2026` branch (worktree
`~/GitHub/austraits.build-flora-extractions`): *Hibbertia doleritica* (new species),
*H. basaltica* (re-described) and *H. dispar* (key and introduction only). Its generator,
if kept, is a scratch script; the CSVs are the deliverables.

## Where it goes

- Taxon-description extractions live with the flora extractions: branch `flora-extractions-2026`,
  worktree `~/GitHub/austraits.build-flora-extractions`, folder `data/<Surname_Year>/raw/`.
- Map against **that branch's** `config/traits.yml` (parse it with
  `yaml::read_yaml(...)$traits$elements`). New traits keep being added, so re-read it each time
  rather than trusting a remembered list.
- Reuse the candidate names already in `data/ABRS_2026/raw/candidate_traits.csv` and the ABRS_2026
  data.csv headers (`sepal_shape`, `petal_count`, `leaf_adaxial_hairs`, `ovary_hairs`,
  `aril_texture`, `flower_sepal_width`, ...) before inventing one. That way taxon descriptions and
  the FoA extraction share one character vocabulary. If ABRS_2026 already has the taxon, compare
  the two: they are independent descriptions and useful as a check.

## What to read

Read the whole paper. Every part can hold characters:

| Section | Holds |
|---|---|
| Introduction | group-level statements ("X and Y have an unusual stamen arrangement ... only one other species, Z, shares it"), habitat, conservation listing |
| Key | contrasting characters, often the only data for a taxon not otherwise described |
| Description | the bulk; go clause by clause |
| Diagnostic features / diagnosis | usually repeats the description; add only new characters (e.g. "protruding tubercles absent") and put disagreements in `notes` of the description row |
| Phenology | `flowering_time`, `fruiting_time` (12-char y/n), peak as an extra row |
| Distribution & habitat | substrate, soil, vegetation, elevation, rainfall, associated species |
| Conservation status / Etymology | listing status; etymology can name the substrate ("Jurassic dolerite") |
| Materials and methods | how parts were measured (dried vs reconstituted material): quote it in `sampling_strategy` |
| Notes | comparisons (see below), within-species variation by subpopulation (keep, with `population_region`) |
| Figure captions | habit words ("wiry subshrub") and confirmations ("petals not overlapping") |

Skip pure comparisons ("much hairier than H. basaltica"). They can't be scored for either taxon. But
a comparison that **states the other taxon's own value** is that taxon's data: "the prostrate habit
(erect for S. tephropelina)", "obovate leaves (versus linear for S. glabella)", "pedicels 10--24 mm
long (1--10 mm long for S. tephropelina)" give rows for S. tephropelina / S. glabella, with
`source_section` "Affinities". Score the mirror value for the described taxon from its description,
not from the comparison (Bean_2018). Characters a note reads off an image of another taxon (Bean_2018:
*Stemodia* sp. Tanami from the Flora NT website) are scored too, with the image named in `notes`.

**BHL scans:** the text layer often runs words together or garbles digits ("37-4.5 mm" for 3.7--4.5,
"07-1.4", "debate" for deltate). Read the descriptions, keys and specimen lists from page images
(`pdftoppm -r 120 -gray -png`), and use the text layer only to check the transcription afterwards.

## Taxon treatment and first-description year (every taxon, user 2026-11-10)

Every taxon in the dataset gets two per-taxon fields: what kind of treatment the paper gives it, and
the year the taxon concept was first described. Use the vocabulary of the `family-taxon-table` skill
(user-level, `~/.claude/skills/family-taxon-table/`), restated here:

| `treatment_type` | when |
|---|---|
| `new species` | sp. nov.; also a comb./stat. nov. raising a taxon to species rank with a full description (note it) |
| `new infra taxa` | subsp./var./f. nov., including the autonym when the paper describes it |
| `new name (without a description)` | nom. nov., comb. nov. or stat. nov. with no description (diagnostic notes or comparisons only) |
| `revision` | an existing taxon described or redescribed, including reinstated taxa and a nom./comb. nov. that does get a description (note "nom. nov., with description") |
| `not described` | in the dataset only through keys, comparisons, notes or an annotated list (distribution and habitat only). family-taxon-table leaves these taxa out; datasets keep them because their key characters are data |

`original_concept_year`: the year of the earliest validly published name for the same concept, under
any name, **as cited in this paper's** nomenclature and synonymy (accepted name, basionym or replaced
synonym, heterotypic synonyms). Ignore misapplied names, pro parte and doubtful synonyms, nom. nud. and
phrase names. Illegitimate but validly published names count (flag them). A new taxon with no earlier
name gets the paper's year. If the paper does not give the author's year (key-only taxa usually), leave
it blank: never fill it from outside knowledge. Record the basis ("earliest: heterotypic synonym
Glaphyria nitida Jack 1823").

Write them to `raw/<Surname_Year>_taxa.csv` (one row per taxon: `taxon_name, treatment_type,
original_concept_year, original_concept_basis, source_section, notes`), and have the script join
`treatment_type` and `original_concept_year` onto every row of `raw/trait_data.csv` and `data.csv`,
right after `taxon_name`. They are unmapped columns; metadata.yml `notes` says so. Report the counts by
type and every new taxon whose year differs from the paper's year.

## Columns of `raw/trait_data.csv`

```
taxon_name,treatment_type,original_concept_year,entity_type,rank_scored_from,commonness_qualifier,population_region,entity_measured,trait,in_traits.yml,trait_value_raw,trait_value,trait_value_clean,value_type,unit,source_section,notes
```

- `entity_type` is `species` throughout (a taxonomic description, numbers included).
- `rank_scored_from`: blank when the taxon's own text gives the value. When a value applies to the
  taxon only through a statement about a higher rank or a group (a genus or family description in
  the introduction, a key heading covering several species, "only one other species shares this
  arrangement"), write that rank (`genus`, `family`, `species_group`). Keep such key values (e.g.
  "terrestrial", "alternate leaves") even when the taxon's description never repeats them. Never copy
  hedged statements ("most species", "usually in the genus").
- `commonness_qualifier`: see below. `population_region`: subpopulations or sites the value is
  restricted to ("Richmond and Tea Tree", "some subpopulations"). `entity_measured`: the part
  measured when it differs within one trait (`outer sepals` / `inner sepals`, `exposed outer
  surfaces`, `fruiting pedicels`, `branches`, `simple hairs`, `abaxial midrib`).
- No location, error or n columns: species descriptions have none. Don't make a
  `location_data.csv`; the localities go in the vouchers file.
- `trait_value_clean`: the AusTraits level for a real trait; for a candidate trait, a suggested
  draft value (as ABRS_2026 does). Leave it blank when nothing fits.
- `in_traits.yml`: `x` when the trait exists in the branch's traits.yml, blank for a candidate, so
  the table can be filtered quickly. Set it automatically by checking the trait name against
  traits.yml. `notes` on candidate traits also start with
  `Candidate trait, not in config/traits.yml (checked YYYY-MM).`

## Character rules

- **Commonness rows.** Any commonness word splits the trait into rows. The unqualified value gets
  `usually`, each qualified value its own row with the source word (`rarely`, `often`,
  `occasionally`, `quite_common`). "Petals 5 (rarely 4)" → 5 `usually` + 4 `rarely`. A lone
  qualified value with no alternative ("margins often irregularly lobed") is one row with that
  word.
- **Parenthetical extremes** with no commonness word ("2.5–4(–10) mm", "(1)2–5(–7)", "Carpels
  2(3)") → typical range on `usually` rows, the extreme values on `extreme_range` rows (`value_type`
  minimum/maximum). Only an explicit word ("5 (rarely 4)", "1 (rarely 0)") gives a `rarely` row
  (user, 2026-10-16).
- **Equal-weight alternatives** stay in one space-delimited cell in text order: "Prostrate to
  procumbent or decumbent" → `prostrate procumbent decumbent`; "spreading and erect" →
  `spreading erect`. "0 or 1" → numeric minimum 0 + maximum 1.
- **Peaks**: "Flowering September–November, peaking in late October" → `nnnnnnnnyyyn` plus a
  `peak` row `nnnnnnnnnynn`. A hedged peak ("with a likely peak in late September") gets no peak row,
  only a note. "Collected flowering in October" from a few specimens is still `flowering_time`, noted.
- **Stamen totals**: "Stamens 10, all on one side of the gynoecium" → `flower_fertile_stamens_count`
  + `stamen_arrangement: unilateral`; one-side / opposite-side counts only when the source splits
  them (Thiele_2024 vs Thiele_2025).
- **Every character**, mapped or not: each organ's hairs split into density/indumentum term
  (`<organ>_hairs`), hair type (`<organ>_hair_type`: simple, fascicled, stellate, forked,
  acicular), hair orientation and hair length (`<organ>_hair_length`, with `entity_measured` naming
  the hair type); shapes, apices, margins, surfaces, textures, persistence, fusion, orientation,
  attachment and counts of sepals, petals, stamens, staminodes, carpels, ovules, bracts; aril
  shape, texture and coverage. Diagnostic-group characters get their own candidate trait (e.g.
  `stamen_arrangement` + `stamen_count_one_side` / `stamen_count_opposite_side` for Hibbertia;
  `petal_overlap`).
- **Ecology**: `plant_growth_substrate` (soil words → terrestrial), resprouting, `elevation` and
  `annual_precipitation` (min/max), and the floras' `habitat_description` (verbatim) +
  `soil_terms` / `geology_terms` / `habitat_terms` in source words, with `population_region` rows
  where the habitat differs by site. Conservation listing and common name are kept as unmapped
  rows. Extent of occurrence, linear range and population size are left out: they are not traits.
  Mention them in the report.
- **"Woody rootstock"** is root woodiness: candidate trait `root_woodiness` (`woody_rootstock` →
  `woody`). Don't use `woodiness_detailed: woody_root` (it scores the whole plant by its root).
  Woodiness alone says nothing about buds, but when the text ties the organ to resprouting
  ("resprouting from a woody rootstock") it is a bud bank: also score `bud_bank_location`
  (rootstock → `basal_stem_buds`, the dictionary's synonym).
  "Resprouting from a woody rootstock after fire or browsing" → `resprouting_capacity: resprouts` +
  `resprouting_capacity_non_fire_disturbance: resprouts_non_fire_disturbance` (Thiele_2025).
- Numbers stay in source units (cm for plant height or branch length if the source says cm). Write
  ranges inside text as `a--b`. "c. 3 mm" → `value_type: mean`. A subglobular seed's "diam." →
  `seed_length`, noted.
- `trait_value_raw` is the clause the value came from, so one clause feeds several rows. Use `x`
  for the multiplication sign and `+/-` for ±.
- "Leaves 2.4--4 times longer than broad" → `leaf_length_to_width_ratio`. A part measured against
  another part ("bracteoles 0.3--0.6 times as long as sepals") → a candidate
  `<organ>_to_<organ>_length_ratio`.
- **Description and key disagree** (Bean_2018: "Prostrate shrub ... with woody stems" in the
  description, "Prostrate forb" in the key): record both on their own rows, let `source_rank` map the
  description, and raise a `questions:` entry.

## Papers scored mostly through keys (Bean_1992)

A partial revision can describe only a few taxa in full and leave the rest to an annotated list plus keys. The keys then hold
the data, so transcribe them as tables and let the script assign values to taxa (`data/Bean_1992/raw/scripts/make_trait_data.py`).

- **Multi-access key:** one row per taxon and character, with the states it is listed under in text order in one cell. Check that
  every taxon appears under each character: a gap usually means a misprint (Bean_1992 prints 1 for 11). Fix it only with
  evidence, with a note on the rows and a `questions:` entry.
- **Dichotomous key:** follow the printed couplet numbers. A lead covering one taxon is that taxon's own value; a lead covering
  several is `rank_scored_from: species_group`. A taxon reached by two paths gets one row per value, naming both couplets.
- **Numbers in a group lead are not copied** ("fruits 4--7 mm in diameter" above four species). They go in a `<trait>_class`
  candidate, as do bounds on any lead ("Largest leaves <9 mm", "Pedicels >8 mm"): a bound is not a measured value.
- **Several sources for one character:** keep them all in data.csv. custom_R_code maps only the best one per taxon, trait, region
  and part measured: own text, then multi-access key, then dichotomous key (`source_rank`, confirm the order with the user).

## `raw/<Surname_Year>_vouchers.csv`

One row per specimen the paper cites as the basis of the description (type and "other specimens
examined"), for every taxon in one file:

```
taxon_name,specimen_status,institution_code,catalog_number,collector,record_number,collection_date,locality,state,country,source_section,notes
```

- `specimen_status`: `holotype`, `isotype`, `lectotype`, ..., `specimen examined`.
- `catalog_number` = the herbarium accession (DarwinCore catalogNumber); `record_number` = the
  collector's number (recordNumber), `s.n.` kept verbatim. One row per institution for isotypes.
- Expand grouped citations ("de Salas 425–427 (HO 572646–572648)") into one row per specimen,
  pairing numbers in order, and note that the pairing is inferred.
- Dates ISO (`2024-10-30`); locality verbatim.
- Syntypes / types of heterotypic synonyms listed under the accepted name are vouchers too: one row
  each, with the synonym named in `notes`.
- Specimens cited only as "Locality (accession)" (common in WA papers): `catalog_number` + `locality`,
  and the paper's ALA batch-search URL in `notes` so collector and date can be recovered. Specimens
  named only in Notes ("R.D. Royce 7977") get a row with blank accession. Flag an accession cited
  under two taxa (Thiele_2024: PERTH 6692699 under H. parvula and H. xenandra).
- Flag inconsistencies in `notes`: the same sheet cited as `s.n.` in the type and with a number
  under specimens examined, or a non-standard herbarium code ("HOBART" for HO). Also note any
  phrase-name voucher (`'H. sp. Richmond dolerite (R.Schahinger HO528055)'`).

## Report back

- Files written, row counts per taxon, numbers of mapped and candidate traits (list the candidates).
- Each taxon's `treatment_type` and `original_concept_year`, with the counts by type.
- Numbered judgement calls and questions (they become `questionN:` entries in metadata.yml later):
  conflicts between description, diagnosis, key and notes; mappings to check; values left out.
- metadata.yml, substitutions and data.csv are only written when the request is to build the
  dataset (now the usual request). Then follow `data/Bean_2018/raw/scripts/` (one paper, one new
  species) or `data/Bean_1992/raw/scripts/` (a partial revision scored mostly from keys):
  `make_trait_data.py` writes trait_data.csv, the taxa file and data.csv, `make_vouchers.py` the
  vouchers, `make_meta.py` metadata.yml (substitutions from the script's `SUBS` map), then run
  `dataset_process` and `dataset_test`, and add the dataset's lines to `build.R`.
