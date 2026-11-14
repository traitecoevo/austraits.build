---
name: draft-apd-definitions
description: Draft, review or revise trait definitions (entries) for austraits.build `config/traits.yml` that will go into the AusTraits Plant Dictionary (APD) -- the trait name, label, ontology-tagged description, comments, units/levels and synonyms, and the four controlled metadata fields (structure_measured, keywords, trait_group, characteristic_measured). Also covers carrying renamed traits/levels through to the datasets that use them. Use when the user asks to "draft APD definitions", "write a traits.yml entry", "add a new trait", "fill in the [XX]s", or to proofread/merge new branch traits before an APD release. Trigger phrase: "draft APD definitions".
---

# Draft APD trait definitions

A traits.yml entry is published in the APD, so every word is curated: names follow
develop's conventions, descriptions are tagged with published ontology classes, levels are
genuine botanical concepts, and the four metadata fields use only APD vocabulary. These rules
come from the 2026-11 review of 69 branch traits (`~/GitHub/new_traits_review/STATUS.md`),
where the curator had to correct the same mistakes repeatedly.

Do not tell the user the trait "needs to go to APD" -- they decide when traits are merged.

## Sources to have open

| What | Where |
|---|---|
| Existing definitions (the style to copy) | `git show origin/develop:config/traits.yml` |
| APD vocabulary for keywords / characteristic_measured | `~/GitHub/APD/data/published_classes.csv`, `APD_glossary.csv` |
| APD trait hierarchy (trait_group chains) | `~/GitHub/APD/data/APD_trait_hierarchy.csv` |
| Local ontologies (PO, TO, PATO, FLOPO, GO, CHEBI, NCIT, ENVO, EnvThes, ECSO, BSPO) | `~/Library/CloudStorage/OneDrive-UNSW/Documents/data_for_GitHub_repos/APD/ontologies/` |
| Helper scripts (copies in `scripts/` here) | `~/GitHub/new_traits_review/` |

Before drafting, find the trait's closest siblings in develop (`leaf_length` for
`leaf_length_juvenile_leaves`, `flower_bract_length` for `flower_bract_width`) and copy their
structure, wording, IDs, units and metadata fields. A new trait should read like it was written
by the same hand.

## 1. Is it really a new trait?

- Search develop (names, labels, descriptions) for an existing trait first.
- A categorical counterpart of a numeric trait is `<numeric_name>_categorical`
  (`leaf_secondary_vein_angle_categorical`, `flower_filament_fusion_categorical`).
- Don't create a trait that is arithmetically another one (e.g. stem water content per fresh
  mass = 1 - stem dry matter content): convert the data in the dataset instead.

## 2. Trait name

- Organ prefix first, matching develop: `leaf_`, `flower_` (all floral and inflorescence
  organs, incl. bracts, bracteoles, involucre, hypanthium, style, ovary), `fruit_`, `seed_`,
  `stem_`, `root_`, `bark_`, `wood_`. So `leaf_stipule_length`, `leaf_petiole_presence`,
  `flower_bracteole_length`, `flower_involucre_height` -- never bare `stipule_length`.
- Then structure, then quality: `leaf_intramarginal_vein_position`.
- Content traits always state the denominator: `_per_dry_mass`, `_per_fresh_mass`,
  `_per_area` (never bare `_per_mass`).
- Presence/absence traits end `_presence` (or are named for the structure, as develop does).
- Counts: say what is counted per what: `extrafloral_nectary_count_per_leaf`,
  `leaf_primary_vein_count`. Use the ontology's term (primary vein, not main vein).
- The name and label must agree on the quality: an involucre measured base-to-tip is a
  `height`, so the name is `_height` and the label "Involucre height".
- Renaming a trait that already has data means updating every dataset that uses it (step 9).

## 3. Label

Sentence case, short, no abbreviations: "Stipule presence", "Number of primary leaf veins",
"Secondary vein angle, categorical", "Juvenile phase leaf length", "Leaf starch content per
unit leaf dry mass", "Time to 50% seed germination (T50)".

## 4. Description

Two parts separated by `;` -- a formal sentence with ontology IDs, then a plain-language
sentence without IDs:

```
A <trait class> [TO:...] which is the <quality> [PATO:...] of <structure> [PO:...], ...;<Plain sentence.>
```

- Open with the most specific trait class that also heads `trait_group`
  ("A floral organ morphology trait [TO:0000862]", "A leaf morphology trait [TO:0000748]").
  Where TO has no class, use develop's pattern: "A gynoecium [PO:0009062] morphology trait".
- Tag every anatomical structure, quality and process on first use in the formal sentence,
  including small words like base, apex, cross section, whorl, flower, leaf, stem.
- Reuse the ID develop already uses for a term -- grep it:
  `grep -oh "whorl \[[A-Za-z]*:[^]]*\]" dev_traits.yml | sort | uniq -c`.
  Common ones: leaf PO:0025034, stem PO:0009047, flower PO:0009046, petiole PO:0020038,
  leaf apex PO:0020137, base NCIT:C90067, apex NCIT:C25423, cross section NCIT:C63795,
  whorl PO:0025023, bract PO:0009055, stamen PO:0009029, sepal PO:0009031, petal PO:0009032,
  length PATO:0000122, width PATO:0000921, height PATO:0000119, thickness PATO:0000915,
  shape PATO:0000052, 2-D shape PATO:0002006, 3-D shape PATO:0002266, colour PATO:0000014,
  dominant colour "(primary [NCIT:C25251])", ratio PATO:0001470, count NCIT:C25463.
- Otherwise look the term up in the local ontologies (exact label/synonym match):
  `python3 scripts/onto_lookup.py terms.txt out.tsv` (NCIT.owl takes a few minutes).
- Use the right class, not a near-miss: stipule PO:0020041 (not spine stipule PO:0025174);
  3-D shape PATO:0002266 (not 3-D extent PATO:0001710); intramarginal vein PO:0025520.
- Prefer a class's primary ID over an `alt_id` (style is PO:0009074; PO:0006469 is an alt_id).
- Never hand over `[XX]` placeholders; if no published class exists, leave the word untagged
  and tell the user.
- Say what is measured precisely enough to reproduce: at anthesis, mature, fully expanded,
  measured at widest point, including/excluding the petiole.

## 5. Comments

Use for: which sibling trait to use instead (in backticks: "Fusion of the perianth as a whole
is recorded under `flower_perianth_fusion`"), when the trait applies ("only score for species
that have intramarginal veins"), method dependence ("documenting methods is essential"), and
how a `monthly_presence` trait is keyed. Plain text only -- terms with no published class
(e.g. heteroblasty) can appear here but never in keywords.

## 6. Numeric traits

Units in develop's UCUM style: `mm`, `mg/g`, `g/m2`, `g/g`, `mm2/mm2`, `uL`, `d`,
`'{count}'`, `'{count}/{count}'`, `'{delta}[ppth]'`. Set `allowed_values_min/max` to
physically plausible limits for any Australian plant, not the range of one dataset.

## 7. Categorical levels and synonyms

**Levels are concepts, not flora phrases.** Never take a phrase from a flora or parser
output and turn it into a level or synonym by replacing spaces with `_` (`just_within_margin`,
`rounded_in_cross_section`, `at_right_angles`, `paler_below` were all removed). Phrase
variants belong in each dataset's `substitutions`.

- Choose the smallest set of standard botanical terms that describe *this* organ's character.
  Values that describe another organ or trait (corolla shapes in petal shape, stigma shapes in
  style shape) are parser mismaps: list them for the dataset's parser issues, don't add levels.
- Don't split levels the trait doesn't need (e.g. a level per lobe count when `lobed`
  covers it). When removing a level that has data, say how many values it drops and ask.
- Presence/absence levels name the structure: `stipule_present` / `stipule_absent`,
  `intramarginal_vein_present`, `extrafloral_nectaries_absent` -- never bare `present`.
- Position levels name the structure: `leaf_apex`, `leaf_base`, `leaf_middle`, `rachis`, `stem`.
- Colour traits use the condensed set (black, blue, brown, green, grey, orange, pink, purple,
  red, white, yellow), with shades as synonyms (cream -> white, mauve -> pink,
  lilac/violet/magenta -> purple, maroon/reddish_brown -> red, gold -> yellow,
  indigo -> blue, turquoise -> green). Detailed shades only in `*_colour_detailed`.
- Level descriptions: one sentence naming this trait's organ ("Ovary colour is white.") --
  check copy-pasted level blocks were reworded for the new trait.
- Shape level wording follows develop's `leaf_shape` (2-dimensional / length:width ratios).
- Multi-word levels and synonyms use underscores (`spirally_twisted`, `reddish_brown`).

**Synonyms are executable.** traits.build develop (`process_replace_synonyms`) rewrites every
`(Synonyms, a, b)` term found in any dataset's values to its level, word-bounded and
lower-cased. So a synonym list must:

- contain only true synonyms in standard usage (sessile = `petiole_absent`,
  petiolate = `petiole_present`, pyriform -> `obovoid`, doliiform -> `cylindrical`),
  not number variants (`2-fid`, `3-lobed`, `4-angled`) or phrases;
- never include a term that is also a level of the same trait (that level can then never
  survive), or list one term under two levels;
- be spelt as the data spell it (parsers emit `reddish_brown`, not `reddish-brown`).

Run `python3 scripts/check_synonyms.py FILE` (works on traits.yml or the review file).

## 8. The four metadata fields

- `structure_measured`: only terms already used in develop (leaf, leaflet, stem, root, bark,
  wood/secondary xylem as `wood, secondary xylem`, whole plant, reproductive shoot system,
  flower, inflorescence, fruit, seed, bud). Floral traits: `reproductive shoot system; flower`.
  Separate terms with `; ` (not commas).
- `keywords`: every term a label in `published_classes.csv`, `APD_glossary.csv` or
  `APD_trait_hierarchy.csv` (labels can contain "; " or ", ", e.g. `ratio; proportion`,
  `whorl, floral whorl`). Use the exact label (`plant ovary` not `ovary`, `secondary leaf
  vein` not `secondary vein`). Reproductive traits carry `reproduction`.
- `trait_group`: the full chain from `APD_trait_hierarchy.csv`, most specific first, e.g.
  `perianth morphology trait; floral organ morphology trait; reproductive shoot system
  morphology trait; plant structure morphology trait; plant morphology trait`. Several groups
  are fine: list each chain, and a shared ancestor once, after all its descendants.
- `characteristic_measured`: APD labels for the quality (`length; size`, `width; size`,
  `shape; 2-D shape`, `colour / color`, `organismal quality`, `amount (presence or absence in
  organism)`, `has number of; count`, `position`, `temporal pattern`, `mass; mass fraction;
  ratio; proportion`).
- No term twice, within a field or across the four fields.
- A term from a published ontology that isn't in APD yet may be added to
  `published_classes.csv` (template: `scripts/add_pc.py`; rows go next to their vocabulary,
  with the ontology's own label and definition). Never invent a term; ask the user before
  adding anything essential that isn't published or needs a new ontology download.

Check with `python3 scripts/check.py FILE` -> must report `0 traits with problems`.

## 9. Carrying changes through to the data

A renamed trait, a renamed/removed level, a removed synonym or a type change silently drops
data on every branch that uses it. After definitions change:

1. Write the edited entries into each source branch's traits.yml
   (`scripts/apply.py EDITED.yml BRANCH TARGET/config/traits.yml`; renames listed in
   `scripts/impact2_renames.py` are replaced in place).
2. Rename `trait_name:` in every `data/*/metadata.yml` (traits, substitutions,
   exclude_observations). Leave `var_in`/column names alone.
3. Simulate the build to find values that no longer map:
   `scripts/impact2.py BRANCH EDITED.yml BRANCH_TRAITS_YML --data-root CHECKOUT`
   (value substitutions -> `match: word` substitutions -> traits.yml synonyms, as traits.build
   does). It separates newly broken values from pre-existing ones.
4. Add `match: word` substitutions for genuine mappings (`scripts/add_subs.py ROOT rules.tsv`);
   list mismaps for the user instead of adding levels.
5. Run `traits.build::dataset_test()` on every touched dataset.

Dataset edits need the branch checked out. The user moves the main checkout between branches;
don't create worktrees.

## 10. Hand-over checklist

- [ ] No `[XX]`, `YYY` only in `entity_URI`
- [ ] `check.py` and `check_synonyms.py` clean; file parses as YAML
- [ ] Name, label and description agree (organ, quality, units)
- [ ] Each new trait placed after its closest develop sibling
- [ ] Renames/level changes propagated to datasets, or listed for the user
- [ ] Decisions and open questions noted in `~/GitHub/new_traits_review/STATUS.md`
