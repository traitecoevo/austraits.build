---
name: extract-traits-from-floras
description: Build a dataset's wide-format `data.csv` from free-text taxon descriptions -- flora treatments, herbarium/web fact sheets, scraped species profiles (e.g. Florabase, WA Orchids fact sheets, ABRS, PlantNET) -- by scripting a reproducible text-to-trait extraction. Each categorical trait is written as a verbatim `*_description` column plus a column mapped to config/traits.yml levels; numbers go into `_min`/`_max` columns. Use when the user hands over a CSV/scrape of descriptive text (description, distribution & habitat, distinguishing features, flowering months, notes) and asks to "extract all the traits you can", "make a data.csv from these fact sheets/flora descriptions", or similar. Trigger phrase: "extract traits from floras".
---

# Extract traits from floras

Turns descriptive text into an AusTraits `data.csv`. Three worked examples, all on the
`flora-extractions-2026` branch (checked out at `~/GitHub/austraits.build-flora-extractions`):

- `data/Newmann_2026/` (WA Orchids fact sheets, 595 taxa): **formulaic** text, one formula
  sentence per sheet. Its `raw/build_data_csv.R` is the reference for fact sheets.
- `data/WildNet_2026/` (Queensland WildNet threatened-species profiles, 302 taxa, many
  families): **free botanical prose**. Its `raw/build_data_csv.R` is the reference for
  prose (sentence/clause parsing, subject rules, a month-range parser, vetted ecology tables).
  See "Free-prose profiles" below.
- `data/LucidFerns_2026/` (Lucid "Australian Tropical Ferns and Lycophytes" fact sheets, 339
  taxa): **Flora-of-Australia organ-led clauses** ("Rhizome …; Stipe …; Lamina …; Primary pinnae
  …"). Its `raw/build_data_csv.R` is the reference for clause-by-organ parsing with
  division-level context rows. See "Ferns and organ-led descriptions" below.

Copy the closest one's structure rather than starting from scratch.

**Branch workflow:** new flora extractions go on the `flora-extractions-2026` branch, worked in
its separate worktree `~/GitHub/austraits.build-flora-extractions` (created from
`origin/develop`). Don't switch the main checkout: it usually carries unrelated uncommitted
work. Commit / push only when the user asks; they usually do once data.csv is built. Note:
traits added locally to `config/traits.yml` elsewhere (e.g. `leaf_count`) may be missing on this
branch; check every mapped trait exists in that branch's traits.yml and flag gaps.

## Principles (the user's standing rules)

- **Script it, never hand-edit data.csv.** All extraction lives in
  `data/<id>/raw/build_data_csv.R`, which reads the source and writes `data/<id>/data.csv`.
  The build never runs it; it exists for provenance and re-extraction. Irregular sentences
  that regex can't parse go in a small override CSV in `raw/` (e.g. `habitat_overrides.csv`).
  Source typos are fixed explicitly at the top of the script, with a comment
  (e.g. a reversed range "20–15 mm" → "15–20 mm").
- **Pair every categorical trait.** Write `<trait>_description` (the verbatim source phrase)
  next to `<trait>` (a best-effort mapping to `config/traits.yml` levels, space-delimited when
  there are several). The mapping can then be revised later without re-reading the source.
- **Text order and commonness rows apply to every categorical trait** (user rule), not just
  growth form: values are listed in the order they appear ("shrub or tree" → `shrub tree`), and
  any commonness modifier splits the trait onto `commonness_qualifier` rows ("tree or rarely
  shrub" → tree `usually`, shrub `rarely`; "white, more rarely pink" → same for flower colour).
  A qualified climbing habit is a growth-form alternative too: "Shrub to 3 m high, rarely
  climbing" → shrub `usually` + climber_woody `rarely` (climber_herbaceous for herbs; "climbing",
  "scandent", "twining" count, "scrambling" alone does not).
  **Regional qualifiers too**: "becoming a shrub in southern part of its range" → shrub on a
  `population_region` row.
- **`_description` holds the source's own terms, so mappings can be changed later** (and users may
  want different mappings from AusTraits'): for categorical traits the matched words in text
  order with their qualifier ("blue-purple; dark blue; lilac", "sometimes scrambling"); for
  numeric traits one `<trait>_description` per trait with the verbatim measurement.
- **Never drop parenthetical extremes.** "(3.7–) 10 (–30) m", "2–4 (rarely 6) mm": typical
  range in `_min`/`_max`, extremes in `_extreme_min`/`_extreme_max` (unmapped for now).
- **Genus and family copy-down (FoA-style treatments)**: a categorical value stated singly and
  unqualified in the genus (else family) description is copied to member taxa silent on that
  trait (`trait_scoring_method` = `inferred_from_genus` / `inferred_from_family`). Drop
  sentences scoped to some species ("most species", "or introduced … species") and segments
  with commonness words first, and segments naming a genus in brackets ("(Hydrocleys)"). A
  statement counts only if it names exactly one term, counting terms with no level ("flat,
  terete or triquetrous" is not "all terete"). Copy only if every member taxon that describes
  the trait itself includes the value. Never copy numbers.
- **Terms with no AusTraits level are still recorded** (e.g. sagittate, emarginate, connate)
  in `_description`, with the mapped column left blank, so they can be mapped later.
- Corolla length → `flower_length` when the flower itself isn't measured (user's choice).
- **Very large sources (e.g. FoA online, 20k profiles; `data/ABRS_2026` on the flora branch):**
  one general organ-led parser, run family by family (sorted family → genus → species); stop
  after each family (or ~200 rows of small families) for the user to check. Extract all traits,
  including candidate traits not yet in traits.yml, under descriptive names.
- **Keep numbers in source units.** Use `<trait>_min` / `<trait>_max` columns; units are
  converted in metadata.yml. Exception: when one field mixes mm, cm and m (prose profiles),
  convert each column to a single unit in the script (heights m, other lengths mm) and keep
  the original wording in the `_description` column; `unit_in` is per trait, not per row.
- **Write numeric ranges inside text as `a--b`, never `a-b` or `a–b`**, because Excel turns
  hyphenated numbers into formulas or dates. Do this as the script's final step over every
  character column.
- **Score only what the text states.** Specifically:
  - Don't infer pollinators from morphology or genus knowledge, e.g. a hinged labellum →
    wasp, or Pterostylis → fungus gnat. Leave them blank.
  - Don't fill "nearly universal" genus traits across species (tuberous geophyte, perennial,
    orchid mycorrhiza) unless the text says so for that taxon.
- **Genus → species copy-down only for unhedged universal statements.** A genus sheet sentence
  like "Didymoplexis are leafless, non-green and saprophytic" or "Habenaria species have an
  ovoid tuber" may be copied to the genus's species, and only where the species sheet is
  silent. Never copy hedged statements:
  - "most", "many", "some", "often" (e.g. "most species possessing pseudobulbs")
  - ranges across species (e.g. "evergreen to deciduous")
  - statements that are merely implied (e.g. "Unlike most WA geophytic orchids…, Pterostylis
    species have…")

  Keep the vetted (genus, trait) pairs in an explicit `tribble` in the script, with a comment
  on any non-obvious one. Copied values go on their own rows with
  `trait_scoring_method = inferred_from_genus`, and their description is prefixed
  "[genus fact sheet]".
- **Unmapped columns are fine.** Useful non-trait fields such as conservation code, common
  name, URL, distribution text and clumping text stay in data.csv; metadata simply doesn't map
  them.

## Workflow

1. **Inspect the source.**
   - Check dimensions, columns, empty counts and taxon rank.
   - Read about a dozen random records in full to learn the templates. Fact sheets are
     usually formulaic, e.g. "A common species 50–250 mm high with a smooth leaf … and up to
     eight green and white flowers 8–10 mm across."
2. **Survey the phrasing before writing rules.**
   - Split all text into sentences tagged with the taxon, and grep for keyword families into a
     scratch file. Read the scratch file, not the raw CSV.
   - Families: pollination (`pollinat|self-poll|cleistog|nectar|hinged|insect-like`), fire
     (`fire|burnt|unburnt`), clonality (`colon|clonal|daughter|clump`), storage organs
     (`tuber|pseudobulb|rhizom`), substrate (`epiphyt|lithophyt|host`), scent
     (`fragrant|scented|odour|perfume`), photosynthesis (`leafless|saprophyt|chlorophyll`) and
     phenology (`evergreen|deciduous|withered|shrivelled`).
3. **Check the dictionary.** Find candidate traits and their levels in `config/traits.yml`
   (parse with `yaml::read_yaml`). Look for precedents in similar datasets:
   - `WAH_2023_2`, `ABRS_2023` and `NHNSW_2022-2024` for herbarium-description conventions
     and contexts.
   - Exact trait names matter. For example, there is no `plant_diameter`; rosette width is
     `plant_width`.
4. **Write the script** (structure below), run it, then QA (below). Iterate until the QA
   tables are clean.
5. **Report back** with coverage counts and a numbered list of judgement calls and questions.
   Those questions later become `questionN:` entries in metadata.yml.
6. **Refresh the dataset tracker** (`update-tracker` skill) after creating or restructuring
   data.csv.

## Script structure

- `extract_one(row)` returns one wide row per taxon. Parse numbers from the **first
  description sentence** (the formula sentence). Parse flags, such as fire, scent and
  substrate, from all text fields combined.
- A `grab_sentences(text, pattern)` helper fills the `*_description` columns with whole
  sentences.
- After `map_dfr`, apply in order: habitat cleanup and overrides, explicit taxon fixes,
  context rows, and genus copy-down rows. Then bind the rows, convert ranges to `--`, and
  write.

### Contexts are row-level, so contextual values get their own rows

traits.build applies a context to every trait on a row. The main row per taxon carries no
contexts. Each contextual value goes on an extra row that holds only `id_cols`, the
context(s) and that trait:

| Context column | Values | Carries |
|---|---|---|
| `population_region` | e.g. `northern populations`, `southern populations` | `flowering_time` when the sheet splits the calendar |
| `entity_measured` | `rosette` | `plant_width` (rosette diameter) |
| | `basal_leaf`, `cauline_leaf` | `leaf_count`; only split when both counts are given, so they are never summed |
| | `pseudobulb`, `tuber`, … | `storage_organ_length` / `_diameter` (the main row keeps `storage_organ`) |
| `trait_scoring_method` | `inferred_from_genus` | genus copy-down values |
| `commonness_qualifier` | `usually`, `rarely`, … (the source's word) | `plant_growth_form` / `plant_growth_substrate` when the text qualifies an alternative ("tree or rarely a shrub", "Terrestrial or occasionally lithophytic"); precedent: Wenk_2025 |
| `leaf_type` | `fronds`, `lamina`, `pinnae`, `longest primary pinnae`, `ultimate segments`, `sterile pinnae`, `lateral leaves`, … | leaf / leaflet sizes and counts, plus margin / apex / hairs at that division (ferns; Wenk_2025 convention) |
| `plant_organ_measured` | `stipe`, `rhizome`, `trunk`, `branches`, `fronds` | `petiole_length` (stipe), `stem_length` / `stem_diameter` (rhizome), `plant_height` (trunk, fronds) |

The context names follow WAH_2023_2 and ABRS_2023 (`method_context`). Sort the output by
taxon, then main row first.

## Mapping rules learned

**Flowering time**
- 12-character lowercase `y`/`n` string from month abbreviations. Detect months by substring,
  because the source can have glitches like "Sep· · ·".
- A "(northern populations) … (southern populations)" calendar becomes two
  `population_region` rows.

**Growth form**
- Score from the "is a … <noun>" phrase only, not the whole sentence, so host trees
  ("on the branches of rainforest trees"), "kangaroo grass" and "tree-like" aren't picked up.
  Drop phrases that name no growth form (e.g. "is yet to be formerly described").
- **Multiple forms follow the text order**: "shrub or small tree" → `shrub tree`,
  "tree or shrub" → `tree shrub`. Find each form's position and blank matched words (same
  length) so "tree fern" / "sub-shrub" aren't re-matched as tree / shrub.
- **Qualified alternatives split into rows**: "tree or rarely a shrub" → one row per form with
  `commonness_qualifier` (`usually` for the unqualified form, the source word for the other).
  The qualifier must govern the form itself (after "or", a comma or "is", with at most a size
  word between); "rarely dioecious shrub", "mainly glabrous shrub" are not qualified forms.
  Simple "or" with no qualifier stays on the main row.
- **"tufted"/"tufts" on a graminoid → add `tussock`**, in text order (e.g. "tufted grass-like
  plant" → `tussock graminoid`). Not for tufted herbs or ferns.
- Mappings: grass / sedge / grass-like → `graminoid`; palm, cycad, banana → `palmoid`; tree fern
  → `fern palmoid`; orchid, aquatic plant → `herb`; vine/climber → `climber`
  (`climber_woody` / `climber_herbaceous` when stated, and then drop the generic `climber`);
  Lycopodiaceae/Selaginellaceae "fern" or "clubmoss" → `lycophyte`.

**Height**
- For geophytes and other plants without a permanent shoot, "N–M mm high (when flowering)"
  maps to `plant_height_reproductive`, not `plant_height`.
- Catch "(occasionally up to N mm)" as the maximum.
- Orchid heights are to the top of the flowering stem → `plant_height_reproductive`; a
  separately described scape → `plant_height_reproductive`; climbers →
  `plant_height_climbing_plant`. "Grows to 35 cm in length" / "can reach 8 cm" in a length
  sentence is stem length, not height. Cycad "trunk N m tall" was scored as `plant_height`
  (flag it).

**Flower colour** (`flower_colour` levels: `white_cream`, `yellow_orange`, `red_brown`, `pink`,
`blue_purple`, `green`, `black`, `grey`)
- Capture the phrase between the count ("up to eight", "a single", "one (rarely two)") and
  "flower(s)". The count may be introduced by "and", "with up to", "of up to",
  "comprise up to" or "producing up to".
- Drop rare variants ("(rarely white)", "more rarely white") and markings ("brown marked",
  "red-striped", "blotched", "suffused", "-tinged").
- For hyphenated compounds, map only the head colour: "greenish-yellow" → `yellow_orange`,
  "creamy-white" → `white_cream`, "purplish-red" → `red_brown`.
- Turn "straw-coloured" into "straw coloured" before stripping, and strip "glossy-".
- Strip non-colour adjectives from the description column (sizes, "translucent",
  "self-pollinating", scent words).

**Flower size and count**
- Use `\bflowers?\b` with word boundaries; plain "flower" also matches "(when flowering)" and
  picks up the leaf width that follows it.
- "mm across" on spider-orchid-type flowers is the tip-to-tip span of long petals, so
  100–220 mm is real. Don't "correct" it; say so in the `flower_diameter` methods.
- `flower_count_maximum` takes the largest number in the count phrase, including any
  "(rarely to N)".

**Leaves**
- Extract the leaf phrase "with … leaf/leaves" from the first sentence.
- Hairs:
  - "smooth" / "glabrous" / "almost hairless" → `glabrous`
  - "hairy" / "scarcely hairy" / "sparsely hairy" → `hairy`
  - "smooth margined" says nothing about hairs; exclude it.
- Shape:
  - "heart-shaped" → `cordate`, "tubular" / "terete" → `terete`, "rounded" → `orbicular`,
    "oval" → `elliptical`
  - "narrow" → `linear`, but only when no more specific shape word is present.
- Colour: map only the main leaf colour, never tinges ("often basally reddish-purple",
  "red backed").
- "Withered/shrivelled at the time of flowering" (or "when plants are in flower") →
  `leaf_phenology = withered_at_flowering`. This level was added locally to traits.yml.
  Species-level "often"/"usually" is OK here.
- Leaf counts: number words → numerals; "a/one/single leaf" → 1. Exclude cauline counts from
  the main count (negative lookahead). `leaf_count` was added locally to traits.yml.

**Post-fire flowering** (search all fields; ignore "firebreak/graded track" sentences)
- "flowering best", "greater profusion", "most prolifically", "particularly/especially common
  after fire" → `fire_enhanced_flowering`
- "only in the season following fire", "rarely flowers in / rare or absent in unburnt",
  "predominantly after fire" → `fire_dependent_flowering`
- "does not require (summer) fire to flower" or "equally well in burnt and unburnt" →
  `fire_independent_flowering`. But "does not require fire **but** flowers in greater numbers"
  → `fire_enhanced_flowering`.
- If the response differs by area ("in some areas only…, in others every year") →
  `fire_dependent_flowering fire_enhanced_flowering`.
- "More easily seen after fire" is about visibility, not flowering: keep the description and
  leave the mapping blank.

**Pollination and breeding** (explicit text only)
- "self-pollinating" → `pollination_syndrome = self`, `breeding_system = autogamy`. Add
  `cleistogamy` when the text says "rarely opening" or "cleistogamous".
- Watch for negation: "insect pollinated rather than self-pollinating" →
  `pollination_syndrome = insect`.
- "nectar producing glands" or "nectary spur" → `flower_nectar_production = nectar_produced`.
- Put morphology cues (hinged / irritable / insect-like labellum, temperature-sensitive
  opening) in `pollination_description` only. Temperature-sensitive opening is **not** a
  `flowering_cues` value.

**Scent**
- Match lowercase and case-sensitively, so that common names ("Scented Sun Orchid", "Fragrant
  China Orchid", "Lemon-scented …") are skipped. Use the lookbehind `(?<![A-Za-z-])`.
- Any scent phrase, pleasant or not → `flower_scent_production = scent_produced`.

**Clonality**
- "colony-forming", "clonal", "daughter tubers", "vegetative reproduction" →
  `vegetative_reproduction_ability = vegetative`, plus `clonal_spread_mechanism = clonal`
  (or the named organ, e.g. `stem_tuber`).
- "Grows in clumps" / "clumping habit" is **not** vegetative reproduction. It is unresolved
  whether it is an individual growth form or a cluster of individuals, so keep it as
  `clumping_description` text only. Exclude "spinifex clump".

**Other traits**
- Substrate:
  - "epiphytic", "on trees", "unknown host" or "tree hosts" → `epiphyte`; "lithophytic" →
    `lithophyte`.
  - Soil words in the habitat text → `terrestrial`.
- Photosynthetic organ: "non-green", "lack chlorophyll", "leafless saprophytic" or
  "entire life cycle below the soil" → `plant_photosynthetic_organ = non-photosynthetic_plant`.
- Leaf length type: "leafless" → `leafless`; "leaves reduced to bracts" → `scale_leaves`.
- Storage organ: "pseudobulb" → `pseudobulb` (plus dimensions on `entity_measured` rows);
  "tuber" / "stem tuber" → `tuber` / `stem_tuber`; succulent rhizome → `rhizome_fleshy`. Don't
  score `rhizome` when it is just the creeping stem carrying pseudobulbs.

**Habitat** (the user will define a habitat trait later; use the source's own terms)
- `distribution_description`: the range text before "growing …".
- `habitat_description`: all "growing in/on …" clauses plus other habitat sentences, minus
  fire sentences and extra-WA distribution text.
- `soil_terms`: words before soil/sand/loam/clay (sandy; sandy-clay; lateritic; granitic;
  peaty; loamy …). Keep "sand over limestone/granite/laterite" as one term.
- `habitat_terms`: the clause after "soils in/on …", split on commas and "and".
  - Strip qualifiers ("often", "also", "more rarely", "occasionally", "in", "a").
  - Harmonise spelling ("Mallee" → "mallee", "seasonally-damp" → "seasonally damp",
    "runoff" → "run-off").
- Then list every taxon with a term used fewer than 3 times, together with its full source
  text, and hand-curate the broken ones into the override CSV. Expect about 15% of a flora.

## Free-prose profiles (WildNet-style)

Prose descriptions have no formula sentence, so the fact-sheet regexes don't transfer. What
worked:

- **Sentence splitting must protect abbreviations** ("A. baueri", "subsp.", "c. 14 cm"), and
  measurement parsing works best on **clauses** (also split on ";").
- **Drop comparison sentences** before scoring morphology ("differs from…", "similar to…",
  "distinguished from…", or any other abbreviated binomial than the taxon's own), so another
  species' characters aren't attributed to this one.
- **Organ measurements need a subject rule.** Take a size only when the organ is the clause's
  subject (e.g. "The flowers are 4–5 mm long"; not "The inflorescences have few flowers, and
  are 3–6.5 cm long"), and reject a number when another organ is named just before it
  ("on stalks 5 mm long", "central axis 5–14 cm long", "a 2–4 cm long rachis"). Leaflet
  mentions anywhere before a number mean it's a leaflet size, not a leaf size.
- **Unit traps:** "0.8 mm" can be read as 0.8 "m" + "m" unless the lower-bound unit is only
  allowed when a range follows. "0.9 to 1.2 by 0.7 to 1 mm": the first dimension takes the
  second's unit. Strip parenthetical extremes "(3-)5 to 12 (-15) mm".
- **Colour:** anchor colour words with a left word boundary ("flowered" contains "red"), allow
  decimals inside the window ("2.5 mm … deep purple"), ignore colours that directly qualify
  another organ ("release fine white seeds"), and strip citations ("(White, 1936)").
- **Flowering/fruiting time from prose:** split each Reproduction sentence into flowering vs
  fruiting clauses by keyword ("flowers and fruits" counts for both; "flower buds" for
  neither). Expand ranges cyclically ("December to March", "between the months of X and Y",
  "late November to early January", "X, rarely to Y"); seasons are southern-hemisphere
  (spring = Sep–Nov) with early/mid/late = first/middle/last month; "throughout the year" →
  all months, but "most of the year" → leave blank. A hedge word ("probably", "possibly",
  "thought to be", "may also") voids the rest of its sentence. No fruiting time for cycads or
  conifers (cones, not fruit).
- **Ecology traits are vetted, not regexed.** Pollination, dispersal, fire response, seed bank,
  germination, clonality, defences and lifespan are too varied: script the verbatim
  `*_description` columns, then score values in commented per-taxon `tribble`s. Stated →
  `pollination_vector_known`; hedged → `pollination_vector_possible`; hedged dispersal / fire /
  breeding statements stay text-only. Group-level hedges ("Like other Lysiana spp., …
  probably", "Most cypress pines are …") and ambiguous breeding systems ("flowers bisexual or
  male") stay unscored. QA that every scored value has a description.
- **Watch for copy-paste errors in the source**: a profile describing another species
  (Livistona fulva's text about L. nitida). Don't score; list as a question.
- Keep habitat and distribution prose verbatim plus `soil_terms`; there's too little formula to
  split prose into `habitat_terms`.

## Ferns and organ-led descriptions (LucidFerns-style)

User's rules for ferns: **stipe = petiole, frond / lamina = leaf, pinnae / pinnules / segments =
leaflets**; record which division each size/count belongs to (Wenk_2025 contexts above).

- **Parse clause by clause, tracking the current organ.** Each clause led by an organ noun sets
  the current division (rhizome, stipe, fronds, lamina, "<qualifier> pinnae/pinnules/segments/
  lobes", leaves, branches); clauses led by "margins", "apex", "upper surface", "veins" inherit
  it. A subject-less "longest 13–60 cm long" belongs to the division just described ("longest
  primary pinnae"). Keep the source's qualifier words in the `leaf_type` value.
- **Don't protect "diam." / "mm." from sentence splitting**: they often end a sentence, and
  protecting them merged "…4–8 cm diam. Fronds to 2.6 m long" into the rhizome clause.
- Accept "10–82 cm or more long"; read "Fronds 70–130 by 10–12 cm" as dimensions, not a frond
  count; a stipe length inside a frond clause ("Fronds with a winged stipe 5–8 cm long") →
  `petiole_length`.
- **Pinna counts in pairs**: keep pairs as stated in a single range column (`leaflet_count_pairs`
  = "15--25") with `unit_in: pairs`; config/unit_conversions.csv converts pairs → `{count}` (x2).
  Counts of individual pinnae ("with 27–65 pinnae") go to `leaflet_count`.
- **Height**: tree-fern trunk heights (Cyatheaceae, Dicksoniaceae, "forming a trunk") →
  `plant_height`, organ = trunk; other erect-rhizome heights → `stem_length`, organ = rhizome;
  lycophyte "branchlet systems … tall" → `plant_height`. Where no explicit height exists, frond
  length is also recorded as `plant_height` with `plant_organ_measured = fronds` (the user's
  choice, following Wenk_2025).
- **Rhizome**: `rhizome_form` (short_creeping, long_creeping, slender, stout, branched, woody;
  "short, erect" is *not* short_creeping) plus `stem_growth_habit` = `rhizomatous` + erect /
  creeping / climbing / decumbent / stoloniferous, as Wenk_2025 did.
- **Substrate** from the opening of "Habit and habitat", in text order, with qualifier rows.
  Catch source typos ("Litophytic", "Lithophyhytic", "somtimes"); "semi-aquatic" →
  `semiaquatic`; "grows on boulders", "mats on rocks", "damp crevices" → lithophyte; "on rocks
  and on trees" → + epiphyte; soil-only wording → terrestrial.
- **Frond traits**: `leaf_lamina_division` (1-pinnate → pinnately_compound, 2-pinnate →
  bipinnate, 3-pinnate → tripinnate, pinnatifid, bipinnatifid, trifoliate, palmatifid →
  palmately_lobed; "3-pinnatifid" and "4-pinnate" have no level, so flag); expand "2–3-pinnate"
  to both degrees; `leaf_compoundness`; dimorphic / monomorphic fronds → `leaf_heterogeneity`
  anisophyllous / isophyllous; tufted / crowded / scattered → `leaf_arrangement`. Map every
  apex term in the phrase ("apex acute to acuminate" → `acute acuminate`).
- **Growth form** is definitional by family: fern, lycophyte (Lycopodiaceae, Selaginellaceae,
  Isoetaceae), tree fern → `fern palmoid`; climbing ferns add `climber_herbaceous`.
- **Clonality**: stoloniferous → `stolon`; bulbils / proliferous fronds / frond tip rooting into
  a plantlet → `aboveground_clonal`; "roots proliferous" / proliferous root tubers →
  `root_buds`.
- Sori, indusia and spores have no AusTraits traits; keep them as description text.
- Join context rows to the taxon list with an **inner** join (a left join adds a blank row for
  taxa with no measurements).

## QA checklist (run every iteration)

- Numeric columns: n, range, and any rows where min > max (usually a source typo; fix it
  explicitly).
- **Misses:** rows where the source text contains the pattern ("mm high", "long by",
  "mm across", a count word) but the column is NA. Print the text and widen the regex.
- Unique `description → mapping` tables for colour, leaf, fire, scent, substrate and
  storage. Read them all. This is where common-name hits, tinges, "Herb Foote"-style name
  collisions and negations show up.
- Spot-check outliers against the source before calling them errors (big spider orchids are
  real).
- Check for duplicate (taxon × contexts) keys, a 12-character `flowering_time`, and no
  digit–digit hyphens left in the file.
- Read the full flowering/fruiting table next to the source sentences; that's where range
  and hedge bugs show up.
- List clauses that contain a measurement but produced no value, grouped by leading word;
  most leftovers should be scales, sori, spores, hairs or spacing ("0.2 mm apart").
- Check that every mapped trait exists in the working branch's `config/traits.yml`.
