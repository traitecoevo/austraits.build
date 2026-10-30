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
- **Parenthetical calendar extremes** ("Flowers (Oct.–) Dec.–Feb. (–July)"): typical months on a `usually`
  row, the extra extreme months on a `rarely` row (user's choice).
- **Qualified "tree-like"** ("Shrub to 2 m high, or rarely tree-like to c. 5 m") → tree on a `rarely` row
  (shrub `usually`); unqualified "tree-like" is still not a tree.
- Climber "stems to 10 m long" → `stem_length` (traits.yml allows it for climbers).
- **"at any time but mainly X"** → X on a `mainly` row + an all-year row with `occasionally` (user's choice).
- **Cues (user's rule): always document wording that flowering, fruiting or germination is triggered by rainfall,
  fire, smoke, floods / water levels, temperature.** `flowering_cues` (fire, floods, rain, rain_summer …) +
  verbatim `flowering_cues_description`; `germination_cues` (now a trait: burial, disturbance, fire, floods,
  heat, light, rain_*, smoke, temperature_cool/warm; issue #863) and `fruiting_cues` likewise; metadata.yml maps
  `fruiting_cues` to the trait `flowering_cues` (user's choice).
  Nursery/seedbank protocols (scarify, hot water, stratification) go to `seed_germination_treatment`. Hedged
  ("probably in response to rain") → description only. Field seedlings/germination after fire → also
  `post_fire_recruitment`.
- **Climbers:** "vine" = `climber_herbaceous` unless stated woody; "liana" = `climber_woody`; "twiner" stays
  `climber` (twining is kept in plant_climbing_mechanism / stem_growth_habit).
- **Growth form by family when the text names none:** Arecaceae -> `palmoid`; ferns -> `fern`;
  Centrolepidaceae -> `graminoid herb`. "with woody base" -> `subshrub`; "woody perennial" says nothing.
- **Species -> infraspecific inheritance:** a subspecies/variety/form silent on a trait inherits its species' own
  values (categorical and numeric), `trait_scoring_method = inferred_from_species`. Annual/perennial -> herb is
  NOT applied (user deferred).
- **Rosette** is both `stem_growth_habit` and `leaf_arrangement`: map to both wherever either is found.
- **Tussock-only growth form** (no herb / grass noun, e.g. Lomandra "Tussock c. 10 cm diam.") → add `graminoid`;
  Lomandra "Plant tufted" with no form noun → `graminoid`.
- **Very large sources (e.g. FoA online, 20k profiles; ABRS on the flora branch):**
  one general organ-led parser, run family by family (sorted family → genus → species); stop
  after each family (or ~200 rows of small families) for the user to check. Extract all traits,
  including candidate traits not yet in traits.yml, under descriptive names.
  - **Keep each data.csv under ~90 MB** (GitHub limit 100 MB): ABRS is split by family into
    `ABRS_AtoM_2026`, `ABRS_NtoZ_2026` and one dataset each for Asteraceae, Fabaceae, Myrtaceae,
    Proteaceae (user's choice, 2026-10-19). Dataset IDs must be valid R names (no hyphens: the
    targets pipeline makes them object names). One parser (in ABRS_AtoM_2026/raw) writes all of
    them; each file drops its all-empty columns but keeps every column its metadata.yml maps.
  - Very large families are done genus by genus (`genera_done`, env `FOA_GENERA`); check in with
    the user every ~500 taxa.
- **Inferred typical values (user's rule, 2026-10-19):** traits normally stated only when atypical
  get the typical value on a `trait_scoring_method = inferred` row when nothing suggests otherwise:
  `plant_growth_substrate` terrestrial (woody plants with no aquatic / epiphytic / lithophytic / marine
  / stem-parasite wording; herbs also need habitat text with no wetland words; not aquatic or
  epiphyte-rich families), `plant_photosynthetic_organ` leaf (leaves described; no leafless, scale,
  phyllode, cladode, photosynthetic-stem or achlorophyllous wording), `parasitic` not_parasitic (green
  plant outside parasitic families / genera, no parasitism or host named), `leaf_type` broadleaf
  (flat-lamina shapes, or leaves >= 3 mm wide, or a divided fern lamina; no terete / needle / involute).
- **Fabaceae (ABRS_Fabaceae_2026; user's choices 2026-10-19):** written in LONG format (user: long makes edits more
  traceable in Git and value-level contexts easier to trace; NtoZ is long since 2026-10-22, Myrtaceae is to be
  converted to long after Fabaceae is complete, AtoM stays wide for now because of file size), in 500-taxon batches (`partial_n`), parsing gated by `legume_mode` so other files stay
  byte-identical. Long-format value_type: single measurement = mean, single count = mode, "to X" maximum, "X or
  more" minimum; parenthetical extremes are `measurement_range = extreme` rows, filtered out in custom_R_code.
  - Phyllodes: `plant_photosynthetic_organ` phyllode; `leaf_type` from the cross-section (flat → broadleaf;
    terete / quadrangular / filiform / acicular → needle); flat phyllodes count for the inferred broadleaf rule;
    phyllodes reduced to scales / aphyllous → stem + scale / leafless.
  - Foliar glands (phyllode, petiole, rachis, between leaflets) = extrafloral nectaries: candidates
    `extrafloral_nectaries`, `_position`, `_count`, `_distance_above_pulvinus`.
  - Seed outline → 3-D `seed_shape`: oblong / elliptic → ellipsoid, ovate → ovoid, circular / globose → globoid,
    `_flattened` when compressed. Fleshy funicle → `dispersal_appendage` aril. Seeds harvested by ants → dispersers ants.
  - "On granite rocks", "in crevices", "granite outcrops" → terrestrial on a CHECK-flagged inferred row (not
    lithophyte); cliffs → plain terrestrial; rock names in Habitat → candidate `habitat_geology`.
  - "Generally killed by fire but can regenerate from the base" → fire_killed (usually) + resprouts (sometimes)
    + bud_bank_location; "salt-tolerant" → halophyte_moderate, "highly saline soils" → halophyte, sea spray →
    salt_spray_tolerant; allelopathy → candidate `allelopathy`; d.b.h. → `plant_diameter_breast_height`.
  - "Branchlets … aphyllous" beside a described phyllode: only the spiny branchlets lack phyllodes (not leafless).
  - Papilionaceous corolla: standard / wing / keel sizes → `flower_petal_length` / `_width` on `entity_measured`
    rows (standard, wing petals, keel petals); the standard's basal "flare" colour → candidate `standard_flare_colour`,
    not `flower_colour`; "Pistil N mm long" → candidate `pistil_length`.
  - "Regenerates (prolifically) from seed following fire" → `germination_cues` fire + `post_fire_recruitment`;
    "dense regrowth populations following fire" is resprouting wording, "regeneration … after fire" of a seeder is
    recruitment. Editor-queried resprouting ("Tolerant to fire [?resprouting from rootstock]") → resprouts, with
    `bud_bank_location` on a `trait_scoring_method = uncertain` row.
  - Drift seeds / "recorded as flotsam" → `dispersal_syndrome` hydrochory + `dispersers` water_currents.
- **Orchids (FoA Orchidaceae = Jones 2021):** growth form herb by family; substrate read from the
  Habitat text ("on trees, boulders" → epiphyte lithophyte, in text order, qualifiers from the same
  comma segment; "in ... sandy soil" → terrestrial; region only for sentences opening "On Lord Howe
  Island ..."); "Flower stem N mm tall/long" → both `plant_height_reproductive` and `inflorescence_length` for
  terrestrial orchids, `inflorescence_length` only for epiphytes, lithophytes and reed-stem orchids with a
  stated height (user's choice); dorsal
  and lateral sepals → `flower_sepal_length/width` on `entity_measured` rows; labellum length from
  "L × W" (lamina, not stalk / spur); labellum and callus colours kept out of `flower_colour`; callus,
  column, labellum stalk and sepal clubs are their own organs. Pollinators, self-pollination
  (`self` + `autogamy`, + `cleistogamy` when flowers do not open), nectar, scent, tubers / daughter
  tubers (`stem_tuber`) are vetted from Ecology text (genus statements → `inferred_from_genus`).
  `orchid_mycorrhizal` is not copied from the family description (user: a separate dataset); non-pollination, non-dispersal
  mutualisms go to the candidate trait `mutualisms_with_animals` (user, 2026-10-19; draft values ant_housing,
  ant_defence, ant_food_rewards), only when stated (leaf domatia alone are morphology, not a stated mutualism).
  - ABRS_NtoZ_2026 is written in LONG format from the second Orchidaceae batch on (user, 2026-10-22).
  - ABRS_Myrtaceae_2026 was converted from wide to LONG format (user, 2026-10-29): only ABRS_AtoM_2026 is still wide.
    The metadata is regenerated by a make_meta script (one block per trait, `value_type` read from the data); a
    wide -> long value check matched every mapped value (single counts are `mode` rows in long format).
  - Terrestrial orchid genera (genus description opens "terrestrial orchids"): rock / outcrop / boulder Habitat
    words → terrestrial (term flagged "(terrestrial orchid genus)"), rotting logs / stumps / tree roots → no
    substrate; explicit "lithophytic" / "epiphytic" wording and living trees still count.
  - Caladenia-type sepal clubs ("sepals and petals with brown clubs 5–15 mm long") → own clause
    (`sepal_club_length`, `sepal_club_colour`), never tepal length or flower colour; "Lamina calli", "Basal
    calli" → callus (not leaf lamina); the column's "basal glands" → organ `column_gland`; "Labellum …, lamina
    ovate" → labellum lamina; "(when flattened)" is not a shape.
  - Subspecies diagnoses ("Differs from … by its smaller flowers (20–30 mm across), … narrower labellum
    (9–15 × 12–15 mm)") are rewritten as organ-led sentences; comparatives dropped, items naming no organ dropped.
  - "Leaf (often) withered at flowering" → `leaf_phenology` withered_at_flowering (with qualifier).
  - "Flowers freely only in wet years", "poorly in dry years", "in years of good rainfall" → `flowering_cues` rain
    (temperate "wet seasons" are rain, not the tropical wet season → rain_summer); "flowers freely after fire" →
    fire_enhanced_flowering; "strongly inhibited following fires" → fire_suppressed_flowering (and no fire cue).
  - Pollination vetting: "attracted by sexual deceit", pseudocopulation, "deceived into visiting for nectar" →
    `flower_pollinator_reward` deceit; nectar on the labellum → nectar; thynnine wasps → `pollination_syndrome` wasp +
    `flower_visitor` thynnine_wasp; scoliid wasps → wasp + wasp; sawflies → hymenoptera; visitors photographed
    carrying pollinia / pollen → `pollination_vector_possible`. A genus "Most species …, but one species, C. x,
    is self-pollinating" is copied to every species except the named one (flagged as a question). "Apomixis" →
    `breeding_system` apomixis.
- **Flower lifespan** ("flowers last 1–3 days", "each flower lasting a single day") → `flower_lifespan`
  (days); **time of opening** ("open during the day, closing at night", "day-flowering", "opening in
  the morning") → candidate `flower_opening_time` (day / morning / afternoon / evening / night).
- **Sunk genera appended to a genus profile** ("... Gaura L.; from the Greek ...: Annual or perennial
  herbs ...") must be split off (fix_text → "Now includes Gaura: ") so their characters are not copied
  to the whole genus.
- **Extract every documentable character, not just existing traits** (user rule, 2026-10-08):
  - hairiness of every organ (stem, leaf surfaces, bracts, calyx, corolla, ovary, fruit)
  - every count (petals, sepals, tepals, staminodes, carpels, styles, style branches, locules, ovules, bracts)
  - every shape, colour, texture, surface, orientation, fusion and persistence
  - sizes of organs with no length or width trait yet (stipules, bracteoles, ovary, petal and sepal width, ...)

  For a character with no traits.yml trait, write `<organ>[_<part>]_<character>_description` (the
  source words) next to `<organ>[_<part>]_<character>` (a suggested value from a draft vocabulary).
  The levels are refined once many families are done. In the ABRS parser this is the "generic organ ×
  character matrix" block of `raw/build_data_csv.R`, with vocabularies `gen_*_dict`, `gen_counts`
  and `gen_sizes`. Pitfalls:
  - Strip phrases naming another organ before scanning ("enclosed in the persistent calyx",
    "leaving a persistent columella").
  - "Style 3-branched" counts style branches, not styles.
  - "distinct" is not a fusion term.
  - Never name dplyr loop variables after unit columns (`si`, `ci`).
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

## Rules added during the ABRS N–Z run (user's answers, 2026-10-28)

- **Never drop region-, country- or cultivation-specific values.** Put them on their own rows with the region (source
  wording) as `population_region` ("Kimberley region of W.A.", "New Zealand", "in cultivation"); values for the whole
  species / Australia stay on a row with no region. Applies to calendars (`phenology_overrides.csv`) and ecology
  (`ecology_vetted_regions.csv`).
- **Calendars:** a bare parenthetical month ("Flowers (April) October–January", "Aug.–Dec. (May)") is a `rarely` row;
  "mainly / chiefly / commonly / peak" windows get their own commonness rows; "persisting until X", "extending to X",
  "as late as X" make a continuous range; "all year but not recorded in X" leaves X out; "Fertile collections made …",
  "Florets observed …" = flowering_time; tropical "dry season" = May–Oct, "wet season" = Nov–Apr; hedged months
  ("apparently Dec–Mar", "or possibly all year") are kept on their own rows with the hedge word as the
  `scoring_certainty` context (user, 2026-10-29); a hedge about the cause ("probably in response to rain") leaves the
  months stated.
- **Grasses (Poaceae; growth form and height rules also Cyperaceae, Juncaceae, Restionaceae):** growth form `graminoid`
  (never `herb`), `tussock graminoid` for tussock / tufted / caespitose, `hummock graminoid` for hummock; culm / plant
  heights to **both** `plant_height` and `plant_height_reproductive` (bamboos: plant_height only). Grass organs are
  candidate traits (glume, lemma, palea, awn, awn column, callus, callus hairs, ligule, lodicule, rachilla, coma,
  florets, culm internodes; lower / upper glume, lemma and lateral / median awn on `entity_measured` rows); caryopsis
  length = fruit_length; awns and callus hairs are **not** `dispersal_appendage`. Inflorescence axes / branches /
  internodes are inflorescence parts, not stems.
- **Restionaceae:** photosynthetic organ `stem` (family statement), never an inferred `leaf`; "wind-pollinated" →
  pollination_syndrome wind; "killed by fire" / "obligate seeder" → fire_killed; "regenerates by seed after fire" →
  post_fire_recruitment + germination_cues fire.
- **Ecology mappings:** "tolerant of fire" / "fire-tolerant" = `resprouting_capacity` resprouts; seed-eating animals =
  `dispersers` on a `trait_scoring_method = inferred_from_seed_predation` row (no dispersal_syndrome); "fruit eaten by X"
  = dispersers + endozoochory; vivipary (seed germinates on the plant) = `dispersal_unit` seedling; pseudovivipary
  (plantlets instead of seed) = `clonal_spread_mechanism` viviparous; resurrection plants = candidate
  `desiccation_tolerance` desiccation_tolerant; root suckers = root_buds, unspecified suckers = clonal; stems rooting at nodes = adventitious_roots; colonies from "stoloniferous roots" = root_buds; a tumbleweed (whole plant blown) = dispersers wind + anemochory; "dying back to the rootstock in drought" = resprouts_after_drought + root_crown.
  `flower_visitor` levels are singular (bee, bird, fly, beetle, honeyeater): validate every vetted value against
  traits.yml.
- **Copy-down:** a broad family term ("fragrant") or a comma list of alternatives ("herbs, epiphytes, climbers, shrubs
  or trees") is not a value for every member: add it to `copydown_exclude`.
- **Inferred defaults** (terrestrial / leaf / not_parasitic) unless the text names a special case: the broad water /
  epiphyte / rock test reads the habit sentence + Habitat / Ecology / Notes (morphology only for unambiguous wording;
  "midrib submerged", "on secondary branches" are not habitats); herbs without Habitat text get the default; plants of
  swamps / marshes (herbs and woody plants alike) are inferred `terrestrial swampy` (`swampy`: a substrate level added
  2026-10-30, replacing `semiaquatic` for swamp plants in every dataset; explicit "semi-aquatic" wording stays semiaquatic); other wet wording (creek beds,
  lake margins) leaves herbs unscored. Habitat
  text is kept verbatim as `habitat_description`.
- **Source unit typos:** fix in `fix_text` only with evidence (a later description, or the profile's own diagnostic
  features), and flag them; e.g. Austrostipa heights given in m for dm.
- **Measurements:** "to c. 0.5 m" is a maximum; an extreme in its own unit ("to 40 cm (–1 m) across") is converted to the
  main unit; an underground organ's size (rhizome, tuber, corm …) is not plant width.
- **Build:** set every family mode at the top of `extract_one()` (copy-downs otherwise depend on how taxa are split
  between workers); an `FOA_DATASETS` run can drop copy-downs from genus profiles filed under another dataset's family,
  so always finish with a full build (only the ABRS_*_2026 datasets, never AusTraits) before committing. While
  working a batch, build only the dataset it changes (`FOA_DATASETS=NtoZ`); the ~25-minute full build is the
  pre-commit step.
- **Post-fire recruitment:** a population response to fire is `post_fire_recruitment`, even when no seedlings are
  named: "common / abundant after fire", "appearing (often) after fire", "early coloniser after fire", "colonising
  species common after fire or disturbance", "often found following fire", "common in recently burnt scrub", "fire
  ephemeral", "regenerates prolifically following fire" (short-lived species). Keep often / usually / frequently /
  commonly as `commonness_qualifier`. Not resprouting statements or orchid visibility / flowering after fire. Hedged
  ones ("apparently", "reportedly common after fire") are scored with the hedge word as `scoring_certainty` (user
  2026-10-29); "common" is abundance, not a commonness qualifier.
- **Seed regeneration after fire** ("regenerates (after fire) by seed") gives `post_fire_recruitment` plus
  `germination_cues` = fire.
- **Disturbance:** germination, establishment, colonising or regeneration from seed following disturbance (roadworks,
  clearing, forestry, "a disturbance event") gives `germination_cues` = disturbance. Give "fire or other disturbance"
  both cues.
- **"Any time of (the) year" / "any month" / "non-seasonal"** gives all 12 months, even when hedged with "may" or
  "could". With a main window ("May–October, but could occur any time"), the window goes on its own row and the
  all-year row is `occasionally` (or the source's own word, such as "sporadically"). "Much of / most of the year" stays blank.
  "Sporadic records outside these months" gives the remaining months a `sporadically` row; "all months except
  Dec.–Feb." is Mar–Nov, never all year.
- **Cycads and conifers:** "Seed cones …", "Pollen cones …", "Male / female cones …" are their own organ, never the
  seed; the seed sizes come from the "Seeds …" sentence. Cone sizes are the candidate traits `seed_cone_length`,
  `seed_cone_diameter`, `pollen_cone_length` and `pollen_cone_diameter` ("female / ovulate" = seed, "male" = pollen),
  and the cone text is kept verbatim in `cone_description` (user, 2026-10-29). Seed-ripening and cone months are
  `fruiting_time`, with `trait_scoring_method` = inferred_from_seed_ripening, inferred_from_cones or
  inferred_from_pollen_cones. "Cone formation often follows fire" is `flowering_cues` fire (often, inferred_from_cones).
- **Proteaceae (ABRS_Proteaceae_2026, long format from the start, user 2026-10-29):**
  - Informal groups filed as species ("Acacioides Group", "Banksii Subgroup") are not taxa and are left out.
  - Candidate organs: pollen presenter (`pollen_presenter_length`, `_shape`, `_surface` …), `pistil_length`,
    `perianth_limb_length` ("limb 3 mm long", "including limb of 3–4.5 mm") and `fruits_per_infructescence`
    ("Follicles up to 6 per head"). A fruit's "high" beside a length is `fruit_height`.
  - Fire, scored as in Myrtaceae:
    - "Killed by fire, regenerating from seed" → `fire_killed` + `post_fire_recruitment` + `germination_cues` fire.
    - "Fire-tolerant" or "sprouting from …" → `resprouts` + `bud_bank_location`: lignotuber = basal_stem_buds,
      trunk / branches / epicormic = epicormic_buds, rootstock = root_crown.
    - Region-specific fire responses go on `population_region` rows.
  - Serotiny (user 2026-10-29):
    - "follicles (usually) opening with fire" → `serotiny` serotinous, with the qualifier;
    - "remaining closed until burnt" or "opening only with fire" → serotiny_high;
    - both kinds stated ("spontaneously or with fire", "with or without fire", "opening without fire after several
      years", "some open after 2–3 years, the others remain closed until burnt") → `serotinous not_serotinous`.

    Each also gives `seedbank_location` canopy_seedbank.
  - "Mycorrhiza are not found in Proteaceae" (family Notes) → `mycorrhizal_association_type` non_mycorrhizal,
    inferred_from_family. Proteoid roots ("many taxa") are not scored (user).
  - Lignotuber presence alone is `storage_organ` only, never resprouting (user). Hedged lignotubers
    ("?lignotuberous", "possibly lignotuberous", "apparently lacking lignotubers", "lignotuber apparently absent")
    are kept with `scoring_certainty`.
  - Grevillea "Regenerates from …" statements are fire responses:
    - seed → `post_fire_recruitment` + `germination_cues` fire; "seed only" → also `fire_killed`;
    - an organ → `resprouts` + `bud_bank_location` (lignotuber basal_stem_buds, epicormic epicormic_buds, rootstock
      root_crown, root suckers bud-bearing_root, basal suckers / shoots basal_buds, rhizomes rhizome);
    - "after floods" → `resprouting_capacity_non_fire_disturbance`;
    - "sometimes", "mainly", "(at least in some populations)" are qualifiers; "?", "possibly", "probably" are
      certainties.
  - Hakea habit wording: "resprouting from base" → resprouts + basal_buds; "non-sprouting" or "not resprouting" →
    fire_killed; "resprouting or non-sprouting" → `resprouts fire_killed`; "suckering from horizontal roots" →
    clonal root_buds. A sterile species "reproducing only by suckering" → `vegetative_obligate`. Fire records
    that a note gives for another named species (Groom & Lamont: "H. horrida is a nonsprouter") are scored on
    that species.
  - Hedged statements (fire, pollination: "Presumed insect-pollinated", "Probably bird-pollinated") are scored with
    the hedge word as `scoring_certainty` (user 2026-10-29: hedged fire statements; pollination by extension).
  - Conospermum "X tall in flower, Y tall when vegetative" → `plant_height_reproductive` X, `plant_height` Y.
- **Asteraceae (ABRS_Asteraceae_2026, long format, user 2026-10-29):** the capitulum (head) is the inflorescence.
  - Floret counts are `flowers_per_inflorescence`, with the floret type on `entity_measured` ("ray florets",
    "disc florets", "female florets", "functionally male florets" …); "Capitula 3–8 per …" / "solitary" is
    `inflorescence_count`. Involucral bract counts are `involucral_bract_count`; ligule sizes `corolla_ligule_length`
    / `_width`; pappus `pappus_length`, `pappus_element_count`; receptacle `receptacle_diameter`.
  - Achene sizes of a named type ("ray achenes …", "outer achenes 5–8 mm long …, inner achenes 10–16 mm long",
    "marginal / central achenes") go on `entity_measured` rows; the general fruit size comes only from the text before
    the first type. A bracketed type ("2-angled (disc achenes) or 3-angled (ray achenes)") only labels a shape. A
    typed achene measured only as its body below the beak gets "[achene body]" in the value description.
  - The "lamina" of a dissected leaf ("3–4-pinnate …; lamina 4–20 cm long") is the whole blade, never a leaflet.
  - Rulings at the batch-1 check-in (user 2026-10-29):
    - "vulnerable to periodic hot fires" → `fire_killed`;
    - heads dispersed whole ("capitula fall as a unit", "spiny clusters of capitula … rolled or blown") →
      `dispersal_unit` infructescence (a new level, not yet in traits.yml);
    - "common on roadsides where it may benefit from regular disturbance" → `germination_cues` disturbance +
      `seedling_establishment_conditions` establish_post_disturbance (possibly);
    - variant-specific calendars ("Lowland plants of this variant are distinctive for their spring flowering") go on
      `population_region` rows; dormancy wording ("summer dormancy", "winter-dormant") is too ambiguous to score;
    - receptacle paleae are `receptacle_palea_*` (names to be reviewed by the user after the scraping).
- **ABRS_2022 / ABRS_2023 reconciliation rulings (user 2026-10-30)** — 2026 follows the earlier extracts' conventions:
  - **Inferences between traits** (rows with trait_scoring_method = inferred_from_growth_form / inferred_from_life_history,
    only where the taxon has no value of its own or copied): life_history perennial for trees, shrubs, mallees, palmoids,
    woody climbers, geophytes (a generic climber may accompany them); woodiness_detailed from growth form (herb, graminoid,
    fern, tussock, hummock, herbaceous climber, geophyte → herbaceous; tree, shrub, mallee, woody climber, palmoid → woody; both
    for mixed forms; subshrubs and generic climbers none; a tree fern is a fern); plant_growth_form herb from a stated life
    history when no growth form is named. woodiness_detailed is also scored from the habit text (woody base / suffruticose →
    woody_base, woody rootstock / taproot → woody_root, soft-wooded → semi_woody).
  - **Genus / family copy-down:** a commonness word governs what follows it in its segment, not what precedes ("Fruit a
    capsule with a usually woody hypanthium" copies capsule; "Leaves alternate or rarely opposite" copies alternate); a
    bracketed qualifier "(mostly)" goes with its brackets; a group sentence naming a subset ("but some species …", "in most
    species …") keeps what precedes that segment; a leading "usually" governs the whole list only when no later item has its
    own commonness word; "simple or palmately lobed" is simple + lobed-or-not; "lamina simple" is leaf_compoundness simple;
    drupe and pyrene do not conflict. Myrtaceae petal / sepal orientation is not copied (operculate eucalypts).
  - **Fruit colour:** reddish brown / red-brown / rufous → `reddish_brown` (new level in traits.yml).
  - **Diameters:** "X mm diam." of a fruit or seed is its width and height; of a globose fruit or seed with no length also
    its length (all three dimensions).
  - **Seed shape (all families, replacing the Fabaceae-only rule):** oblong / elongate → ovoid_elongated; elliptic /
    ellipsoid / obloid → ellipsoid; ovoid / ovate / obovate / pyriform → ovoid; globose / spherical → globoid; circular →
    discoid; orbicular → orbicular; angular / rhombic / quadrate / triangular → polyhedral; D-shaped →
    hemispheric_flattened; biconvex → lenticular; compressed / flattened (not turgid) → *_flattened of an ellipsoid / ovoid /
    globoid outline; any seed wing (also as its own clause of the seed sentence, Banksia "wing notched") → winged.
  - **Storage organ:** plain "tuber(s)" / "tuberous" → stem_tuber (root_tuber only for tuberous roots / root tubers).
  - **Synonyms of dictionary levels** (the raw term stays in value_description): ascending → decumbent; subcordate → cordate;
    cuneate / wedge-shaped / fan-shaped leaves → obtriangular (also in "cuneate-oblanceolate"); "single or clustered" →
    scattered clustered.
  - **Graminoids:** plant_growth_form graminoid (± tussock), without herb.
  - **Fixes from the comparison:** a floral organ's "blade" (Diuris petals, labellum) is not the leaf lamina; blades and
    leaves inside a juvenile-leaf clause are juvenile (only after juvenile / seedling / coppice / intermediate leaves, not "young
    leaves light green"; leaves typed cauline, basal or adult stay the plant's); "Crown of juvenile / intermediate / adult leaves" is the mature foliage;
    lamina posture includes involute / convolute / revolute; "entire to toothed" keeps toothed; "undivided" / "entire,
    rarely lobed" give unlobed; "green or glaucous" gives not_glaucous too; comma lists before "at base"; "narrowly or broadly
    ovate"; "30–c. 200 cm" ranges; qualified heights of a second growth form ("occasionally a tree to 12 m high") and
    "sometimes 3–4 m" go on their own commonness rows in every family.
- **Stem and canopy form (all families, user 2026-10-30):**
  - `stem_branching_form` is read from the habit and the main stem / branch / culm clauses (branched hairs, inflorescences,
    roots, rhizomes excluded), with every APD level: "branched / branching from (near) the base", "several stems from a
    lignotuber" → multiple_basal_stems (+ much- / sparsely-branched when so qualified); "branching just above the base" →
    multiple_near_basal_stems; "single trunk", "straight / clear bole" → single_basal_stem; "1 to several stems" →
    single_basal_stem multi-stemmed; "many-, well-, freely branched", "twiggy" → much-branched; "simple or sparsely branched",
    "not or sparingly branched" → unbranched sparsely-branched; monopodial, moderately-, compactly-, horizontally-branched,
    tangled, gnarled, dichasially-branched.
  - `stem_constitution` from the same text: rigid / stiff, robust / stout, wiry, slender, straggly, flexible, wispy, spindly.
  - `plant_canopy_form` (was Fabaceae only): obconic, rounded, flat-topped, domed → hemispherical, pyramidal, globose,
    columnar, broom-like, pine-like, diffuse, dense- / open-crowned, mounded.
  - `stem_base_form`: buttressed / plank buttresses, fluted (sentences about the trunk, bole or tree).
  - Trunk and bole diameters ("bole to 65 cm diam.", "fluted bole to 1.2 m diam.") → `stem_diameter`; a bole mention alone
    says nothing about stem number.
  - `stem_count`: "with 1–3 main trunks", "with 5–12 trunks"; `plant_diameter_breast_height` ("d.b.h. to 36 cm") in all
    families (was Fabaceae only).
  - traits.build replaces `(Synonyms, …)` terms with their level at build time (process_replace_synonyms); the parser
    maps straight to the level anyway, keeping the source term in value_description.
- **Leaf types (all families):** sizes of basal (radical, rosette), cauline (stem), upper and lower leaves go on
  `entity_measured` rows (basal_leaf, cauline_leaf, upper_leaf, lower_leaf), as the leaf counts do; a lamina unit
  belongs to the leaf type named before it ("Basal leaves rosette-forming; petiole …; lamina … 4–30 mm long").
- **Bracketed exclusions (all families):** "Achenes (excluding corolla) 2–3 mm long" is the achene's length; the
  bracketed organ does not own the number, nor does an unbracketed outgrowth ("Capsule …, including appendages
  4.5–6 mm long" is the capsule; but Euphrasia "hood …, including lobes c. 4–6 mm wide" is the hood, not the corolla). "Extension(s)" and "appendage(s)" are other organs ("inner bracts with hyaline extension
  c. 0.2 mm long" is not the bract; Grevillea "style-end with … appendage 2–4 mm long" is not the style); a grass
  "rachilla extension c. 1 mm long" is the candidate `rachilla_extension_length`.
- **Leaf type inference:** the "leaves at least 3 mm wide" test uses the general leaf width, and typed (basal,
  cauline …) widths only when there is none.
- **Phenology (all families):** "Flowers & fruits" = "Flowers and fruits"; "with a peak X" after a window adds a
  peak row; old, empty heads left on the plant ("old empty capitula remaining year round", "old fruiting heads (seeds
  dispersed) present August–April") are not fruiting; "fruits until about March" with no start is left blank.
  Also: "outlying / isolated records from June, July and December" are rarely months; "mature cypselae recorded in
  February" is fruiting; "most of year, but mostly / mainly / most commonly X" →
  X on a mostly / mainly row; "with a slight / distinct peak" is a peak row; "(but) also (at) other times / months" →
  the remaining months on an occasionally row; "April–July & sporadically in October–January" and "July–November and
  rarely December" split like the comma form; regional windows ("June–October (northern parts) and August–December
  (southern parts)") are `population_region` overrides.
- **Source glitches (all families):** the figure dash "‒" (U+2012) is a range dash; a soft hyphen between two digits
  ("3[soft hyphen]8 cm wide") is a range dash, elsewhere it is dropped.
- **Comparison descriptions (all families):** "(Very) similar to X, but differing in (having / being / the following
  respects:) …" keeps only what follows as this taxon's own description; clauses that join two organs ("the leaves
  being …, and the conceptacles longer, 22–30 mm long") are rewritten in `fix_text`.
- **Asteraceae organs added in batch 2:** Xanthium conceptacles (the burr of the female head) →
  `conceptacle_length` / `_width`.
- **Calendar wordings (all families):** "sporadically / occasionally in other months" → the remaining months on a row
  with that word; "most / all months, peaking X" → a peak row; a lone "(Apr.)" month → rarely; a queried month
  ("?Aug.–Nov.", "Aug.–?Nov.") ends the stated part, and the rest goes on a `scoring_certainty` = possibly row.
- **Hair words are not orientation (all families):** "appressed-pubescent" and "spreading hairs" describe the hairs,
  never the organ's orientation; "appressed to the axis" still is.
- **Record everything:** every character goes in, defined trait or not. Hairs, colours, shapes, fusion and floral
  part sizes go into candidate columns (listed in `raw/candidate_traits.csv`); "no trait yet" is never a reason to
  drop text.
- **Fruit "high":** "Fruit 10–14 mm high, 18–25 mm wide" (Tribulus) and "Cocci 4 mm high" (Rutaceae) give the
  base-to-apex `fruit_length` when no length is given.
- **Grass-trees (Xanthorrhoea):** "Trunk 1–2 m long" gives `plant_height` on a `plant_organ_measured` = trunk row, as
  for tree-fern trunks.
- **Suckering in the habit sentence** ("root suckering", "often suckering", "with suckers") is vetted outside
  Fabaceae (the parser reads it only in legume mode): `clonal_spread_mechanism` (root_buds for root suckers, else
  clonal) + `vegetative_reproduction_ability`. Fruit eaten by named birds or animals gives `endozoochory` plus
  `dispersers`.

## EUCLID 2.0 (CPBR_2026, user 2026-10-30)

- **Scope:** all 935 EUCLID profiles (`data_extra/euclid_species_profiles.csv`), even though ~895 share near-identical text
  with the FoA eucalypt treatment already in ABRS_Myrtaceae_2026 (user's choice). Dataset id CPBR_2026 (after CPBR_2002, the
  earlier EUCLID extract); long format.
- **Same parser, EUCLID mode:** `FOA_SOURCE=euclid FOA_CORES=12 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R` (~1.5 min)
  writes `data/CPBR_2026/data.csv`. The CSV's columns are mapped to the FoA fields: the "Cultivated seedlings …" text that
  closes each description → Seedlings, `flowering_time` → Phenology, `notes` → Notes. The FoA fields EUCLID lacks are empty
  input only; data.csv is long and has no empty columns (empty context columns are dropped, the link column is `euclid_url`).
  No genus / family profiles, so no copy-down. Override and vetted CSVs live in `CPBR_2026/raw`. Metadata:
  `python3 ignore/jobs/cp1/make_meta.py` (keeps the file's dataset fields, questions and taxonomic_updates).
- **EUCLID notation → FoA form:** "reddish-brown" → "reddish brown" (head colour, as FoA), "ca 15 m" → "c. 15 m", "to20 m",
  a second height without its own "to" ("Shrub or mallee to 5 m, occasionally a tree to 12 m tall"). "sub-opposite",
  "sub-basifixed" stay as written (FoA itself has both "sub-basifixed" and "subdorsifixed"; open question to the user).
- **EUCLID notes mix the taxon's own habitat with comparisons, place names and group names** (Bacchus Marsh, "the swamp gum
  E. camphora", "non-swampy habitat", series "swamp-dwellers"), so swamp / marsh words are not read there: swampy is
  vetted from each taxon's own habitat sentence. Bark phrases ("rough bark on the lower trunk", "on larger trees") are not
  epiphyte wording. Region-limited habitat ("in the north-east of the range, on seasonally swampy ground") → population_region.
- **QA:** compare taxon by taxon with ABRS_Myrtaceae_2026 (`ignore/jobs/cp1/cmp.py`, examples `ex.py <trait> <n> foa_only|
  euclid_only|differ`): every systematic difference is a parser gap or a real source difference. Calendars are checked
  against EUCLID's month lists ("Flowering has been recorded in March, May and July").
- **Ecology vetting rulings used:** "seed-only means of regeneration after crown-loss", "non-lignotuberous, obligate-seeder" →
  fire_killed; "lack of a lignotuber" alone is NOT fire_killed (mirror of the lignotuber-presence rule); rhizomes with "their
  role in regrowth … after fire" → resprouts + rhizome bud bank + clonal; "Seedling regeneration … common after flooding" →
  germination_cues floods; "highly attractive to honeyeaters / lorikeets / insects and birds" → flower_visitor (lorikeet =
  parrot); "abundant nectar producer" → nectar_produced; "salt-tolerant", "obvious salt tolerance" → plant_tolerance_salt
  halophyte_moderate ("appears to tolerate" → scoring_certainty apparently); "survives … in heavily frosted areas like
  Canberra" (horticulture) → plant_tolerance_frost frost_hardy on a population_region = in cultivation row. A statement in
  another species' profile about a named species is scored on that species, its evidence prefixed "[E. alipes profile:]".

## Where this skill lives

Edit THIS copy, in the flora worktree (`~/GitHub/austraits.build-flora-extractions/.claude/skills/extract-traits-from-floras/`),
not the main checkout's: the user switches branches in the main checkout, and its uncommitted skill edits get stashed
(user 2026-10-30).

## Every trait for every taxon (user 2026-10-30)

- **A trait proposed in any family or group is checked for all other taxa.** The per-dataset `raw/candidate_traits.csv`
  lists hid family-only traits, so the post-build step writes one register over all datasets,
  `data/ABRS_AtoM_2026/raw/candidate_traits_all.csv` (`ignore/jobs/cp1/cand_all.py`): taxa, families and datasets per
  candidate, with `family_confined` flagging traits found in only one or two families. For each flagged trait, ask whether it
  is confined because only that group has the organ (glume, pollen presenter, pleurogram) or because the code writing it runs
  only in a family mode (`legume_mode`, `orchid_mode`, `grass_mode`, `fern_mode`, `asteraceae_mode`). Ungate the second kind,
  checking the output where the words mean something else (a Myrtaceae oil gland is not a nectary).
- **Venation (all families):** `leaf_venation` (pinnate, parallel, anastomosing, reticulate, striate), `leaf_main_vein_count`,
  `leaf_parallel_veins_per_mm` (first written for Acacia phyllodes); `leaf_secondary_vein_angle_categorical` ("side-veins
  greater than 45° to midrib" → greater_than_45_degrees; less_than_N_degrees, acute, wide_angle, perpendicular, parallel) and a
  stated angle → numeric `leaf_secondary_vein_angle`; candidates `leaf_reticulation` (very_dense … absent, obscure),
  `leaf_intramarginal_vein` and `_position` (close_to_margin, remote_from_margin). "Intramarginal vein parallel to … margin"
  and "side-veins sub-parallel" are not parallel venation. Also ungated from Fabaceae: leaf curvature, symmetry, mucro length,
  leaves per node, rachis and pulvinus length, length : width ratio; extrafloral nectaries outside Fabaceae only from explicit
  wording ("extrafloral nectaries", "nectar-secreting glands").
- **Also ungated (batch 2):** from Fabaceae, inflorescences per axil, flower heads per inflorescence, peduncle diameter, stamen
  length, calyx tube and lobe length, fruit stipe length and wing width, seed orientation in the fruit, seed height from the
  thickness, `habitat_geology`; from ferns, `rhizome_form` and (outside ferns, leaf clauses only) `leaf_heterogeneity`; from
  Asteraceae, `inflorescence_count` (outside Asteraceae only with "per stem / plant / scape / culm": Acacia "heads 1 or 2 per
  axil" is per axil); from Poaceae, stem / culm `internode_length`. Kept in their mode: Acacia raceme axes and spike widths, the
  papilionaceous standard / wing / keel rows, pleurogram, standard flare, pappus counts, habit-sentence suckering (vetted elsewhere).
  Sepal and petal widths were already general (the organ-size table); the orchid / legume rules only add entity rows.
- **`flower_colour_detailed`** (traits.yml): the same flower colour words as `flower_colour`, mapped to the detailed levels
  (white, cream, yellow, gold, orange, red, maroon, reddish_brown, brown, pink, magenta, purple, mauve, lilac, violet, indigo,
  blue, turquoise, green, black, grey), keeping the head colour of a compound.
- **Flowering vs fruiting cues:** metadata maps `fruiting_cues` to `flowering_cues`, so taxa with both carry the context
  `cue_scored_from` (flowering / fruiting). "rainforest" is not rain; "floodplains" is habitat; a clause naming another species
  ("E. microtheca has …") and resprouting after floods are not this taxon's cues.
- **Value fixes** for terms of another organ (petal_shape curvature, style-heads → `stigma_shape`, ribbed stems → `stem_surface`,
  fruit sheen → `fruit_surface_reflectivity`) are a `value_fixes` table in the parser (see `raw/parser_issues.md`).
- **metadata.yml is updated add-only** (`ignore/jobs/cp1/meta_update.py`): hand-made mappings (var_in ≠ trait_name with
  `plant_organ_measured`, context value lists, substitutions) are never regenerated away. The old make_meta regenerators dropped
  them and must not be run on an existing metadata.yml.

## Fixes from the EUCLID comparison (all sources, 2026-10-30)

- A lignotuber sentence ("Tree to 20 m high. Forming a lignotuber.", "Non-lignotuberous.") is moved into the habit sentence,
  so storage_organ is read (120 FoA Myrtaceae profiles had none).
- "leaves 0" in the leafless test is a zero count only, not "leaves 0.3–0.7 cm wide"; "leafless terminal / axillary
  inflorescences" are not leafless plants.
- "occasionally / sometimes / rarely taller (higher) to 15 m" is a qualified second height.
- "(recorded in) all months except November", "… except September and October", "… except Dec.–Feb." → the other months;
  "all months from September to April" → that window (both were read as all year).
- **"sub" terms (user 2026-10-30):** "subspherical", "sub-basifixed", "subopposite", "subterminal" map to the trait's sub- level
  if there is one (`subglaucous`, `subshrub`, `subspinescent`), else to its semi- level ("subwoody" → `semi_woody`,
  "subaquatic" → `semiaquatic`, "subdeciduous" → `semi_deciduous`), else to the base value (globoid, basifixed, opposite,
  terminal); the source word stays in value_description. The parser looks the sub- / semi- levels up in traits.yml, so levels
  added later are used without changing the script.
- A hedged swamp habitat ("may be anticipated along streams and in swampy areas", E. rudis) → terrestrial + a secondary `swampy`
  row with scoring_certainty possibly. A habitat sentence for the species that says "habitat depending on the subspecies" is
  scored only on the subspecies whose own paragraph names that habitat.
- Hybrids written "Eucalyptus x phylacis" get rank species (they were dropped when the profile had no rank).
- The FoA scrape loses text after a "<" ("branches <8 cm diameter smooth" → "branches Juvenile growth …"; ~17–29 profiles,
  mostly eucalypts): a scraper issue, reported to the user.

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
- Hedged responses are scored with the hedge word as `scoring_certainty` (user 2026-10-29): "flowering is probably
  enhanced by fire", "may be promoted by fire" → `fire_enhanced_flowering` (+ `flowering_cues` fire) with
  probably / possibly; "flowers freely without fire, but flowering may be enhanced after a burn" →
  `fire_enhanced_flowering` (possibly), not fire_independent; "seems to flower only after fires" →
  `fire_dependent_flowering` (apparently). "Flowering is inhibited in the first year or two after a fire", "may be
  badly impacted … take several seasons to recover", "habitat becomes densely overgrown soon after fire, inhibiting
  the flowering" → `fire_suppressed_flowering` (no fire cue).
- Hedged rain cues ("probably in response to rain", "presumably dependent on rains", "likely in response to
  rainfall", "appears to flower in response to summer rainfall") → `flowering_cues` rain / rain_summer … with the
  hedge word (likely, apparently, presumably …).

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
  "apparently", "thought to be", "may also") ends the stated part of its sentence. The months the hedged part adds
  go on their own rows, with the hedge word as `scoring_certainty`. For cycads and conifers, fruiting time is
  scored from seed ripening or cones, with that as the `trait_scoring_method`.
- **Ecology traits are vetted, not regexed.** Pollination, dispersal, fire response, seed bank,
  germination, clonality, defences and lifespan are too varied: script the verbatim
  `*_description` columns, then score values in commented per-taxon `tribble`s. Pollination (user's
  rule, 2026-10-19): `pollination_vector_known` only for studies where a flower netted after one visit
  sets seed, or pollen from another individual is found on the visitor, so never from a flora;
  `pollination_vector_possible` when visitors are recorded carrying pollen / pollinia or moving a
  pollinarium to another flower (rare in floras); "visited by honeyeaters", "pollinators include birds" →
  `flower_visitor`; morphology / visual cues ("dark spot visible to pollinating bees") →
  `pollination_syndrome`. Hedged dispersal / fire / breeding statements stay text-only. Group-level hedges ("Like other Lysiana spp., …
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

Ready-made QA helpers for the ABRS_*_2026 build live in `scripts/` next to this file (see `scripts/README.md`):
categorical mapping tables, copy-down review, calendar review, measurement misses, and a before/after diff of
data.csv to catch regressions in already-checked families.


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
