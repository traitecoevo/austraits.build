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
  range in `_min`/`_max`, extremes in `_extreme_min`/`_extreme_max` (long format: minimum / maximum rows with measurement_range =
  extreme). **Settled (user 2026-11-05):** they are always scraped into data.csv but not mapped into AusTraits (custom_R_code filters
  them out); no metadata question about them.
- **Genus and family copy-down (FoA-style treatments)**: a categorical value stated singly in
  the genus (else family) description is the group's default and is copied to member taxa silent
  on that trait (`trait_scoring_method` = `inferred_from_genus` / `inferred_from_family`). Drop
  sentences scoped to some species ("most species", "or introduced … species") first, and
  segments naming a genus in brackets ("(Hydrocleys)"). A statement counts only if it names
  exactly one term, counting terms with no level ("flat, terete or triquetrous" is not "all
  terete"). Never copy numbers.
  - **HARD RULE (user 2026-11-08, restated: "how does this rule keep getting ignored?"):** copy ONLY when the group text gives that
    one value, unambiguously. Ambiguity is itself a reason not to copy. **A group's Habitat text is never copied**: it lists where the
    group's species grow, mostly in words that are no trait level, so one level looks like the only value. A habitat water reader
    added for NT_2026 ran on group profiles and copied semiaquatic to 505 Grevillea ("Occurs in most habitats, but only a few species
    alpine, few in swamps"), 224 NHNSW Cyperaceae ("mostly in damp or swampy places"), Stellaria ("a wide range of habitats …
    including … swamps"), Andersonia, Terminalia ("Many species … often of riverine and swampy habitats"), Dichanthium ("from marshes
    to subdesert"), Phalaris. Nor is a statement about some of the species copied ("a few / some / many / most species"). Any new or
    widened reader: check whether it runs on genus / family profiles and could pass the copy-down filter, and read the "group copies
    gained" table of every rebuild diff first.
    The group's substrate is taken from its description in the full read when the group filter drops the segment for an unrelated
    "or" phrase ("Deciduous terrestrial orchids growing singly or in loose groups": Microtis, Drakaea, Cyanicula, Elythranthera;
    Caladenia "Most above-ground parts of these terrestrial orchids are hairy") and it is the only substrate anywhere in the group's
    text, Habitat included. Their Habitat had stood in for it, so excluding the Habitat had left 78 orchids without a substrate or a
    geophyte height type. Corunastylis says "terrestrial" only in its Habitat, so it is not copied.
    **Every group substrate copy passes a clause check** (`sub_clean_ok`, 2026-11-08). Each clause (split at . ;) of the group text that
    names the substrate must have no "or" / "to" next to another substrate or habitat word. Its comma segments that name a substrate
    or habitat must have no hedge, quantity or conditional word (some, many, few, sometimes, rarely, occasionally, mostly, usually,
    often …, not, when, if, where). A hedge in another segment of the clause doesn't count: Lastreopsis "Terrestrial ferns, rhizome
    usually creeping" and Todea "usually arborescent" are copied. Juncaginaceae "emergent when aquatic" is not. "Annual or perennial aquatic herbs" still counts, and "singly or
    in loose groups" is ignored. Mistletoe groups are never copied. Blocked so far: Ericaceae "epiphytic or not" (128 NHNSW taxa in
    rebuild 3); also, already in HEAD, NHNSW Loranthaceae "terrestrial, root-parasitic shrubs or trees, or aerial, stem-parasitic shrubs"
    (terrestrial on 38 mistletoes), Isolepis "… OR aquatic …", Lindsaeaceae "Terrestrial [or sometimes epiphytic]"; and Polypodiaceae
    "Epiphytic or growing on rocks, rarely terrestrial", Ficus "some epiphytic", Rorippa "some aquatic", Hydrocharitaceae "Freshwater or
    marine", Myriophyllum "aquatic to fully emergent", Pandanus, Triglochin, Typha "aquatic or semi-aquatic". The check covers substrate
    only; other traits still use the group filter alone.
  - **An "or" makes only its own words alternatives (user 2026-11-05):** Hakea "Fruit a smooth or rugose to tuberculate woody
    modified follicle" is a woody follicle — the "X or Y" words (each a word or an "A to B" range) are removed from the group text and
    the rest of the segment is kept. A segment still goes whole when the alternative is a phrase ("solitary or in racemes", "a capsule
    or a berry") or "X or not".
    **Only when the "or" joins two plain modifiers of what follows** (build10 review, 2026-11-05): the segment also goes whole when the
    next word continues the alternative ("stipules absent or small and caducous", "bisexual or unisexual and then plants dioecious",
    "opposite or alternate and spiral", "smooth or covered in imbricate scales", "entire or serrate (... leaves needle-like)"), when the
    terms are plural nouns ("climbers or leafless parasites"), "-ing" words ("splitting to base or remaining fused") or adverbs ("simple
    or palmately lobed"), or when words before the pair join it to more ("free and alternating with the tepals or fused"). Read loosely,
    the rule had copied Campanulaceae / Asparagaceae dioecious, Convolvulaceae parasitic, Apocynaceae caducous stipules, Amaranthaceae
    spiral leaves and Ericaceae needle leaves to every member — caught before commit.
  - **Species descriptions name only the exceptions (user 2026-10-31)**, as for terrestrial vs
    aquatic: a member taxon that scores the trait itself keeps its own value, and a single
    contradicting member does not stop the copy to the silent members (Hibbertia "Fruit a
    follicle" is copied although H. spanantha says "capsule"). **Majority veto (user's choice):**
    the copy is blocked only when at least 2 reporting members contradict the group value and
    they outnumber those that agree: that is usually a misread of the group description
    (Dampiera "usually blue …, rarely red, pink …" → pink; Ficus "woody endosperm"). Lists whose
    alternatives belong to other traits ("creeping rhizome, annually renewed tuber or thickened
    rootstocks"; "simple, pinnatifid, pinnate or bipinnate") are not caught by the veto: add them to
    `copydown_exclude`, and read every new copy-down reaching ≥40 taxa after a build.
  - **Hedged group statements are copied with their hedge word as commonness_qualifier (user 2026-11-05; before: a plain value)**:
    "Fruit usually a capsule" → capsule `usually`, "Leaves mostly basal" → `mostly`, "connate (mostly)" → `mostly`; and the main value
    of "X or rarely Y" / "X, rarely Y" → X `usually` (the rare Y is not copied). Read from the group's own text (the hedge words are
    removed only for the segment filter). An "or" between plain alternatives copies nothing for that trait. Older wording:
    "Fruit usually a capsule" (Orchidaceae), "Fruit mostly a caryopsis …, rarely a nut, berry,
    or an utricle" (Poaceae) → capsule / caryopsis. Copied hedges: usually, mostly, mainly,
    generally, commonly, typically, predominantly, normally, chiefly. Not "often" /
    "frequently" (= many members), and the rare alternatives ("rarely a nut") are not copied
    and do not make the trait variable. In the script these hedge words are removed from the
    group text before its segment filter ("Fruit usually a capsule" → "Fruit a capsule"), and a
    rare word carries across a fruit-type list as across a colour list ("rarely a nut, berry, or
    an utricle"). A hedged word often belongs to another organ ("mesocarp usually fleshy", "keel
    usually green", "hermaphroditic (functionally unisexual)"): read the new large copy-downs.
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
  `climber` (twining is kept in plant_climbing_mechanism / stem_growth_habit). The "vine" rule holds only for a source that also uses
  "liana". In a source that never says "liana", "vine" covers woody and herbaceous climbers alike, so it is plain `climber` (user
  2026-11-09; Cooper_2004_2). Check the whole source before choosing.
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
- **Genus → species copy-down only for unhedged universal statements** (genus *web fact sheets*, e.g. WA Orchids; FoA /
  PlantNET genus and family descriptions follow the copy-down rule under Principles instead). A genus sheet sentence
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
  "midrib submerged", "on secondary branches" are not habitats); herbs without Habitat text get the default; other wet wording
  (creek beds, lake margins) leaves herbs unscored. Habitat text is kept verbatim as `habitat_description`.
- **Water plants and swamps (user 2026-11-07, after Marsilea hirsuta lost "aquatic" in four 2026 datasets; all sources):**
  - **Swamp and marsh plants (herbs and woody plants alike) are `semiaquatic`** (user: a swamp is not fully aquatic). This
    replaces "terrestrial semiaquatic" (2026-10-28) and "terrestrial swampy" (2026-10-30 to 11-06); the `swampy` level in
    traits.yml is no longer used here. "Swampy" is a habitat as much as a substrate, and AusTraits has no habitat trait, so the
    swamp detail lives in `habitat_description`. Not plants beside a swamp ("edges of mangrove swamps", "near swamps", "swamp
    margins") or swamp-named trees ("swamp gum").
  - **The Habitat text is read for water plants** when the description names no substrate: "in (shallow / still / running ...)
    water", "in water bodies", "submerged", "floating in the water", "emergent aquatic", "aquatic weed" → aquatic; "amphibious",
    "semi-aquatic", "partly submerged / immersed", "drying mud", herbs "in / on (wet) mud" (not woody plants on mud, nor
    mudflats), swamps, marshes → semiaquatic. Text order, commonness word of the comma segment, negated segments skipped. FoA
    Marsilea hirsuta "Grows in shallow water and drying mud" → aquatic semiaquatic. Refinements from the chunk test: trees and
    shrubs standing in water (mangroves "in brackish waters", Ilex arnhemensis) are semiaquatic; water plants (waterlily families,
    or floating / submerged leaves in the description: Nymphoides, Myriophyllum glomeratum) in a swamp are aquatic; "water" before a
    place noun ("in freshwater swamps / lakes / clay pans") is read by that noun; place names ("Bacchus Marsh", "Tootgarook Swamp")
    are not habitats (also in the inferred rule). A hedged-only mention keeps its hedge (Triglochin hexagona "rarely swamps" →
    semiaquatic rarely) and, being the taxon's own text, replaces a family copy.
  - **"Aquatic or terrestrial" plants of water or mud are amphibious:** the terrestrial alternative becomes semiaquatic (Marsilea
    "Aquatic or terrestrial herb. Grows in shallow water or mud." → aquatic semiaquatic; also in the LucidFerns_2026 script).
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

## PlantNET Flora of NSW (NHNSW_2026, user 2026-10-31)

- **Whole file at once** (user's choice), dataset id NHNSW_2026, long format. Do NOT look at NHNSW_2022 / NHNSW_2023 while
  extracting: they are the comparison case afterwards (user).
- **Same parser, PlantNET mode:** `FOA_SOURCE=plantnet FOA_CORES=12 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R` (~14 min)
  reads `data_extra/plantnet_nsw_flora.csv` (family, genus, species and infraspecific profiles of all NSW families; Flora of NSW
  prose, like FoA), so the family modes and genus / family copy-down apply. Fields: Description; Flowering → Phenology
  ("Flowering spring to summer"); "Distribution and occurrence" → Habitat; Notes; Text by → profile author; introduced → biostatus.
  Overrides and vetted ecology in `NHNSW_2026/raw`. Metadata first created by `ignore/jobs/pn1/make_meta.py`, then add-only.
- **Names** lose their authorities ("Correa baeuerlenii F.Muell." → Correa baeuerlenii); hybrid formulae, hybrid subspecies
  ("subsp. x coriacea"), phrase names (APC form with voucher and herbarium) and informal variants in quotes ('Alstonia constricta
  "Pubescent form"', "Bertya tasmanica variant 'Typical'") are kept as written; "Senna artemisioides group" (an informal aggregate)
  is left out, as FoA's informal groups.
- **File size:** the profile URL repeated on every row was 44% of data.csv and took it to the 100 MB limit; it is written once per
  taxon to `raw/taxon_urls.csv` instead.
- **Heights (all sources, 2026-10-31):** when no plant height is found, a height on the scapes or inflorescences in the first
  sentence ("Herb with a single scape 9-15 cm high") is plant_height_reproductive; on a palm / cycad trunk ("sturdy trunk to 2 m
  tall") plant_height on a trunk row; on culms, stems, branches or branchlets ("with branches ascending to 50 cm high") the plant
  height. This runs last, only when no other height rule recorded anything, and not for orchids (their flower-stem heights are
  assigned later, from the inflorescence length): run earlier, it pre-empted the scape, palm-trunk, fern-frond and orchid rules.
  "Root-suckering" is no root organ.
- **Ecology vetted** from "Distribution and occurrence": suckering / stolons / rhizomes → clonal; "forms colonies" for perennial ferns,
  orchids and herbs only (not annuals, biennials or palms); fire, dispersal (seeds spread by birds → dispersers birds + endozoochory;
  tumbleweed), pollination and cleistogamy; hedged with the hedge word. dispersers levels are plural (birds, mammals).

## WATTLE: Acacias of Australia (WATTLE_2026, user 2026-11-02)

- **Scope:** all 1277 WATTLE profiles (`data_extra/wattle_fact_sheets.csv`: Acacia, Vachellia, Senegalia, Acaciella; species,
  infraspecific taxa, hybrids, informal variants, phrase names), dataset id WATTLE_2026, long format; most text is the FoA Acacia
  treatment (as in ABRS_Fabaceae_2026). Do not look at Maslin_2012 (WATTLE 2) while extracting: it is the comparison case.
- **Same parser, WATTLE mode:** `FOA_SOURCE=wattle FOA_CORES=12 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R` (~2 min).
  Fields: Description, Habitat, Distribution, Notes, Phenology, Conservation, Common Name, Author; the link column is
  `wattle_url`. No genus profiles, so no copy-down. Names as written; hybrids ("A. ampliceps x sclerosperma subsp.
  sclerosperma") and informal variants ("(narrow phyllode variant)") have rank species. Metadata first created by
  `ignore/jobs/wt1/make_meta.py` (contexts and substitutions from ABRS_Fabaceae_2026), then add-only.
- **Phenology:** WATTLE abbreviates states and months ("mid-Apr.-July in coastal Qld; ... in N.S.W."): both are expanded to
  the FoA wording before parsing ("Aug. in inland Qld" is not a sentence end). Regional calendars are hand overrides in
  `WATTLE_2026/raw/phenology_overrides.csv` (from WATTLE's own months), as in the ABRS overrides.
- **Ecology vetted** (`ignore/jobs/wt1/vet.py` -> `raw/ecology_vetted.csv`): the ABRS vetted rows whose evidence is verbatim in
  WATTLE, plus every WATTLE Habitat / Notes sentence on fire, suckering, salt / frost tolerance, lifespan and establishment read
  by hand (`eco_cand.py` lists the candidates).
- **Source limits (comparison 2026-11-02):** only 5 of the 224 infraspecific taxa have a species profile in the scrape, so their
  shared characters (pods, seeds, flowers) are missing; 868 profiles have no flowering text. WATTLE itself has no species pages
  for divided species (checked 2026-11-02: e.g. acacia_mutabilis.htm is 404; the subspecies pages hold only their own
  description), and the FoA species profiles are NOT copied in (user 2026-11-02: no mixing of sources).
- **Phyllodinous taxa (all sources, user 2026-11-02):** leaf_compoundness simple and leaf_phenology evergreen are inferred
  (trait_scoring_method inferred) when the profile scores neither, as in Maslin_2012.

## AusGrass3: Grasses of Australia (AusGrass3_2026, user 2026-11-02)

- **Scope:** all AusGrass3 profiles (`data_extra/AusGrass3_fact_sheets.csv`: 247 genera, 1555 species and infraspecific taxa;
  1354 taxa have a description), dataset id AusGrass3_2026, long format. Do not look at GrassBase_2014 or the ABRS Poaceae while
  extracting: they are the comparison case.
- **Same parser, AusGrass mode:** `FOA_SOURCE=ausgrass FOA_CORES=12 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R` (~3 min).
  The DELTA fields Habit, Inflorescence, Spikelets, Glumes and Florets, in that order, are the description (grass mode). Names
  lose the naturalised marker ("Cynodon dactylon*"); naturalised = biostatus Introduced. Flowering times come from the Notes'
  "Flowers ..." sentence (months abbreviated, expanded). A genus profile whose fields hold several descriptions joined by " | "
  (a species-style description appended, or stitched genera such as Cenchrus) is split into parts, so only what every part
  states is copied down; in species profiles " | " only starts another floret type.
- **Rhizomes, stolons, spread (user 2026-11-02), read exactly from the DELTA statements:** "Rhizomes present, short" /
  "elongated" / "short or elongated" → storage_organ rhizome, stem_growth_habit rhizomatous, rhizome_form short_creeping /
  long_creeping; only elongated rhizomes (and stolons) are clonal spread (clonal_spread_mechanism rhizome / stolon,
  vegetative_reproduction_ability vegetative). "absent or present" → the same on a commonness_qualifier = sometimes row.
  "Stolons present" → stoloniferous + stolon + vegetative; "Stolons absent" → clonal_spread_mechanism stolon_absent; "Rhizomes
  absent" → no row. Rhizome, stolon and spread traits are not copied down from grass genera (they vary between species).
- **Other:** culm orientation → stem_growth_habit ("geniculately ascending" = decumbent); "Anthers 3." → flower_fertile_stamens_count
  (one stamen per anther, all grasses); photosynthetic_pathway from the genus Kranz anatomy statement, copied to its species
  (inferred_from_genus) only when it names a single pathway. In group descriptions a segment followed by an "or ..." segment is an
  alternative ("Culms woody and persistent, or herbaceous"); not a rare alternative ("usually bisexual, or rarely unisexual" keeps
  bisexual), nor the "or" left when a range is removed (Restionaceae "Stems green and photosynthetic, terete to angular or
  flattened" keeps green). Genus-level exclusions that apply only to one source's wording (AusGrass Triodia "Prickly perennial")
  are limited to that source mode.
- **Ecology vetted** (`ignore/jobs/ag1/vet.py`): Notes / Habitat sentences on stolons, rhizomes, mats, bulbs, cleistogamy,
  apomixis, salt / frost tolerance, fire (candidates from `eco_cand.py`).
- **Comparison (2026-11-02):** GrassBase_2014 (4 traits) agrees on leaf and culm sizes; its seed_length is the grain, which
  2026 records as fruit_length. ABRS Poaceae: FoA's "Tufted perennial" (caespitose) has no AusGrass species-level equivalent.

## Florabase (WAH_2026_1 to _4, user 2026-11-04)

- **Scope:** `data_extra/florabase_profiles.csv`, current and not-current names (one profile per cleaned name, current first);
  marine algae (Huisman's Algae of Australia and their families), fungi and protists left out. **One dataset per kind of text**
  (user): WAH_2026_1 Descriptive Catalogue brief descriptions, WAH_2026_2 C. Hollister & K.R. Thiele key descriptions,
  WAH_2026_3 DELTA genus / family descriptions (L. Watson, H.R. Coleman, J. Gathe ...) + prose species descriptions (R. Davis
  Ptilotus), WAH_2026_4 Swan NRM management notes. Do not look at WAH_1998 / WAH_2022 / WAH_2023 while extracting.
- **Same parser, Florabase mode:** `FOA_SOURCE=florabase FB_TEXT=brief|key|delta Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R`;
  build brief and key before delta (delta reads their data.csv). WAH_2026_4 has its own script (`data/WAH_2026_4/raw/build_data_csv.R`).
  Names from the classification path's last element without authority ("×molesta" -> "x molesta"); metadata first created by
  `ignore/jobs/fb1/make_meta.py`, then add-only. **Names (2026-11-05):** drop only the authority words, never an epithet or rank
  (two ranks, nothovar., cv., "forma" -> "f.", hybrid formulae "A b x c" and intergrades "A b - c" all kept); phrase names kept as
  written: "sp.", a rank before a capitalised or quoted word ("subsp. Badgingarra (S.D. Hopper 7114)", "subsp. A Kimberley Flora"),
  "variant", a bracketed voucher. Reducing these to the species name dropped ~390 profiles as duplicates of the species.
  "Tree-like monocot" -> palmoid (Pandanus, Xanthorrhoea, Kingia; without it a "perennial" fell back to herb).
- **Brief descriptions:** text before "Fl." = description; "Fl. <colours>, <months>" -> "Flowers <colours>." + phenology; the rest is
  habitat. Colours joined by "-", "/" or "&" are **each** scored ("pink-red-brown" -> pink, red, brown; user). Bracketed peaks
  "(mainly Apr-May)", "(mainly 8-10)" -> mainly rows; "or Mar (occasionally)" -> occasionally; "(depending on rainfall)" -> cue.
- **Keys:** the raw character lists (943, mostly Fabaceae: "Stems Indumentum hairy, Pustules or glands absent, ...") are rewritten as
  the key's own prose before parsing (sizes and hairs first, margins / tubercles / glands after ";"). "Pustules or glands" are
  surface glands, not extrafloral nectaries; "apparently absent" stipules = absent; "indehiscent and splitting into sections
  (schizocarps)" = indehiscent schizocarp; sizes coded 0 are left out and "0-X" is "to X". **"(?)" states are scored with
  scoring_certainty = uncertain** (user): each such profile is parsed again with every state, and values only that reading finds
  go on uncertain rows.
- **DELTA:** sections Habit and leaf form, Reproductive type, Inflorescence and flower, Fruit and seed, Physiology, Special features,
  Additional characters; anatomy, seedling and pollen sentences, geography, etymology, uses left out; "To X m high." joins the habit
  sentence. "Entomophilous" etc. -> pollination_syndrome; "Photosynthetic pathway: C3 and C4" -> c3 c4; "Dispersal unit the fruit" ->
  dispersal_unit. **Genus / family rows are taxa of WAH_2026_3 and are copied down** (user) to every Florabase taxon with a
  description: a taxon is silent only when none of its texts (WAH_2026_1, _2, _3) scores the trait, and those values count for the
  veto; taxa with no DELTA / prose text carry only their copies in WAH_2026_3.
  - Genus / family rows are read in full (alternatives kept); the group filter is only for the copy-down, where conditional clauses
    ("when compound, pinnate") are dropped and a DELTA pollination / pathway / dispersal-unit statement counts only as a single
    unqualified term ("Anemophilous (rarely ...), or entomophilous (nearly always)" is not copied).
  - **Family copies only where the families match (user 2026-11-04):** the DELTA family descriptions are Watson's pre-APG families
    (narrow Asparagaceae, Malvaceae without Sterculiaceae, Scrophulariaceae without Myoporaceae), so a family value goes only to genera
    whose own DELTA profile names that family ("Family Sterculiaceae." in Lasiopetalum); genera with another or no header get none.
  - Not copied (read 2026-11-04): Myoporaceae / Eremophila "deciduous" (Eremophila are evergreen; user: kept on the genus row,
    flagged), Orchidaceae "Floral nectaries present", seed wings of Eucalyptus / Corymbia / Grevillea / Banksia, Petrophile
    "helicophylla spirally twisted". "neither X nor Y" negates both; "(the ... fleshy endosperm ...)" and "hooded by the petals" are not
    seed or stamen characters; "not gland-dotted" is not an extrafloral nectary.
- **Management notes:** General Biology fields mapped by tables (growth / life form, reproduction -> clonal spread, dispersal agents ->
  dispersers + dispersal_syndrome, time to first flowering -> reproductive_maturity, seedbank persistence -> seedbank_longevity_class
  (Short = transient, Medium = short_persistent, Long = long_persistent; years only when no class word), fire response, regeneration
  strategy -> resprouting_capacity_non_fire_disturbance, woody structure -> wood_porosity); calendars with "(O)" = occasionally;
  Germination months -> recruitment_time and "Type of stem. Succulent" -> plant_succulence succulent_stems (user 2026-11-05);
  Active Growth / Dormant months -> active_growing_time / dormancy_time (new y/n calendar traits, user 2026-11-05: two traits because
  they are complements for only 57 of 87 taxa). Plain "Reproduction. Seed." is not scored not_vegetative. "Generally
  survives fire" is resprouts (usually), confirmed by the user 2026-11-05.
- **All sources (2026-11-04):** a hyphenated "non-" negates only its own word ("Non-lignotuberous shrub" is a shrub).

## VicFlora: Flora of Victoria (RBGV_2026, user 2026-11-05)

- **Scope:** `data_extra/vicflora_profiles.csv` (8,399 profiles: families, genera, species, infraspecific taxa), dataset id RBGV_2026,
  long format. **Vascular plants only** (user): profiles whose parent chain reaches Bryophyta, Marchantiophyta or Anthocerotophyta are
  left out (chains that break below the phylum, e.g. Australina -> Urticaceae, are kept). **Introduced and extinct taxa included** (user).
  Do not look at RBGV_2022 / RBGV_2023 while extracting: they are the comparison case.
- **Same parser, VicFlora mode:** `FOA_SOURCE=vicflora FOA_CORES=12 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R` (~11 min). Fields:
  Description; Distribution and habitat → Habitat; Notes; common_name; profile_author (else profile_source) → author; establishment_means
  introduced → biostatus; Bioregions (treatment) + Outside Victoria → distribution; EPBC / FFG / Victorian threat status → conservation.
  Names are written without authorities in the source ("×" → "x "); phrase names ("Astrotricha sp. 1 subsp. 2", "Caladenia sp. aff.
  concolor (Violet Town)") kept as written. Metadata first created by `ignore/jobs/vf1/make_meta.py`, then add-only.
- **Phenology:** the "Flowers Sep.–Oct." / "Fruits mostly Dec.–Feb." / "Flowers and fruits ..." sentences of the description are moved to
  the phenology with the months written out as in FoA ("Flowers September–October"; a sentence-final "Oct." keeps its full stop);
  "Description based on Victorian specimens only." is dropped. "Flowers most of (the) year" stays blank (earlier rule).
- **Herbarium-specimen months (user: also scored):** each profile's counts of specimens with flowers / fruit / buds per month →
  flowering_time / fruiting_time / candidate flower_bud_time, "y" where at least one specimen, on rows with trait_scoring_method =
  from_herbarium_specimens (counts in value_description), beside the description's own calendar. Added after the species →
  infraspecific inheritance and the copy-down, so they neither stop nor are passed on by them. Taxa with no description are not written
  (question2 in metadata: should their specimen months be?).
- **Copy-down exclusions found here (all sources, 2026-11-05; NHNSW had the same):** Acacia "Leaves bipinnate, reduced to phyllodes
  or rarely absent" (not bipinnate for phyllodinous species); Poaceae ligule "membranous, a row of hairs ..." (not membranous);
  Cyperaceae "Leaves basal and/or cauline" / "only at base of the culm or also cauline" (not basal) — alternatives with no level of the trait.
- **Ecology vetted** (`ignore/jobs/vf1/eco_cand.py` → 651 sentences, `vet.py` → raw/ecology_vetted.csv, 766 rows): fire response,
  clonality, dispersal, pollination and breeding systems, germination, lifespan, salt tolerance; habitat-only wording ("in saline
  swamps", "seasonally flooded") is not a tolerance.
  - **A genus / family Notes / Ecology statement is copied to the members only when it states one value that clearly applies to every
    member (user 2026-11-05, all sources; a member whose own text differs still overrides the copy). Never when any word suggests
    ambiguity ("most" definitely means don't copy). This is for the vetted ecology statements only: the description copy-down keeps
    its hedge list ("Fruit usually a capsule" is still copied; user 2026-11-05). ABRS: 12 hedged statements dropped by
    `ignore/jobs/vf1/drop_hedged_group_copies.py` (Chiloglottis "Most species", Zieria "Usually pollinated by flies", Euphrasia
    "primarily by native bees", Viola "Cleistogamous flowers are common" ...).**
  - VicFlora: not copied when hedged ("usually open only on the death of the branch"), conditional ("plants grown in shaded sites"),
    about some species ("several have become naturalised"), or contradicted by a member's own text or by the same note ("Some species
    apparently producing seed without fertilization"). Copied: Chiloglottis thynnine wasps (C. cornuta self; VicFlora states it for the
    genus without "most"), Cryptostylis ichneumon wasp, Glossodia bees, Stylidium / Levenhookia insects, Najas / Ruppiaceae /
    Posidoniaceae / Cymodoceaceae water, Amphibolis seedling, Taraxacum natives self-compatible, Hakea resprouts / killed read from each
    species' own lignotuber wording. Not copied: Hakea serotiny (varies between Hakea species; user), Prasophyllum insects / nectar,
    Salix vegetative, Fumaria cleistogamy, Cotoneaster birds, Gyrostemonaceae lifespan / fire germination, Taraxacum introduced apomixis
    ("all introduced taxa investigated to date").

## SA Flora: Flora of South Australia online (SAH_2026, user 2026-11-05)

- **Scope:** `data_extra/saflora_profiles.csv` (6,949 accepted names: 1,249 genera, 4,906 species, 794 infraspecific taxa, 2 cultivars;
  no family profiles; all vascular), dataset id SAH_2026 (user), long format, introduced taxa included ("Naturalised" and "Questionably
  naturalised" -> biostatus introduced). Do not look at SAH_2014 / SAH_2022 / SAH_2023 while extracting: they are the comparison case.
  The user asked for a first pass without questions: judgement calls go to metadata questions and the report.
- **Same parser, SA Flora mode:** `FOA_SOURCE=saflora FOA_CORES=8 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R`. Fields:
  Description (paragraphs joined by " | " in the scrape: a full stop ends each paragraph, since " | " is the parser's own unit
  separator; "8–30 X 1–8 mm" is a dimension); Habitat; Notes + Biological Notes + Taxonomic Notes -> Notes; Common Name; Author (else
  Authors); Native status -> biostatus; Distribution (SA regions, "*" = naturalised there, then other states); EPBC / SA conservation
  status. Names are written without authorities in the source; phrase names, hybrid formulae ("Acacia dodonaeifolia x Acacia
  paradoxa") and cultivars ("Platanus x hispanica cv. Acerifolia", rank "unranked" -> species rows) kept as written. Profile links in
  the saflora_url column. Metadata first created by `ignore/jobs/sa1/make_meta.py`, then add-only.
- **Misfiled text (the web pages themselves):** about 10 profiles have description sentences under "Common Names" (Arthropodium,
  Digitaria violascens, Gratiola pumilo, Trifolium cernuum), "Phenology" (Hibbertia, Soleirolia, Rumex pulcher) or "Published
  Illustrations" (Solanum coactiliferum, Tecticornia calyptrata); `sa_mv` in the SA block moves them into the description; the Ptilotus
  hair-terminology note under "Phenology" is a note.
- **Phenology:** "Flowers: Aug.-Oct." -> "Flowers August-October."; a field with no flowering / fruiting word ("July-October.", "Mainly
  winter", "Irregular periods, mainly August-November.") is the flowering time; "Flowers and/or fruits: ..." (133 profiles) is read as
  flowering and fruiting; the Acacia notes (Whibley & Symon: "collected in flower between December and February and in pod between
  February and June") are read as written; those the parser misreads (regional calendars "in adjacent N.T. and Qld it flowers ...",
  "(in Vic.)", "principally", "Irregular periods, mainly ...") are hand-read into raw/phenology_overrides.csv by
  `ignore/jobs/sa1/phen_over.py` (34 taxa; other states / regions on population_region rows). "mainly X and occasionally at other times
  throughout the year" is "occasionally at other times" (the rest of the year on an occasionally row).
- **Genus Habitat is not read:** it describes the genus worldwide ("About 60 species in Europe, Asia ...; confined primarily to saline
  and gypseous soils"), so it would copy Frankenia gypsum to every SA species.
- **Source slips (fix_text):** OCR slips ("15–2 mm wide", "i.5", "D.6", "2.-5") and cm / m for mm (Ecballium ovary, Medicago truncatula
  pod) corrected; illegible pod lengths (Acacia menzelii "2.514.5 cm", A. symonii "345 cm") put in square brackets so they are not read.
- **plant_width (all sources, found here):** a width is not the plant's when the last noun before it is an organ that leads its own
  comma segment ("articles cylindrical, ... 2–3 mm diam.", "joints ... 7–16 cm wide", "lowest 2 leaves ... 2.5–20 mm broad", "Perennial
  stems arising from a rhizome, to 130 cm high, c. 4 mm diam."), an organ inside a phrase with a width under 2 cm ("with branches
  decumbent ..., 10–18 mm diam."), a lamina-like organ given its length first ("adult leaves to 12 cm long, 9 cm wide", "cladodes ...,
  28–40 cm long, 6.5–20 cm wide", "leaves 2.5–6 cm long and wide"), or a bracketed size before an organ ("thin (less than 5 mm diam.)
  grooved branches"). It stays the
  plant's after a plant noun or spread word, with a spread word in the measurement ("forming dense mats up to 30 cm diam") or "across",
  and for larger widths after an organ inside a phrase ("shrub with woody rootstock, to 1 m diam.", "small tree, usually with a few
  thick trunks, to 10 m high, 10 m wide"). (Two stricter versions lost real ABRS / NHNSW / RBGV widths: checked on the build12 / 13
  diffs before this one.)
- **Comparison with SAH_2014 / 2022 / 2023 (2026-11-06, `ignore/jobs/sa2/`: `proc_sah.R` processes all four with traits.build,
  `cmp2.R` aligns names to APC via config/taxon_list.csv, `cmp4.R` numeric minimum / maximum, `cmp5.R` categorical value words):**
  2,849 of the old datasets' 3,705 accepted taxa are in SAH_2026; the other 856 have no description on the SA Flora website (641) or
  are under a name it does not list (215), so the old datasets stay the only SA source for them. 91% of 12,703 shared minimum /
  maximum pairs are identical and 89% of shared categorical values agree; most differences are the website's revised text (Eucalyptus,
  Pultenaea heights), old extraction errors (Banksia infructescence as fruit width, Fumaria height 2 mm) or vocabulary
  (lobed / lobed_deep). Parser and source errors it found, fixed for all sources:
  - "ram" after a number is an OCR slip for mm (21 SA profiles); source unit slips in fix_text (Swainsona campestris pod, Gyrostemon
    ramulosus leaves);
  - thistle "marginal spines ..., 2-6 mm long" and Cirsium "0.8-2 mm long spinules" are not leaf lengths (`marginal spines` is a leaf
    part; spinules an outgrowth);
  - Frankenia "long-shoot leaves 2-13 mm long" are the leaves (long- / short-shoot qualifiers; "shoot" after "long-" is not another
    organ), not the "leaf bases ... 0.1-0.5 mm long" that follow;
  - Proteaceae: a "cone" is never the fruit - "Fruiting cone ... 1.5-4 cm long" (Petrophile, Isopogon) is the infructescence
    (infructescence_length / _width), and so is a bare "Cones ovoid, 15-40 mm long" (FoA) or "Cone with persistent scales, 11-14 mm
    long" (Florabase key) in those two genera ("cone scales" are its bracts); Hakea "pollen-presenter ..., central cone 0.1-0.35 mm
    long" is a part of the presenter;
  - plant_photosynthetic_organ cladode (and leaf_type leafless) only for leafless plants: "broom-like" alone is not leafless; "radical /
    cauline / basal / lobed / linear leaves absent", "stems / scapes / branchlets leafless", "often appearing leafless", "leafless except
    for 2 leaves", "leafless ... or leafy" are partial, but "leafless except for the sheaths" (Restionaceae) or "except a small scale"
    is leafless; the Florabase pea key's "absent or apparently so (reduced to scales)" (109 taxa: Jacksonia, Daviesia, Bossiaea ...)
    is rewritten to "absent or reduced to scales" before the hedge filter cuts it; Centrolepis, Aphelia and Gaimardia are leafy genera of Restionaceae (APG IV);
    Gastrodia and the other leafless mycoheterotrophic orchid genera are non-photosynthetic;
  - a hyphenated "leaf-blade ... 0.7-1 mm wide" unit after a leaf length with no width gives the leaf width (Frankenia);
  - leaf hairs count as "leaves described" for the photosynthetic-organ / parasitic inference (thistles have no leaf size or shape).
  Left as they are: genus woodiness copied onto a few species whose own text says "shrubs" (8 SA taxa, mostly soft-wooded succulents,
  where semi_woody is right); flowering-stem height as plant_height of herbs (Ranunculus lappaceus) and frond length of ferns (Todea).
- **User answers (2026-11-06):** "Flowers and/or fruits" -> both flowering_time and fruiting_time; "Questionably naturalised" ->
  introduced; genus Habitat never used; Acacia notes on studies done elsewhere (Cape Peninsula, Perth, Sydney, Victoria) are scored as
  the species' traits with no region, unless the note explicitly states the trait differs between regions (then population_region
  rows). The remaining question (illegible values) is in metadata.yml (`ignore/jobs/sa1/add_questions.py`).
- **Ecology vetted** (`ignore/jobs/sa1/eco_cand.py` -> 388 sentences, `vet.py` -> raw/ecology_vetted.csv): the Acacia notes are long
  study summaries (germination trials, seed predation, insect faunas): scored only where they state a trait of the species (fire-killed,
  ant / bird dispersal, suckering, bird pollination of A. pycnantha ...); seed treatment experiments, "seed eaten by birds", mistletoe /
  dodder host lists, "regenerating around planted trees" and horticultural notes are not scored. Genus copies only for statements
  that hold for every member: Calochilus self, Chiloglottis thynnine wasps (not C. cornuta: self-pollinated in SA), Corybas and
  Pterostylis flies; not Acianthus "mainly", Diuris / Glossodia "chiefly", Microtis / Thelymitra / Oxalis "most", Prasophyllum (by
  season), Nicotiana "may", Fumaria (shade / late plants), Amphibromus "fairly common".

## FloraNT: Northern Territory flora online (NT_2026, user 2026-11-07)

- **Scope:** the user asked for "all traits" from `split_sentences_NT_Flora.csv` (the 2022 scrape; OneDrive
  `data_for_GitHub_repos/.../floras/`), named NT_2026. That file is only each factsheet's Description, with infraspecific names cut
  back to the species (71 names repeated) and 24 unnamed family descriptions; the user chose to re-scrape the full factsheets
  (`ignore/jobs/nt1/scrape.py` -> `data_extra/ntflora_profiles.csv`, pages cached in `data_extra/NTFlora_html`, 0.5 s between requests:
  6,817 factsheets, 1,156 genera, 4,980 species incl. 292 phrase names and hybrids, 681 infraspecific; no family factsheets exist).
  The site HAS changed since 2022: 2,867 of the 3,003 old descriptions are identical, 33 lightly edited, 46 revised under the same name,
  30 under names no longer on the site, 4 now without a description; 798 descriptions are new. Do not look at NTH_2014 / 2022 / 2023 /
  2025 while extracting: they are the comparison case.
- **Scrape gotcha:** browse rows mark phrase names as `<species normal >sp. Watarrka</species>`: match tags with attributes
  (`<species\b[^>]*>`), or every phrase name is lost.
- **Same parser, NT Flora mode:** `FOA_SOURCE=ntflora FOA_CORES=4 nice -n 19 Rscript data/ABRS_AtoM_2026/raw/build_data_csv.R`. Fields:
  Description; Habitat (species only: a genus Habitat is the genus as a whole); Notes + Diagnostic Characters + Similar Taxa -> Notes
  (comparative, so not read for morphology); Flowering / Fruiting (Ecological Attributes: month lists "Mar, Apr, May" or ranges ->
  "Flowers March, April, May. Fruits ..."; "not recorded" dropped; the ferns' "Fertile plants: Apr-Aug" kept as written, once, without
  "Flowers", so it is not flowering time); Common Name without the genus name that opens every list ("Avicennia, White Mangrove");
  Flora Description Source -> Author; Introduced Status "Introduced to NT" / "Formerly introduced" -> biostatus introduced;
  Bioregion + Distribution Notes + International -> Distribution; EPBC / TPWCA conservation status. Profile links in ntflora_url.
- **Diagnostic Characters as description:** only for the 98 factsheets with no Description, and only their sentences that name or
  compare no other taxon ("differs from P. micrantha", "P saxatilis has ...", "Var. angustifolia and var. distans have ...", text after
  "----"); sentences split after full words only ("Ewart & O.B. Davies").
- **Lost characters in the site text:** "¬" and soft hyphens are line-break hyphens ("other¬wise", "40¬-70mm"); `\x1a` is a lost quote
  mark (restored as curly quotes) or fraction ("2/3\x1a", "c. \x1a the length" -> "[?]"); "¿" -> "[?]".
- **Parser fix found here (all sources):** "annual stems / shoots / culms / branches / growth" is not life_history annual ("subshrub with
  annual stems", Flemingia parviflora; also ~10 ABRS taxa, Linum marginale, Solanum hystrix ...).
- **Review of the first build (traits.build `dataset_process`: 117,318 traits, 66 excluded values; `ignore/jobs/nt1/proc_nt.R`):**
  source slips in fix_text (m for mm: Coccinia / Aegiceras styles, Jacksonia divisa seeds, Grevillea miniata / versicolor and Suriana
  leaf widths; "ree or mallee", "shub", "nflorescence", "lnflorescence", lowercase "vine. petioles", "slender. fruit"; Sersalisia sericea's
  page starts mid-leaf, marked "[start of the description lost on the FloraNT page]"; "Leafy twigs", Agave / Aloe "tuft of ... leaves"
  and Polygala longifolia "plant often leafless ..., linear ..." leaf sizes were plant widths). Real extremes stay (Nelumbo pedicels
  1-3.5 m, Barringtonia filaments 8-15 cm, palm leaves 4-6 m) and are excluded only by the traits.yml ranges.
- **Swamps in land-unit lists (all sources; user 2026-11-07: "terrestrial semiaquatic"):** a swamp named in a sentence that also names
  two or more dry land units ("rocky or gravelly hills or rises ..., sandplains, ..., temporary freshwater swamps and claypans", FloraNT's
  central Australian lists, 95 factsheets; Florabase "hills, dunes, ridges, flats, freshwater swamps, salt lakes", ~69 WAH_2026_1 taxa;
  ~12 ABRS / NHNSW sentences) gives terrestrial AND semiaquatic, in text order (the Habitat reader adds terrestrial beside the swamp,
  value_description "[habitat] plains, hills; swamp"); the inferred substrate ignores those swamp words (`swamp_list_strip`), so they
  never give semiaquatic alone. (85 NT dryland taxa, Atriplex vesicaria ..., had been semiaquatic only.) Mistletoes are skipped: their
  Habitat lists their hosts' country. "around swamp or creek margins" and "fringes of temporary freshwater swamps" are beside a swamp,
  not in one; but "within or on the fringes of X" is in X (`swamp_in_re`), and "swamps and creek margins" / "swamps and immediate
  fringes" are the swamps themselves (a broader edge pattern that ate them left "in temporary freshwater" and gave aquatic: Verbena).
- **User 2026-11-07: site only.** The 57 descriptions only in the 2022 file (30 renamed / removed taxa, 4 now without a description,
  23 family descriptions) are not added.
- **Mistletoes (all sources):** "mistletoe" and "stem parasitic" (with a space) are stem_parasitic; when the habit says nothing, the
  Habitat's "parasitic on Eucalyptus" is read (stem_parasitic in Loranthaceae / Viscaceae). FloraNT has no family factsheets, so the
  Loranthaceae family statement that covers the mistletoes in the other sources is missing here.
  (Rebuild review, user 2026-11-08) The Viscaceae genera, now in Santalaceae (Korthalsella, Viscum, Notothixos, Ginalloa …), are
  mistletoes too: generic "parasitic" → stem_parasitic, and their Habitat is not read for swamps. A generic "parasitic" beside a specific
  kind (stem / root parasitic) is dropped. **Subject and object:** "frequently parasitised by the mistletoe Lysiana …" (Acacia
  brachystachya) and "host of / for …" name the plant as a host, not a parasite: not read. A commonness word before "parasitic on"
  is about the host ("Usually parasitic on Syzygium smithii"; Cuscuta "usually parasitic on … Polygonaceae"), not a qualifier.
- **Eucalypts:** FloraNT's 92 "Eucalypts of Australia" descriptions are EUCLID's ("Eucalypt descriptions courtesy of Centre for Plant
  Biodiversity Research", site credits), so they repeat CPBR_2026 text.
- **Not read:** propagation fields (Seed / Cutting / Division Collection and Propagation: horticultural), Ethnobotany, Weeds Status
  (dates), Etymology, Taxonomic Literature, NT Parks, Restricted Range / NT Endemic flags.
- **Ecology vetted** (`ignore/jobs/nt1/eco_cand.py` -> 585 sentences, `vet.py` -> raw/ecology_vetted.csv, 201 rows / 112 taxa):
  "seen (mostly) in recently burnt areas" -> post_fire_recruitment + establish_post_fire with the frequency word; "flowers after dry
  season fires" -> fire_enhanced_flowering; "obligate seeder" / "fire sensitive" -> fire_killed; mangrove notes (Floodplain Flora:
  buoyant fruit -> hydrochory, pollinators); a Similar Taxa statement about the other species is scored on that species (Acacia
  salicina root-suckering from the A. ligulata factsheet). Evidence sentences end at full stops not after abbreviations ("A. salicina",
  "et al.", "e.g."). Genus copies only Casuarina (wind) and Macaranga (gap colonisers). Not scored: host lists, "vine thicket" habitats,
  colonies not stated clonal, fire-protected habitats, fire-regime susceptibility, timber / firewood uses.

## Book scans: Cooper & Cooper 2004 (Cooper_2004_2, user 2026-11-08)

- **Scope:** "Fruits of the Australian Tropical Rainforest" (Nokomis Editions), photographed by the user as four PDFs
  (`OneDrive .../taxon_descriptions/resources-books/Cooper_2004_pages001-100.pdf` ... `401-560.pdf`) plus two sets of rescanned pages
  (`..._pages_rescanned.pdf`, `..._rescanned2.pdf`, re-photos of existing pages, some without a readable page number: matched to their
  page by shared words). **Dataset: a new `Cooper_2004_2`** beside the existing `Cooper_2004` (21 dispersal modes from a spreadsheet;
  user: "New dataset alongside"). User: scrape the book only; **don't rebuild the other sources** for it, so every parser change made
  for it is gated by `cooper_mode` until the user asks for a rebuild.
- **OCR:** the PDFs' own text layer (Adobe Paper Capture) is unusable (columns interleaved, "50-IO0", "8 111"). Re-OCR with Apple Vision
  (`VNRecognizeTextRequest`, accurate, en-AU; a small Swift tool), at the scans' native 240 dpi. A full-page pass drops lines ("long;
  calyx green. Sep.-Dec." under Annona glabra), so a second pass OCRs each text column on its own and keeps the union. Columns: cluster
  the x0 of full-page lines of 20+ characters and under a column's width (not the running heads, which start mid-page, nor lines Vision
  ran across two columns); a cluster starting inside the previous column's width is its indented key lines, not a column. Each column
  is OCR'd with a 0.04 margin into its neighbours and rebuilt from Vision's **word boxes**, keeping the words whose centre lies in the
  column's core (between the mid-gutters): fixed strips clipped lines or left centred genus headings outside every strip, and dropping
  "cut" lines lost whole lines that Vision had joined to a neighbour's fragment. Lines wider than a column (a genus introduction set
  across the page, Syzygium p. 356) are taken whole from the full-page pass and read as a band between the columns above and below it.
  Strips of the facing page caught at a photo's edge (5+ narrow lines outside the page's own text) are dropped. Pieces of one visual
  line returned separately (the bold "Leaves" a little lower than its line) are joined. Check recall against the Adobe layer word by
  word per page (`recall2.py`: from ~1,190 missing words to ~770, mostly running heads and fragments).
  Vision slips seen: "mm" read "m" (6 sizes, fixed in fix_text after checking the Adobe layer; the palms' metre sizes are real), "Old"
  for Qld, "Elacocarpus", "Excoccaria", "Julv", "Mav", "var," - all corrected.
  Scripts and OCR output: `~/GitHub/austraits.build-data.scraping.scripts/data_extra/Cooper_2004_ocr/`; the assembled entries:
  `data_extra/cooper2004_profiles.csv`.
- **Assembly into entries** (one row per taxon, fields habit / leaves / inflorescence / flower / male & female cones / fruit / notes /
  distribution; plus common name, etymology, other names, book page, genus text). Pitfalls found:
  - Plate captions repeat names ("Annona squamosa" under the painting): a bare name is a caption when the name has a proper heading
    (authority on the line or the next) elsewhere, or when it repeats the entry being read; two-line captions ("... fitzalanii" /
    "subsp. tenuipes"). Not body lines that look like names ("Fruit unknown.", "Any month.", "Domatia present").
  - Authorities may be full names on the next line ("Michael J. Parsons", "Peter G. Wilson") or carry the infraspecific name ("Lam. subsp.
    integrifolia", "(F. Muell.) Puttock subsp. fitzalanii", "forma grayi"); phrase-name brackets may be on the next line ("Medicosma sp."
    / "(Karnak)"); infraspecific phrase names in brackets ("Rapanea subsessilis (subsp. Gordonvale)").
  - Genus headings misread ("ACACI", "DIOSPY.", "MACARAI"): the genus as spelt in its etymology line ("Acacia from ..."), or the genus
    word of the species headings when it heads 2+ species (never build a name from the current genus when the heading names another real
    genus). Etymology lines are not species ("Asystasia possibly from ...", "Abrus thought to ..."); habit lines are not species
    ("Pandanus tree to 10 m", "Vine twiner" ~ Vigna).
  - Keys (numbered couplets) pause the entry; it resumes only at a field it has not reached yet, below a gap, not on a second couplet
    lead ("Fruit receptacle conspicuous") - a key to varieties sits between a species heading and its description (Stephania japonica,
    Ficus virens, Carnarvonia araliifolia).
  - Gaps between lines are measured to the line above that overlaps horizontally (a caption or key answer beside a heading made its gap 0).
  - Line-break hyphens are told from compounds by the words seen whole elsewhere ("Cas-sowaries" but "Rat-kangaroos", "fruit-doves").
  - Ficus has a "Figs" paragraph instead of Fruit. A family heading ends the entry being read, but a running head repeating the current
    family does not (it cut off continued entries). A name with two entries (a key answer inside a family key read as a heading) keeps
    the entry with the most fields; adjacent entries whose epithets are clipped forms of one another ("Bridelia leichhardt") are merged.
  - QA: entries per genus against the book's "Rainforest tropical Queensland: N species" (`genus_counts.csv`: 495 of ~520 counted genera
    match; most others are infraspecific taxa the book does not count), duplicate names, entries without fields. Result: 2,416 taxa.
- **Same parser, Cooper mode:** `FOA_SOURCE=cooper`. The months that end the Flower and Fruit paragraphs ("Sep.-Dec.", "Jan.-Apr. & Aug.",
  "Any month", "Season unknown") go to the phenology text ("Flowers Sep.-Dec. Fruits Oct.-June."). FoA forms: "diam. 50-80 mm" -> "50-80
  mm diam.", "about" -> "c.", "x" -> "×", "mm through" (third fruit dimension) -> "thick". "Flower petals 6, ..." -> "Petals 6, ...". The
  habitat is the Distribution's opening (before "from / in the ... [place]"; "a native of X now naturalised in ..." -> introduced). Family
  modes use the current family (config/taxon_list.csv by genus); the book's family is in raw/taxon_info.csv. **All sources** (user
  2026-11-08; the others take it at their next rebuild): "separate male and female plants" -> dioecious. Cooper-only (gated; user
  2026-11-08): the hedges "may be", "may have", "can have", "can be" are rare-type qualifiers recorded as **sometimes** ("may be
  buttressed", 930 in the book; the words stay in value_description); **"Vine" is plain `climber`** (the user's rule: "vine" is
  climber_herbaceous only in a source that also uses "liana"; the word "liana" appears nowhere in this book, text layer or OCR). The book
  never says woody or herbaceous for a climber either, so a plain climber takes the habit's hints (user 2026-11-09): corky bark, older
  stems corky or lenticels, or a shrub/tree alternative ("Shrub or vine twiner") -> `climber_woody`; a herb alternative ("Herb or vine
  trailer") -> `climber_herbaceous`; Deeringia arborescens -> `climber_woody` from its etymology ("this vine becoming large and woody");
  a stem diameter alone is not a hint (Cassytha); the other ~350 climbers stay `climber`; a fruit given only as "diam. X mm" gets width and height, not length
  (user: the globose-only rule stands); altitude ranges are left in the distribution text (user: skip). A vine's habit size in mm is an
  organ's, not the climbing length
  (Capparis "spines ... 1-3 mm long"). Ranks are set from the names ("forma" -> "f."; "Rapanea subsessilis (subsp. Gordonvale)" is a
  subspecies). Nothing is copied down: the genera and families have no morphological description. Build check (traits.build
  dataset_process, `ignore/jobs/cooper1/proc_cooper.R`): 83,473 values, 2,416 taxa, 119 traits, 1 value out of range (Dolichandrone
  petals 125-175 mm, real).
- **Notes -> dispersers** (`data/Cooper_2004_2/raw/vet_notes.R` -> raw/ecology_vetted.csv; user 2026-11-08, as in Cooper_2013): "fruit /
  flesh / aril eaten by X" (also "seeds and aril eaten by") -> `dispersers` (Cassowaries -> cassowaries, flying-foxes -> flying_foxes,
  bats, rats / melomys -> rodents, rat-kangaroos / pademelons / possums / pigs -> mammals, any bird -> birds; a shared head noun "Regent
  and Satin Bowerbirds"); "seeds eaten by X" alone -> candidate `seed_predators`. The clause is the evidence.

## Book PDFs with a text layer: Brophy, Craven & Doran 2013 (Brophy_2013, user 2026-11-09)

- **Scope:** "Melaleucas: their botany, essential oils and uses" (ACIAR Monograph 156), chapter 7 species accounts, five PDFs in
  `OneDrive .../resources-books/` (`Melaleucas_mn156-species-a-c_1.pdf`, `-d-k`, `-p-r`, `-s-z`, and `mn156-species-l-o_1.pdf`, named
  differently). 290 species (every index entry), 27 subspecies, 10 varieties. One new dataset, `Brophy_2013` (none existed). The book's
  non-Australian taxa (7 New Caledonian species and M. sphaerodendra's 2 varieties, M. cajuputi subsp. cumingiana of South-East Asia) stay
  in data.csv and are listed under metadata.yml `exclude_observations` (variable taxon_name; user 2026-11-09: **this is how a source's
  non-Australian taxa are handled**). Lord Howe Island taxa (M. howeana) are Australian.
- **Text:** born-digital (InDesign), so the PDFs' own text layer is used, no OCR: `pdftotext -bbox-layout` blocks, read left column then
  right, rotated running heads and page numbers dropped (`data_extra/Brophy_2013_text/scripts/blocks.py`); a species starts at its heading
  block ("Melaleuca acacioides F.Muell.", near the page top) and is split at the book's labels (`profiles.py` ->
  `data_extra/brophy2013_profiles.csv`). Text-layer slips: soft hyphens; hard line-break hyphens told from compounds by word counts over
  the book (line-internal words only: a line's first word may be the broken piece); three letter-spaced lines and "paper y", "hair y";
  label variants ("Natural Occurrence", "Natural Distribution", "References on essential oils :"); M. nesophila's publication printed
  under "Description:".
- **Same parser, Brophy mode** (`FOA_SOURCE=brophy`, all gated): Ecology "Recorded as occurring in ..." -> habitat ("Grows in ..."),
  Flowering time "Recorded as flowering from May to November" -> phenology as written, Natural occurrence -> distribution. A species with
  subspecies / varieties has one "subsp. X:" paragraph per taxon in those fields and a key in its Notes ("The two subspecies are
  distinguished as follows: subsp. X: ...; subsp. Y: ..."; also "have been distinguished", "can be recognised ... as follows"): each
  infraspecific taxon gets a row with its paragraphs and its key text as its description (the species description inherited where silent);
  the species row keeps the species description, and the key is removed from its Notes. Craven's leaf blade: the cross-section shapes
  ("in transverse section transversely elliptic, transversely linear or ...", over several commas up to ", the base") are not leaf_shape
  (they had leaked into 212 taxa); an indumentum clause before the outline ("blade glabrescent, the indumentum as on the branchlets,
  linear ...") is bracketed so the outline stays in the blade's unit (17 species had no leaf_shape). **All sources:** "Stamens 6-7 per
  bundle" is not read as a stamens-per-flower count. **Brophy mode** (user 2026-11-09): it is scored per flower, times the bundles (5 in
  Melaleuca, or as stated, "usually in 5 bundles or occasionally in 6, 15-26 per bundle when in 5 bundles" -> x 5), the words and the
  multiplier in value_description ("6--7 per bundle [x 5 bundles]"), extremes kept; a per-flower count later in the clause ("Stamens free
  or ..., 52-71 per flower") is read too. Not scored (no traits.yml trait): bundle claw, ovules per locule, triads / monads per inflorescence, inflorescence width, length:width ratio, oil gland density,
  cotyledons, filament colour (candidate, as in ABRS_Myrtaceae_2026).
- **Oil yield** (`data/Brophy_2013/raw/oil_yield.R` -> raw/ecology_vetted.csv): dry-weight yields ->
  `leaf_essential_oil_per_dry_mass`, fresh-weight yields -> **`leaf_essential_oil_per_fresh_mass`** (added to config/traits.yml by the user's
  request 2026-11-09, mirroring the dry-mass trait); unit_in %, as written; "<0.1%" -> max, "nil" -> 0, "trace" not a value; mixed
  chemotype / collection / basis statements by hand (range over them, per basis); the two yields the book calls unreliable are left out.
  The oil composition has no trait.
- Build check (`ignore/jobs/brophy1/proc_brophy.R`): 12,691 values, 317 taxa, 59 traits; nothing excluded beyond the 10 non-Australian taxa. 43 names are not in
  config/taxon_list.csv (the New Caledonian and Asian taxa, ex-Callistemon names, new infraspecific taxa).

## Australian Tropical Rainforest Plants online (ATRP_2026, user 2026-11-09)

- **Scope:** the Lucid online edition 8 species pages, scraped 2026-11-08 (`data_extra/atrp_fact_sheets.csv`, pages in `ATRP_html/`):
  2,762 species and infraspecific taxa; the 176 family pages describe families world-wide and are not read. **New dataset `ATRP_2026`**
  beside ATRP_2020 (growth form and a few other traits from the 2022 scrape; user: "New ATRP_2026").
- **Names:** use `index_name`, not `taxon_name` - the scrape's taxon_name drops the infraspecific part of 209 names (both subspecies of
  Actephila petiolaris were "Actephila petiolaris"); "( L.S.Smith" -> "(L.S.Smith".
- **Same parser, ATRP mode** (`FOA_SOURCE=atrp`, gated): Stem, Leaves, Flowers and Fruit paragraphs are the description; Seedlings the
  seedling text; Distribution and Ecology is the distribution, and its habitat sentences ("grows in", "found in", "A characteristic tree
  of ...", "Parasitic on rain forest plants", any sentence naming a vegetation type or substrate, but not "Occurs in CYP ...", "Endemic
  to ...", "Also found in ...", "Altitudinal range ...") the habitat; "An introduced species" / "naturalised" -> introduced. Page link,
  RFK code, synonyms and icons go to raw/taxon_info.csv, not on every row.
- **Habit icons** (`ATRP_scripts/page_extras.py` -> `atrp_page_extras.csv`; user 2026-11-09): the icons are the growth form and the text's
  growth forms the icons lack are added ("[ATRP habit icon] ..." in value_description): Tree, Shrub, Herb, Grass -> graminoid, "Palm,
  pandan or cycad" -> palmoid, **Vine -> climber_woody** (its pages say "stem diameters to N cm recorded"), **Slender Vine ->
  climber_herbaceous** (stem diameter up to c. 2 cm); a text climber beside an icon climber is dropped; Mistletoe -> stem_parasitic
  (when the text says nothing), Epiphyte -> epiphyte substrate; **Weed is ignored** (introduced status comes from the text). A vine's
  climbing height must be in m (the Stem paragraph's "Vine stem diameters to 3 cm recorded" is not a height; as for Cooper).
- **Seedlings** (`data/ATRP_2026/raw/seedlings.R` -> raw/ecology_vetted.csv): seedling_germination_location from the seedling photo
  caption ("Cotyledon stage, epigeal germination"); seed_germination_time from "Seed germination time 15 to 19 days" ("ca." read,
  "to 43 days" -> max). Germination times over 1,000 days (Acacia auriculiformis "9 to 3664 days") are excluded by the traits.yml range.
- Text slips fixed (fix_text): m for mm (Argyrodendron sp. Boonjee flowers, Ristantia filaments, Diospyros rugosula corolla lobes), Cocos
  leaflets given width x length, Philydrum "bulb-shaped" seeds read as a bulb.
- Build check (`ignore/jobs/atrp1/proc_atrp.R`): 2,762 taxa, 152 traits; ~48 values excluded (germination times > 1,000 d, 0.5 mm
  tubes, palm prophylls as bracteoles, corolla shapes such as urceolate / campanulate given as petal shapes).
- Known parser issue seen here (all sources, not fixed): a pedicel length after a comma inside a flower clause ("Male flowers: ... on
  filiform pedicels, 5-9 mm long") is also read as flower_length.

## Book scans: George 2002, Verticordia (George_2002, user 2026-11-09)

- **Scope:** E.A. George (2002) "Verticordia: the turner of hearts" (plates by M. Pieroni), the species accounts: six photographed PDFs
  (`resources-books/Verticordia George compressed_Part1.pdf` ... `_Part6.pdf`, 145 pages, 150 ppi). `Verticordia George.pdf` in the same
  folder is the same 145 pages at 560 ppi: OCR it (page numbers match). 101 species, 23 subspecies, 44 varieties. New dataset.
- **OCR** (`data_extra/Verticordia_ocr/scripts/page.py`): the Paper Capture layer is unusable (scrambled lines). Apple Vision on the page
  rendered at 250 dpi, three ways: the whole page, the two text columns cropped, and three overlapping full-width bands. Layout: a
  full-width species description and key, then two columns of notes. Vision's word boxes inside a line are too rough to split a line it
  ran across both columns, so a wide line is replaced by the column crops' lines when the left crop's text ends short of the gutter and
  the right crop's starts at it. Lines the full page missed are added from the bands (full-width text, first) and then the crops. Full-page
  "lines" much taller than their kind's median are Vision garbling several lines and are dropped (compared with lines of the same kind:
  the full-width text is set larger than the columns; never drop entry headings). Map labels, page numbers and the running foot dropped;
  key answers at the far right ("...... var. major") joined to their line. Recall vs the Adobe layer (`recall.py`): ~410 missing words, mostly
  the layer's own split words ("culti vation").
- **Entries** (`entries.py` -> `data_extra/verticordia_profiles.csv`): headings "4. Verticordia endlicheriana Schauer (1846)", "4a. ... var.
  endlicheriana", hybrid "91. Verticordia × eurardyensis", wrapped "21b. ... subsp. cylindracea A.S. George" / "var. cylindracea (1991)"
  (names keep every rank); an infraspecific heading's species epithet is its species entry's (OCR "servata"). Species: common name,
  etymology / type, description (first sentence that opens a habit, "A shrub ...", "Usually a slender spindly shrub ...", even without a
  full stop before it), key ("There are five varieties:", "The species has two subspecies, one of which contains two varieties:", numbered
  or unnumbered two-taxon keys). Each taxon's notes: labelled DISTRIBUTION, HABITAT, FLOWERING PERIOD, DISTINGUISHING CHARACTERISTICS,
  RELATED (AND CONFUSING) SPECIES, OTHER NAME(S) USED, CONSERVATION STATUS, CULTIVATION, PLATE. An infraspecific taxon's opening
  paragraph is split by sentence: name / collection sentences -> etymology_type, the rest -> its description. Key paths: each taxon gets
  the leads on its path through the key (answers matched fuzzily to the entries, "subsp. roet"); used only when the key reads cleanly.
- **Same parser, Verticordia mode** (`FOA_SOURCE=verticordia`, gated): species description; infraspecific description + key path (species
  inherited where silent); Habitat, Distribution, "Flowers <Flowering period>"; Notes = distinguishing + related species (not cultivation:
  garden plants). "Floral leaves" sizes -> `entity_measured` floral_leaf (beside "Stem leaves" cauline_leaf); their shapes are still in
  leaf_shape with the stem leaves'. OCR slip fixed: V. dasystylis subsp. dasystylis key "petals 2-2.5 m long".
- Build check (`ignore/jobs/vert1/proc_vert.R`): 168 taxa, 56 traits, nothing excluded; 75 candidate traits (staminodes, hypanthium,
  sepal colour ...) not in traits.yml.

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
- **Organs opened to other groups (batch 3):** sedges, restiads and other graminoids (`gram_families`: Cyperaceae, Restionaceae,
  Centrolepidaceae, Anarthriaceae, Ecdeiocoleaceae, Juncaceae ...) have glumes, awns, rachilla, sheaths, ligules and auricles as
  organs, as Poaceae (glume_*, leaf_sheath_*, ligule_*, awn_length, glume vein count and awn presence, inflorescence branches);
  restiad male / female spikelets keep their entity_measured rows. Gingers (`ginger_families`) have the leaf sheath, ligule and
  auricles. Lemma, palea, lodicule and callus stay grass-only words. Involucral bracts of heads in any family are counted as
  `involucral_bract_count` (Pimelea "involucral bracts 4-6"), separate from `bract_count`.
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
- A hedged swamp habitat ("may be anticipated along streams and in swampy areas", E. rudis) → terrestrial + a secondary
  `semiaquatic` row (`swampy` until 2026-11-07) with scoring_certainty possibly. A habitat sentence for the species that says "habitat depending on the subspecies" is
  scored only on the subspecies whose own paragraph names that habitat.
- Hybrids written "Eucalyptus x phylacis" get rank species (they were dropped when the profile had no rank).
- The FoA scrape loses text after a "<" ("branches <8 cm diameter smooth" → "branches Juvenile growth …"; ~17–29 profiles,
  mostly eucalypts): a scraper issue, reported to the user.

## Fixes from the NHNSW_2022 / 2023 comparison (all sources, 2026-10-31)

Compared NHNSW_2026 with the hand-curated NHNSW_2022 / 2023 after scraping (not before). Where they differ, the 2026 value was
usually right (the old datasets read "c. 10 cm" fruit as the stalk / style / beak beside it). Real 2026 gaps, now fixed:
- Leaf types "uppermost / lowermost / lowest / middle / median leaves" get their own entity_measured rows (upper_leaf,
  lower_leaf, middle_leaf), like basal / cauline.
- Lobation: lobes described in their own clause ("with 2-9 acute lobes") → lobed; lyrate → lobed; "deeply divided / parted /
  partite / pinnatifid / bipinnatifid / pinnatisect" → lobed_deep. Not a notched apex ("bilobed at apex"), not ferns.
- 1-foliolate (compound) leaves: margin terms of the lamina are scored.
- Lamina posture from "upper / adaxial surface flat / concave / convex".
- Herbs (from the life-history words) are woodiness herbaceous (trait_scoring_method inferred_from_life_history).
- Outside ferns, a rhizome clause or "rhizomatous" habit → stem_growth_habit rhizomatous.
- Height: "Shrub, 20-100 cm, occasionally to 180 cm high" (no dimension word on the typical range) → 0.2-1 m typical + 1.8 m
  occasionally; "to 0.5 m high, occasionally erect and to 2 m high" → a second, occasional height.
- Leaf width: "6-50 mm wide, lamina pinnatisect with the undivided part of lamina 1-2 mm wide" → 6-50 mm (the undivided part /
  central part is not the leaf); a leaf-unit width is used when the lamina unit gives only a length.
- "up to 5 or more cm wide" is read as "5 cm or more".
- New traits for every taxon: `petiole_width` ("petiole 2-6 mm long, 1-2 mm wide") and `leaf_pendulousness` (only from the
  leaf clause naming hanging leaves; an erect / spreading alternative in that clause → not_pendulous; unscored otherwise).
Differences that are 2026 conventions, not gaps: leaflet / pinna sizes go to leaflet_* traits (the old datasets put them under
leaf_* with an entity). (Fern rhizomes as storage organs: reinstated 2026-11-01, see below.) The old datasets' extra fruit types (Pterostylis capsule, Poa
caryopsis, Hibbertia follicle) led to the copy-down change under Principles (hedged group statements copied; members' own
values no longer block the copy).

## Rules from the NHNSW taxon-by-taxon review (user 2026-11-01)

- **Not copied down from genus / family** (they vary between species): leaf_lobation, leaf_margin, leaf_margin_posture,
  leaf_lamina_posture, stem_growth_habit.
- **Fern rhizome and caudex are storage organs** (storage_organ rhizome / caudex), as in NHNSW_2023 (reinstated).
- Storage organs: "tuberous corm" = corm; "fleshy tuberous rhizome", "rhizome ..., tuberous" = rhizome_fleshy; tubers on the
  roots ("roots ... tuberous", "roots ending in tubers", "taproot ... tuberous", "tubers on roots") = root_tuber; tubers on
  rhizomes = stem_tuber; a bulbous culm / stem base is not a bulb; "bulbs and bulbils absent" is no bulb; "rhizomed" = rhizome;
  "Rhizome 7-18 mm thick, woody" = rhizome_woody.
- plant_photosynthetic_organ leaf: a taxon is leafy if any own leaf character is scored (count, lobation, posture, margin,
  arrangement), not only a size or shape; the leafless / scale-leaf test reads the habit, stem and leaf text only (not
  "leafless racemes", "flowers subtended by scale leaves") and ignores partial leaflessness ("basal leaves absent", "leafless
  in the lower half", "sometimes ± leafless", "leafless in drought").
- Leaf types with two modifiers: "Upper stem leaves", "Lower stem leaves", "Mid-stem and upper-stem leaves", "Mid-stem leaves"
  are leaf units (upper_leaf / lower_leaf / middle_leaf).
- Margin posture: a posture word in the unit after the margin unit is the margin's ("margins entire, revolute"); compound-leaf
  margins in the leaf's own unit count ("Leaves 1-foliolate ...; margins often crenate"); "plane" = flat, "crinkled" = undulate.
- Lobation also from margin clauses ("margins entire or shallowly lobed"); "entire or with ... lobes", "entire to
  pinnatisect" add unlobed.
- Stem habit: postures of hairs are not stem habits ("stems with spreading ... hairs"); "tussock-forming" = caespitose;
  "with loose tufts" = tufted. "stem-clasping" does not start a stem clause.
- Seed texture also from a separate "surface ..." / "testa ..." clause; nouns count (ribs, ridges, pits, tubercles, papillae).
- Sizes: "A-B cm, rarely to C cm long/wide/high" keeps the typical range; "A-B (rarely to C m) tall" (unit in the bracket);
  "to 20 cm long and wide" gives the width too; "10-45 X 1-4 cm"; "c.2mm"; "3.5-6.5 cm in diam."; "2-7 cm long on stalks
  2-5 cm long" is the fruit's (a preposition before the next organ); "skin / rind / valves ... thick" is not the fruit's
  height; "Multiple / Individual fruit", "Outer / Ray / Disc achenes", "Female cones", "winged seeds" are fruit / seed
  clauses; a globose fruit "3-4 mm wide / across" is its diameter (length when no length is given).
- An infraspecific description that opens with diagnostic organs ("Peduncles glabrous ... Shrub to 2 m high") takes its
  habit from the later sentence that opens with a growth form.
- Fruit type: "cocci" = mericarp; the seed cones of cycads and conifers = strobilus (not Casuarina "cones"). In a group
  description a rare alternative takes its list with it ("Fruit a caryopsis ..., rarely a nut, berry, or an utricle" copies
  caryopsis only).
- **Decided 2026-11-02 (user):**
  - **Cladode** for the green stems of leafless or scale-leaved plants (Casuarinaceae, Cactaceae, Restionaceae, Psilotaceae,
    Equisetaceae, Tecticornia / Salicornia, Exocarpos, Cassytha, Ephedra, scale-leaved legumes; traits.yml: "a photosynthetic
    shoot, regardless of form"), as in NHNSW_2023; achlorophyllous plants (mycoheterotrophs, holoparasites; Thismia,
    Rhizanthella) are non-photosynthetic_plant. Inferred rows (trait_scoring_method inferred).
  - **Globose fruits and seeds given only a length** also get that width and height ("Capsule globose, 9-12 mm long"; the
    description says "[globose fruit, from the length]"), as a diameter gives length, width and height.
  - **Carex utricle (perigynium):** sizes go to fruit_length / fruit_width (+ height from a diameter) with
    plant_organ_measured = utricle; when the nut is measured too, the nut's sizes carry plant_organ_measured = nut. The fruit
    type stays nut. ("Utricle" in Cyperaceae is the perigynium, never a fruit type.)
  - **Leaflet, pinna and pinnule characters are scored in the leaf traits** (shape, base, apex, margin, margin / lamina posture,
    lobation, surface colour, discolority, reflectivity, glaucousness, texture, hairs, venation) with the `leaf_division`
    context (leaflets / pinnae / pinnules): "from the perspective of those traits they are better all scored in one trait".
    Not leaf_phyllotaxis, which is about leaves only. Leaflet sizes and counts stay in the leaflet_* traits.

## Post-scraping step: plant_height_type (user 2026-11-02)

For every taxon with a profile but **no `plant_height`** in that dataset, `plant_height_type` records why, from the taxon's own
scored traits, so it is clear which other trait holds its length (`plant_height_reproductive`, `plant_height_climbing_plant`,
`stem_length`). Never scored when `plant_height` is. Only where a reason applies (user: nothing otherwise; `plant_height_missing`
is not used). Several reasons are space-separated; trait_scoring_method inferred; the description gives the evidence and the
trait holding the length ("[inferred: no plant_height in the profile; growth form climber_woody; length recorded as
plant_height_climbing_plant]").
- marine; aquatic / aquatic_floating / aquatic_submerged (aquatic substrate, floating or submerged habit);
- epiphyte, lithophyte (that substrate, not only as a rare alternative);
- climbing (climber growth form, or a climbing-plant height);
- geophyte (a herb with a bulb, corm or tuber, or a terrestrial orchid; with or without a recorded flowering height);
- rosette (a reproductive height and a rosette or basal leaves); leaf_tuft (a reproductive height and a tufted / caespitose
  habit or graminoid);
- prostrate (prostrate or creeping, not also erect); mat-forming (mat- or cushion-forming);
- rhizome (ferns with a rhizome).
Heights feeding this step: an orchid "Flowering plants 20-60 cm tall" is plant_height_reproductive; an orchid "Inflorescence / Raceme
/ Scape ... high" is a flower-stem length (terrestrial: also plant_height_reproductive); a scape or inflorescence height later in
any description ("... Inflorescence to 70 cm high") is plant_height_reproductive when no height was found.

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
- **Mallet, marlock (user 2026-11-05):** scored as the source term (`mallet`, `marlock`; "Mallett" too), mapped to `mallee` (user's mapping) by a
  `match: word` substitution in each dataset's metadata.yml so the mapping can change later; they count as trees for the life
  history / woodiness inferences.
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
  creeping / climbing / decumbent / stoloniferous, as Wenk_2025 did; and `storage_organ` = rhizome (caudex for a trunk-like
  caudex), as NHNSW_2023 did (user 2026-11-01).
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
