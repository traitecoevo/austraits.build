# Pending changes to the ABRS_*_2026 / CPBR_2026 data.csv files

**Status (2026-10-30, flora session):** section 1 applied (context `cue_scored_from` in build_data_csv.R and in the AtoM,
NtoZ and Myrtaceae metadata; 13 + 6 + 4 taxa as listed). Section 2/3 mappings are safe: metadata is now updated add-only
(`ignore/jobs/cp1/meta_update.py`), which keeps var_in != trait_name mappings, context values and substitutions; the lost
`fruiting_cues` mappings were restored. The parser still writes the draft names / values (the optional clean-up in 2-3 is not done).

Changes decided but not yet applied, because another session was editing the data.csv files at the time.
Apply them in `build_data_csv.R` (which builds every ABRS_*_2026 dataset and, in EUCLID mode, CPBR_2026), then rebuild.

## 1. Mark flowering vs fruiting cues with a "cue scored from" context (user's request 2026-10-30)

**Background.** metadata.yml now maps `fruiting_cues` to the trait `flowering_cues` (alongside `flowering_cues` itself).
Taxa whose text gives the same cue for flowering and fruiting ("Flowers and fruits after rain") therefore get the
same `flowering_cues` value twice. Affected taxa (2026-10-30 data, all with identical values):

- ABRS_AtoM_2026 (13): Aponogeton queenslandicus; Drymaria cordata, subsp. cordata, subsp. diandra; Dysphania congestiflora;
  Alchornea ilicifolia; Croton aridus, C. insularis, C. phebalioides; Excoecaria dallachyana, E. parvifolia;
  Haemodorum thedae; Litsea breviumbellata
- ABRS_NtoZ_2026 (6): Passiflora cinnabarina; Synostemon trachyspermus; Petalostigma banksii, P. pachyphyllum,
  P. pubescens; Tripogon loliiformis
- ABRS_Myrtaceae_2026 (4): Aluta maisonneuvei, subsp. auriculata, subsp. maisonneuvei; Decaspermum humile

**Change.** For taxa with both a `flowering_cues` and a `fruiting_cues` row, record which statement each value was scored from:

1. `build_data_csv.R`: add a context column `cue_scored_from` to `ctx` (line ~4315). After the cue records are built,
   for every taxon with both traits, set `cue_scored_from = flowering` on its `flowering_cues` rows and
   `cue_scored_from = fruiting` on its `fruiting_cues` rows. Leave it blank everywhere else.
2. Each affected dataset's metadata.yml `contexts:` block (AtoM, NtoZ, Myrtaceae; add to the others too if a rebuild
   gives them affected taxa):

   ```yaml
   - context_property: cue scored from
     category: method_context
     var_in: cue_scored_from
     values:
     - find: flowering
       value: flowering
       description: Cue scored from a statement about the taxon's flowering.
     - find: fruiting
       value: fruiting
       description: Cue scored from a statement about the taxon's fruiting.
   ```

3. Rebuild, then check with `traits.build::dataset_process()` that each affected taxon has two `flowering_cues` values
   with different `cue_scored_from` contexts.

Open question: should `fruiting_cues` rows of taxa *without* a flowering cue (e.g. the 2 mapped rows in CPBR_2026)
also be marked `fruiting`? As requested, only the taxa with both are marked.

## 2. metadata.yml mappings at risk of being dropped by metadata regeneration (2026-10-30)

Another session regenerates the ABRS metadata.yml `traits:` blocks and `custom_R_code` whitelists from traits.yml and
appears to keep only mappings whose `var_in` equals `trait_name`. The mappings below differ from that pattern; after
any regeneration, check they are still present and re-add any that are missing. Each mapped entry is a numeric
(mm, `basis_of_value: measurement`) entry, and its `var_in` must also be in the dataset's `custom_R_code` whitelist.

**fruiting_cues -> flowering_cues** (user's choice): ABRS_AtoM_2026, ABRS_NtoZ_2026, ABRS_Fabaceae_2026,
ABRS_Myrtaceae_2026, ABRS_Proteaceae_2026, CPBR_2026. (At 20:05 already lost from AtoM, Myrtaceae, Proteaceae and CPBR.)

**Family-specific organ terms mapped to the generic trait, with a trait-level `plant_organ_measured`** (user's request:
"glume_length = bract_length with structure measured = glume"):

| var_in | trait_name | plant_organ_measured | datasets |
|---|---|---|---|
| glume_length | flower_bract_length | glume | NtoZ |
| lemma_length | flower_bract_length | lemma | NtoZ |
| palea_length | flower_bract_length | palea | NtoZ |
| involucral_bract_length | flower_bract_length | involucral bract | AtoM, NtoZ, Asteraceae, Proteaceae |
| spathe_length | flower_bract_length | spathe | AtoM, NtoZ |
| inflorescence_bract_length | flower_bract_length | inflorescence bract | Fabaceae |
| receptacle_palea_length | flower_bract_length | receptacle palea | Asteraceae |
| pappus_length | flower_sepal_length | pappus | Asteraceae |
| labellum_length | flower_petal_length | labellum | AtoM, NtoZ |
| floret_length | flower_length | floret | NtoZ |
| spikelet_length | inflorescence_length | spikelet | AtoM, NtoZ |
| glume_width | flower_bract_width | glume | NtoZ |
| lemma_width | flower_bract_width | lemma | NtoZ |
| palea_width | flower_bract_width | palea | NtoZ |
| involucral_bract_width | flower_bract_width | involucral bract | AtoM, NtoZ, Asteraceae, Proteaceae |
| spathe_width | flower_bract_width | spathe | AtoM, NtoZ |
| receptacle_palea_width | flower_bract_width | receptacle palea | Asteraceae |
| labellum_width | flower_petal_width | labellum | AtoM, NtoZ |
| leaflet_colour | leaf_surface_colour | leaflet | AtoM, NtoZ, Fabaceae |
| leaf_abaxial_colour | leaf_surface_colour | leaf abaxial surface | AtoM, NtoZ, Asteraceae, Fabaceae, Myrtaceae, Proteaceae |
| leaf_adaxial_colour | leaf_surface_colour | leaf adaxial surface | AtoM, NtoZ, Asteraceae, Fabaceae, Myrtaceae, Proteaceae |

**Other cross-name mappings** (no `plant_organ_measured`):

| var_in | trait_name | datasets |
|---|---|---|
| tepal_length | flower_tepal_length | AtoM, NtoZ, Fabaceae, Proteaceae |
| pistil_length | flower_pistil_length | AtoM, NtoZ, Fabaceae, Proteaceae |
| ovary_length | flower_ovary_length | AtoM, NtoZ, Myrtaceae, Proteaceae |
| juvenile_leaf_length | leaf_length_juvenile_leaves | AtoM, NtoZ, Asteraceae, Myrtaceae, Proteaceae, CPBR |
| hypanthium_width | flower_hypanthium_diameter | AtoM, NtoZ, Myrtaceae, CPBR |
| ovary_width | flower_ovary_width | AtoM, NtoZ, Myrtaceae, Proteaceae |
| bracteole_width | flower_bracteole_width | AtoM, NtoZ, Fabaceae, Myrtaceae, Proteaceae |
| tepal_width | flower_tepal_width | AtoM, NtoZ, Proteaceae |
| juvenile_leaf_colour | leaf_surface_colour_juvenile_leaves | AtoM, NtoZ, Fabaceae, Myrtaceae, Proteaceae, CPBR |
| anther_colour | flower_anther_colour | all seven |
| style_colour | flower_style_colour | AtoM, NtoZ, Asteraceae, Myrtaceae, Proteaceae |
| ovary_colour | flower_ovary_colour | AtoM, NtoZ, Fabaceae, Myrtaceae, Proteaceae |

(Simplest permanent fix for the renamed traits: have `build_data_csv.R` write the traits.yml names, so `var_in` =
`trait_name`.)

ABRS_Fabaceae_2026 also needs a `plant organ measured` context (entity_context, `var_in: plant_organ_measured`) in
`contexts:` for its inflorescence_bract_length mapping.

The `plant organ measured` context of each of these datasets needs an explicit `values:` list (value + description)
naming every organ above (including spikelet in AtoM / NtoZ) plus the values already in data.csv (fronds, trunk in
AtoM / NtoZ). Without it, traits.build
reads the context values off the data.csv column only and silently ignores the trait-level `plant_organ_measured`.
`dataset_test()` then reports these values as "not detected in context values from data file"; that test only checks
the data.csv column, and dataset_process() links the contexts correctly.

The new traits stipule_length, bracteole_length and involucre_length map `var_in` = `trait_name`, so regeneration
should keep them.

## 3. Batch of 2026-10-30 evening: cross-name mappings, value substitutions and two script splits

**Cross-name mappings** (check after any metadata regeneration; `var_in` must also be in the `custom_R_code` whitelist):

| var_in | trait_name | datasets |
|---|---|---|
| leaf_attachment | petiole_presence | all seven |
| anther_attachment | flower_anther_attachment | all but Asteraceae |
| fruit_surface | fruit_surface_texture | all seven |
| leaf_texture | leaf_constitution | all seven |
| juvenile_leaf_width | leaf_width_juvenile_leaves | all but Fabaceae |
| petal_fusion | flower_petal_fusion | AtoM, NtoZ, Fabaceae, Myrtaceae, CPBR |
| petal_shape | flower_petal_shape | AtoM, NtoZ, Fabaceae, Myrtaceae |
| corolla_lobe_length | flower_corolla_lobe_length | AtoM, NtoZ, Asteraceae, Fabaceae, Proteaceae |
| hypanthium_length | flower_hypanthium_length | AtoM, NtoZ, Fabaceae, Myrtaceae, CPBR |
| sepal_fusion | flower_sepal_fusion | AtoM, NtoZ, Fabaceae, Myrtaceae |
| style_shape | flower_style_shape | all seven |
| fruit_texture | fruit_constitution | all seven |
| filament_fusion | flower_filament_fusion_categorical | all but Proteaceae |
| branchlet_hairs | stem_hairs, with `plant_organ_measured: branchlet` (+ `branchlet` in the context values; CPBR got a new `plant organ measured` context) | all seven |

(`stipule_presence`, `fruit_shape` and `leaf_intramarginal_vein` map `var_in` = `trait_name`.)

**Value substitutions** (`substitutions:` block of each metadata.yml, `match: word`; the parser's draft values differ from
the traits.yml levels). Simplest permanent fix: have `build_data_csv.R` write the traits.yml levels directly, then drop
the substitutions.

| trait_name | find -> replace |
|---|---|
| stipule_presence | present -> stipule_present; absent -> stipule_absent |
| petiole_presence | sessile -> petiole_absent; petiolate -> petiole_present |
| leaf_intramarginal_vein | present -> intramarginal_vein_present; absent -> intramarginal_vein_absent |
| fruit_shape | globose -> globoid; hemispherical -> hemispheric; turbinate -> obconical |
| flower_petal_shape | elliptic -> elliptical; rhomboid -> rhomboidal; clavate -> spathulate |
| flower_style_shape | hemispherical -> hemispheric; cylindrical -> terete |
| leaf_secondary_vein_angle_categorical | acute -> less_than_45_degrees; wide_angle -> greater_than_45_degrees |
| fruit_surface_texture | rugose -> wrinkled; papillose, tuberculate, verrucose -> bumpy; striate -> ribbed; reticulate -> netted; muricate, echinate -> spiny |

Parser fixes in `build_data_csv.R` (the fruit_surface reflectivity split, petal_shape terms, misreads, cue false
positives) are listed in `parser_issues.md`.

Not yet mapped: `anther_dehiscence` (user asked what doesn't fit). slits -> `longitudinal_slit` and pores -> `poricidal`
fit `flower_anther_dehiscence`; introrse, latrorse and extrorse (direction of dehiscence) would need their own trait.
