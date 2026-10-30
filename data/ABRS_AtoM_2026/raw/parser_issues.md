# Parser issues in build_data_csv.R (ABRS_*_2026 and CPBR_2026)

**Status (2026-10-30, flora session):** sections 1, 1b, 1c, 2, 3 and 4 applied in build_data_csv.R (`value_fixes`,
`num_drops` and the cue fixes); fruit sheen goes to `fruit_surface_reflectivity`, glands / viscid to the candidate
`fruit_surface_glands`, winged to the candidate `fruit_wings`. Section 5 (writing traits.yml names directly) is not done.

Extraction errors found while defining new traits (2026-10-30), to fix in `data/ABRS_AtoM_2026/raw/build_data_csv.R`
and then rebuild. Metadata-only follow-ups (mappings, substitutions, contexts) are in `pending_changes.md` next to this
file. Counts are value words in the data.csv files of 2026-10-30.

## 1. petal_shape: terms that are not the shape of a single petal

`petal_shape` (mapped to the trait `flower_petal_shape`) must describe one petal. 97% of its values do: the outline
terms, plus hooded, bilobed, trilobed, lobed, boat-shaped, saccate and trigonous. The values below do not and should no
longer be written to `petal_shape` (traits.yml deliberately has no levels for them, so they are excluded until fixed):

| value | n | what the source says | fix |
|---|---|---|---|
| curved | 50 | *Lysiana* (12 taxa, genus description copied down): "Petals 6, united into a curved tube": the curvature of the corolla tube. Orchids (*Caladenia*, *Chiloglottis*, *Diuris*, *Microtis* ... ~35 taxa): mostly the sepals, e.g. "Dorsal sepal ... decurved", "tip often recurved". *Diplopeltis stuartii* (+2 varieties). | Drop: curvature is not petal shape, and in orchids the term belongs to another organ (check how sepal clauses leak into the petal unit). |
| salverform | 6 | *Pittosporum angustifolium*, *P. bracteolatum*, *P. lancifolium*, *P. multiflorum*, *P. spinescens*; *Billardiera cymosa* subsp. *pseudocymosa*: "Petals cohering in the throat then recurving ..., salverform" | The whole corolla: score `flower_shape = salverform` instead. |
| tubular | 6 | *Laxmannia compacta*, *Pittosporum tenuifolium*, *Aquilegia vulgaris* (genus): the petals together form a tube. *Lepidium catapycnon*, *L. echinatum*, *L. pedicellosum*: "pseudo-tubular", one petal rolled inwards | The corolla: score `flower_shape = tubulate` where the petals form the tube; drop for *Lepidium*. |
| flattened | 2 | *Lepidium platypetalum*: "Petals ... rhomboid when flattened"; *L. amelum*: "distal c. one-third flat" | Drop: describes how the petal was measured, not its shape. |
| cylindrical | 1 | *Philotheca nutans*: the term belongs to another organ | Drop. |
| straight | 1 | *Diuris refracta*: "stalk 3-6 mm, straight or curved" (the petal stalk) | Drop. |
| cup-shaped | 8 | *Laxmannia brachyphylla*, *L. grandiflora* (+2 subspecies), *L. omnifertilis*, *L. paleacea*, *L. squarrosa*: "Petals cupular" | The flower: score `flower_shape = cup-shaped` instead (user's call: a cup-shaped petal is a flower shape). |
| cup-shaped (+ orbicular) | 1 | *Garcinia mestonii*: "outer pair joined, D-shaped, cupular" (the sepals) | Drop: another organ. |
| trigonous | 1 | *Andersonia ferricola*: "Bracteoles ... apex acuminate, trigonous" | Drop: the bracteoles. (Keep trigonous for *Xylopia maccreae*: "Outer petals ... trigonous".) |
| obovate, lobed | 1 | *Hypericum calycinum*: the sentence describes the sepals ("sepals 5, markedly unequal, elliptic, orbicular or semi-orbicular to obovate ...") | Drop: another organ. |

## 1b. style_shape: terms from the stigma, style branches or other organs

`style_shape` (mapped to `flower_style_shape`) must describe the style itself. traits.yml has no levels for the values
below, so they are excluded until fixed:

| value | n | what the source says | fix |
|---|---|---|---|
| globose | 38 | Apocynaceae: "Style-head conical-globose" (*Leichhardtia*, *Gymnema* ...): the style-head is the stigmatic head; *Carissa*: "Fruit globose" | Style-head shapes -> `stigma_shape` (candidate); drop the fruit sentence. |
| capitate | 35 | *Cassytha*, *Elatine*, *Asparagus* ("style ... capitate or minutely trifid"), *Lysicarpus* ("Style stout, capitate"): the stigma | -> `stigma_shape`. |
| ovate, cordate, triangular | 24 | *Hypoxis*, *Pauridia*: "Style columnar ... including ovate / cordate / triangular lobes" (the stigmatic lobes); *Iris*, *Moraea*: style "crests" | Drop from style shape (stigmatic lobes -> `stigma_shape`). |
| spathulate | 2 | *Babiana stricta*, *Sparaxis villosa*: "style branches ... spathulate" | Drop: the style branches. |
| cup-shaped | 7 | *Stirlingia* (Proteaceae) | Not the style (check: probably the pollen presenter or stigma). |
| plumose | 2 | *Doryphora*: "style plumose" | Hairiness, not shape (-> `style_hairs`, candidate). |
| tubular | 2 | *Eucalyptus discreta*: "a tubular downgrowth of the inner roof of the operculum" | Drop: the operculum. |
| discoid, obovoid | 2 | *Parthenium hysterophorus*, *Voacanga grandifolia* | Not confirmed as style; drop or re-check. |

Kept levels that still carry some mis-attributed records (fix the records, keep the levels):
conical (Apocynaceae "Style-head conical-elongate" -> stigma; Vitaceae styles are genuinely conical),
lanceolate (*Iris*, *Moraea* "crests lanceolate"; *Petalostigma* styles are genuine), oblong (*Hypoxis* lobes,
*Dittrichia* "style branches ... oblong"; *Petalostigma*, *Andersonia* styles are genuine).

## 1c. stem_shape: ribbed is surface, not shape

`stem_shape` = ribbed (760; from ribbed, ridged, striate, grooved, sulcate: "Branchlets slightly ribbed", "Young stems
ridged", "Stems ... terete, striate") describes the stem surface. traits.yml `stem_shape` has no ribbed level (as in
`seed_surface_texture` / `fruit_surface_texture`, ribbed is a texture). Move these terms to `stem_surface` (candidate).

## 2. fruit_surface: sheen terms belong to a separate trait

`fruit_surface` is mapped to `fruit_surface_texture`. Write shiny (274), dull (64) and glaucous (418) to a separate
column `fruit_surface_reflectivity` (trait now in traits.yml) instead, and add its mapping to each metadata.yml (+
`custom_R_code` whitelist). glandular (197), viscid (150), gland_dotted (5) and winged (209) have no trait yet (user's
choice); leave them out of `fruit_surface`. Until this is done, the build excludes the 1,289 `fruit_surface` records
that contain any of these words, including their texture terms.

(`fruit_shape` needs no split: its curvature terms straight, curved, coiled and twisted are now `fruit_shape` levels.)

## 3. Width / diameter misreads (ABRS_AtoM_2026)

Found as values above the traits.yml maximum (100 mm):

- *Cereus uruguayanus*, `tepal_width` 100-140 mm from "10-14 cm wide": the width of the flower, not of a tepal.
- *Goodenia panduriformis*, `bracteole_width` 120 mm from "to 12 cm diam": the diameter of something other than a bracteole.
- *Nypa fruticans*, `bracteole_width` 200-250 mm from "20-25 cm wide" is probably correct (large palm bracts); no fix
  needed, but it will stay excluded unless `flower_bracteole_width`'s maximum is raised.

## 4. Environmental cue false positives (`*_cues`)

- *Corymbia torelliana* (CPBR_2026), `fruiting_cues = rain`: "rain" matched in "rainforest" ("marginal rainforest
  habitat"). `cue_re` / `cue_dict` need `rain\w*` restricted so that "rainforest" does not match.
- *Eucalyptus distans* (CPBR_2026), `fruiting_cues = floods`: from "prefers seasonally wet sites ... on floodplains", in a
  sentence about another species (*E. microtheca*). The floodplain exclusion in `mtxt` misses the plural "floodplains";
  and comparisons with other species ("E. X has ...") should not be scored for the profile's own taxon.
- *Eucalyptus coolabah* (CPBR_2026), `fruiting_cues` description only: a comparison with *E. acroleuca* and
  *E. largiflorens*, not a cue.
- *Grevillea costata* (ABRS_Proteaceae_2026), `fruiting_cues` description only: "Regenerates from basal shoots and
  possibly lignotuber after floods" is resprouting after floods, not a fruiting cue.

## 5. Draft values and names that differ from traits.yml (optional clean-up)

The parser writes several draft trait names and values that metadata.yml currently renames or substitutes (listed in
`pending_changes.md`, sections 2 and 3). Writing the traits.yml names and levels directly would remove the need for those
mappings and make them safe from metadata regeneration.
