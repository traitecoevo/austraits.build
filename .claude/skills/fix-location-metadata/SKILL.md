---
name: fix-location-metadata
description: Normalise a researcher-supplied `locations:` block in an existing dataset's `data/<id>/metadata.yml` -- rename property keys to match the vocabulary already used across this repo's other datasets, add a short `description` as the third field, and convert any projected coordinates (Easting/Northing + datum/zone, or other UTM-style pairs) to decimal-degree `latitude (deg)`/`longitude (deg)`. Trigger phrase: the user typing "fix location metadata" (with or without naming a dataset -- ask which one if it's not obvious from context). Also use it whenever a curator hands over a `metadata.yml` whose `locations:` block still has the contributor's own column names (Title_Case, underscores, unit-less numbers) and wants it brought in line with the rest of the database.
---

# Fix location metadata

Takes a `locations:` block that still looks like whatever the data
contributor called their columns (`Elevation_m`, `Veg_formation`,
`Easting`/`Northing`) and rewrites it to match the property names already
established across `data/*/metadata.yml` in this repo -- the same
"reuse an existing name, flag a genuinely new one" principle
`paper-trait-location-extraction`'s Step 2 uses when building a location
table from scratch, applied here to a block that already exists and just
needs tidying. Read that skill's Step 2 first if you haven't -- the
`description`-vs-`locality` split, the `geology (stratigraphic map unit)`
convention, and the "numeric property is a bare number, nothing else" rule
all apply unchanged here and aren't re-explained below.

## Step 1 -- build the vocabulary index, don't guess from memory

Before renaming anything, pull the actual property names in use across the
repo (there are 100+, and eyeballing a handful of files will miss the exact
spelling that already exists):

```python
import yaml, glob, collections
counter = collections.Counter()
for fn in glob.glob("data/*/metadata.yml"):
    d = yaml.safe_load(open(fn))
    for props in (d.get("locations") or {}).values():
        if isinstance(props, dict):
            counter.update(props.keys())
```

Search this index for the target dataset's own column names by *meaning*,
not string similarity -- `Elevation_m` won't fuzzy-match `elevation (m)` by
edit distance, but a human reads them as the same concept instantly.
Common, high-confidence renames (case/underscore/unit-placement differences
only) you'll hit repeatedly:

| Contributor's column | This repo's property |
|---|---|
| `Elevation_m`, `Elev_m`, `Altitude_m` | `elevation (m)` |
| `Soil`, `Soil_type` | `soil type` |
| `Landform` | `landform` |
| `Slope` (a number of degrees) | `slope angle (degrees)` |
| `Location`, `Site_description` (a full positional description) | `locality` |
| `Rainfall_mm`, `MAP` | `precipitation, MAP (mm)` |
| `Temp_C`, `MAT` | `temperature, MAT (C)` |

## Step 2 -- when the source splits or conflates something the repo's vocabulary handles differently

Two shapes worth checking for on every dataset, not just spelling:

- **A single contributor column mixing two kinds of value** -- e.g. an
  `Aspect` column holding both compass directions (`NNW`, `NW`, `N`) and a
  bare `0` for flat ground. This repo already splits aspect into `aspect`
  (numeric degrees) and `aspect.cardinal` (compass string) -- see
  `data/Bryant_2024/metadata.yml` for the worked pattern. Route each value
  to the column it actually is; don't force a compass string into a numeric
  field or vice versa. A `0` that coincides with `0` slope almost always
  means "no discernible aspect" (flat), not "north" -- say so explicitly in
  that row's `notes` rather than letting `aspect.cardinal` silently read as
  north.
- **No existing property fits at all** -- rename anyway, using the repo's
  house style (lowercase, natural spacing, a parenthetical unit for numeric
  fields) rather than leaving the contributor's `Title_Case`/`snake_case`.
  Flag it explicitly in the chat report as new vocabulary so the curator
  can decide whether to keep it, fold it into an existing property, or
  rename it differently -- same flag `paper-trait-location-extraction`
  raises for a genuinely new property.

## Step 3 -- add `description` as the third field

Right after `latitude (deg)`/`longitude (deg)`, matching the order
essentially every other `metadata.yml` in this repo already uses. Fill it
with a **short phrase** describing the vegetation community or the site's
single defining characteristic (`paper-trait-location-extraction`'s
`description` rule, same register: `"Sydney Coastal Dry Sclerophyll
Forests"`, not a sentence).

**Reuse, don't paraphrase.** If the source already has a column whose values
are exactly this -- a vegetation-class/community name, most often -- copy
those values into `description` verbatim, on top of keeping that column
under its own (possibly renamed) property too. The same string legitimately
appears twice: once as the quick-scan `description`, once under its own
named property (`vegetation class`, or whatever it was renamed to). This
isn't duplication to clean up -- `description` and the source property serve
different readers (a quick compare-across-sites glance vs. the full
classification detail).

**Always include a `state` field**, the Australian state/territory
abbreviation (`WA`, `NSW`, `QLD`, `VIC`, `SA`, `TAS`, `NT`, `ACT`) the
location falls in -- every location gets one, even when the source data
didn't supply it explicitly (infer it from the coordinates or place name).
Pairs with `locality` (a place name, e.g. `near Denmark, WA` or `Corrigin
Nature Reserve`) rather than replacing it -- both fields are populated
alongside `description`, not instead of it.

## Step 4 -- convert projected coordinates to decimal degrees

A contributor's raw GPS log is frequently in a projected system (Easting/
Northing plus a stated datum and zone -- MGA/UTM is the near-universal case
for Australian field data) rather than lat/long. Convert properly, don't
approximate:

1. **Identify the EPSG code from the stated datum + zone.** For Australian
   MGA (Map Grid of Australia), the projected EPSG code follows the datum's
   own numbering: GDA2020 MGA zone `N` is `EPSG:78`+`N` (zone 56 ->
   `EPSG:7856`); GDA94 MGA zone `N` is `EPSG:283`+`N` (zone 56 ->
   `EPSG:28356`). Non-Australian or WGS84 UTM data follows `EPSG:326`+`N`
   (northern hemisphere) / `EPSG:327`+`N` (southern hemisphere). If the
   source states a datum/zone this pattern doesn't cover, look up the
   correct EPSG code rather than guessing.
2. **Convert with `pyproj`, never by hand.** A manual UTM formula is exactly
   the kind of arithmetic that's easy to get subtly wrong (hemisphere sign,
   false easting/northing, ellipsoid) and hard to notice is wrong -- use the
   real projection library:

   ```python
   from pyproj import Transformer
   # projected EPSG -> the datum's own geographic EPSG (GDA2020: 7844; GDA94: 4283)
   transformer = Transformer.from_crs("EPSG:7856", "EPSG:7844", always_xy=True)
   lon, lat = transformer.transform(easting, northing)  # note: always_xy=True -> (x, y) = (easting, northing) in, (lon, lat) out
   ```
   Round to 6 decimal places (~0.1 m) to match this repo's usual precision.
3. **Sanity-check the result** against any place name/locality text the
   source gives -- converted coordinates for a "Ku-ring-gai Chase National
   Park" site should land at roughly -33.6, 151.2 (Sydney), not somewhere
   else entirely. A gross mismatch means the wrong EPSG/zone was used.
4. **Drop the original Easting/Northing/Datum/Zone fields once converted**
   -- they're inputs to the conversion, not properties this repo's other
   datasets retain (checked: no dataset keeps a `datum` or UTM-style `zone`
   location property). Record what was used for the conversion in that
   location's `notes` (`"Converted from Easting/Northing (GDA2020, MGA Zone
   56, EPSG:7856) to decimal degrees (EPSG:7844)."`) so it's checkable
   without needing the original columns to survive.

## Step 5 -- edit the file, minimal diff

Splice in the rewritten `locations:` block in place -- don't round-trip the
whole `metadata.yml` through `read_metadata`/`write_metadata` for this (that
reflow is for a *new* dataset's Phase 5/8, per `traits-build-add-dataset`'s
non-negotiables; here it would bury a location-only fix under an
unreviewable whole-file reformat). After editing, reload the file with
`yaml.safe_load` to confirm it still parses and every location has the same
key set.

## Step 6 -- report back

Old name -> new name for every renamed property; any property that's
genuinely new vocabulary (Step 2); the EPSG codes used for the coordinate
conversion and one worked example so the curator can spot-check it; any
column dropped (Easting/Northing/Datum/Zone) and where its information
ended up (`notes`).
