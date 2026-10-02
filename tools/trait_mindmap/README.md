# AusTraits trait mind map

A single-file, static HTML page (`index.html`) that visualises every trait in
`config/traits.yml` as a collapsible tree, grouped by `trait_group`. No
build step, no server — open it directly in a browser, or host it anywhere
that serves static files.

## What it does

- Renders the full `trait_group` hierarchy as a zoomable/pannable tree.
- Each semicolon-separated entry in a trait's `trait_group` field names a
  category it belongs to in its own right (not a chain to read top-to-
  bottom); a trait belonging to more than one end group appears as its own
  leaf under *each* one, rather than being forced into one "primary" home.
  A category not found in the upstream APD ontology (typo, or a genuine
  gap) is shown with a dashed ring.
- Browse by facet (sidebar): Terms, Structure measured, Characteristic
  measured, or Trait group. Click a pill to select a value.
- Two ways to use an active facet/search selection:
  - **Highlight** (default) — dims everything else, keeps the full tree.
  - **Filter** — redraws the tree showing only the matching branches.
- Search box for traits by name, id, or term.
- Click any leaf for its full definition, type/units, and every facet value
  it shares with other traits.

## Regenerating the data

Re-run this one command any time `config/traits.yml` changes (new traits,
edited `trait_group`, new keywords/terms, …). It writes `trait_data.json`
*and* splices it straight into `index.html` — nothing else to do by hand:

```bash
cd tools/trait_mindmap
python3 generate_trait_data.py
```

It prints a short summary each run (trait count, how many belong to more
than one category, how many end-group placements don't resolve against the
APD hierarchy yet — worth a glance, since that last number flags both real
ontology gaps and typos in `traits.yml`'s `trait_group` field).

`generate_trait_data.py` requires a sibling checkout of the
[APD repo](https://github.com/traitecoevo/APD) (for
`data/APD_trait_hierarchy.csv`, used to validate each trait's `trait_group`
categories against the upstream ontology) and `pyyaml`.

After regenerating, open `index.html` locally to sanity-check it (or re-run
the Playwright smoke test from this tool's development session) before
publishing.

## History

This is a simplified rebuild of an earlier three-mode version (Trait group /
Concepts / Similarity). The Concepts and Similarity modes were dropped since
they didn't meaningfully improve searchability; Structure measured was added
as its own facet, and the Highlight/Filter toggle and multi-branch leaves
were added in their place. The `trait_group` parsing itself was also
corrected: earlier versions mis-read a trait's multiple end-group
categories as one broken "chain" needing a single auto-corrected fix; it's
now parsed as what it actually is — each entry is an independent category,
and a trait can plainly belong to more than one.
