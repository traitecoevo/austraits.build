# austraits.build — agent & contributor guide

`austraits.build` is the **data compendium that compiles the AusTraits database** — an open-source
compilation of traits for Australian plant species (Falster et al. 2021,
doi:10.1038/s41597-021-01006-6). It holds the raw source datasets and the configuration that the
`traits.build` engine harmonises into the released AusTraits resource. Type is `Compendium`, not a
package; it `Depends` on `traits.build (>= 2.1.0)`.

## Repo-local guidance

- **Source data:** `data/` — one subdirectory per contributed source (300+), each typically a
  `data.csv` + `metadata.yml` pair. This is the bulk of the repo and the thing most PRs touch.
- **Config (build inputs):** `config/` —
  - `traits.yml` — trait definitions. **Generated** from the AusTraits Plant Dictionary (APD); see
    below — don't hand-edit it as the source of truth.
  - `metadata.yml` — resource-level metadata, `unit_conversions.csv`, `taxon_list.csv`.
- **Build script:** `build.R` — the top-level pipeline. It loads the schema/config, then for each
  source runs `dataset_configure()` → `dataset_process()` → `dataset_update_taxonomy()`, and
  assembles the combined `austraits` object. **`build.R` is auto-generated** (see header) from
  `remake.yml.whisker` — regenerate it with
  `traits.build::build_setup_pipeline(method = "furrr", database_name = "austraits", workers = 1)`
  rather than editing by hand.
- **Custom build helpers:** `R/` — `build_align_taxon_names.R`, `build_update_taxon_list.R` (taxonomy
  alignment via **APCalign**), and `custom_R_code.R`.
- **Maintenance scripts:** `scripts/` — notably `build_traits_yml_from_APD.R` (regenerates
  `config/traits.yml` from the APD repo), plus `release.Rmd`, `news.Rmd`, `dictionary.Rmd`,
  reporting scripts.
- **Output:** `export/` — the compiled `austraits` object is written under `export/data` after a
  build.

**Build/run:** the README's recipe is install `traits.build`, clone this repo, then `source("build.R")`
(it can use multiple CPUs — raise `workers`). After running you get an `austraits` object in the
workspace and a saved copy in `export/data`. Tests live in `tests/` (testthat). Default branch is
`develop`.

> Gotcha 1: `config/traits.yml` is **generated from APD** via `scripts/build_traits_yml_from_APD.R`
> (which pulls from `traitecoevo/APD`). To change trait definitions, fix APD upstream and regenerate,
> don't patch the YAML in place.
>
> Gotcha 2: `build.R` is machine-generated — edit `remake.yml.whisker` / rerun `build_setup_pipeline()`
> instead of editing `build.R` directly, or your changes get clobbered on the next regeneration.

---

## AusTraits family — cross-package context

`austraits.build` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the actual AusTraits dataset
compilation: it wires together APD (vocabulary) + APCalign (taxonomy) + the traits.build engine over
raw source datasets, and produces the released `austraits-X.Y.Z.rds`. Family-wide concerns are
documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
