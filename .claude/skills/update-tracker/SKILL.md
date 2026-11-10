---
name: update-tracker
description: Refresh the AusTraits Dataset Pipeline tracker (https://claude.ai/artifact/VPeKe1UpoxJGZ6PoER3ASS) by scanning every austraits.build branch for in-progress datasets and pushing the results to the page's database. Use when the user types /update-tracker or says "update the tracker" / "update the dataset tracker", and after generating or restructuring a dataset's data.csv or metadata.yml.
---

# Update the dataset pipeline tracker

The tracker page lists every new dataset sitting on a branch of
`/Users/z3524079/GitHub/austraits.build`. For each one it shows which files exist,
how much placeholder metadata is left and the user's three sign-off ticks. The page
can't see git, so this skill does the scan and writes the results to its database.

- Artifact: `https://claude.ai/artifact/VPeKe1UpoxJGZ6PoER3ASS`
- Scanner: `~/.claude/austraits-tracker/scan_datasets.py`
- Page source: `~/.claude/austraits-tracker/dataset-pipeline.html`. Republish it only if the page itself changes.

## Database layout: what you may touch

| Collection | Written by | Contents |
|---|---|---|
| `datasets/<id>` | this skill | one doc per tracked dataset: per-branch file flags, unknown fields, question count, last commit |
| `meta/scan` | this skill | scan time, base ref, per-branch info (PR, pushed/unpushed, current) |
| `stages/<id>` | **the user only** | `checked` / `report_sent` / `ready_to_merge` ticks and `note`. **Never write, update or delete these.** |

## Steps

Load `ArtifactData` with ToolSearch (`select:ArtifactData`) if it isn't loaded yet.
Use the session scratchpad as `$SP`.

1. **Get the current versions.** Every write to an existing doc needs `if_version`.
   Run two `ArtifactData` `list` calls with `out_dir: $SP/dbdump`. The out_dir form prints one
   compact line per doc and keeps doc bodies out of the conversation:
   - `collection: "datasets"`, `query: {"limit": 1000}`
   - `collection: "meta"`

   Write both results to `$SP/listing.txt`. Keep each `N documents from collection "X"` header
   line, and for each doc line keep only `- "<id>"  <n> bytes  version <v>`. Drop the trailing
   file path to keep it short.

2. **Scan.**
   ```bash
   python3 ~/.claude/austraits-tracker/scan_datasets.py --out $SP/scan --versions $SP/listing.txt
   ```
   It fetches origin and compares each branch with `origin/develop`. It writes one JSON per doc
   and `batch_N.json` files of up to 50 writes each, with pins and deletes already filled in. Its
   stderr line reports the dataset count and which datasets will be deleted.

3. **Push.** For each `$SP/scan/batch_N.json`, read it and pass its array unchanged as
   `writes` to one `ArtifactData` `batch` call. Send independent batches in parallel.
   If a batch fails with `version_mismatch`, nothing in it was written: redo steps 1–3.

4. **Report** in two or three lines: datasets and branches tracked, anything newly added or
   deleted, and any dataset whose status changed in a way the user would care about (e.g.
   metadata.yml appeared, or the placeholder count dropped to 0). No need to restate the page.

## Rules built into the scanner (don't undo them)

- **Datasets already on `origin/develop` are not tracked.** Branch edits to them are one-off minor
  edits. Only new datasets count. Datasets that drop off every branch, usually because they were
  merged, are deleted from `datasets/`. Their `stages/` doc stays.
- **Only placeholder `unknown`s count as "metadata incomplete".** `replicates`, `collection_date`
  (`unknown/YYYY`), `ORCID`, `assistants` and substitution `find`/`replace` are valid as `unknown`.
  This list is `VALID_UNKNOWN` in the scanner. Add to it only when the user says a field is
  legitimately unknown.
- Reads git objects only: never check out branches or touch the working tree. The checked-out
  branch's uncommitted changes are read from disk and flagged "uncommitted".
