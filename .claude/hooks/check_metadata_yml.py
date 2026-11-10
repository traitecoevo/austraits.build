#!/usr/bin/env python3
"""
PostToolUse hook: catches a handful of metadata.yml gotchas that are
mechanical (no judgement call) but produce silent, build-error-free wrong
data if missed -- see .claude/skills/fill-metadata-yml/SKILL.md for the
full explanation of each one. Fires after Edit/Write/MultiEdit on any
data/<id>/metadata.yml; silent (no output) if nothing is found.

Checks:
  A. A value parsed as a YAML boolean instead of a string -- almost always
     the bare y/n/yes/no-becomes-boolean gotcha.
  B. `custom_R_code:` not immediately followed by `collection_date:` --
     read_metadata() extracts custom_R_code by scanning raw lines up to
     the next `  collection_date:` line, not through YAML parsing.
  C. (soft reminder only) every trait in the file is entity_type: species
     while locations: carries real site data -- location_id will be NA
     for all of them; sometimes intentional, so phrased as a reminder,
     not a reported bug.
"""
import json
import os
import re
import sys

BOOL_WHITELIST = {"dataset.data_is_long_format"}


def find_booleans(node, path, out):
    if isinstance(node, dict):
        for k, v in node.items():
            find_booleans(v, path + [str(k)], out)
    elif isinstance(node, list):
        for i, v in enumerate(node):
            find_booleans(v, path + [str(i)], out)
    elif isinstance(node, bool):
        p = ".".join(path)
        if p not in BOOL_WHITELIST:
            out.append((p, node))


def check_custom_r_code_adjacency(lines):
    notes = []
    for i, line in enumerate(lines):
        if re.match(r"^  custom_R_code:", line):
            for j in range(i + 1, len(lines)):
                m = re.match(r"^  ([A-Za-z0-9_]+):", lines[j])
                if m:
                    if m.group(1) != "collection_date":
                        notes.append(
                            f"`custom_R_code:` (line {i + 1}) is followed by "
                            f"`{m.group(1)}:` (line {j + 1}) instead of "
                            f"`collection_date:`. traits.build's read_metadata() "
                            f"extracts custom_R_code by scanning raw lines up to the "
                            f"next `  collection_date:` line, not through normal YAML "
                            f"parsing -- anything else sitting between them gets "
                            f"silently swallowed into the R code string and fails to "
                            f"parse() with a confusing, unrelated error. Move "
                            f"`collection_date:` to immediately follow `custom_R_code:`."
                        )
                    break
            break  # only one dataset-level custom_R_code field expected
    return notes


def main():
    try:
        payload = json.load(sys.stdin)
    except Exception:
        return

    file_path = (payload.get("tool_input") or {}).get("file_path") or ""
    norm = file_path.replace("\\", "/")
    if not re.search(r"/data/[^/]+/metadata\.yml$", norm):
        return
    if not os.path.isfile(file_path):
        return

    try:
        text = open(file_path, encoding="utf-8").read()
    except Exception:
        return
    lines = text.splitlines()

    notes = []
    reminders = []

    try:
        import yaml
        data = yaml.safe_load(text)
    except Exception as e:
        notes.append(
            f"File does not parse as plain YAML ({e}). If this involves "
            f"custom_R_code, check for the no-'#'-comments, doubled-apostrophe, "
            f"or missing-';'-between-statements gotchas (folded to one line at "
            f"build time)."
        )
        data = None

    if data is not None:
        bool_hits = []
        find_booleans(data, [], bool_hits)
        for p, v in bool_hits:
            notes.append(
                f"`{p}` parsed as the YAML boolean `{v}`, not a string. This is "
                f"almost always the bare y/n/yes/no-becomes-boolean gotcha (R's "
                f"yaml package parses these, any case, as booleans). If this was "
                f"meant to be the literal text, quote it (`'n'`) or rename the "
                f"source column away from a bare y/n/yes/no."
            )

    notes.extend(check_custom_r_code_adjacency(lines))

    if data is not None:
        locs = data.get("locations")
        traits = data.get("traits")
        if isinstance(locs, dict) and len(locs) > 0 and isinstance(traits, list) and traits:
            entity_types = {t.get("entity_type") for t in traits if isinstance(t, dict)}
            if entity_types == {"species"}:
                reminders.append(
                    "Every trait in this file is `entity_type: species` while "
                    "`locations:` carries real site data. traits.build's "
                    "dataset_process() sets location_id to NA for every "
                    "entity_type:species row, with no build error/warning, so this "
                    "location data may end up unlinked from every trait value. If "
                    "that's intentional (locations kept for provenance only), no "
                    "action needed; otherwise double-check whether any of these "
                    "traits are really population/individual-level, and confirm "
                    "with `unique(result$traits$location_id)` after a build."
                )

    if not notes and not reminders:
        return

    parts = []
    if notes:
        parts.append(
            f"metadata.yml check -- likely issue(s) in {file_path}:\n"
            + "\n".join(f"- {n}" for n in notes)
        )
    if reminders:
        parts.append(
            f"metadata.yml check -- for your awareness, in {file_path}:\n"
            + "\n".join(f"- {n}" for n in reminders)
        )

    print(json.dumps({
        "hookSpecificOutput": {
            "hookEventName": "PostToolUse",
            "additionalContext": "\n\n".join(parts),
        }
    }))


if __name__ == "__main__":
    main()
