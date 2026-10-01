#!/usr/bin/env python3
"""
Regenerate tools/trait_browser/index.html (and the trait_data.json next to
it) from the current config/traits.yml. Re-run this any time traits.yml
changes -- new traits, edited trait_group values, new keywords/terms, etc.

Usage:
    python3 generate_trait_data.py
    # writes trait_data.json AND splices it into index.html's
    # <script type="application/json" id="trait-data"> block, in one step.

Requires: pyyaml (and a checkout of the APD repo alongside this one, for
APD_trait_hierarchy.csv -- see APD_REPO_PATH below).
"""
import csv
import json
import re
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
TRAITS_YML = REPO_ROOT / 'config' / 'traits.yml'
APD_REPO_PATH = REPO_ROOT.parent / 'APD'
APD_HIERARCHY_CSV = APD_REPO_PATH / 'data' / 'APD_trait_hierarchy.csv'
OUT_PATH = Path(__file__).resolve().parent / 'trait_data.json'
INDEX_HTML = Path(__file__).resolve().parent / 'index.html'
TRAIT_DATA_SCRIPT_RE = re.compile(
    r'(<script type="application/json" id="trait-data">).*?(</script>)', re.DOTALL
)


def split_semi(s):
    if not s:
        return []
    return [x.strip() for x in str(s).split('; ') if x.strip()]


def parse_description(s):
    """traits.yml descriptions are often '<ontology-tagged version>;<plain
    language version>' -- the plain-language part is the last ';'-separated
    segment."""
    if not s:
        return ''
    return str(s).split(';')[-1].strip()


def norm(s):
    return re.sub(r'\s+', ' ', s.strip().lower())


def load_hierarchy_paths():
    """label (normalized) -> authoritative root-to-leaf path, from APD's own
    tier_1..tier_N columns.

    A handful of APD_trait_hierarchy.csv rows have their own deepest tier
    wrong -- it names a different, unrelated category instead of repeating
    the row's own label (seen for "carbohydrate content trait", "lipid
    content trait ", "phenolic compound content trait", "pigment content
    trait" and "root morphology trait"). The ancestor tiers before that are
    still trustworthy, so repair these by substituting the row's own label
    back in as the last tier, rather than discarding the whole path -- the
    alternative leaves these categories with no resolved ancestor chain at
    all, which makes them look like unrelated top-level branches instead of
    nesting under their real parent.
    """
    with open(APD_HIERARCHY_CSV, encoding='latin1') as f:
        rows = list(csv.DictReader(f))
    tier_cols = sorted(c for c in rows[0].keys() if c.startswith('tier_'))
    path_for_label = {}
    for row in rows:
        label = row['label'].strip()
        tiers = [row[c].strip() for c in tier_cols if row.get(c, '').strip()]
        if not tiers:
            continue
        if norm(tiers[-1]) != norm(label):
            tiers = tiers[:-1] + [label]
        path_for_label[norm(label)] = tiers
    return path_for_label


def resolve_token(token, path_for_label):
    """Resolve one trait_group token to its own root-to-leaf APD path.

    Returns (path, resolved): `resolved` is False when the token isn't a
    known APD category (or the hierarchy row for it is internally corrupted
    -- see the guard below) -- in that case the token is used as a one-item
    fallback path, and the caller may want to flag it as unvalidated.
    """
    authoritative = path_for_label.get(norm(token))
    # Guard against corrupted APD hierarchy rows where a label's own deepest
    # tier doesn't match itself (seen, e.g., for "phenolic compound content
    # trait", whose tier_3 wrongly reads a different category's name) --
    # treat those as unreliable rather than silently mis-filing traits.
    if authoritative and norm(authoritative[-1]) != norm(token):
        authoritative = None
    if authoritative:
        return authoritative, True
    return [token], False


def build_trait_groups(raw_str, path_for_label):
    """Parse a traits.yml trait_group string.

    Each semicolon-separated token names a category this trait belongs to in
    its own right -- NOT a single chain to be read top-to-bottom as parent->
    child. A token that is itself an ancestor of another listed token (e.g.
    "life history trait" alongside "regeneration life history trait") is
    just naming that shared ancestor for clarity/legacy reasons and carries
    no information once its descendant is already listed, so it's dropped as
    redundant. Whatever distinct root-to-leaf paths remain after that are
    equally-ranked end groups -- a trait with more than one is genuinely
    filed under more than one category, not "corrected" from one to another.

    Returns a list of {"path": [...], "resolved": bool} dicts, one per
    distinct surviving end group (resolved=False means that category isn't
    in the APD hierarchy yet, so the path is just the raw token on its own).
    """
    L = split_semi(raw_str)
    if not L:
        return []

    resolved_list = []  # [(path, resolved)], de-duplicated by path
    seen_keys = set()
    for tok in L:
        path, resolved = resolve_token(tok, path_for_label)
        key = tuple(norm(x) for x in path)
        if key in seen_keys:
            continue
        seen_keys.add(key)
        resolved_list.append((key, path, resolved))

    # Drop any path that is a strict prefix (i.e. an ancestor) of another
    # path in the set -- it's a redundant mention of a shared ancestor.
    keys = [k for k, _, _ in resolved_list]
    out = []
    for i, (key, path, resolved) in enumerate(resolved_list):
        is_ancestor_of_another = any(
            i != j and len(other_key) > len(key) and other_key[:len(key)] == key
            for j, other_key in enumerate(keys)
        )
        if not is_ancestor_of_another:
            out.append({'path': path, 'resolved': resolved})
    return out


def is_in_apd(uri):
    if not uri or uri in ('XXX', 'YYY'):
        return False
    return uri.startswith('http')


def main():
    with open(TRAITS_YML) as f:
        ty = yaml.safe_load(f)
    elements = ty['traits']['elements']
    path_for_label = load_hierarchy_paths()

    out = []
    for tid, el in elements.items():
        structure_measured = split_semi(el.get('structure_measured'))
        keywords = split_semi(el.get('keywords'))
        trait_groups = build_trait_groups(el.get('trait_group'), path_for_label)

        seen = set()
        terms_core = []
        for v in structure_measured + keywords:
            if v not in seen:
                seen.add(v)
                terms_core.append(v)

        out.append({
            'id': tid,
            'label': el.get('label', ''),
            'description': parse_description(el.get('description')),
            'type': el.get('type', ''),
            'units': el.get('units') or '',
            'structure_measured': structure_measured,
            'characteristic_measured': split_semi(el.get('characteristic_measured')),
            'trait_groups': trait_groups,
            'in_apd': is_in_apd(el.get('entity_URI')),
            'keywords': keywords,
            'terms_core': terms_core,
        })

    out.sort(key=lambda r: r['id'])
    new_json = json.dumps(out, separators=(',', ':'), ensure_ascii=False)

    with open(OUT_PATH, 'w') as f:
        f.write(new_json)
    print(f'wrote {len(out)} traits to {OUT_PATH}')

    html = INDEX_HTML.read_text(encoding='utf-8')
    html2, n = TRAIT_DATA_SCRIPT_RE.subn(
        lambda m: m.group(1) + new_json + m.group(2), html, count=1
    )
    if n != 1:
        raise SystemExit(
            f'expected exactly one <script id="trait-data"> block in {INDEX_HTML}, found {n} -- '
            'not touching the file.'
        )
    INDEX_HTML.write_text(html2, encoding='utf-8')
    print(f'updated {INDEX_HTML}')

    n_multi = sum(1 for r in out if len(r['trait_groups']) > 1)
    n_unresolved = sum(1 for r in out for g in r['trait_groups'] if not g['resolved'])
    max_depth = max((len(g['path']) for r in out for g in r['trait_groups']), default=0)
    print(f'{n_multi} traits belong to more than one end group')
    print(f'{n_unresolved} end-group placements use a category not found in the APD hierarchy')
    print(f'deepest branch: {max_depth} levels')


if __name__ == '__main__':
    main()
