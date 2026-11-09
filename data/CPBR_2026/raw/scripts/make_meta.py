# SUPERSEDED 2026-10-30 by ignore/jobs/cp1/meta_update.py (add-only): regenerating drops hand-made mappings. Do not run on an existing metadata.yml.
# CPBR_2026/metadata.yml (long format; EUCLID 2.0 eucalypt profiles read by the ABRS parser in EUCLID mode): source / contributors /
# dataset text for EUCLID, the contexts present in data.csv, and one traits block per traits.yml trait present in data.csv, reused
# from ABRS_Myrtaceae_2026 (else the other ABRS_*_2026 datasets). Keeps the existing file's dataset fields and questions.
# Rerun after every CPBR_2026 rebuild.
import csv, os, yaml

def find(d):
    if isinstance(d, dict):
        if "post_fire_recruitment" in d: return d
        for v in d.values():
            r = find(v)
            if r: return r
defined = find(yaml.safe_load(open("config/traits.yml")))
csv.field_size_limit(10**9)
rows = list(csv.DictReader(open("data/CPBR_2026/data.csv")))
cols = list(rows[0].keys())
present = sorted({r["trait_name"] for r in rows} & set(defined))
blocks = {}
for d in ["Myrtaceae", "NtoZ", "Fabaceae", "Proteaceae", "Asteraceae", "AtoM"]:
    for b in yaml.safe_load(open(f"data/ABRS_{d}_2026/metadata.yml"))["traits"]:
        t = b["trait_name"]
        if t in blocks: continue
        b = {k: v for k, v in b.items() if not k.startswith("question")}
        b["var_in"] = t; b["value_type"] = "value_type"
        blocks[t] = b
present = [t for t in present if t in blocks]

src = open("data/ABRS_NtoZ_2026/metadata.yml").read()
ctx_all = yaml.safe_load(src)["contexts"]
ctx = [c for c in ctx_all if c["var_in"] in cols]

head = """source:
  primary:
    key: CPBR_2026
    bibtype: Online
    author: '{Centre for Plant Biodiversity Research}'
    year: '2026'
    title: 'EUCLID 2.0: Eucalypts of Australia'
    url: https://apps.lucidcentral.org/euclid/text/intro/index.html
    note: accessed August 2026
contributors:
  data_collectors:
  - last_name: Wenk
    given_name: Elizabeth
    ORCID: 0000-0001-5640-5910
    affiliation: Evolution & Ecology Research Centre, School of Biological, Earth,
      and Environmental Sciences, UNSW Sydney, Australia
    notes: Extracted the trait data from the EUCLID profiles
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes
"""
cust = "  custom_R_code: '\n    data |>\n      dplyr::filter(\n        trait_name %in% c(\n" + \
       ",\n".join("          " + ", ".join(f'"{t}"' for t in present[i:i + 4]) for i in range(0, len(present), 4)) + \
       "\n        ),\n        is.na(measurement_range) | measurement_range != \"extreme\"\n      )\n    '\n"
mid = """  collection_date: '2026'
  taxon_name: taxon_name
  trait_name: trait_name
  value: value
  description: To compile plant trait values from the taxon descriptions in EUCLID 2.0 (Eucalypts of Australia), the eucalypt
    identification system of the Centre for Plant Biodiversity Research; Eucalyptus, Corymbia and Angophora.
  basis_of_record: preserved_specimen
  life_stage: adult
  sampling_strategy: 'The 935 species, subspecies and variety profiles of EUCLID 2.0 (apps.lucidcentral.org/euclid) were
    downloaded. Trait values were extracted from the description, flowering time and notes text with the scripted parser
    written for the Flora of Australia online (ABRS_AtoM_2026/raw/build_data_csv.R, EUCLID mode): each description is split
    into sentences, clauses and comma-separated segments, each segment is assigned to the organ it describes (habit, bark,
    branchlets, juvenile growth, adult leaves, inflorescence, flowers, fruit, seed, ...), and categorical terms and
    measurements are read from the segments of the relevant organ. The "Cultivated seedlings" text closing each description
    is read as seedling text, and the lignotuber sentence as part of the habit. Categorical terms are mapped to AusTraits
    trait values, with the source terms kept verbatim in value_description; several values are listed in the order they
    appear in the text, and alternatives qualified by a commonness word (usually, rarely, sometimes ...) or a region are
    recorded on separate rows. A measured range is recorded as minimum and maximum values, a single measurement as a mean, a
    single count as a mode, "to X" as a maximum and "X or more" as a minimum; parenthetical extremes are kept on separate rows
    (measurement_range = extreme). Hedged statements are scored with the hedge word as scoring_certainty. EUCLID has no genus
    descriptions, so nothing is copied down from genera. Subspecies and varieties whose own description is silent on a trait
    take the values from the description of their species, where EUCLID describes the species. Values that descriptions state
    only when atypical (terrestrial substrate, leaf as photosynthetic organ, not parasitic, broadleaf) are recorded on
    trait_scoring_method = inferred rows when nothing in the profile suggests otherwise; growth in swamps (terrestrial
    semiaquatic) was vetted by reading each taxon''s own habitat sentence, because the notes also name swamps in place names
    and comparisons. The values were compared taxon by taxon with ABRS_Myrtaceae_2026, which was extracted from the
    near-identical Flora of Australia eucalypt text, and every systematic difference was checked against the source.'
  original_file: euclid_species_profiles.csv (EUCLID 2.0 profiles, downloaded August 2026)
"""
notes = """  notes: 'EUCLID and the Flora of Australia eucalypt treatment share their descriptions (895 of the 935 EUCLID taxa have a
    near-identical Flora of Australia profile, extracted in ABRS_Myrtaceae_2026), so most values here repeat those of
    ABRS_Myrtaceae_2026; EUCLID adds about 40 taxa (phrase-name taxa, subspecies and hybrids) and gives flowering months as
    lists. Earlier EUCLID extracts are CPBR_2002 and Stephens_2024. Candidate traits not yet in traits.yml are listed with
    their coverage in raw/candidate_traits.csv; custom_R_code keeps only the traits mapped below. Rows whose trait_name is a
    text field (habit_description, leaf_description, notes, ...) carry the verbatim organ-level text and are unmapped.
    Parenthetical extremes (measurement_range = extreme) are in data.csv but not mapped.'
"""
path = "data/CPBR_2026/metadata.yml"
cur_q = None
if os.path.exists(path):
    cur = open(path).read()
    if "\n  collection_date:" in cur: mid = cur[cur.index("\n  collection_date:") + 1:cur.index("\n  notes:") + 1]
    cur_q = yaml.safe_load(cur).get("questions")
out = head + cust + mid + notes + "identifiers: .na\nlocations: .na\n" + yaml.safe_dump({"contexts": ctx}, sort_keys=False, width=120) + "traits:\n"
for t in present:
    out += yaml.safe_dump([blocks[t]], sort_keys=False, allow_unicode=True, width=120)
# EUCLID names in the spelling of config/taxon_list.csv (hybrids with "x", phrase names with their voucher; checked 2026-10-30);
# E. paludicola and E. sp. Dorsiventralis have no taxon_list entry and are left as they are
tu_reason = "EUCLID name written as in the AusTraits taxon list (hybrid sign, phrase-name voucher or spelling; 2026-10-30)"
tu = [(f, r, "species") for f, r in [
    ("Eucalyptus brachyphylla", "Eucalyptus x brachyphylla"), ("Eucalyptus calyerup", "Eucalyptus x calyerup"),
    ("Eucalyptus carnabyi", "Eucalyptus x carnabyi"), ("Eucalyptus lateritica", "Eucalyptus x lateritica"),
    ("Eucalyptus comitaevallis", "Eucalyptus cometae-vallis"),
    ("Eucalyptus sp. Cockburn", "Eucalyptus sp. Cockburn (M.I.H.Broooker 12741 & A.V.Slee)"),
    ("Eucalyptus sp. Kalbarri", "Eucalyptus sp. Kalbarri (M.I.H.Brooker 7937)"),
    ("Eucalyptus sp. Southern Wheatbelt", "Eucalyptus sp. Southern wheatbelt (D.Nicolle & M.French DN 5507)"),
    ("Eucalyptus sp. Undulans", "Eucalyptus sp. Undulans (J.R.Connors 1004 & D.Nicolle)")]] + [
    ("Eucalyptus eudesmioides subsp. Pallida", "Eucalyptus eudesmioides subsp. Pallida (D.F.Blaxell 1981 et al.)", "subspecies"),
    ("Eucalyptus eudesmioides subsp. eudesmioides", "Eucalyptus eudesmioides", "species")]
out += "substitutions: .na\n" + yaml.safe_dump({"taxonomic_updates": [
    {"find": f, "replace": r, "reason": tu_reason if f != "Eucalyptus eudesmioides subsp. eudesmioides" else
     "The taxon list has no subspecies eudesmioides (only the species and var. eudesmioides): aligned to the species (2026-10-30)",
     "taxonomic_resolution": res} for f, r, res in tu]}, sort_keys=False, allow_unicode=True, width=120) + "exclude_observations: .na\n"
default_q = {
    "question1": "Terms with the prefix sub- (sub-spherical, sub-basifixed, sub-opposite; FoA also writes subspherical, subdorsifixed) are mapped to the base term only when written with a hyphen, as the parser matches the word after it. Should sub-X and subX both map to X (as subcordate -> cordate), or neither?",
    "question2": "Eucalyptus rudis \"may be anticipated along streams and in swampy areas\" is not scored terrestrial semiaquatic (hedged). Score it with scoring_certainty?",
    "question3": "Corymbia maculata: flowering recorded March-July and September, but the notes add \"As it flowers in winter ...\", which adds August. Keep the winter statement?",
    "question4": "Eucalyptus largiflorens \"Seedling regeneration of both E. largiflorens and E. camaldulensis is common after flooding\" is scored for E. largiflorens only (germination_cues floods); EUCLID describes E. camaldulensis only as subspecies. Score the subspecies too?",
    "question5": "Parenthetical extremes (measurement_range = extreme) are filtered out in custom_R_code. Should they be mapped, e.g. as separate minimum / maximum records with a context?",
}
out += yaml.safe_dump({"questions": cur_q or default_q}, sort_keys=False, allow_unicode=True, width=120)
yaml.safe_load(out)
open(path, "w").write(out)
print(len(present), "traits mapped;", len(ctx), "contexts:", [c["var_in"] for c in ctx])
