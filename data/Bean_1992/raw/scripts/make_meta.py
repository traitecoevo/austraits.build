#!/usr/bin/env python3
"""Bean_1992: writes metadata.yml from data.csv and the term map in make_trait_data.py. Written as text, not through a YAML dumper,
so custom_R_code stays laid out. Run after make_trait_data.py, from the repository root; the build never runs this script."""
import csv
import re
import textwrap
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[4]
DS = ROOT / "data" / "Bean_1992"
TRAITS = yaml.safe_load((ROOT / "config" / "traits.yml").read_text())["traits"]["elements"]
src = (Path(__file__).parent / "make_trait_data.py").read_text()
SUBS = eval(re.search(r"^SUBS = (\{.*?^\})", src, re.S | re.M).group(1))

rows = list(csv.DictReader(open(DS / "data.csv")))
mapped = sorted({r["trait"] for r in rows if r["in_traits.yml"] == "x"})
cands = sorted({r["trait"] for r in rows if r["in_traits.yml"] != "x"})


def rank(r):
    s = r["source_section"]
    return 3 if s.startswith("Dichotomous") else 2 if s.startswith("Multi-access") else 1


# what custom_R_code keeps: mapped traits, no extremes, the best source per taxon x trait x region x part
groups = {}
for r in rows:
    if r["in_traits.yml"] == "x" and r["commonness_qualifier"] != "extreme_range":
        groups.setdefault((r["taxon_name"], r["trait"], r["population_region"], r["entity_measured"]), []).append(r)
kept = [r for g in groups.values() for r in g if rank(r) == min(rank(x) for x in g)]

UNITS = {"plant_height": "m", "flowers_per_inflorescence": "{count}/{count}", "flower_fertile_stamens_count": "{count}",
         "leaf_main_vein_count": "{count}", "leaf_length_to_width_ratio": "{dimensionless}"}
EXTRA = {
    "bark_texture": " Multi-access key states naming several textures (papery/fibrous, fibrous/stringy) are recorded as all of them.",
    "fruit_length": " The first dimension of the fruit measurements, or dehisced fruit length from the dichotomous key (entity measured).",
    "fruit_width": " The second dimension of the fruit measurements, or the fruit diameter in the dichotomous key.",
    "flower_diameter": " The flower diameter.",
    "leaf_length_to_width_ratio": " Stated directly as how many times longer than wide the leaves are.",
    "leaf_hairs_adult_leaves": " Young leaves described as glabrescent are read as adult leaves glabrous.",
    "plant_growth_substrate": " Soil, rock, forest and heath habitats are terrestrial, swamps and swampy heath semiaquatic; where the text names no habitat the typical value is recorded as trait_scoring_method = inferred.",
    "stem_shape": " Angular or triangular twigs, cited in the Stem flanges section from earlier works.",
    "fruit_type": " From the genus statement in the Introduction.",
    "leaf_phyllotaxis": " From each species description, or the genus statement in the Introduction.",
    "flower_anther_attachment": " From each species description, or the genus statement in the Introduction.",
    "plant_photosynthetic_organ": " The typical value, recorded where leaves are described and nothing suggests otherwise (trait_scoring_method = inferred).",
    "parasitic": " The typical value, recorded for a green-leaved plant with no parasitism or host named (trait_scoring_method = inferred).",
    "leaf_type": " The typical value, recorded for a flat-lamina leaf shape or leaves at least 3 mm wide (trait_scoring_method = inferred).",
}
SEC_WORDS = [("Description", "the species descriptions"), ("Distribution", "the Distribution and habitat text"), ("Flowering", "the Flowering period statements"),
             ("Affinities", "the Affinities text"), ("Note", "the Notes"), ("Introduction", "the Introduction"), ("Seed morphology", "the Seed morphology section"),
             ("Stem flanges", "the Stem flanges section"), ("Multi-access", "the multi-access key"), ("Dichotomous", "the dichotomous key")]


def methods(trait):
    secs = {r["source_section"] for r in kept if r["trait"] == trait}
    words = [w for k, w in SEC_WORDS if any(s.startswith(k) for s in secs)]
    if not words:
        return EXTRA.get(trait, "").strip()
    if trait in ("plant_photosynthetic_organ", "parasitic", "leaf_type"):
        return EXTRA[trait].strip()
    lead = "Scored from " + (words[0] if len(words) == 1 else ", ".join(words[:-1]) + " and " + words[-1]) + "."
    return lead + EXTRA.get(trait, "")


def fold(key, text, indent=2, quote=True):
    """A long string as a YAML single-quoted scalar, wrapped."""
    pad = " " * indent
    body = text.replace("'", "''") if quote else text
    lines = textwrap.wrap(f"{key}: '{body}'", width=90, subsequent_indent=pad + "  ", break_long_words=False, break_on_hyphens=False)
    return "\n".join(pad + l if i == 0 else l for i, l in enumerate(lines))


def custom_code():
    lines, cur = [], "          "
    for t in mapped:
        item = f'"{t}", '
        if len(cur) + len(item) > 88:
            lines.append(cur.rstrip())
            cur = "          "
        cur += item
    lines.append(cur.rstrip().rstrip(","))
    vec = "\n".join(lines)
    return f"""  custom_R_code: '
    data |>
      dplyr::filter(
        trait %in% c(
{vec}
        ),
        is.na(commonness_qualifier) | commonness_qualifier != "extreme_range"
      ) |>
      dplyr::mutate(
        source_rank = dplyr::case_when(
          grepl("^Dichotomous", source_section) ~ 3,
          grepl("^Multi-access", source_section) ~ 2,
          TRUE ~ 1
        )
      ) |>
      dplyr::group_by(taxon_name, trait, population_region, entity_measured) |>
      dplyr::filter(source_rank == min(source_rank)) |>
      dplyr::ungroup()
    '"""


out = []
out.append("""source:
  primary:
    key: Bean_1992
    bibtype: Article
    year: 1992
    author: A. R. Bean
    title: The genus Leptospermum Forst. et Forst. f. (Myrtaceae) in northern Australia and Malesia
    journal: Austrobaileya
    volume: 3
    number: 4
    pages: 643--659
    doi: 10.5962/p.366003
contributors:
  data_collectors:
  - last_name: Bean
    given_name: Anthony
    ORCID: 0000-0001-5697-1664
    affiliation: Queensland Herbarium, Australia
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes""")
out.append(custom_code())
out.append("""  collection_date: '1992'
  taxon_name: taxon_name
  trait_name: trait
  value: trait_value""")
out.append(fold("description", "To resolve the taxonomic problems remaining in the northern Australian and Malesian species of Leptospermum, in particular the circumscriptions of L. parviflorum and L. javanicum, and to describe new taxa from Queensland, the Northern Territory and Western Australia."))
out.append("  basis_of_record: preserved_specimen\n  life_stage: adult")
out.append(fold("sampling_strategy", "A revision of the genus Leptospermum was recently published by Joy Thompson (Thompson 1989), which elucidated many matters of concern to taxonomists and provided a basis for further research. Readers are referred to that publication for descriptions and full synonymy of species not treated in this paper. ^p The seeds of most of the northern Leptospermum species have been examined in the present study. In most cases, seeds were collected from plants in their natural habitat, rather than from herbarium specimens. Thus, seed maturity could be properly assessed. ^p I would like to thank the Directors of A, DNA, L, NSW, PERTH and SING for the loan of their herbarium material and types, and the directors of BRI, JCT and QRS for access to their herbaria."))
out.append(fold("original_file", "Myrtaceae Leptospermum partial_revision Bean_1992 Austrobaileya part366003.pdf (BHL scan of Austrobaileya 3(4): 643--659)"))
notes = ("data.csv is raw/trait_data.csv (a hand transcription of every clause of the paper, from the page images) with trait_scoring_method added from rank_scored_from "
         "(inferred_from_genus for the genus statement in the Introduction, inferred_from_species for subspecies silent on a character, inferred_from_species_group for a key lead or group statement "
         "covering several taxa, inferred for typical values stated only when atypical). Type and cited specimens are in raw/Bean_1992_vouchers.csv. "
         "Six species are described in full (L. pallidum, L. madidum and its two subspecies, L. parviflorum, L. venustum, L. javanicum, L. amboinense); the rest are annotated with distribution and habitat "
         "and scored through the two keys. Each value is recorded in the source words (trait_value_raw, the clause; trait_value, the source terms) and mapped with substitutions. "
         "Several values are listed in text order; a commonness word splits a character onto rows (commonness_qualifier), and parenthetical extremes are on extreme_range rows, in data.csv but not mapped. "
         "When a taxon has a character from several places custom_R_code keeps the highest-ranked source for each taxon, trait, region and part measured: the taxon own text, then the multi-access key, then the dichotomous key. "
         "Numbers in a key lead covering several taxa, and bounds such as Largest leaves <9 mm or Pedicels >8 mm, are not values for a taxon; they are kept in the *_class candidates. "
         "L. madidum subsp. sativum growth rate and soils in cultivation (Brock 1988) are candidate rows. "
         "Candidate traits not yet in traits.yml (unmapped, dropped in custom_R_code): " + ", ".join(cands) + ".")
out.append(fold("notes", notes))
out.append("""identifiers: .na
locations: .na
contexts:
- context_property: commonness qualifier
  category: entity_context
  var_in: commonness_qualifier
- context_property: population region
  category: entity_context
  var_in: population_region
- context_property: entity measured
  category: method_context
  var_in: entity_measured
- context_property: trait scoring method
  category: method_context
  var_in: trait_scoring_method
traits:""")
for t in mapped:
    numeric = TRAITS[t]["type"] == "numeric"
    unit = UNITS.get(t, "mm") if numeric else ".na"
    if numeric and "{" in unit:
        unit = f"'{unit}'"
    out.append(f"""- var_in: {t}
  unit_in: {unit}
  trait_name: {t}
  entity_type: species
  value_type: value_type
  basis_of_value: {"measurement" if numeric else "expert_score"}
  replicates: .na""")
    out.append(fold("methods", methods(t)))
out.append("substitutions:")
n_sub = 0
for t in mapped:
    levels = TRAITS[t].get("allowed_values_levels") or {}
    toks = []
    for r in kept:
        if r["trait"] == t:
            for tok in r["trait_value"].split(" "):
                if tok in SUBS.get(t, {}) and tok not in toks:
                    toks.append(tok)
    for tok in sorted(toks):
        q = lambda s: f"'{s}'" if re.search(r"[+/\- ]", s) else s
        out.append(f"- trait_name: {t}\n  find: {q(tok)}\n  replace: {q(SUBS[t][tok])}\n  match: word")
        n_sub += 1
out.append("taxonomic_updates: .na")
out.append("""exclude_observations:
- variable: taxon_name
  find: Leptospermum javanicum, Leptospermum parviflorum, Leptospermum recurvum
  reason: Non-Australian taxa (Malesia, New Guinea, Borneo and Sulawesi)""")
QUESTIONS = [
    "Multi-access key, Leaf apex: the state acute, prickly is printed 1,18,23,25. Read as a misprint for 11 (L. microcarpum): 1 (L. pallidum) is also listed under acute, not prickly and its description has no prickly wording, 11 is under no other leaf apex state, and couplet 16 keys L. microcarpum as prickly. Scored for L. microcarpum, not L. pallidum, with a note on each row. Confirm.",
    "When a taxon has a character from several places only the highest-ranked source is mapped: the taxon own text, then the multi-access key, then the dichotomous key (all stay in data.csv). Confirm the order. The multi-access key ranks above the dichotomous key because it scores each species singly, whereas most dichotomous leads cover a group.",
    "Stem flanges are one of the main diagnostic characters of the genus and are scored for all 31 species (candidate stem_flanges, stem_flange_prominence, stem_flange_extent). Consider a stem_flanges trait; stem_shape winged or angular is not the same character.",
    "Values with no level, mapped to the nearest: bark scaly -> flaky (Bean separates flaky from scaly under L. variabile; Lim_2010 also maps scales to flaky), seed shape linear -> ovoid_elongated (the elongate rule), seed striate -> ribbed and reticulate -> netted (precedent across the repo), anthers opening by narrow slits -> longitudinal_slit, not recurved margins -> flat, recurved margins -> revolute, creamy bark -> white, triangular twigs -> angular. Check these.",
    "L. petersonii leaf apex minutely retuse has no level in leaf_apex_shape and goes to excluded_data as an unsupported value. The multi-access state obtuse or minutely retuse is substituted to obtuse. Consider adding retuse to leaf_apex_shape.",
    "Inequalities in the keys (Largest leaves <9 mm, 9--15 mm, >15 mm; Fruit diameter <5 mm, >5 mm; Pedicels >8 mm; Leaves mostly >4 mm wide) are kept as *_class candidates, not leaf_length, fruit_width or pedicel_length, because a bound is not a measured value. They are the only leaf-length data for many taxa; say if you would rather map them as maximum or minimum values.",
    "Numbers that disagree within the paper, recorded as printed: L. madidum 22--70 mm leaf length and 2.5--5.0 mm fruit width against subsp. sativum 20--45 mm and 2.0--3.5 mm; L. venustum hypanthium 3--4 mm (description) against 3.5--4.0 mm (couplet 32). Categorical disagreements: L. amboinense narrowly elliptical leaves (description) against lanceolate (Introduction), and sepals not persistent (description) against both states (multi-access key); L. variabile bark flaky (Note) against papery or fibrous (key).",
    "Seed morphology group 1 (seeds and chaff identical, linear, striate) is stated for all woody-fruited species. It is applied, as inferred_from_species_group, to L. gregarium, L. wooroonooran, L. oreophilum and L. variabile, which couplet 23 calls woody and which have no seed text of their own. L. recurvum (woody only by implication in the Introduction) and L. petersonii (never called woody) are left without seed values. Confirm.",
    "The paper has no Methods section, so sampling_strategy quotes the Introduction, the Seed morphology section and the Acknowledgements. Bean gave no statement of how characters were scored in the keys.",
    "L. brachyandrum hillside and creek-dwelling plants are alike in being protected from fire, and Bean adds that this is perhaps a determinant of its distribution. This is kept in habitat_description and habitat_terms; no fire response is scored.",
]
out.append("questions:")
for i, q in enumerate(QUESTIONS, 1):
    out.append(fold(f"question{i}", q))
(DS / "metadata.yml").write_text("\n".join(out) + "\n")
print(len(mapped), "traits;", n_sub, "substitutions;", len(kept), "rows kept of", len(rows))
