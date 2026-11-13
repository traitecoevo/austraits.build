#!/usr/bin/env python3
"""Bean_2018: writes metadata.yml from data.csv and the term map in make_trait_data.py. Written as text, not through a YAML dumper,
so custom_R_code stays laid out. Run after make_trait_data.py, from the repository root; the build never runs this script."""
import collections
import csv
import re
import textwrap
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[4]
DS = ROOT / "data" / "Bean_2018"
TRAITS = yaml.safe_load((ROOT / "config" / "traits.yml").read_text())["traits"]["elements"]
src = (Path(__file__).parent / "make_trait_data.py").read_text()
SUBS = eval(re.search(r"^SUBS = (\{.*?^\})", src, re.S | re.M).group(1))

rows = list(csv.DictReader(open(DS / "data.csv")))
taxa = list(csv.DictReader(open(DS / "raw" / "Bean_2018_taxa.csv")))
mapped = sorted({r["trait"] for r in rows if r["in_traits.yml"] == "x"})
cands = sorted({r["trait"] for r in rows if r["in_traits.yml"] != "x"})


def rank(r):
    return 2 if r["source_section"].startswith("Key") else 1


groups = {}
for r in rows:
    if r["in_traits.yml"] == "x":
        groups.setdefault((r["taxon_name"], r["trait"], r["entity_measured"]), []).append(r)
kept = [r for g in groups.values() for r in g if rank(r) == min(rank(x) for x in g)]

# one unit per numeric trait, as the source gives it
UNIT_IN = {"count": "{count}", "ratio": "{dimensionless}"}
units = {}
for r in rows:
    if r["in_traits.yml"] == "x" and r["unit"]:
        units.setdefault(r["trait"], set()).add(r["unit"])
for t, u in units.items():
    if len(u) > 1:
        raise SystemExit(f"{t}: several units {u}")
UNITS = {t: UNIT_IN.get(next(iter(u)), next(iter(u))) for t, u in units.items()}
UNITS["flowers_per_inflorescence"] = "{count}/{count}"

MEASURE = ("The measurements for corolla, stamens and style are based on material reconstituted with hot water; "
           "other plant parts were measured from dried material.")
EXTRA = {
    "flower_length": " Corolla length (entity measured), as the flower itself is not measured.",
    "leaf_length_to_width_ratio": " Stated directly as how many times longer than broad the leaves are.",
    "plant_growth_substrate": " Seasonally inundated low-lying areas with sandy soils, read as terrestrial.",
    "plant_width": " The plant diameter.",
    "pedicel_length": " Measured at anthesis (entity measured) where the source says so.",
    "plant_photosynthetic_organ": "The typical value, recorded where leaves are described and nothing suggests otherwise (trait_scoring_method = inferred).",
    "parasitic": "The typical value, recorded for a green-leaved plant with no parasitism or host named (trait_scoring_method = inferred).",
    "leaf_type": "The typical value, recorded for an obovate or lanceolate (flat) leaf (trait_scoring_method = inferred).",
}
SEC_WORDS = [("Description", "the species description"), ("Distribution", "the Distribution and habitat text"), ("Phenology", "the Phenology text"),
             ("Affinities", "the Affinities text, which states values for the species compared"), ("Notes", "the Notes"),
             ("Key", "the key to the Queensland species")]


def methods(trait):
    if trait in ("plant_photosynthetic_organ", "parasitic", "leaf_type"):
        return EXTRA[trait]
    secs = {r["source_section"] for r in kept if r["trait"] == trait}
    words = [w for k, w in SEC_WORDS if any(s.startswith(k) for s in secs)]
    lead = "Scored from " + (words[0] if len(words) == 1 else ", ".join(words[:-1]) + " and " + words[-1]) + "."
    text = lead + EXTRA.get(trait, "")
    if TRAITS[trait]["type"] == "numeric" and any(s.startswith("Description") for s in secs):
        text += " ^p " + MEASURE
    return text


def fold(key, text, indent=2):
    pad = " " * indent
    body = text.replace("'", "''")
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
        )
      ) |>
      dplyr::mutate(
        source_rank = dplyr::if_else(grepl("^Key", source_section), 2, 1)
      ) |>
      dplyr::group_by(taxon_name, trait, entity_measured) |>
      dplyr::filter(source_rank == min(source_rank)) |>
      dplyr::ungroup()
    '"""


types = collections.Counter(t["treatment_type"] for t in taxa)
out = ["""source:
  primary:
    key: Bean_2018
    bibtype: Article
    year: 2018
    author: A. R. Bean
    title: Stemodia anisata A.R.Bean (Plantaginaceae), a new species from Queensland and the Northern Territory
    journal: Austrobaileya
    volume: 10
    number: 2
    pages: 242--246
    doi: 10.5962/p.299905
contributors:
  data_collectors:
  - last_name: Bean
    given_name: Anthony
    ORCID: 0000-0001-5697-1664
    affiliation: Queensland Herbarium, Australia
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes""", custom_code(), """  collection_date: '2018'
  taxon_name: taxon_name
  trait_name: trait
  value: trait_value"""]
out.append(fold("description", "To describe a new species of Stemodia from far western Queensland and the adjacent Northern Territory, and to provide a key to the Queensland species of Stemodia."))
out.append("  basis_of_record: preserved_specimen\n  life_stage: adult")
out.append(fold("sampling_strategy", "This study is based on a morphological examination of specimens held at BRI, and photographic images of specimens held at DNA. " + MEASURE))
out.append(fold("original_file", "Plantaginaceae Stemodia anisata new_species Bean_2018 Austrobaileya part299905.pdf (BHL copy of Austrobaileya 10(2): 242--246)"))
notes = ("data.csv is raw/trait_data.csv (a hand transcription of every clause of the paper, checked on the page images) with trait_scoring_method added from rank_scored_from "
         "(inferred_from_species_group for a key lead covering several species, inferred for typical values stated only when atypical). Type and cited specimens are in raw/Bean_2018_vouchers.csv. "
         "data.csv also carries two unmapped per-taxon columns from raw/Bean_2018_taxa.csv: treatment_type ("
         + ", ".join(f"{n} {k}" for k, n in types.items()) + "; not described = key, Affinities or Notes only) and original_concept_year, the year of the earliest name for the taxon concept "
         "as cited in the paper (blank where the paper gives no year). "
         "Only Stemodia anisata is described. The other Queensland species are scored from the key, S. glabella and S. tephropelina also from the values the Affinities text states for them, "
         "and Stemodia sp. Tanami from the author reading of an image on the Flora NT website. "
         "When a taxon has a character in the text and the key, custom_R_code keeps the text for each taxon, trait and part measured. "
         "Numbers in a key lead covering several species are kept in *_class candidates, not copied to each species. "
         "Candidate traits not yet in traits.yml (unmapped, dropped in custom_R_code): " + ", ".join(cands) + ".")
out.append(fold("notes", notes))
out.append("""identifiers: .na
locations: .na
contexts:
- context_property: entity measured
  category: method_context
  var_in: entity_measured
- context_property: trait scoring method
  category: method_context
  var_in: trait_scoring_method
traits:""")
for t in mapped:
    numeric = TRAITS[t]["type"] == "numeric"
    unit = UNITS[t] if numeric else ".na"
    if "{" in unit:
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
out.append("taxonomic_updates: .na\nexclude_observations: .na")
QUESTIONS = [
    "Stemodia anisata growth form: the description says Prostrate shrub to 10 cm high ... with woody stems, the key says Prostrate forb. Both are in data.csv; the description (shrub, woodiness woody) is mapped because the taxon own text ranks above the key. Confirm, or map both (shrub herb).",
    "S. anisata grows in seasonally inundated low-lying areas with sandy soils. plant_growth_substrate is terrestrial, not semiaquatic, because nothing names a swamp or standing water. Confirm.",
    "Values the Affinities text states for the species compared (erect for S. tephropelina, versus linear for S. glabella, pedicels 1--10 mm long for S. tephropelina, seeds papillose) are scored for those species. Characters of Stemodia sp. Tanami are the author reading of an image on the Flora NT website. Confirm both should be kept.",
    "Stemodia sp. Tanami is written as printed, (P.K. Latz 8218); config/taxon_list.csv has Stemodia sp. Tanami (P.K.Latz 8218), without the space. Left for APCalign.",
    "Mappings to check: seed surface lacunose -> pitted; papillose and minutely papillose -> bumpy; corolla purple or mauve -> blue_purple; forb -> herb; corolla fused for most of its length -> connate; corolla length -> flower_length (entity measured corolla).",
    "The two Northern Territory specimens cited under S. anisata bear the phrase name Stemodia sp. Manners Creek, whose own voucher (T.S.Henshall 1779) Bean says is Elacholoma prostrata. Nothing is scored for Stemodia sp. Manners Creek; the vouchers file records it.",
]
out.append("questions:")
for i, q in enumerate(QUESTIONS, 1):
    out.append(fold(f"question{i}", q))
(DS / "metadata.yml").write_text("\n".join(out) + "\n")
print(len(mapped), "traits;", n_sub, "substitutions;", len(kept), "rows kept of", len(rows))
