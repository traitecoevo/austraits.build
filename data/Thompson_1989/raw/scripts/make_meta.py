#!/usr/bin/env python3
"""Thompson_1989 step 3: writes metadata.yml from data.csv (run make_trait_data.py first). Written as text, not through a
YAML dumper, so custom_R_code stays laid out. The build never runs this script."""
import csv
import textwrap
from pathlib import Path

import yaml

DS = Path(__file__).resolve().parents[2]
ROOT = DS.parents[1]
TRAITS = yaml.safe_load((ROOT / "config" / "traits.yml").read_text())["traits"]["elements"]

rows = list(csv.DictReader(open(DS / "data.csv")))
mapped = sorted({r["trait"] for r in rows if r["in_traits.yml"] == "x"})
cands = sorted({r["trait"] for r in rows if r["in_traits.yml"] != "x"})
kept = [r for r in rows if r["in_traits.yml"] == "x" and r["commonness_qualifier"] != "extreme_range"]

SEC_WORDS = [("Description", "the species descriptions"), ("Subspecies description", "the subspecies descriptions"),
             ("Key to subspecies", "the key to subspecies"), ("Introduction", "the Introduction"),
             ("Putative natural groups", "the discussion of putative natural groups")]
EXTRA = {
    "plant_growth_form": " Hand transcription of each habit sentence: shrub or small tree is shrub tree, an occasional or rare tree form is its own row.",
    "plant_height": " Heights stated separately for the shrub and tree forms are both recorded, with a note.",
    "leaf_shape": " Shape terms mapped as in the ABRS_2026 parser (cuneate to obtriangular, oval to elliptical, broadly X to widely_X).",
    "leaf_length": " In the units printed (cm for four species).",
    "stem_constitution": " The younger stems (entity measured), stout mapped to robust, and habit words (slender, rigid, straggling, robust).",
    "flowers_per_inflorescence": " Flowers single (solitary) or a few together in bract-axils; several (up to N) recorded as a maximum.",
    "flower_ovules_per_functional_carpel_count": " Ovules per loculus.",
    "fruit_width": " The fruit diameter.",
    "flowering_time": " The Main flowering period; estimated by Thompson from herbarium material. Hedged periods (probably, ?) are noted.",
    "plant_growth_substrate": " Terrestrial where the habitat names soils, rocks or vegetation (trait_scoring_method = inferred).",
    "resprouting_capacity": " Stated for L. trinervium and L. subglabratum (epicormic shoots after fire) and L. myrsinoides (Thompson citing Burrell 1981).",
    "bud_bank_location": " As for resprouting_capacity.",
    "seed_viability": " Germination of seed samples, stated for two species.",
    "flower_nectar_production": " Stated for all Group 2 species (nos 39-79): the ovary-top often shining with nectar (trait_scoring_method = inferred_from_species_group).",
}


def methods(trait):
    secs = {r["source_section"] for r in kept if r["trait"] == trait}
    words = [w for k, w in SEC_WORDS if any(s.startswith(k) for s in secs)]
    lead = "Scored from " + (words[0] if len(words) == 1 else ", ".join(words[:-1]) + " and " + words[-1]) + "." if words else ""
    copied = any(r["trait"] == trait and r["rank_scored_from"] == "species" for r in kept)
    tail = " Subspecies silent on the character take the species value (trait_scoring_method = inferred_from_species)." if copied else ""
    return (lead + EXTRA.get(trait, "") + tail).strip()


def fold(key, text, indent=2):
    pad = " " * indent
    lines = textwrap.wrap(f"{key}: '{text.replace(chr(39), chr(39) * 2)}'", width=90, subsequent_indent=pad + "  ",
                          break_long_words=False, break_on_hyphens=False)
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
      )
    '"""


out = ["""source:
  primary:
    key: Thompson_1989
    bibtype: Article
    year: 1989
    author: J. Thompson
    title: A revision of the genus Leptospermum (Myrtaceae)
    journal: Telopea
    volume: 3
    number: 3
    pages: 301--449
    doi: 10.7751/telopea19894902
contributors:
  data_collectors:
  - last_name: Thompson
    given_name: Joy
    ORCID: .na.character
    affiliation: National Herbarium of New South Wales, Royal Botanic Gardens Sydney, Australia
  dataset_curators: Elizabeth Wenk
dataset:
  data_is_long_format: yes""", custom_code(), """  collection_date: '1989'
  taxon_name: taxon_name
  trait_name: trait
  value: trait_value_clean""",
       fold("description", "To give an accurate account of the genus Leptospermum as a whole once its generic limits had been redefined: a new generic description, a key to and formal treatment of its 79 species, and a discussion of its putative natural groups, their relationships and likely history."),
       "  basis_of_record: preserved_specimen\n  life_stage: adult",
       fold("sampling_strategy", "Verbatim from the Introduction and Acknowledgements (the paper has no Methods section): "
            "It is the younger stems that are to be found in herbarium specimens, and characters from these can be useful for distinguishing species. ^p "
            "Estimations of flowering time given here are based on herbarium material and are not necessarily exact. ^p "
            "Length was measured from herbarium specimens in order to relate all species but this reduces considerably the measurements that would be found in fresh specimens. ^p "
            "I would like to thank the Directors of the following herbaria for kindly lending or allowing me access to their collections: AD, BRI, CANB, CBG, HO, L, MEL, NSW, PERTH. "
            "Many of my colleagues at NSW and elsewhere have been most helpful, especially with regard to collection of material."),
       fold("original_file", "Myrtaceae Leptospermum revision Thompson_1989 Telopea telopea19894902.pdf (Telopea 3(3): 301--449)"),
       fold("notes", "data.csv is raw/trait_data.csv with trait_scoring_method added from rank_scored_from (inferred_from_species for "
            "subspecies silent on a character, which take the species value; inferred_from_species_group for a statement about a group of "
            "species; inferred for plant_growth_substrate read from the habitat). Growth form, habit, branching, height and leaf shape and "
            "size are a hand transcription of each habit and leaf sentence (raw/scripts/spec.txt); the other characters are read by "
            "raw/scripts/extra.py from the regular sentences of the descriptions (Flowers, Hypanthium, Sepals, Petals, Stamens, Ovary, Fruit, "
            "Mature seeds, Main flowering period, Distribution), and were reviewed taxon by taxon. Each value keeps its source clause "
            "(trait_value_raw) and source words (trait_value); trait_value_clean is the AusTraits value and is what the build reads. A "
            "commonness word splits a character onto rows (commonness_qualifier); parenthetical extremes are on extreme_range rows, in "
            "data.csv but not mapped. Ecology from the Introduction and the discussion of groups is scored only where stated without "
            "hedging; hedged remarks (fire survival, ant dispersal) are in the notes of the nearest character. Candidate traits not yet "
            "in traits.yml (unmapped, dropped in custom_R_code): " + ", ".join(cands) + "."),
       """identifiers: .na
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
traits:"""]
for t in mapped:
    numeric = TRAITS[t]["type"] == "numeric"
    out.append(f"""- var_in: {t}
  unit_in: {"unit" if numeric else ".na"}
  trait_name: {t}
  entity_type: species
  value_type: value_type
  basis_of_value: {"measurement" if numeric else "expert_score"}
  replicates: .na""")
    out.append(fold("methods", methods(t)))
out.append("""substitutions: .na
taxonomic_updates: .na
exclude_observations:
- variable: taxon_name
  find: Leptospermum javanicum, Leptospermum recurvum
  reason: Non-Australian taxa (South East Asia; Mt Kinabalu, Borneo)""")
QUESTIONS = [
    "The build reads trait_value_clean (the AusTraits value worked out in the extraction scripts), not trait_value with substitutions as in Bean_1992: most trait_value entries are source phrases (white or pink, broadly obovate to cuneate) rather than single terms. Say if you want them recast as terms plus substitutions.",
    "L. subtenue: the Latin diagnosis gives leaves 1--1.5 cm long, narrowly elliptic to oblanceolate; the English description 3--4 mm long, elliptical. The description is transcribed.",
    "L. incanum height: the text layer reads usually 12 m tall; the page image shows 1--2 m, which is what is recorded.",
    "L. argenteum leaves are about 1/2 as wide as long (5--15 mm); no width is recorded.",
    "Fruit persistence (candidate fruit_persistence: long-persistent, tending to persist -> persistent; readily shed, not persisting, tardily deciduous -> deciduous) is not mapped to serotiny, because Thompson calls it a myth that all species hold their seed until fire and gives no serotiny per species. Say if persistent should be scored serotinous.",
    "Resprouting of L. myrsinoides and the seed viability of L. scoparium (in notes) are Thompson citing Burrell (1981, 1965); consider Burrell as a source_id or secondary reference.",
    "Several names are not current APC names: L. parviflorum (APC: L. madidum subsp. sativum), L. petersonii subsp. lanceolatum (APC: L. amboinense), L. petersonii subsp. petersonii. L. javanicum and L. recurvum are excluded as non-Australian.",
    "Leaf shapes with no level and left unmapped: very narrowly clavate and almost triquetrous (L. confertum); seed shape oblong.",
]
out.append("questions:")
for i, q in enumerate(QUESTIONS, 1):
    out.append(fold(f"question{i}", q))
(DS / "metadata.yml").write_text("\n".join(out) + "\n")
print("metadata.yml:", len(mapped), "traits,", len(cands), "candidates")
