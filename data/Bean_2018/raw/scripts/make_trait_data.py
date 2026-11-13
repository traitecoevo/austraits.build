#!/usr/bin/env python3
"""Bean_2018: writes raw/trait_data.csv, raw/Bean_2018_taxa.csv and data.csv from the hand transcription below.

Bean, A.R. (2018) Stemodia anisata A.R.Bean (Plantaginaceae), a new species from Queensland and the Northern Territory.
Austrobaileya 10(2): 242--246. The BHL text layer garbles some digits, so every clause was checked on the page images
(`pdftoppm -r 120`). Run from the repository root; the build never runs this script.

Each `c()` / `n()` / `m()` call is one clause. SUBS maps the source's own terms to trait levels (or draft values for candidate
traits); make_meta.py turns it into metadata.yml substitutions.
"""
import csv
import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[4]
DS = ROOT / "data" / "Bean_2018"
TRAITS = yaml.safe_load((ROOT / "config" / "traits.yml").read_text())["traits"]["elements"]
CHECKED = "Candidate trait, not in config/traits.yml (checked 2026-11)."

S = "Stemodia "
ANI, GLA, TEP, LYT, FLO, VIS, PUB, LAT = (S + x for x in ("anisata", "glabella", "tephropelina", "lythrifolia", "florulenta",
                                                             "viscosa", "pubescens", "lathraia"))
TAN = S + "sp. Tanami (P.K. Latz 8218)"
ALL_TAXA = [ANI, LYT, FLO, VIS, PUB, LAT, GLA, TEP, TAN]

SUBS = {
    "plant_growth_form": {"forb": "herb"},
    "stem_growth_habit": {"upright": "erect"},
    "stem_hairs": {"glandular_hairs_moderately_dense": "glandular_hairy"},
    "leaf_hairs_adult_leaves": {"densely_hairy": "hairy"},
    "leaf_margin": {"obscurely_toothed": "toothed", "serrate": "toothed_serrate"},
    "leaf_phyllotaxis": {"in_whorls_of_three": "whorled"},
    "inflorescence_type": {"spicate": "spike"},
    "flower_colour": {"purple": "blue_purple", "mauve": "blue_purple"},
    "flower_petal_fusion": {"fused_for_most_of_its_length": "connate"},
    "fruit_shape": {"narrowly-ovoid": "ovoid"},
    "fruit_dehiscence": {"splitting_loculicidally": "dehiscent"},
    "seed_shape": {"ellipsoidal": "ellipsoid"},
    "seed_colour": {"yellow": "yellow_orange", "brown": "red_brown"},
    "seed_surface_texture": {"lacunose": "pitted", "papillose": "bumpy", "minutely_papillose": "bumpy"},
    # draft values for candidate traits
    "fruit_dehiscence_type": {"splitting_loculicidally": "loculicidal"},
    "seed_papillae": {"without_papillae": "absent", "non-papillose": "absent", "papillose": "present", "minutely_papillose": "present"},
    "corolla_shape": {"2-lipped": "bilabiate", "strongly_two-lipped": "bilabiate"},
}

rows = []


def clean(trait, value):
    subs = SUBS.get(trait, {})
    t = TRAITS.get(trait)
    levels = (t.get("allowed_values_levels") or {}) if t else {}
    out = []
    for tok in str(value).split(" "):
        v = subs.get(tok, tok)
        if t and t.get("type") == "categorical" and levels and any(x not in levels for x in v.split(" ")):
            continue
        if v not in out:
            out.append(v)
    return " ".join(out)


def norm(s):
    s = str(s).replace("×", "x").replace("±", "+/-").replace("–", "--").replace("—", "--")
    return re.sub(r"(\d)-(\d)", r"\1--\2", s)


def add(taxon, trait, raw, value, vt, unit, sec, q="", pop="", ent="", rank="", note=""):
    in_yml = "x" if trait in TRAITS else ""
    notes = (CHECKED + " " + note).strip() if not in_yml else note
    tv = str(value)
    rows.append(dict(taxon_name=taxon, entity_type="species", rank_scored_from=rank, commonness_qualifier=q, population_region=pop,
                     entity_measured=ent, trait=trait, **{"in_traits.yml": in_yml}, trait_value_raw=norm(raw), trait_value=tv,
                     trait_value_clean=clean(trait, tv), value_type=vt, unit=unit, source_section=sec, notes=notes))


class T:
    def __init__(self, taxon, sec, **default):
        self.t, self.sec, self.d = taxon, sec, default

    def c(self, trait, raw, value, **k):
        add(self.t, trait, raw, value, "mode", "", self.sec, **{**self.d, **k})

    def n(self, trait, raw, lo, hi, unit, **k):
        if lo is not None:
            add(self.t, trait, raw, lo, "minimum", unit, self.sec, **{**self.d, **k})
        if hi is not None:
            add(self.t, trait, raw, hi, "maximum", unit, self.sec, **{**self.d, **k})

    def m(self, trait, raw, v, unit, vt="mean", **k):
        add(self.t, trait, raw, v, vt, unit, self.sec, **{**self.d, **k})


def month(a, b):
    return "".join("y" if a <= i <= b else "n" for i in range(1, 13))


# ------------------------------------------------------------------ Stemodia anisata: description, p.242-243
d = T(ANI, "Description, p.242")
r = "Prostrate shrub to 10 cm high and 80 cm diameter with woody stems, branches not rooting at the nodes"
d.c("plant_growth_form", r, "shrub")
d.c("stem_growth_habit", r, "prostrate")
d.n("plant_height", r, None, 10, "cm")
d.n("plant_width", r, None, 80, "cm")
d.c("woodiness_detailed", r, "woody", ent="stems")
d.c("stem_rooting_at_nodes", r, "not_rooting", ent="branches")
r = "Indumentum on branchlets of spreading, eglandular hairs, 0.2-0.5 mm long"
d.c("stem_hairs", r, "hairy", ent="branchlets")
d.c("stem_hair_type", r, "eglandular", ent="branchlets")
d.c("stem_hair_orientation", r, "spreading", ent="branchlets")
d.n("stem_hair_length", r, 0.2, 0.5, "mm", ent="branchlets")
r = "Leaves opposite, +/- sessile, obovate, chartaceous, 8-25 mm long, 2.4-6.2 mm wide, 2.4-4 times longer than broad"
d.c("leaf_phyllotaxis", r, "opposite")
d.c("leaf_attachment", r, "+/-_sessile")
d.c("leaf_shape", r, "obovate")
d.c("leaf_texture", r, "chartaceous")
d.n("leaf_length", r, 8, 25, "mm")
d.n("leaf_width", r, 2.4, 6.2, "mm")
d.n("leaf_length_to_width_ratio", r, 2.4, 4, "ratio")
r = "midvein readily seen, but no other veins visible"
d.c("leaf_midrib_visibility", r, "readily_seen")
d.c("leaf_secondary_vein_visibility", r, "not_visible")
d = T(ANI, "Description, p.243")
r = "both surfaces densely hairy with white eglandular spreading hairs, and with numerous sessile yellow glands"
d.c("leaf_hairs_adult_leaves", r, "densely_hairy")
d.c("leaf_adaxial_hairs", r, "densely_hairy")
d.c("leaf_abaxial_hairs", r, "densely_hairy")
d.c("leaf_hair_colour", r, "white")
d.c("leaf_hair_type", r, "eglandular")
d.c("leaf_hair_orientation", r, "spreading")
d.c("leaf_glands", r, "numerous_sessile_glands")
d.c("leaf_gland_colour", r, "yellow")
d.c("leaf_margin", "margins entire or with a few obscure teeth", "entire obscurely_toothed")
d.c("leaf_apex_shape", "apex obtuse", "obtuse")
d.c("inflorescence_type", "Flowers solitary in leaf axils", "solitary axillary")
d.m("flowers_per_inflorescence", "Flowers solitary in leaf axils", 1, "count", vt="mode")
r = "pedicel 10-24 mm long at anthesis, shorter before anthesis, longer at fruiting stage, with a mixture of glandular and eglandular hairs"
d.n("pedicel_length", r, 10, 24, "mm", ent="at anthesis")
d.c("pedicel_elongation", r, "elongating_from_bud_to_fruit")
d.c("pedicel_hair_type", r, "glandular eglandular")
r = "Bracteoles 2, opposite to sub-opposite, positioned at distal end of pedicel, linear to narrowly deltate, 1.1-1.9 mm long, 0.3-0.6 times as long as sepals"
d.m("bracteole_count", r, 2, "count", vt="mode")
d.c("bracteole_arrangement", r, "opposite sub-opposite")
d.c("bracteole_position", r, "distal_end_of_pedicel")
d.c("bracteole_shape", r, "linear narrowly_deltate")
d.n("bracteole_length", r, 1.1, 1.9, "mm")
d.n("bracteole_to_sepal_length_ratio", r, 0.3, 0.6, "ratio")
r = "Sepals 5, equal, narrowly-deltate, 3.1-4.1 mm long, with many glandular hairs and some eglandular hairs throughout"
d.m("sepal_count", r, 5, "count", vt="mode")
d.c("sepal_size_uniformity", r, "equal")
d.c("sepal_shape", r, "narrowly-deltate")
d.n("flower_sepal_length", r, 3.1, 4.1, "mm")
d.c("sepal_hairs", r, "glandular eglandular", note="Many glandular and some eglandular hairs.")
r = "Corolla 11-14 mm long, fused for most of its length, 2-lipped"
d.n("flower_length", r, 11, 14, "mm", ent="corolla")
d.c("flower_petal_fusion", r, "fused_for_most_of_its_length")
d.c("corolla_shape", r, "2-lipped")
r = "the lower lip with 3 obtuse lobes 3-4.5 mm long, purple or mauve; the upper lip with 2 mostly fused lobes, 2-3.5 mm long, purple"
d.m("corolla_lobe_count", r, 3, "count", vt="mode", ent="lower lip")
d.m("corolla_lobe_count", r, 2, "count", vt="mode", ent="upper lip")
d.c("corolla_lobe_shape", r, "obtuse", ent="lower lip")
d.c("corolla_lobe_fusion", r, "mostly_fused", ent="upper lip")
d.n("corolla_lobe_length", r, 3, 4.5, "mm", ent="lower lip")
d.n("corolla_lobe_length", r, 2, 3.5, "mm", ent="upper lip")
d.c("flower_colour", r, "purple mauve", note="Lips purple or mauve; the tube is pale yellow to brown (corolla_tube_colour).")
r = "corolla tube 6.5-9 mm long, inner surface pale yellow, with very sparse hispid eglandular hairs; outer surface pale yellow to brown, with very sparse glandular and eglandular hairs"
d.n("corolla_tube_length", r, 6.5, 9, "mm")
d.c("corolla_tube_colour", r, "pale_yellow", ent="inner surface")
d.c("corolla_tube_colour", r, "pale_yellow brown", ent="outer surface")
d.c("corolla_hairs", r, "very_sparse_hispid", ent="tube inner surface")
d.c("corolla_hair_type", r, "eglandular", ent="tube inner surface")
d.c("corolla_hairs", r, "very_sparse", ent="tube outer surface")
d.c("corolla_hair_type", r, "glandular eglandular", ent="tube outer surface")
r = "Stamens 4, all fertile, epipetalous, didynamous"
d.m("flower_fertile_stamens_count", r, 4, "count", vt="mode")
d.c("stamen_attachment", r, "epipetalous")
d.c("stamen_arrangement", r, "didynamous")
r = "anthers 2-celled, 0.9-1.1 mm long, white"
d.m("anther_locule_count", r, 2, "count", vt="mode")
d.n("flower_anther_length", r, 0.9, 1.1, "mm")
d.c("flower_anther_colour", r, "white")
r = "outer filaments 2.5-3 mm long, inner filaments 4-4.5 mm long"
d.n("flower_filament_length", r, 2.5, 3, "mm", ent="outer filaments")
d.n("flower_filament_length", r, 4, 4.5, "mm", ent="inner filaments")
r = "Ovary 2-locular, stigma a broad flap, glabrous"
d.m("ovary_locule_count", r, 2, "count", vt="mode")
d.c("stigma_shape", r, "broad_flap")
d.c("stigma_hairs", r, "glabrous")
r = "Capsule included in the persistent calyx, narrowly-ovoid, 3.7-4.5 mm long, splitting loculicidally"
d.c("fruit_type", r, "capsule")
d.c("calyx_persistence", r, "persistent")
d.c("fruit_enclosure", r, "included_in_calyx")
d.c("fruit_shape", r, "narrowly-ovoid")
d.n("fruit_length", r, 3.7, 4.5, "mm")
d.c("fruit_dehiscence", r, "splitting_loculicidally")
d.c("fruit_dehiscence_type", r, "splitting_loculicidally")
r = "seeds ellipsoidal, 0.2-0.3 mm long, black; surface lacunose, without papillae"
d.c("seed_shape", r, "ellipsoidal")
d.n("seed_length", r, 0.2, 0.3, "mm")
d.c("seed_colour", r, "black")
d.c("seed_surface_texture", r, "lacunose")
d.c("seed_papillae", r, "without_papillae")

d = T(ANI, "Distribution and habitat, p.243")
r = ("Stemodia anisata occurs in far western Queensland, from Mount Isa to Ethabuka Nature Refuge, and the immediately "
     "adjacent parts of the Northern Territory (Map 1).")
d.c("distribution_description", r, r)
r = ("It inhabits seasonally inundated low-lying areas, either treeless or with scattered Eucalyptus coolabah Blakely & Jacobs, "
     "gidgee (Acacia georginae F.M.Bailey or A. cambagei R.T.Baker), or A. chisholmii F.M.Bailey. The soils are sandy, at least near the surface.")
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial", note="Seasonally inundated low-lying areas with sandy soils; read as terrestrial, not semiaquatic.")
d.c("habitat_terms", r, "seasonally_inundated low-lying treeless scattered_trees")
d.c("associated_species", r, "Eucalyptus_coolabah Acacia_georginae Acacia_cambagei Acacia_chisholmii")
d.c("soil_terms", r, "sandy", ent="near the surface")
d = T(ANI, "Phenology, p.243")
r = "Flowers have been recorded from April to August; fruits from June to August."
d.c("flowering_time", r, month(4, 8))
d.c("fruiting_time", r, month(6, 8))
d = T(ANI, "Affinities, p.243")
r = "the former differs by the prostrate habit, hairy branchlets, obovate leaves ... the relatively long hairs on the pedicel and calyx, and black, non-papillose seeds"
d.c("pedicel_hair_length", r, "relatively_long", note="Relative to S. glabella.")
d.c("sepal_hair_length", r, "relatively_long", note="Relative to S. glabella.")
d.c("seed_papillae", r, "non-papillose")
r = "the lack of glandular hairs on the branchlets"
d.c("stem_hair_type", r, "eglandular", ent="branchlets")
T(ANI, "Conservation status, p.244").c("conservation_status", "Conservation status: Least Concern.", "least_concern")
T(ANI, "Etymology, p.244").c("leaf_scent", "The Latin epithet anisata refers to the aniseed aroma of the crushed leaves, mentioned independently by two collectors.",
                              "aniseed", ent="crushed leaves")

# ------------------------------------------------------------------ Affinities: values stated for S. glabella and S. tephropelina
d = T(GLA, "Affinities, p.243")
r = "obovate leaves (versus linear for S. glabella)"
d.c("leaf_shape", r, "linear")
r = "black, non-papillose seeds (versus yellow to brown, papillose for S. glabella)"
d.c("seed_colour", r, "yellow brown")
d.c("seed_surface_texture", r, "papillose")
d.c("seed_papillae", r, "papillose")
d = T(TEP, "Affinities, p.243")
d.c("stem_growth_habit", "the prostrate habit (erect for S. tephropelina)", "erect")
r = "the lack of glandular hairs on the branchlets (glandular hairs moderately dense for S. tephropelina)"
d.c("stem_hairs", r, "glandular_hairs_moderately_dense", ent="branchlets")
d.c("stem_hair_type", r, "glandular", ent="branchlets")
d.c("leaf_margin", "the entire leaf margins (serrate for S. tephropelina)", "serrate")
d.n("pedicel_length", "the pedicels 10-24 mm long (1-10 mm long for S. tephropelina)", 1, 10, "mm",
    note="The S. anisata pedicels it is compared with are measured at anthesis.")
r = "the lacunose seeds without papillae (minutely papillose for S. tephropelina)"
d.c("seed_surface_texture", r, "minutely_papillose")
d.c("seed_papillae", r, "minutely_papillose")

# ------------------------------------------------------------------ Notes: Stemodia sp. Tanami, from an image
d = T(TAN, "Notes, p.243", note="Read by the author from an image on the Flora NT website (Northern Territory Herbarium 2015).")
r = ("An image of Stemodia sp. Tanami (P.K. Latz 8218) on the Flora NT website shows an upright plant with small lanceolate leaves, "
     "very short pedicels, and strongly two-lipped flowers with a narrow throat.")
d.c("stem_growth_habit", r, "upright")
d.c("leaf_shape", r, "lanceolate")
d.c("leaf_size_categorical", r, "small")
d.c("pedicel_length_class", r, "very_short")
d.c("corolla_shape", r, "strongly_two-lipped")
d.c("corolla_throat_width", r, "narrow")

# ------------------------------------------------------------------ Key to the Queensland species of Stemodia, p.245
# lead: (text, taxa it covers, specs). One taxon = its own value; several = species_group (categorical only; numbers of a
# group lead go to *_class candidates).
SEC = "Key, couplet {}, p.245"
KEY = [
    (1, "Inflorescence/infructescence spicate with more than 10 sessile flowers/fruits", [LYT],
     [("inflorescence_type", "spicate"), ("flowers_per_inflorescence_class", ">10"), ("flower_attachment", "sessile")]),
    (1, "Inflorescence/infructescence with 1-3-pedicellate flowers/fruits in leaf axil", [FLO, VIS, PUB, ANI, LAT, GLA],
     [("inflorescence_type", "axillary"), ("flowers_per_inflorescence_class", "1--3"), ("flower_attachment", "pedicellate")]),
    (2, "Some leaf axils with 2 or 3 flowers/fruits, some leaf axils with a single flower", [FLO],
     [("flowers_per_inflorescence", 1, 3, "count")]),
    (2, "Flowers always solitary", [VIS, PUB, ANI, LAT, GLA], [("inflorescence_type", "solitary")]),
    (3, "Bracteoles 0.7-1.4 times as long as sepals, and about as wide as sepals", [VIS, PUB],
     [("bracteole_to_sepal_length_ratio_class", "0.7--1.4"), ("bracteole_width_relative_to_sepals", "about_as_wide")]),
    (3, "Bracteoles 0.3-0.6 times as long as sepals, and considerably narrower than sepals", [ANI, LAT, GLA],
     [("bracteole_to_sepal_length_ratio_class", "0.3--0.6"), ("bracteole_width_relative_to_sepals", "considerably_narrower")]),
    (4, "Pedicels 7-29 mm long at anthesis; leaves in whorls of three; corolla 11-16 mm long; bracteoles 0.9-1.4 times as long as sepals", [VIS],
     [("pedicel_length", 7, 29, "mm", "at anthesis"), ("leaf_phyllotaxis", "in_whorls_of_three"), ("leaves_per_whorl", 3, 3, "count"),
      ("flower_length", 11, 16, "mm", "corolla"), ("bracteole_to_sepal_length_ratio", 0.9, 1.4, "ratio")]),
    (4, "Pedicels 0.5-3 mm long at anthesis; leaves opposite; corolla 9-11 mm long; bracteoles 0.7-0.9 times as long as sepals", [PUB],
     [("pedicel_length", 0.5, 3, "mm", "at anthesis"), ("leaf_phyllotaxis", "opposite"), ("flower_length", 9, 11, "mm", "corolla"),
      ("bracteole_to_sepal_length_ratio", 0.7, 0.9, "ratio")]),
    (5, "Prostrate forb; leaves obovate, 2.4-4 times longer than wide", [ANI],
     [("plant_growth_form", "forb"), ("stem_growth_habit", "prostrate"), ("leaf_shape", "obovate"), ("leaf_length_to_width_ratio", 2.4, 4, "ratio")]),
    (5, "Erect forbs; leaves linear, 8-27 times longer than wide", [LAT, GLA],
     [("plant_growth_form", "forb"), ("stem_growth_habit", "erect"), ("leaf_shape", "linear"), ("leaf_length_to_width_ratio_class", "8--27")]),
    (6, "Corolla 6-9 mm long; sepals with long glandular hairs", [LAT],
     [("flower_length", 6, 9, "mm", "corolla"), ("sepal_hairs", "glandular"), ("sepal_hair_length", "long")]),
    (6, "Corolla 11-16 mm long; sepals with short glandular hairs or glabrous", [GLA],
     [("flower_length", 11, 16, "mm", "corolla"), ("sepal_hairs", "glandular glabrous"), ("sepal_hair_length", "short")]),
]
for num, text, taxa, specs in KEY:
    group = len(taxa) > 1
    for tx in taxa:
        d = T(tx, SEC.format(num), rank="species_group" if group else "")
        for spec in specs:
            if isinstance(spec[1], str):
                d.c(spec[0], text, spec[1])
            else:
                trait, lo, hi, unit = spec[:4]
                ent = spec[4] if len(spec) > 4 else ""
                if group:
                    raise SystemExit(f"numeric group lead {num}: {spec}")
                if lo == hi:
                    d.m(trait, text, lo, unit, vt="mode", ent=ent)
                else:
                    d.n(trait, text, lo, hi, unit, ent=ent)

# ------------------------------------------------------------------ Inferred typical values
for tx in ALL_TAXA:
    leaves = any(r0["taxon_name"] == tx and r0["trait"] in ("leaf_shape", "leaf_phyllotaxis", "leaf_margin") for r0 in rows)
    if leaves:
        add(tx, "plant_photosynthetic_organ", "Leaves described; no leafless, phyllode or photosynthetic-stem wording", "leaf", "mode", "", "Inferred", rank="inferred")
        add(tx, "parasitic", "Green-leaved plant; no parasitism or host named", "not_parasitic", "mode", "", "Inferred", rank="inferred")
    shapes = {r0["trait_value"] for r0 in rows if r0["taxon_name"] == tx and r0["trait"] == "leaf_shape"}
    if shapes & {"obovate", "lanceolate"}:
        add(tx, "leaf_type", "Flat-lamina leaf shape (obovate or lanceolate)", "broadleaf", "mode", "", "Inferred", rank="inferred")

# ------------------------------------------------------------------ One key row per taxon x trait x part: categorical values from
# several leads are joined in key order (couplet 1 axillary + couplet 2 solitary), so the build gets no duplicate rows.
merged, keep = {}, []
for r0 in rows:
    if r0["source_section"].startswith("Key") and r0["value_type"] == "mode" and not r0["unit"]:
        k = (r0["taxon_name"], r0["trait"], r0["entity_measured"])
        if k in merged:
            m0 = merged[k]
            m0["trait_value"] = " ".join(dict.fromkeys(m0["trait_value"].split(" ") + r0["trait_value"].split(" ")))
            m0["trait_value_clean"] = clean(r0["trait"], m0["trait_value"])
            m0["trait_value_raw"] += " / " + r0["trait_value_raw"]
            nums = re.findall(r"couplets? ([\d, ]+),", m0["source_section"])[0].strip(", ") + ", " + re.search(r"couplet (\d+)", r0["source_section"]).group(1)
            m0["source_section"] = f"Key, couplets {nums}, p.245"
            m0["rank_scored_from"] = m0["rank_scored_from"] if r0["rank_scored_from"] == m0["rank_scored_from"] else ""
            continue
        merged[k] = r0
    keep.append(r0)
rows = keep

# ------------------------------------------------------------------ Treatment type and first-description year, per taxon
ND = "not described"
KEYN = "In the key only."
TAXA = [
    (ANI, "new species", 2018, "sp. nov., this paper", "Taxonomy, p.242", ""),
    (LYT, ND, "", "", "Key, p.245", KEYN + " Author and year not given."),
    (FLO, ND, "", "", "Key, p.245", KEYN + " Author and year not given."),
    (VIS, ND, "", "", "Key, p.245", KEYN + " Author and year not given."),
    (PUB, ND, "", "", "Key, p.245", KEYN + " Author and year not given."),
    (LAT, ND, "", "", "Key, p.245", KEYN + " Author and year not given."),
    (GLA, ND, "", "", "Affinities, p.243; Key, p.245", "Key and Affinities only. Cited as S. glabella W.R.Barker, without a year."),
    (TEP, ND, "", "", "Affinities, p.243", "Affinities only. Cited as S. tephropelina W.R.Barker, without a year."),
    (TAN, ND, "", "", "Notes, p.243", "Phrase name; characters read from an image on the Flora NT website."),
]
assert sorted(t[0] for t in TAXA) == sorted(ALL_TAXA)
with open(DS / "raw" / "Bean_2018_taxa.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["taxon_name", "treatment_type", "original_concept_year", "original_concept_basis", "source_section", "notes"])
    w.writerows(TAXA)
TT = {t[0]: (t[1], str(t[2])) for t in TAXA}
for r0 in rows:
    r0["treatment_type"], r0["original_concept_year"] = TT[r0["taxon_name"]]

rows.sort(key=lambda r0: ALL_TAXA.index(r0["taxon_name"]))
COLS = ["taxon_name", "treatment_type", "original_concept_year", "entity_type", "rank_scored_from", "commonness_qualifier", "population_region",
        "entity_measured", "trait", "in_traits.yml", "trait_value_raw", "trait_value", "trait_value_clean", "value_type", "unit", "source_section", "notes"]
with open(DS / "raw" / "trait_data.csv", "w", newline="") as f:
    w = csv.DictWriter(f, COLS)
    w.writeheader()
    w.writerows(rows)

# data.csv = trait_data.csv + trait_scoring_method, from rank_scored_from, without all-empty columns
METHOD = {"species_group": "inferred_from_species_group", "inferred": "inferred"}
drows = [dict(r0, trait_scoring_method=METHOD.get(r0["rank_scored_from"], "")) for r0 in rows]
DCOLS = [c for c in COLS + ["trait_scoring_method"] if any(r0[c] for r0 in drows)]
with open(DS / "data.csv", "w", newline="") as f:
    w = csv.DictWriter(f, DCOLS, extrasaction="ignore")
    w.writeheader()
    w.writerows(drows)

print(len(rows), "rows;", len({r0['taxon_name'] for r0 in rows}), "taxa;",
      len({r0['trait'] for r0 in rows if r0['in_traits.yml']}), "mapped traits;",
      len({r0['trait'] for r0 in rows if not r0['in_traits.yml']}), "candidates")
