#!/usr/bin/env python3
"""Bean_1992: writes raw/trait_data.csv, raw/Bean_1992_taxa.csv and data.csv from the hand transcription below (vouchers: make_vouchers.py).

Bean, A.R. (1992) The genus Leptospermum Forst. et Forst. f. (Myrtaceae) in northern Australia and Malesia.
Austrobaileya 3(4): 643--659. The PDF is a BHL scan with a poor text layer, so every clause was read from the page
images (`pdftoppm -r 130`), not parsed. Run from the repository root; the build never runs this script.

Each `c()` / `n()` call is one clause of the paper. Candidate traits (not in config/traits.yml) keep a draft value in
trait_value_clean. SUBS maps the source's own terms to trait levels; metadata.yml's substitutions are made from it.
"""
import csv
import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[4]
DS = ROOT / "data" / "Bean_1992"
TRAITS = yaml.safe_load((ROOT / "config" / "traits.yml").read_text())["traits"]["elements"]
CHECKED = "Candidate trait, not in config/traits.yml (checked 2026-11)."

SP = {
    1: "pallidum", 2: "madidum", 3: "brachyandrum", 4: "parviflorum", 5: "purpurascens", 6: "luehmannii", 7: "speciosum",
    8: "whitei", 9: "trinervium", 10: "lamellatum", 11: "microcarpum", 12: "brevipes", 13: "neglectum", 14: "sericatum",
    15: "parvifolium", 16: "venustum", 17: "semibaccatum", 18: "arachnoides", 19: "liversidgei", 20: "oreophilum",
    21: "polygalifolium", 22: "variabile", 23: "novae-angliae", 24: "minutifolium", 25: "juniperinum", 26: "recurvum",
    27: "javanicum", 28: "wooroonooran", 29: "amboinense", 30: "petersonii", 31: "gregarium",
}
NAME = {k: "Leptospermum " + v for k, v in SP.items()}
MAD, MAD1, MAD2 = NAME[2], NAME[2] + " subsp. madidum", NAME[2] + " subsp. sativum"
ALL_TAXA = [NAME[1], MAD, MAD1, MAD2] + [NAME[i] for i in range(3, 32)]

# Source term -> AusTraits level(s), by trait. Terms already equal to a level need no entry.
SUBS = {
    "flower_colour": {"white": "white_cream", "deep_pink": "pink", "pale_pink": "pink"},
    "leaf_discolority": {"concolourous": "concolorous", "discolourous": "discolorous",
                         "strongly_discolourous": "discolorous", "slightly_discolourous": "discolorous"},
    "leaf_surface_colour": {"pale_yellowish-green": "green_yellow", "mid-green": "green"},
    "leaf_shape": {"narrow-lanceolate": "narrowly_lanceolate", "broadly_elliptical": "widely_elliptical"},
    "leaf_apex_shape": {"obtuse_or_minutely_retuse": "obtuse"},
    "leaf_margin_posture": {"recurved": "revolute", "strongly_recurved": "revolute", "not_recurved": "flat"},
    "stem_hairs": {"virtually_glabrous": "subglabrous", "sparsely_pubescent": "pubescent"},
    "bark_texture": {"longitudinally_fissured": "furrowed", "scaly": "flaky", "+/-_smooth": "smooth",
                     "completely_smooth": "smooth"},
    "bark_colour": {"creamy": "white"},
    "stem_shape": {"triangular": "angular"},
    "leaf_hairs_adult_leaves": {"glabrescent": "glabrous", "sparsely_pubescent": "hairy",
                                "densely_silky-pubescent": "hairy"},
    "leaf_hairs_immature_leaves": {"sparse_long_hairs": "hairy", "appressed_indumentum": "hairy",
                                   "long_marginal_hairs": "hairy", "silky_pubescent": "hairy",
                                   "silky_appressed_hairs": "hairy"},
    "flower_anther_dehiscence": {"narrow_slits": "longitudinal_slit"},
    "fruit_shape": {"hemispherical": "hemispheric", "globular-truncate": "globoid"},
    "fruit_surface_hairs": {"pubescent": "hairy"},
    "fruit_fleshiness": {"succulent": "fleshy"},
    "seed_colour": {"pale_brown": "red_brown", "light_brown": "red_brown", "brown": "red_brown",
                    "dark_brown": "red_brown", "quite_black": "black"},
    "seed_shape": {"linear": "ovoid_elongated", "obovoid": "ovoid", "elliptical": "ellipsoid"},
    "seed_surface_texture": {"striate": "ribbed", "reticulate": "netted", "conspicuously_reticulate": "netted"},
    "inflorescence_type": {"axillary_raceme": "raceme axillary"},
    "fruit_type": {"capsular": "capsule"},
    # draft values for candidate traits
    "hypanthium_hairs": {"silky_pubescent": "sericeous", "silky-hairy": "sericeous", "sparsely_pubescent": "pubescent",
                         "densely_pubescent": "pubescent"},
    "sepal_margin_hairs": {"densely_ciliate": "ciliate"},
    "bract_persistence": {"shed_early_in_bud_development": "caducous", "shed_before_anthesis": "caducous",
                          "shed_well_before_anthesis": "caducous", "shed_just_prior_to_anthesis": "persistent_to_anthesis"},
    "bracteole_persistence": {"shed_early_in_bud_development": "caducous", "shed_well_before_anthesis": "caducous",
                              "shed_just_prior_to_anthesis": "persistent_to_anthesis"},
    "ovary_hairs": {"mostly_glabrous": "glabrous", "glabrous_except_for_short_erect_hairs_at_base_of_style_and_along_valve_margins": "glabrous"},
}

rows = []
vouchers = []


def clean(trait, value):
    """Space-delimited source terms -> levels (or drafts); terms with no level stay out of a mapped trait."""
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


def add(taxon, trait, raw, value, vt, unit, sec, q="", pop="", ent="", rank="", note=""):
    in_yml = "x" if trait in TRAITS else ""
    notes = (CHECKED + " " + note).strip() if not in_yml else note
    tv = str(value).replace("–", "--")
    rows.append(dict(taxon_name=taxon, entity_type="species", rank_scored_from=rank, commonness_qualifier=q, population_region=pop,
                     entity_measured=ent, trait=trait, **{"in_traits.yml": in_yml},
                     trait_value_raw=norm(raw), trait_value=tv, trait_value_clean=clean(trait, tv), value_type=vt, unit=unit,
                     source_section=sec, notes=notes))


def norm(s):
    s = str(s).replace("×", "x").replace("±", "+/-").replace("–", "--").replace("—", "--")
    return re.sub(r"(\d)-(\d)", r"\1--\2", s)


class T:
    """One taxon + one section of the paper."""
    def __init__(self, taxon, sec):
        self.t, self.sec = taxon, sec

    def c(self, trait, raw, value, **k):
        add(self.t, trait, raw, value, "mode", "", self.sec, **k)

    def n(self, trait, raw, lo, hi, unit, **k):
        """lo--hi; either may be None. A count or a 'c.' value goes through m()."""
        if lo is not None:
            add(self.t, trait, raw, lo, "minimum", unit, self.sec, **k)
        if hi is not None:
            add(self.t, trait, raw, hi, "maximum", unit, self.sec, **k)

    def m(self, trait, raw, v, unit, vt="mean", **k):
        add(self.t, trait, raw, v, vt, unit, self.sec, **k)


def month(a, b):
    """Calendar string for month a..b inclusive, 1-based."""
    return "".join("y" if a <= i <= b else "n" for i in range(1, 13))


# ------------------------------------------------------------------ 1. L. pallidum
d = T(NAME[1], "Description, p.645")
r = "Spreading shrub to 3 metres high"
d.c("plant_growth_form", r, "shrub")
d.c("stem_growth_habit", r, "spreading")
d.n("plant_height", r, None, 3, "m")
r = "bark rough, grey, longitudinally fissured, closely adhering, persistent throughout"
d.c("bark_description", r, norm(r))
d.c("bark_texture", r, "rough longitudinally_fissured")
d.c("bark_colour", r, "grey")
d.c("bark_adherence", r, "closely_adhering")
d.c("bark_persistence", r, "persistent_throughout")
d.c("stem_hairs", "branchlets virtually glabrous", "virtually_glabrous", ent="branchlets")
d.c("stem_flanges", "stem flanges absent", "absent")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
r = "sessile or with petioles up to 2 mm long"
d.c("leaf_attachment", r, "sessile petiolate")
d.n("petiole_length", r, None, 2, "mm")
d.c("leaf_discolority", "concolourous", "concolourous")
d.c("leaf_surface_colour", "pale yellowish-green", "pale_yellowish-green")
d.c("leaf_shape", "narrow-lanceolate", "narrow-lanceolate")
r = "35-52 x 5-9 mm"
d.n("leaf_length", r, 35, 52, "mm")
d.n("leaf_width", r, 5, 9, "mm")
d.c("leaf_venation", "more or less triplinerved", "triplinerved")
r = "oil glands numerous, conspicuous"
d.c("leaf_oil_gland_abundance", r, "numerous")
d.c("leaf_oil_gland_prominence", r, "conspicuous")
d.c("leaf_base_shape", "leaf base cuneate", "cuneate")
d.c("leaf_apex_shape", "apex acute to acuminate", "acute acuminate")
r = "young leaves with sparse long hairs on underside, glabrescent"
d.c("leaf_hairs_immature_leaves", r, "sparse_long_hairs", ent="underside")
d.c("leaf_abaxial_hairs", r, "sparse_long", ent="young leaves")
d.c("leaf_hairs_adult_leaves", r, "glabrescent", note="Young leaves described as glabrescent, read as adult leaves glabrous.")
r = "Inflorescence comprising bracteolate monads on condensed axillary shoots, 2-3 flowers on each shoot, often appearing to arise directly from leaf axils"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "axillary")
d.c("inflorescence_unit", r, "bracteolate_monads")
d.n("flowers_per_inflorescence", r, 2, 3, "count")
r = "floral bracts and bracteoles shed in early stages of bud development"
d.c("bract_persistence", r, "shed_early_in_bud_development")
d.c("bracteole_persistence", r, "shed_early_in_bud_development")
d.n("flower_diameter", "Flowers 10-15 mm in diameter", 10, 15, "mm")
r = "pedicels 9-12 mm long, with bracteole scar approximately midway along pedicel"
d.n("pedicel_length", r, 9, 12, "mm")
d.c("bracteole_scar_position", r, "midway_along_pedicel")
r = "hypanthium glabrous, 1.5-2.0 mm long"
d.c("hypanthium_hairs", r, "glabrous")
d.n("flower_hypanthium_length", r, 1.5, 2.0, "mm")
r = "sepals obtuse, glabrous, oil dots conspicuous"
d.c("sepal_shape", r, "obtuse")
d.c("sepal_hairs", r, "glabrous")
d.c("sepal_oil_dots", r, "conspicuous")
r = "petals white, orbicular, 3-5 mm long"
d.c("flower_colour", r, "white")
d.c("flower_petal_shape", r, "orbicular")
d.n("flower_petal_length", r, 3, 5, "mm")
r = "stamens shorter than petals, 30-40 in a single whorl"
d.c("stamen_length_relative_to_petals", r, "shorter_than_petals")
d.n("flower_fertile_stamens_count", r, 30, 40, "count")
d.c("stamen_whorls", r, "single_whorl")
r = "anthers versatile, cells parallel, opening by narrow slits"
d.c("flower_anther_attachment", r, "versatile")
d.c("anther_cell_orientation", r, "parallel")
d.c("flower_anther_dehiscence", r, "narrow_slits")
r = "gland conspicuous, globular"
d.c("anther_gland_prominence", r, "conspicuous")
d.c("anther_gland_shape", r, "globular")
r = "style inset, stigma broad, capitate"
d.c("style_position", r, "inset")
d.c("stigma_shape", r, "broad capitate")
r = "roof of ovary glabrous, ovary 5-locular"
d.c("ovary_hairs", r, "glabrous", ent="roof of ovary")
d.m("ovary_locule_count", r, 5, "count", vt="mode")
r = "Fruits dry, conspicuously domed above hemispherical base, 5-locular, 4.5-5.0 x 5-6 mm, sepals persistent"
d.c("fruit_fleshiness", r, "dry")
d.c("fruit_dome", r, "conspicuously_domed")
d.c("fruit_base_shape", r, "hemispherical")
d.m("fruit_locule_count", r, 5, "count", vt="mode")
d.n("fruit_length", r, 4.5, 5.0, "mm")
d.n("fruit_width", r, 5, 6, "mm")
d.c("sepal_persistence", r, "persistent")
r = "Seeds and chaff identical, pale brown, linear, striate, 2.0-2.5 mm long, 0.2 mm wide"
d.c("seed_chaff_similarity", r, "identical")
d.c("seed_colour", r, "pale_brown")
d.c("seed_shape", r, "linear")
d.c("seed_surface_texture", r, "striate")
d.n("seed_length", r, 2.0, 2.5, "mm")
d.m("seed_width", r, 0.2, "mm")
d = T(NAME[1], "Distribution and habitat, p.645")
d.c("distribution_description", "restricted distribution in northern Queensland", "L. pallidum has a restricted distribution in northern Queensland, mostly in the Greenvale area, but also at Porcupine Gorge, north of Hughenden.")
r = "It occurs on lateritic ridges, often on cliff edges with skeletal soil. It sometimes grows near vine-thicket communities on rocky slopes."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("geology_terms", r, "lateritic")
d.c("habitat_terms", r, "ridges", q="usually")
d.c("habitat_terms", r, "cliff_edges", q="often")
d.c("soil_terms", r, "skeletal", q="often")
d.c("habitat_terms", r, "near_vine_thicket rocky_slopes", q="sometimes")
r = "Associated species include Eucalyptus exserta, E. similis, E. persistens, E. lamprophylla, Myrtella microphylla and Triodia sp."
d.c("associated_species", r, "Eucalyptus_exserta Eucalyptus_similis Eucalyptus_persistens Eucalyptus_lamprophylla Myrtella_microphylla Triodia_sp.")
T(NAME[1], "Flowering period, p.645").c("flowering_time", "Flowering period: March - June.", month(3, 6))
d = T(NAME[1], "Affinities, p.645")
d.m("fruit_valve_count", "larger 5-valved fruits", 5, "count", vt="mode")
d.c("seed_wings", "L. pallidum lacks ... the winged seeds", "absent")
d.c("habitat_terms", "ridgetop habitat", "ridgetop")
T(NAME[1], "Conservation status, p.645").c("conservation_status", "Suggested status is 3RC, as defined by Briggs and Leigh (1988).", "3RC")

# ------------------------------------------------------------------ 2. L. madidum (species)
d = T(MAD, "Description, p.645")
r = "Shrub or tree to 8 m high, ultimate branches pendulous"
d.c("plant_growth_form", r, "shrub tree")
d.n("plant_height", r, None, 8, "m")
d.c("stem_growth_habit", r, "pendulous", ent="ultimate branches")
r = "bark smooth and deciduous throughout, white, creamy or pink"
d.c("bark_description", r, norm(r))
d.c("bark_texture", r, "smooth")
d.c("bark_persistence", r, "deciduous_throughout")
d.c("bark_colour", r, "white creamy pink")
d.c("stem_hairs", "branchlets sparsely pubescent, glabrescent", "sparsely_pubescent glabrescent", ent="branchlets")
d.c("stem_flanges", "stem flanges absent", "absent")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
d.c("leaf_attachment", "sessile", "sessile")
d.c("leaf_discolority", "concolourous", "concolourous")
d.c("leaf_surface_colour", "pale green", "pale_green")
d.c("leaf_shape", "linear", "linear")
r = "22-70 x 1-9 mm"
d.n("leaf_length", r, 22, 70, "mm", note="Subsp. sativum is described as 20--45 mm long, below this species minimum.")
d.n("leaf_width", r, 1, 9, "mm")
d.c("leaf_apex_shape", "apex acute or acuminate", "acute acuminate")
r = "appressed indumentum present on young leaves, but older leaves generally glabrous"
d.c("leaf_hairs_immature_leaves", r, "appressed_indumentum")
d.c("leaf_hair_orientation", r, "appressed", ent="young leaves")
d.c("leaf_hairs_adult_leaves", r, "glabrous", q="generally")
r = "Inflorescence axillary, consisting of several bracteolate monads"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "axillary")
d.c("inflorescence_unit", r, "bracteolate_monads")
d.c("flowers_per_inflorescence_class", r, "several")
d.c("bract_persistence", "floral bracts shed before anthesis", "shed_before_anthesis")
d = T(MAD, "Description, p.646")
d.n("flower_diameter", "Flowers 5-7 mm in diameter", 5, 7, "mm")
d.m("pedicel_length", "pedicels 1 mm long", 1, "mm")
r = "hypanthium sparsely pubescent, 2-3 mm long"
d.c("hypanthium_hairs", r, "sparsely_pubescent")
d.n("flower_hypanthium_length", r, 2, 3, "mm")
r = "sepals obtuse, margins ciliate"
d.c("sepal_shape", r, "obtuse")
d.c("sepal_margin_hairs", r, "ciliate")
d.c("flower_colour", "petals white", "white")
r = "anthers versatile, cells parallel"
d.c("flower_anther_attachment", r, "versatile")
d.c("anther_cell_orientation", r, "parallel")
r = "style inset, stigma capitate"
d.c("style_position", r, "inset")
d.c("stigma_shape", r, "capitate")
r = "roof of ovary mostly glabrous but with hairs at base of style and along valve margins; ovary 3-locular"
d.c("ovary_hairs", r, "mostly_glabrous", ent="roof of ovary")
d.c("ovary_hair_position", r, "base_of_style valve_margins", ent="roof of ovary")
d.m("ovary_locule_count", r, 3, "count", vt="mode")
r = "Fruit thin-walled, glabrous, hemispherical, 3-locular, 2-3 x 2.5-5.0 mm, sepals persistent"
d.c("fruit_texture", r, "thin-walled")
d.c("fruit_surface_hairs", r, "glabrous")
d.c("fruit_shape", r, "hemispherical")
d.m("fruit_locule_count", r, 3, "count", vt="mode")
d.n("fruit_length", r, 2, 3, "mm")
d.n("fruit_width", r, 2.5, 5.0, "mm", note="Subsp. sativum is described as 2.0--3.5 mm wide, below this species minimum.")
d.c("sepal_persistence", r, "persistent")
r = "Seeds light brown, obovoid to elliptical, reticulate, c. 0.75 x 0.3 mm; unfertilised seeds linear to narrowly cuneate"
d.c("seed_colour", r, "light_brown")
d.c("seed_shape", r, "obovoid elliptical")
d.c("seed_surface_texture", r, "reticulate")
d.m("seed_length", r, 0.75, "mm")
d.m("seed_width", r, 0.3, "mm")
d.c("unfertilised_seed_shape", r, "linear narrowly_cuneate")
T(MAD, "Flowering period, p.646").c("flowering_time", "Flowering period: July - October.", month(7, 10))
d = T(MAD, "Affinities, p.646")
r = "pale concolourous leaves which are glabrous or only sparsely pubescent, leaves not recurved when dried, the glabrous or sparsely pubescent floral hypanthium"
d.c("leaf_hairs_adult_leaves", r, "sparsely_pubescent", note="The description says older leaves generally glabrous; the Introduction says glabrous to sparsely pubescent.")
d.c("leaf_margin_posture", r, "not_recurved", ent="dried leaves", note="Not recurved read as flat; Introduction: margins not recurved.")
d.c("hypanthium_hairs", r, "glabrous", note="The description gives sparsely pubescent only.")
T(MAD, "Introduction, p.643").c("habitat_terms", "Thompson's concept of L. parviflorum includes all rheophytic Leptospermum species from New Guinea and far northern Australia", "rheophytic", rank="species_group")

# subspecies
d = T(MAD1, "Description, p.646")
r = "Leaves linear to narrow-lanceolate, 38-70 x 4.5-9.0 mm, pale green"
d.c("leaf_shape", r, "linear narrow-lanceolate")
d.n("leaf_length", r, 38, 70, "mm")
d.n("leaf_width", r, 4.5, 9.0, "mm")
d.c("leaf_surface_colour", r, "pale_green")
r = "Fruits glabrous, hemispherical, 2.5-3.0 x 3.5-5.0 mm"
d.c("fruit_surface_hairs", r, "glabrous")
d.c("fruit_shape", r, "hemispherical")
d.n("fruit_length", r, 2.5, 3.0, "mm")
d.n("fruit_width", r, 3.5, 5.0, "mm")
d = T(MAD1, "Distribution and habitat, p.646")
d.c("distribution_description", "confined to Cape York", "L. madidum subsp. madidum is confined to Cape York peninsula, from Bamaga to Cooktown.")
r = "It occurs on the banks of freshwater creeks and rivers, in sandy soils."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "banks_of_freshwater_creeks banks_of_rivers")
d.c("soil_terms", r, "sandy")
d = T(MAD2, "Description, p.646")
r = "Leaves linear, 20-45 x 1.0-4.5 mm, pale green"
d.c("leaf_shape", r, "linear")
d.n("leaf_length", r, 20, 45, "mm")
d.n("leaf_width", r, 1.0, 4.5, "mm")
d.c("leaf_surface_colour", r, "pale_green")
r = "Fruits glabrous, hemispherical, 2.0-2.5 x 2.0-3.5 mm"
d.c("fruit_surface_hairs", r, "glabrous")
d.c("fruit_shape", r, "hemispherical")
d.n("fruit_length", r, 2.0, 2.5, "mm")
d.n("fruit_width", r, 2.0, 3.5, "mm")
d = T(MAD2, "Distribution and habitat, p.646")
d.c("distribution_description", "eastern Kimberley and northern NT", "L. madidum subsp. sativum occurs in the eastern part of the Kimberley region of Western Australia and throughout the northernmost part of the Northern Territory.")
r = "It grows along riverbanks, and has often been recorded from sandstone gorges, but it is not confined to them."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "riverbanks", q="usually")
d.c("habitat_terms", r, "sandstone_gorges", q="often")
d.c("geology_terms", r, "sandstone", q="often")
d = T(MAD2, "Note, p.646")
r = "This taxon has been widely cultivated in northern Australia for several years. According to Brock (1988), it is fast growing and adapts to a wide range of well-drained soils."
d.c("cultivation_status", r, "widely_cultivated")
d.c("growth_rate", r, "fast_growing", pop="in cultivation", note="Cited from Brock (1988).")
d.c("soil_terms", r, "well-drained", pop="in cultivation", note="Cited from Brock (1988).")

# ------------------------------------------------------------------ 3. L. brachyandrum (annotated list only)
d = T(NAME[3], "Distribution and habitat, p.647")
d.c("distribution_description", "discontinuous distribution", "L. brachyandrum has a discontinuous distribution from northern New South Wales to northern Queensland.")
r = "generally along rivers and creeks. However, in several localities in North Queensland (examples cited above), it inhabits steep rocky slopes or even cliff-faces."
d.c("habitat_description", r, r + " The respective habitats, while seemingly very different, are alike in that they are protected from fire. Perhaps this is an important determinant of its distribution.")
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "rivers creeks", q="generally")
d.c("habitat_terms", r, "steep_rocky_slopes cliff-faces", pop="several localities in North Queensland")
d.c("habitat_terms", "The respective habitats, while seemingly very different, are alike in that they are protected from fire.", "protected_from_fire", note="The author adds 'Perhaps this is an important determinant of its distribution'; no fire response is stated.")
r = "The hillside plants are completely smooth-barked, while the creek-dwelling plants almost always have rough bark at their bases"
d.c("bark_texture", r, "completely_smooth", pop="hillside plants")
d.c("bark_texture", r, "rough", q="almost_always", pop="creek-dwelling plants", ent="stem base")

# ------------------------------------------------------------------ 4. L. parviflorum
d = T(NAME[4], "Description, p.647")
r = "Shrub or tree, 3-12 metres high"
d.c("plant_growth_form", r, "shrub tree")
d.n("plant_height", r, 3, 12, "m")
r = "Bark type not recorded by collectors but appearing smooth and deciduous on herbarium specimens"
d.c("bark_description", r, norm(r))
d.c("bark_texture", r, "smooth", note="Read from herbarium specimens, not recorded in the field.")
d.c("bark_persistence", r, "deciduous", note="Read from herbarium specimens, not recorded in the field.")
d.c("stem_hairs", "young branchlets pubescent", "pubescent", ent="young branchlets")
r = "stem flanges present, conspicuous"
d.c("stem_flanges", r, "present")
d.c("stem_flange_prominence", r, "conspicuous")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
d.c("leaf_attachment", "sessile", "sessile")
d.c("leaf_discolority", "discolourous", "discolourous")
d.c("leaf_shape", "linear", "linear")
r = "20-45 x 2.0-4.5 mm"
d.n("leaf_length", r, 20, 45, "mm")
d.n("leaf_width", r, 2.0, 4.5, "mm")
d.c("leaf_apex_shape", "apex acute to acuminate", "acute acuminate")
d.c("leaf_margin_posture", "margins recurved in dried material", "recurved", ent="dried leaves", note="Affinities: strongly recurved leaf margins on dried specimens.")
d.c("leaf_surface_colour", "'dark green' (fide Foreman LAE60470, Van Royen 4798) above", "dark_green", ent="adaxial surface")
r = "the lower surface much paler, densely silky-pubescent even on older leaves"
d.c("leaf_abaxial_colour", r, "much_paler_than_upper_surface")
d.c("leaf_abaxial_hairs", r, "densely_silky-pubescent")
d.c("leaf_hairs_adult_leaves", r, "densely_silky-pubescent", ent="abaxial surface")
r = "Inflorescence consisting of 3-4 axillary bracteolate monads"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "axillary")
d.c("inflorescence_unit", r, "bracteolate_monads")
d.n("flowers_per_inflorescence", r, 3, 4, "count")
r = "bracts and bracteoles brown, oblong, shed well before anthesis"
d.c("bract_colour", r, "brown")
d.c("bracteole_colour", r, "brown")
d.c("bract_shape", r, "oblong")
d.c("bracteole_shape", r, "oblong")
d.c("bract_persistence", r, "shed_well_before_anthesis")
d.c("bracteole_persistence", r, "shed_well_before_anthesis")
d.n("flower_diameter", "Flowers 5-7 mm in diameter", 5, 7, "mm")
d.n("pedicel_length", "pedicels 1.0-1.5 mm long", 1.0, 1.5, "mm")
r = "hypanthium densely pubescent, 2.0-2.5 mm long"
d.c("hypanthium_hairs", r, "densely_pubescent")
d.n("flower_hypanthium_length", r, 2.0, 2.5, "mm")
r = "sepals obtuse, pubescent"
d.c("sepal_shape", r, "obtuse")
d.c("sepal_hairs", r, "pubescent")
d.c("flower_petal_shape", "petals orbicular, colour unknown", "orbicular", note="Petal colour stated as unknown; flower_colour not scored.")
r = "stamens c. 30, with small parallel anther cells"
d.m("flower_fertile_stamens_count", r, 30, "count")
d.c("anther_cell_orientation", r, "parallel")
d.c("anther_cell_size", r, "small")
r = "style inset, stigma capitate"
d.c("style_position", r, "inset")
d.c("stigma_shape", r, "capitate")
r = "roof of ovary glabrous except for short erect hairs at base of style and along valve margins, ovary 3-locular"
d.c("ovary_hairs", r, "glabrous_except_for_short_erect_hairs_at_base_of_style_and_along_valve_margins", ent="roof of ovary")
d.c("ovary_hair_position", r, "base_of_style valve_margins", ent="roof of ovary")
d.c("ovary_hair_orientation", r, "erect", ent="roof of ovary")
d.m("ovary_locule_count", r, 3, "count", vt="mode")
r = "Fruit thin-walled, pubescent, hemispherical, 1.5-2.0 x 2.0-2.5 mm, with valves not extending above the rim of the hypanthium, 3-locular, sepals persistent"
d.c("fruit_texture", r, "thin-walled")
d.c("fruit_surface_hairs", r, "pubescent")
d.c("fruit_shape", r, "hemispherical")
d.n("fruit_length", r, 1.5, 2.0, "mm")
d.n("fruit_width", r, 2.0, 2.5, "mm")
d.c("fruit_valve_position", r, "not_extending_above_rim")
d.m("fruit_locule_count", r, 3, "count", vt="mode")
d.c("sepal_persistence", r, "persistent")
r = "Seeds and chaff identical, brown, cuneate, striate, c. 0.8 x 0.4 mm"
d.c("seed_chaff_similarity", r, "identical")
d.c("seed_colour", r, "brown")
d.c("seed_shape", r, "cuneate")
d.c("seed_surface_texture", r, "striate")
d.m("seed_length", r, 0.8, "mm")
d.m("seed_width", r, 0.4, "mm")
d = T(NAME[4], "Distribution and habitat, p.647")
d.c("distribution_description", "endemic to New Guinea", "L. parviflorum is endemic to the island of New Guinea, occurring both in Irian Jaya and in Papua New Guinea.")
r = "It grows on the banks of rivers and streams, mostly at altitudes of less than 50 metres."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "banks_of_rivers banks_of_streams")
d.n("elevation", r, None, 50, "m", q="mostly")
T(NAME[4], "Flowering period, p.647").c("flowering_time", "Flowering period: July - September.", month(7, 9))
T(NAME[4], "Affinities, p.647").c("leaf_abaxial_hair_orientation", "discolourous leaves with dense appressed hairs on leaf undersides", "appressed")
T(NAME[4], "Introduction, p.643").c("habitat_terms", "Thompson's concept of L. parviflorum includes all rheophytic Leptospermum species from New Guinea and far northern Australia", "rheophytic", rank="species_group")

# ------------------------------------------------------------------ 5. L. purpurascens
d = T(NAME[5], "Distribution and habitat, p.648")
d.c("distribution_description", "Iron Range", "Endemic to a small area on Cape York peninsula in far northern Queensland, in the vicinity of Iron Range.")
r = "It grows on rocky granitic hillsides."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "rocky_hillsides")
d.c("geology_terms", r, "granitic")
d = T(NAME[5], "Affinities of L. parviflorum, p.647")
r = "These species share the following characters; stem flanges, discolourous leaves with dense appressed hairs on leaf undersides, strongly recurved leaf margins on dried specimens and densely pubescent floral hypanthia."
d.c("stem_flanges", r, "present")
d.c("leaf_discolority", r, "discolourous")
d.c("leaf_abaxial_hairs", r, "dense")
d.c("leaf_abaxial_hair_orientation", r, "appressed")
d.c("leaf_margin_posture", r, "strongly_recurved", ent="dried leaves")
d.c("hypanthium_hairs", r, "densely_pubescent")

# ------------------------------------------------------------------ 6. L. luehmannii
d = T(NAME[6], "Distribution and habitat, p.648")
r = "Endemic to skeletal slopes of trachyte hills and mountains between Elimbah and Beerwah, north of Brisbane."
d.c("distribution_description", r, r)
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "skeletal")
d.c("geology_terms", r, "trachyte")
d.c("habitat_terms", r, "slopes hills mountains")

# ------------------------------------------------------------------ 7. L. speciosum
d = T(NAME[7], "Distribution and habitat, p.648")
r = "Confined to coastal areas from Fraser Island in Queensland to Iluka in northern New South Wales, growing in swamps or heathlands."
d.c("distribution_description", r, r)
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "semiaquatic terrestrial", note="Swamps = semiaquatic; heathlands = terrestrial.")
d.c("habitat_terms", r, "coastal swamps heathlands")

# ------------------------------------------------------------------ 8. L. whitei
d = T(NAME[8], "Distribution and habitat, p.648")
d.c("distribution_description", "Rainbow Beach to Coffs Harbour", "Confined to coastal areas from Rainbow Beach in Queensland to Coffs Harbour in New South Wales.")
r = "It grows in swampy Banksia or Eucalyptus forests."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "semiaquatic")
d.c("habitat_terms", r, "coastal swampy_Banksia_forests swampy_Eucalyptus_forests")

# ------------------------------------------------------------------ 9. L. trinervium
d = T(NAME[9], "Distribution and habitat, p.648")
d.c("distribution_description", "Rockhampton to East Gippsland", "This widespread species extends from Rockhampton, Queensland to the East Gippsland district of Victoria.")
r = "It grows commonly in dry sclerophyll forest in sandy soils, and also in heathland."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "dry_sclerophyll_forest heathland")
d.c("soil_terms", r, "sandy")
d = T(NAME[9], "Introduction, p.644")
r = "L. lamellatum is placed next to L. trinervium on the basis of its brown, papery bark and seeds with small lateral wings"
d.c("bark_colour", r, "brown")
d.c("bark_texture", r, "papery")
d = T(NAME[9], "Seed morphology, p.644")
r = "seeds of L. trinervium have rows of extended cells forming small lateral wings"
d.c("seed_wings", r, "small_lateral_wings")
d.c("seed_shape", r, "winged")

# ------------------------------------------------------------------ 10. L. lamellatum
d = T(NAME[10], "Distribution and habitat, p.648")
d.c("distribution_description", "White Mountains to Milmerran", "Endemic to Queensland, extending from the White Mountains N.P., west of Townsville to the Milmerran area in the south of the state.")
r = "It is found on ridges in shallow sandy soils, usually derived from sandstone."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "ridges")
d.c("soil_terms", r, "shallow sandy")
d.c("geology_terms", r, "sandstone", q="usually")
d = T(NAME[10], "Introduction, p.644")
r = "L. lamellatum is placed next to L. trinervium on the basis of its brown, papery bark and seeds with small lateral wings"
d.c("bark_colour", r, "brown")
d.c("bark_texture", r, "papery")
d.c("seed_wings", r, "small_lateral_wings")
d.c("seed_shape", r, "winged")

# ------------------------------------------------------------------ 11. L. microcarpum
d = T(NAME[11], "Distribution and habitat, p.649")
d.c("distribution_description", "Kilkivan to Grafton", "Occurs in coastal areas from Kilkivan in southern Queensland to Grafton in New South Wales, and also extends west to beyond Warwick.")
r = "It inhabits shallow soils on rocky hills and mountainsides."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "shallow")
d.c("habitat_terms", r, "rocky_hills mountainsides")
T(NAME[11], "Seed morphology, p.644").c("seed_colour", "L. microcarpum seeds are quite black", "quite_black")

# ------------------------------------------------------------------ 12. L. brevipes
d = T(NAME[12], "Distribution and habitat, p.649")
d.c("distribution_description", "Warwick-Inglewood to NE Victoria", "Extends from the Warwick-Inglewood districts of Queensland, throughout New South Wales, to north-eastern Victoria.")
r = "It grows in poor forests on rock outcrops and rocky hillsides, especially on granite."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial", note="On rock outcrops; read as terrestrial, not lithophyte.")
d.c("habitat_terms", r, "poor_forests rock_outcrops rocky_hillsides")
d.c("geology_terms", r, "granite", q="especially")

# ------------------------------------------------------------------ 13. L. neglectum
d = T(NAME[13], "Distribution and habitat, p.649")
d.c("distribution_description", "Mt Mulligan to Tiaro", "Endemic to Queensland and extends from Mt Mulligan in the north to Tiaro in the south.")
r = "It grows both in eucalypt forest and on rocky hillsides with little soil development."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "eucalypt_forest rocky_hillsides")
d.c("soil_terms", r, "little_soil_development")

# ------------------------------------------------------------------ 14. L. sericatum
d = T(NAME[14], "Distribution and habitat, p.649")
d.c("distribution_description", "Leichhardt district", "Confined to sandstone habitats in the Leichhardt district of Queensland, including Carnarvon Gorge, Blackdown Tableland and Isla Gorge.")
r = "It grows on sparsely vegetated sandstone slopes, often rooting into crevices in the rocks."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial", note="Rooting into rock crevices; read as terrestrial, not lithophyte.")
d.c("habitat_terms", r, "sparsely_vegetated_slopes", q="usually")
d.c("habitat_terms", r, "rock_crevices", q="often")
d.c("geology_terms", r, "sandstone")
d = T(NAME[14], "Stem flanges, p.644")
r = "In L. sericatum, the flanges are present but poorly developed."
d.c("stem_flanges", r, "present")
d.c("stem_flange_prominence", r, "poorly_developed")

# ------------------------------------------------------------------ 15. L. parvifolium
d = T(NAME[15], "Distribution and habitat, p.649")
d.c("distribution_description", "Inglewood; NSW to Nowra", "The location given above is the only one known for this species in Queensland. However, it is widespread in New South Wales to as far south as the Nowra district.")
r = "It grows in poor sandy soils on hillsides in dry sclerophyll forest."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "poor sandy")
d.c("habitat_terms", r, "hillsides dry_sclerophyll_forest")

# ------------------------------------------------------------------ 16. L. venustum
d = T(NAME[16], "Description, p.649")
r = "Spreading shrub, 1.5-2.5 m high, with arching branches"
d.c("plant_growth_form", r, "shrub")
d.c("stem_growth_habit", r, "spreading")
d.n("plant_height", r, 1.5, 2.5, "m")
d.c("branch_orientation", r, "arching")
r = "bark rough, grey, scaly, closely adhering, persistent throughout"
d.c("bark_description", r, norm(r))
d.c("bark_texture", r, "rough scaly")
d.c("bark_colour", r, "grey")
d.c("bark_adherence", r, "closely_adhering")
d.c("bark_persistence", r, "persistent_throughout")
r = "branchlets with spreading hairs up to 2 mm long"
d.c("stem_hairs", r, "hairy", ent="branchlets")
d.c("branchlet_hair_orientation", r, "spreading")
d.n("branchlet_hair_length", r, None, 2, "mm")
d.c("stem_flanges", "stem flanges absent", "absent")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
r = "sessile or with petiole up to 1 mm long"
d.c("leaf_attachment", r, "sessile petiolate")
d.n("petiole_length", r, None, 1, "mm")
d.c("leaf_discolority", "concolourous", "concolourous")
d.c("leaf_surface_colour", "green", "green")
d.c("leaf_shape", "broadly elliptical", "broadly_elliptical")
r = "6-13 x 3-4 mm"
d.n("leaf_length", r, 6, 13, "mm")
d.n("leaf_width", r, 3, 4, "mm")
d.n("leaf_main_vein_count", "3-5-veined", 3, 5, "count")
r = "oil glands numerous, conspicuous"
d.c("leaf_oil_gland_abundance", r, "numerous")
d.c("leaf_oil_gland_prominence", r, "conspicuous")
d.c("leaf_base_shape", "leaf base cuneate", "cuneate")
d.c("leaf_apex_shape", "apex acute", "acute")
r = "young leaves with long marginal hairs, glabrescent"
d.c("leaf_hairs_immature_leaves", r, "long_marginal_hairs")
d.c("leaf_margin_hairs", r, "long", ent="young leaves")
d.c("leaf_hairs_adult_leaves", r, "glabrescent", note="Young leaves described as glabrescent, read as adult leaves glabrous.")
r = "Inflorescence consisting of single flowers, borne on short side-branches"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "solitary")
d.m("flowers_per_inflorescence", r, 1, "count", vt="mode")
d.c("inflorescence_position", r, "short_side-branches")
r = "floral bracts red-brown, glabrous, completely enclosing mature buds, shed just prior to anthesis"
d.c("bract_colour", r, "red-brown")
d.c("bract_hairs", r, "glabrous")
d.c("bract_bud_coverage", r, "completely_enclosing_mature_buds")
d.c("bract_persistence", r, "shed_just_prior_to_anthesis")
d.n("flower_diameter", "Flowers 18-25 mm in diameter", 18, 25, "mm")
d.c("pedicel_presence", "pedicels absent or very short", "absent very_short")
r = "hypanthium silky pubescent, 3-4 mm long"
d.c("hypanthium_hairs", r, "silky_pubescent")
d.n("flower_hypanthium_length", r, 3, 4, "mm")
r = "sepals triangular, pubescent"
d.c("sepal_shape", r, "triangular")
d.c("sepal_hairs", r, "pubescent")
r = "petals deep pink fading to pale pink, orbicular, glabrous"
d.c("flower_colour", r, "deep_pink pale_pink")
d.c("flower_petal_shape", r, "orbicular")
d.c("petal_hairs", r, "glabrous")
r = "stamens 30-40, all about the same length"
d.n("flower_fertile_stamens_count", r, 30, 40, "count")
d.c("stamen_length_uniformity", r, "all_about_the_same_length")
r = "anthers versatile, cells about 0.75 mm long, parallel, opening by narrow slits"
d.c("flower_anther_attachment", r, "versatile")
d.m("anther_cell_length", r, 0.75, "mm")
d.c("anther_cell_orientation", r, "parallel")
d.c("flower_anther_dehiscence", r, "narrow_slits")
r = "gland conspicuous, globular, dark-brown"
d.c("anther_gland_prominence", r, "conspicuous")
d.c("anther_gland_shape", r, "globular")
d.c("anther_gland_colour", r, "dark-brown")
d = T(NAME[16], "Description, p.650")
r = "style slightly inset, stigma capitate"
d.c("style_position", r, "slightly_inset")
d.c("stigma_shape", r, "capitate")
r = "roof of ovary tomentose, ovary 5(6)-locular"
d.c("ovary_hairs", r, "tomentose", ent="roof of ovary")
d.m("ovary_locule_count", r, 5, "count", vt="mode", q="usually")
d.m("ovary_locule_count", r, 6, "count", vt="maximum", q="extreme_range")
r = "Fruits fleshy or succulent when fresh, globular-truncate, 5(6)-locular, 6-7 x 7-8 mm, when dried brown and wrinkled, c. 5 x 6 mm; sepals persistent"
d.c("fruit_fleshiness", r, "fleshy succulent")
d.c("fruit_shape", r, "globular-truncate")
d.m("fruit_locule_count", r, 5, "count", vt="mode", q="usually")
d.m("fruit_locule_count", r, 6, "count", vt="maximum", q="extreme_range")
d.n("fruit_length", r, 6, 7, "mm", ent="fresh fruit")
d.n("fruit_width", r, 7, 8, "mm", ent="fresh fruit")
d.c("fruit_colour", r, "brown", ent="dried fruit")
d.c("fruit_surface", r, "wrinkled", ent="dried fruit")
d.m("fruit_length", r, 5, "mm", ent="dried fruit")
d.m("fruit_width", r, 6, "mm", ent="dried fruit")
d.c("sepal_persistence", r, "persistent")
r = "Seeds dark brown, obovoid, conspicuously reticulate, c. 1.2 x 0.6 mm. Unfertilised seeds pale yellow, linear."
d.c("seed_colour", r, "dark_brown")
d.c("seed_shape", r, "obovoid")
d.c("seed_surface_texture", r, "conspicuously_reticulate")
d.m("seed_length", r, 1.2, "mm")
d.m("seed_width", r, 0.6, "mm")
d.c("unfertilised_seed_colour", r, "pale_yellow")
d.c("unfertilised_seed_shape", r, "linear")
d = T(NAME[16], "Distribution and habitat, p.650")
d.c("distribution_description", "west of Eidsvold", "L. venustum is confined to a relatively small area west of Eidsvold.")
r = "where it grows on granitic hillsides and slopes, or beside small watercourses. It grows in eucalypt woodland often dominated by Eucalyptus petalophylla or E. baileyana. On moister sites, Lophostemon suaveolens may be present."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("geology_terms", r, "granitic")
d.c("habitat_terms", r, "hillsides slopes small_watercourses eucalypt_woodland")
d.c("associated_species", r, "Eucalyptus_petalophylla Eucalyptus_baileyana", q="often")
d.c("associated_species", r, "Lophostemon_suaveolens", pop="moister sites")
T(NAME[16], "Flowering period, p.650").c("flowering_time", "Flowering period: July - October.", month(7, 10))
T(NAME[16], "Conservation status, p.650").c("conservation_status", "The suggested status is 2R, as defined by Briggs and Leigh (1988).", "2R")
T(NAME[16], "Affinities, p.650").c("stem_hair_persistence", "persistently hairy branchlets", "persistent", ent="branchlets")

# ------------------------------------------------------------------ 17. L. semibaccatum
d = T(NAME[17], "Distribution and habitat, p.650")
d.c("distribution_description", "Deepwater N.P. to Forster", "Confined to coastal areas from Deepwater N.P. to Forster in New South Wales.")
r = "It is a common component of sandy coastal heathlands."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "sandy")
d.c("habitat_terms", r, "coastal_heathlands")
d = T(NAME[17], "Affinities of L. venustum, p.650")
r = "both species have rather fleshy fruits and silky-hairy hypanthia"
d.c("fruit_fleshiness", r, "fleshy")
d.c("hypanthium_hairs", r, "silky-hairy")
d = T(NAME[17], "Seed morphology, p.644")
r = "Seeds of L. venustum and L. semibaccatum are dark brown, conspicuously reticulate, and only twice as long as wide."
d.c("seed_colour", r, "dark_brown")
d.c("seed_surface_texture", r, "conspicuously_reticulate")
d.m("seed_length_to_width_ratio", r, 2, "ratio")

# ------------------------------------------------------------------ 18. L. arachnoides
d = T(NAME[18], "Distribution and habitat, p.650")
d.c("distribution_description", "Stanthorpe; NSW", "It has a very restricted distribution in Queensland (in the Stanthorpe area), but is widespread in New South Wales.")
r = "It grows in poorly drained heathland or adjacent eucalypt forests."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "poorly_drained_heathland eucalypt_forests")

# ------------------------------------------------------------------ 19. L. liversidgei
d = T(NAME[19], "Distribution and habitat, p.650")
d.c("distribution_description", "Bundaberg to Port Stephens", "Grows only in coastal areas from Bundaberg, Queensland to Port Stephens in New South Wales.")
r = "It inhabits sandy or peaty soil, in swampy heathlands."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "semiaquatic")
d.c("soil_terms", r, "sandy peaty")
d.c("habitat_terms", r, "coastal swampy_heathlands")
d = T(NAME[19], "Introduction, p.644")
r = "it lacks stem flanges, its sepals possess very few hairs, and the bark is grey and scaly"
d.c("stem_flanges", r, "absent")
d.c("sepal_hairs", r, "very_few_hairs")
d.c("bark_colour", r, "grey")
d.c("bark_texture", r, "scaly")

# ------------------------------------------------------------------ 20. L. oreophilum
d = T(NAME[20], "Distribution and habitat, p.651")
d.c("distribution_description", "Mt Coolum and Glasshouse Mountains", "This is a rare species confined to Mt Coolum and to several peaks of the Glasshouse Mountains.")
d.c("conservation_status", "This is a rare species", "rare")
r = "It grows on skeletal slopes on these extinct volcanic peaks, in montane heath communities."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "skeletal")
d.c("geology_terms", r, "volcanic")
d.c("habitat_terms", r, "slopes montane_heath")

# ------------------------------------------------------------------ 21. L. polygalifolium
d = T(NAME[21], "Distribution and habitat, p.651")
d.c("distribution_description", "Cape Flattery to south of Sydney", "Extends from Cape Flattery on Cape York peninsula in Queensland to south of Sydney in New South Wales, and up to 500 km inland.")
r = "It grows in a diversity of habitats, including heathlands, rocky hillsides and in dense eucalypt forest."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "heathlands rocky_hillsides dense_eucalypt_forest")
d = T(NAME[21], "Note under L. variabile, p.651")
r = "the scaly, tightly adhering bark of L. polygalifolium"
d.c("bark_texture", r, "scaly")
d.c("bark_adherence", r, "tightly_adhering")
d = T(NAME[21], "Seed morphology, p.644")
r = "Fertile and infertile seeds are identical in appearance, being linear and striate. ... In this group are all the 'woody-fruited' species e.g. L. polygalifolium, and also L. pallidum."
d.c("seed_chaff_similarity", r, "identical")
d.c("seed_shape", r, "linear")
d.c("seed_surface_texture", r, "striate")
d.c("fruit_texture", r, "woody")

# ------------------------------------------------------------------ 22. L. variabile
d = T(NAME[22], "Distribution and habitat, p.651")
d.c("distribution_description", "S Qld and N NSW mountains", "Occurs on mountains of southern Queensland near the border with New South Wales, and into northern New South Wales.")
r = "It grows on skeletal hillsides of volcanically-derived mountains, in heathland or low woodland."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "skeletal")
d.c("geology_terms", r, "volcanically-derived")
d.c("habitat_terms", r, "hillsides mountains heathland low_woodland")
d = T(NAME[22], "Note, p.651")
r = "My examination of L. variabile in the field revealed that it has quite flaky, loosely adhering bark"
d.c("bark_texture", r, "flaky", note="Key couplet 29 and the multi-access key give papery or fibrous, loosely adhering.")
d.c("bark_adherence", r, "loosely_adhering")

# ------------------------------------------------------------------ 23. L. novae-angliae
d = T(NAME[23], "Distribution and habitat, p.651")
d.c("distribution_description", "Girraween to west of Coffs Harbour", "In Queensland, it is known only from the Girraween N.P. near Stanthorpe. The species extends to west of Coffs Harbour in New South Wales.")
r = "where it grows in shrubland on exposed granitic slopes. The species extends to west of Coffs Harbour in New South Wales, in similar situations."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "shrubland exposed_slopes")
d.c("geology_terms", r, "granitic")

# ------------------------------------------------------------------ 24. L. minutifolium
d = T(NAME[24], "Distribution and habitat, p.651")
d.c("distribution_description", "Girraween to Armidale", "In Queensland, known only from Girraween N.P. and near Christie Target; in New South Wales it extends south to about Armidale.")
r = "It grows in eucalypt forest, often near watercourses."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "eucalypt_forest", q="usually")
d.c("habitat_terms", r, "near_watercourses", q="often")

# ------------------------------------------------------------------ 25. L. juniperinum
d = T(NAME[25], "Distribution and habitat, p.651-652")
d.c("distribution_description", "Fraser Island to Ulladulla", "Extends from Fraser Island in Queensland to Ulladulla in New South Wales.")
r = "In Queensland it is confined to coastal areas in Melaleuca forests, heathlands or sedgelands, but in New South Wales it reportedly also grows on sandstone escarpments (Thompson 1989)."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "coastal Melaleuca_forests heathlands sedgelands", pop="Queensland")
d.c("habitat_terms", r, "sandstone_escarpments", pop="New South Wales", note="Reported, citing Thompson (1989).")
d.c("geology_terms", r, "sandstone", pop="New South Wales", note="Reported, citing Thompson (1989).")

# ------------------------------------------------------------------ 26. L. recurvum
d = T(NAME[26], "Distribution and habitat, p.652")
d.c("distribution_description", "Kinabalu and Sulawesi", "L. recurvum occurs on the upper slopes of Mt Kinabalu in Borneo and on the highest mountains of Sulawesi (Celebes).")
r = "in shallow soils, in dense shrubland or low forest"
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "shallow")
d.c("habitat_terms", r, "upper_slopes dense_shrubland low_forest")
T(NAME[26], "Type, p.652").n("elevation", "Kina Balu, abundant, from 7000-8500 feet, whitening the top of the mountain", 7000, 8500, "ft", pop="Mt Kinabalu")
d = T(NAME[26], "Note, p.652")
r = "The leaves of the Sulawesi specimens are not as strongly recurved, and are somewhat thinner, but are otherwise typical."
d.c("leaf_orientation", r, "not_as_strongly_recurved", pop="Sulawesi")
d.c("leaf_thickness_categorical", r, "somewhat_thinner", pop="Sulawesi")

# ------------------------------------------------------------------ 27. L. javanicum
d = T(NAME[27], "Description, p.652")
r = "Tree to 6 metres high"
d.c("plant_growth_form", r, "tree")
d.n("plant_height", r, None, 6, "m")
d.c("bark_description", "bark fibrous", "bark fibrous")
d.c("bark_texture", "bark fibrous", "fibrous")
d.c("stem_hairs", "branchlets pubescent", "pubescent", ent="branchlets")
r = "stem flanges prominent, expanded and extending beyond leaf-base"
d.c("stem_flanges", r, "present")
d.c("stem_flange_prominence", r, "prominent")
d.c("stem_flange_extent", r, "expanded extending_beyond_leaf-base")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
d.c("leaf_attachment", "sessile", "sessile")
d.c("leaf_discolority", "strongly discolourous", "strongly_discolourous")
d.c("leaf_surface_colour", "dark green above", "dark_green", ent="adaxial surface")
d.c("leaf_shape", "elliptical to obovate", "elliptical obovate")
r = "10-30 x 4-9 mm"
d.n("leaf_length", r, 10, 30, "mm")
d.n("leaf_width", r, 4, 9, "mm")
d.c("leaf_midrib_adaxial_relief", "midrib impressed above", "impressed")
d.c("leaf_apex_shape", "apex obtuse", "obtuse")
r = "young leaves silky pubescent below, especially along margins and midrib, old leaves glabrous"
d.c("leaf_hairs_immature_leaves", r, "silky_pubescent", ent="abaxial surface")
d.c("leaf_abaxial_hairs", r, "silky_pubescent", ent="young leaves")
d.c("leaf_hairs_adult_leaves", r, "glabrous")
r = "Inflorescence consisting of single flowers borne on short side branches"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "solitary")
d.m("flowers_per_inflorescence", r, 1, "count", vt="mode")
d.c("inflorescence_position", r, "short_side-branches")
r = "floral bracts and bracteoles persistent, present around mature buds and shed just prior to anthesis"
d.c("bract_persistence", r, "shed_just_prior_to_anthesis")
d.c("bracteole_persistence", r, "shed_just_prior_to_anthesis")
d.n("flower_diameter", "Flowers 15-20 mm in diameter", 15, 20, "mm")
d.n("pedicel_length", "pedicels 0-1 mm long", 0, 1, "mm")
d.c("hypanthium_hairs", "hypanthium silky-pubescent", "silky_pubescent")
r = "sepals obtuse, margins densely ciliate"
d.c("sepal_shape", r, "obtuse")
d.c("sepal_margin_hairs", r, "densely_ciliate")
d.c("flower_colour", "petals white", "white")
r = "Fruits woody, conspicuously domed above bowl-shaped base, 5-locular, 4-5 x 6-7 mm, sepals not persisting"
d.c("fruit_texture", r, "woody")
d.c("fruit_dome", r, "conspicuously_domed")
d.c("fruit_base_shape", r, "bowl-shaped")
d.m("fruit_locule_count", r, 5, "count", vt="mode")
d.n("fruit_length", r, 4, 5, "mm")
d.n("fruit_width", r, 6, 7, "mm")
d.c("sepal_persistence", r, "not_persisting")
d = T(NAME[27], "Distribution and habitat, p.652-653")
d.c("distribution_description", "Burma to western Malesia", "L. javanicum extends from Burma to western Malesia, including Sumatra, Malaya, Java, Borneo, Philippines and Sulawesi. It is absent from the Lesser Sunda Islands and the Moluccas.")
r = "It grows at altitudes of between 1500 and 3000 metres, according to herbarium specimen label data."
d.c("habitat_description", r, r)
d.n("elevation", r, 1500, 3000, "m")
d = T(NAME[27], "Affinities, p.653")
r = "Some specimens of L. javanicum from Sulawesi ... have obovate leaves about 14 mm long, only 1.5-2.0 times longer than broad, and with dense golden hairs on the undersides."
sul = "Sulawesi (some specimens)"
d.c("leaf_shape", r, "obovate", pop=sul)
d.m("leaf_length", r, 14, "mm", pop=sul)
d.n("leaf_length_to_width_ratio", r, 1.5, 2.0, "ratio", pop=sul)
d.c("leaf_abaxial_hairs", r, "dense", pop=sul)
d.c("leaf_abaxial_hair_colour", r, "golden", pop=sul)
T(NAME[27], "Stem flanges, p.644").c("stem_shape", "Stem flanges had been noted previously, as 'angular' or 'triangular' twigs for L. javanicum (Corner 1940; Backer & Bakhuizen Van Den Brink 1963; van Steenis 1972)", "angular triangular", ent="twigs", note="Cited from the works named.")

# ------------------------------------------------------------------ 28. L. wooroonooran
d = T(NAME[28], "Distribution and habitat, p.653")
d.c("distribution_description", "Mossman and Bellenden Ker", "This species is confined to two small disjunct areas of northern Queensland. One is the Mossman Gorge-Devils Thumb-Main Coast Range area west of Mossman, and the other is along the Bellenden Ker range south of Gordonvale.")
r = "It inhabits high-altitude microphyll 'cloud' forests."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "high-altitude_microphyll_cloud_forests")
T(NAME[28], "Stem flanges, p.644").c("stem_shape", "Stem flanges had been noted previously, as 'angular' or 'triangular' twigs for L. javanicum ... and for L. wooroonooran (Bailey 1900)", "angular triangular", ent="twigs", note="Cited from Bailey (1900).")

# ------------------------------------------------------------------ 29. L. amboinense
d = T(NAME[29], "Description, p.653")
r = "Tree to 9 m high"
d.c("plant_growth_form", r, "tree")
d.n("plant_height", r, None, 9, "m")
r = "bark rough, grey to brown, fibrous, longitudinally fissured, persistent throughout"
d.c("bark_description", r, norm(r))
d.c("bark_texture", r, "rough fibrous longitudinally_fissured")
d.c("bark_colour", r, "grey brown")
d.c("bark_persistence", r, "persistent_throughout")
d.c("stem_hairs", "branchlets glabrous", "glabrous", ent="branchlets")
r = "stem flanges prominent, broad, sometimes extending beyond leaf base"
d.c("stem_flanges", r, "present")
d.c("stem_flange_prominence", r, "prominent")
d.c("stem_flange_extent", r, "broad", q="usually")
d.c("stem_flange_extent", r, "extending_beyond_leaf-base", q="sometimes")
d.c("leaf_phyllotaxis", "Leaves alternate", "alternate")
d.c("leaf_attachment", "sessile", "sessile")
d.c("leaf_discolority", "slightly discolourous or concolourous", "slightly_discolourous concolourous")
d.c("leaf_surface_colour", "pale to mid-green", "pale_green mid-green")
d.c("leaf_shape", "narrowly elliptical", "narrowly_elliptical", note="The Introduction says narrower lanceolate leaves.")
r = "18-30 x 3-5 mm"
d.n("leaf_length", r, 18, 30, "mm")
d.n("leaf_width", r, 3, 5, "mm")
r = "midrib scarcely visible, not impressed above"
d.c("leaf_midrib_visibility", r, "scarcely_visible")
d.c("leaf_midrib_adaxial_relief", r, "not_impressed")
d.c("leaf_apex_shape", "apex acute or obtuse", "acute obtuse")
r = "young leaves with silky appressed hairs on underside, glabrescent"
d.c("leaf_hairs_immature_leaves", r, "silky_appressed_hairs", ent="abaxial surface")
d.c("leaf_abaxial_hairs", r, "silky", ent="young leaves")
d.c("leaf_abaxial_hair_orientation", r, "appressed", ent="young leaves")
d.c("leaf_hairs_adult_leaves", r, "glabrescent", note="Young leaves described as glabrescent, read as adult leaves glabrous.")
r = "Inflorescence consisting of single (rarely up to 4) flowers borne on short side-branches"
d.c("inflorescence_description", r, norm(r))
d.c("inflorescence_type", r, "solitary", q="usually")
d.m("flowers_per_inflorescence", r, 1, "count", vt="mode", q="usually")
d.m("flowers_per_inflorescence", r, 4, "count", vt="maximum", q="rarely")
d.c("inflorescence_position", r, "short_side-branches")
r = "floral bracteoles and bracts shed well before anthesis"
d.c("bract_persistence", r, "shed_well_before_anthesis")
d.c("bracteole_persistence", r, "shed_well_before_anthesis")
d.n("flower_diameter", "Flowers 12-18 mm in diameter", 12, 18, "mm")
d.n("pedicel_length", "pedicels 0-1 mm long", 0, 1, "mm")
r = "hypanthium glabrous or occasionally pubescent, obconical to hemispherical"
d.c("hypanthium_hairs", r, "glabrous", q="usually")
d.c("hypanthium_hairs", r, "pubescent", q="occasionally")
d.c("hypanthium_shape", r, "obconical hemispherical")
r = "sepals obtuse, margins ciliate"
d.c("sepal_shape", r, "obtuse")
d.c("sepal_margin_hairs", r, "ciliate")
d.c("flower_colour", "petals white", "white")
r = "Fruits sessile, woody, conspicuously domed above an obconical to hemispherical hypanthium, (4)5-locular, 3-4 x 4.0-5.5 mm, sepals not persistent on fruit"
d.c("fruit_attachment", r, "sessile")
d.c("fruit_texture", r, "woody")
d.c("fruit_dome", r, "conspicuously_domed")
d.c("fruit_base_shape", r, "obconical hemispherical")
d.m("fruit_locule_count", r, 5, "count", vt="mode", q="usually")
d.m("fruit_locule_count", r, 4, "count", vt="minimum", q="extreme_range")
d.n("fruit_length", r, 3, 4, "mm")
d.n("fruit_width", r, 4.0, 5.5, "mm")
d.c("sepal_persistence", r, "not_persistent", note="The multi-access key lists this species under sepals persistent and not persistent.")
r = "Seeds and chaff identical, brown, linear, striate, c. 2.0 x 0.2 mm"
d.c("seed_chaff_similarity", r, "identical")
d.c("seed_colour", r, "brown")
d.c("seed_shape", r, "linear")
d.c("seed_surface_texture", r, "striate")
d.m("seed_length", r, 2.0, "mm")
d.m("seed_width", r, 0.2, "mm")
d = T(NAME[29], "Distribution and habitat, p.653-654")
d.c("distribution_description", "Malesia and coastal N Queensland", "L. amboinense is widespread in Malesia, occurring in Malaya, Sumatra, Borneo, Sulawesi, the Moluccas, Flores and the Philippines. The species also grows in coastal areas of northern Queensland, from Cooktown to Bowen. It is possibly absent from Java.")
r = "Its altitudinal range in Malesia is 50-2000 metres, according to herbarium specimen label data."
d.n("elevation", r, 50, 2000, "m", pop="Malesia")
r = "It inhabits shallow soils, often adjacent to wet sclerophyll forest or rainforest."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("soil_terms", r, "shallow")
d.c("habitat_terms", r, "adjacent_to_wet_sclerophyll_forest adjacent_to_rainforest", q="often")
d.c("habitat_terms", r.replace(r, "The species also grows in coastal areas of northern Queensland"), "coastal", pop="northern Queensland")
d = T(NAME[29], "Affinities, p.654")
d.c("fruit_dome_apex_position", "in both cases the domed apex is highest away from the style", "highest_away_from_style")
d = T(NAME[29], "Note, p.654")
r = "A form of L. amboinense from Big Tableland near Cooktown is unusual in that it has 2-4 flowers per inflorescence."
d.n("flowers_per_inflorescence", r, 2, 4, "count", pop="Big Tableland near Cooktown")

# ------------------------------------------------------------------ 30. L. petersonii
d = T(NAME[30], "Distribution and habitat, p.654")
d.c("distribution_description", "Mt Tinbeerwah to near Port Macquarie", "L. petersonii extends from Mt Tinbeerwah to near Port Macquarie in New South Wales.")
r = "It grows on rocky escarpments and watercourses, usually adjacent to wet sclerophyll forest."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "terrestrial")
d.c("habitat_terms", r, "rocky_escarpments watercourses", q="usually")
d.c("habitat_terms", r, "adjacent_to_wet_sclerophyll_forest", q="usually")
d.c("habitat_terms", "in Queensland is otherwise confined to skeletal rocky slopes", "skeletal_rocky_slopes", pop="Queensland")
d = T(NAME[30], "Affinities of L. amboinense, p.654")
r = "L. petersonii has broader fruits with a flat or shallow base when open, and leaves which are usually lemon-scented and have a minutely retuse apex."
d.c("fruit_base_shape", r, "flat shallow", ent="open fruit")
d.c("leaf_scent", r, "lemon-scented", q="usually")
d.c("leaf_apex_shape", r, "minutely_retuse")

# ------------------------------------------------------------------ 31. L. gregarium
d = T(NAME[31], "Distribution and habitat, p.654")
d.c("distribution_description", "S Qld and northern NSW tablelands", "Very rare in Queensland, known only from within a few kilometres of the New South Wales border. In the latter state, it extends throughout the northern tablelands and into parts of the western slopes.")
r = "It grows in high-altitude heathy swamps."
d.c("habitat_description", r, r)
d.c("plant_growth_substrate", r, "semiaquatic")
d.c("habitat_terms", r, "high-altitude_heathy_swamps")

# ------------------------------------------------------------------ Introduction: the L. petersonii subgroup
r = "the species in the L. petersonii subgroup have prominent stem flanges, their sepals possess hairs on the margins and apex, and the bark is brown and fibrous"
for i in (30, 29, 27):
    note = "" if i == 30 else "The Introduction places this species in the same subgroup as L. petersonii."
    d = T(NAME[i], "Introduction, p.644")
    d.c("sepal_hair_position", r, "margins apex", rank="species_group", note=note)
    if i == 30:
        d.c("stem_flanges", r, "present", rank="species_group")
        d.c("stem_flange_prominence", r, "prominent", rank="species_group")
        d.c("bark_colour", r, "brown", rank="species_group")
        d.c("bark_texture", r, "fibrous", rank="species_group")

# Seed morphology group 1 reaches the other species the key calls woody-fruited (couplet 23)
r = "Fertile and infertile seeds are identical in appearance, being linear and striate. ... In this group are all the 'woody-fruited' species"
for i in (31, 28, 20, 22):
    d = T(NAME[i], "Seed morphology, p.644")
    note = "Woody-fruited according to key couplet 23."
    d.c("seed_chaff_similarity", r, "identical", rank="species_group", note=note)
    d.c("seed_shape", r, "linear", rank="species_group", note=note)
    d.c("seed_surface_texture", r, "striate", rank="species_group", note=note)
T(NAME[26], "Introduction, p.643").c("fruit_texture", "Thompson has applied the name L. javanicum to all woody-fruited Malesian Leptospermum species except the Mt Kinabalu population of L. recurvum", "woody", rank="species_group", pop="Mt Kinabalu", note="Implied: the sentence counts this population among the woody-fruited species.")

# ------------------------------------------------------------------ Multi-access key, p.655
SEC_M = "Multi-access key, p.655"
MA = [
    ("Distribution (Queensland Pastoral Districts)", [
        ("Moreton or Wide Bay", [3, 6, 7, 8, 9, 11, 13, 17, 19, 20, 21, 22, 25, 30], {"distribution_queensland_pastoral_districts": "moreton_or_wide_bay"}),
        ("Darling Downs", [3, 9, 10, 11, 12, 15, 18, 21, 23, 24, 30, 31], {"distribution_queensland_pastoral_districts": "darling_downs"}),
        ("outside these Districts", [1, 2, 3, 4, 5, 9, 10, 13, 14, 16, 17, 21, 26, 27, 28, 29], {"distribution_queensland_pastoral_districts": "outside_these_districts"}),
    ]),
    ("Stem Flanges", [
        ("Present", [3, 4, 5, 14, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31], {"stem_flanges": "present"}),
        ("Absent", [1, 2, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19], {"stem_flanges": "absent"}),
    ]),
    ("Leaf length (largest leaves)", [
        ("<9 mm", [5, 11, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 26, 31], {"leaf_length_largest_leaves_class": "<9_mm"}),
        ("9-15 mm", [5, 9, 11, 12, 13, 16, 17, 20, 21, 22, 23, 25, 27, 28, 31], {"leaf_length_largest_leaves_class": "9--15_mm"}),
        (">15 mm", [1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 20, 22, 27, 28, 29, 30], {"leaf_length_largest_leaves_class": ">15_mm"}),
    ]),
    ("Bark Type", [
        ("smooth, deciduous", [2, 3, 4, 5, 6, 31], {"bark_texture": "smooth", "bark_persistence": "deciduous"}),
        ("papery/fibrous, loosely attached", [3, 7, 8, 9, 10, 11, 22, 31], {"bark_texture": "papery fibrous", "bark_adherence": "loosely_attached"}),
        ("fibrous/stringy, firmly attached", [1, 8, 14, 15, 20, 24, 26, 27, 28, 29, 30], {"bark_texture": "fibrous stringy", "bark_adherence": "firmly_attached"}),
        ("scaly, not fibrous, closely adhering", [12, 13, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25], {"bark_texture": "scaly", "bark_adherence": "closely_adhering"}),
    ]),
    ("Leaf apex", [
        ("obtuse or minutely retuse", [5, 6, 9, 12, 13, 14, 17, 19, 21, 24, 26, 27, 29, 30], {"leaf_apex_shape": "obtuse_or_minutely_retuse", "leaf_apex_prickliness": "not_prickly"}),
        ("acute, not prickly", [1, 2, 3, 4, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 20, 21, 22, 26, 27, 28, 29, 31], {"leaf_apex_shape": "acute", "leaf_apex_prickliness": "not_prickly"}),
        # Printed "1,18,23,25". 1 (L. pallidum) is already under "acute, not prickly" and its description has no prickly
        # wording; 11 (L. microcarpum) is under no leaf apex state at all and couplet 16 keys it as prickly. Read as 11.
        ("acute, prickly", [11, 18, 23, 25], {"leaf_apex_shape": "acute", "leaf_apex_prickliness": "prickly"}),
    ]),
    ("Floral hypanthium", [
        ("glabrous", [1, 2, 3, 6, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31], {"hypanthium_hairs": "glabrous"}),
        ("pubescent", [2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25, 27, 31], {"hypanthium_hairs": "pubescent"}),
    ]),
    ("Fruit diameter", [
        ("<5 mm", [2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 26, 28, 29], {"fruit_diameter_class": "<5_mm"}),
        (">5 mm", [1, 7, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31], {"fruit_diameter_class": ">5_mm"}),
    ]),
    ("Number of loculi in fruit", [
        ("Three", [2, 3, 4, 5, 6, 7, 8, 11, 17], {"fruit_locule_count": "3"}),
        ("Four", [9, 10, 11, 12, 13, 15, 17, 19, 29], {"fruit_locule_count": "4"}),
        ("Five or more", [1, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31], {"fruit_locule_count": "5_or_more"}),
    ]),
    ("Sepals persistent in fruit?", [
        ("Yes", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 29, 30], {"sepal_persistence": "persistent"}),
        ("No", [19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31], {"sepal_persistence": "not_persistent"}),
    ]),
]
MA_NOTES = {
    ("Leaf apex", 1): "Also printed under acute, prickly (1,18,23,25), read as a misprint for 11; see L. microcarpum.",
    ("Leaf apex", 11): "Printed as 1,18,23,25 under acute, prickly: read as 11, because 1 (L. pallidum) is already under acute, not prickly, 11 (L. microcarpum) is under no other state, and couplet 16 keys L. microcarpum as prickly.",
}
for char, states in MA:
    covered = set()
    for _, nums, _ in states:
        covered |= set(nums)
    missing = sorted(set(SP) - covered)
    if missing:
        print("multi-access gap:", char, missing)
    for i in SP:
        got = [(lab, tr) for lab, nums, tr in states if i in nums]
        if not got:
            continue
        raw = f"{char}: " + "; ".join(lab for lab, _ in got)
        merged = {}
        for _, tr in got:
            for trait, val in tr.items():
                merged.setdefault(trait, [])
                for tok in val.split(" "):
                    if tok not in merged[trait]:
                        merged[trait].append(tok)
        for trait, toks in merged.items():
            # one row per character, its states in text order (no state ranks above another)
            add(NAME[i], trait, raw, " ".join(toks), "mode", "", SEC_M, note=MA_NOTES.get((char, i), ""))

# ------------------------------------------------------------------ Dichotomous key, p.657-659
# couplet lead, text, next (couplet number or terminal taxon number/name), row specs.
# Spec: (trait, value) categorical; (trait, lo, hi, unit) numeric range. Numeric ranges and bounds of a lead covering several
# taxa go in *_class candidates: numbers are not copied from a group statement to its members.
M1, M2 = "madidum1", "madidum2"
KEY = {
    "1a": ("Largest leaves more than 18 mm long", 2, [("leaf_length_largest_leaves_class", ">18_mm")]),
    "1b": ("Largest leaves less than 18 mm long", 16, [("leaf_length_largest_leaves_class", "<18_mm")]),
    "2a": ("Stem flanges present, conspicuous", 3, [("stem_flanges", "present"), ("stem_flange_prominence", "conspicuous")]),
    "2b": ("Stem flanges absent or scarcely visible", 8, [("stem_flanges", "absent_or_scarcely_visible")]),
    "3a": ("Fruits <4 mm in diameter, 3-locular, not domed; 3-8 flowers per inflorescence; bark smooth, at least on branches", 4,
           [("fruit_diameter_class", "<4_mm"), ("fruit_locule_count", "3"), ("fruit_dome", "not_domed"), ("flowers_per_inflorescence_class", "3--8"), ("bark_texture", "smooth")]),
    "3b": ("Fruits >4 mm in diameter, (4)5-locular, domed; 1 flower per inflorescence; bark rough throughout", 5,
           [("fruit_diameter_class", ">4_mm"), ("fruit_locule_count", "(4)5"), ("fruit_dome", "domed"), ("flowers_per_inflorescence_class", "1"), ("bark_texture", "rough")]),
    "4a": ("Hypanthium glabrous; leaves concolourous; roof of ovary glabrous", 3,
           [("hypanthium_hairs", "glabrous"), ("leaf_discolority", "concolourous"), ("ovary_hairs", "glabrous", "roof of ovary")]),
    "4b": ("Hypanthium pubescent; leaves discolourous; roof of ovary hairy along valve margins", 4,
           [("hypanthium_hairs", "pubescent"), ("leaf_discolority", "discolourous"), ("ovary_hairs", "hairy", "roof of ovary"), ("ovary_hair_position", "valve_margins", "roof of ovary")]),
    "5a": ("Sepals glabrous at anthesis; fruits 7-10 mm in diameter", 22, [("sepal_hairs", "glabrous"), ("fruit_width", 7, 10, "mm")]),
    "5b": ("Margins of sepals hairy at anthesis; fruits 4-7 mm in diameter", 6, [("sepal_margin_hairs", "hairy"), ("fruit_diameter_class", "4--7_mm")]),
    "6a": ("Leaves 4-7 times longer than wide", 7, [("leaf_length_to_width_ratio_class", "4--7")]),
    "6b": ("Leaves 2-4 times longer than wide", 27, [("leaf_length_to_width_ratio_class", "2--4")]),
    "7a": ("Leaf apex obtuse or acute; fruits 4.0-5.5 mm in diameter, (4)5-locular, base of open fruit usually hemispherical", 29,
           [("leaf_apex_shape", "obtuse acute"), ("fruit_width", 4.0, 5.5, "mm"), ("fruit_locule_count", "(4)5"), ("fruit_base_shape", "hemispherical", "open fruit", "usually")]),
    "7b": ("Leaf apex minutely retuse; fruits 5-7 mm in diameter, 5-locular, base of open fruit bowl-shaped or almost flat", 30,
           [("leaf_apex_shape", "minutely_retuse"), ("fruit_width", 5, 7, "mm"), ("fruit_locule_count", "5"), ("fruit_base_shape", "bowl-shaped almost_flat", "open fruit")]),
    "8a": ("Pedicels >8 mm long", 1, [("pedicel_length_class", ">8_mm")]),
    "8b": ("Pedicels absent or up to 4 mm long", 9, [("pedicel_length_class", "absent_or_up_to_4_mm")]),
    "9a": ("Fruits 3-locular; inflorescence 3- or more-flowered", 10, [("fruit_locule_count", "3"), ("flowers_per_inflorescence_class", "3_or_more")]),
    "9b": ("Fruits 4-5-locular; inflorescence 1-2-flowered", 14, [("fruit_locule_count", "4 5"), ("flowers_per_inflorescence_class", "1--2")]),
    "10a": ("Flowers and fruits in sessile clusters; bark rough", 11, [("inflorescence_arrangement", "sessile_clusters"), ("bark_texture", "rough")]),
    "10b": ("Flowers and fruits in axillary racemes; bark smooth", 12, [("inflorescence_type", "axillary_raceme"), ("bark_texture", "smooth")]),
    "11a": ("Inflorescence 15-20-flowered; leaves 7-11 mm wide, base obtuse", 7, [("flowers_per_inflorescence", 15, 20, "count"), ("leaf_width", 7, 11, "mm"), ("leaf_base_shape", "obtuse")]),
    "11b": ("Inflorescence 4-6-flowered; leaves 4-5 mm wide, base cuneate", 8, [("flowers_per_inflorescence", 4, 6, "count"), ("leaf_width", 4, 5, "mm"), ("leaf_base_shape", "cuneate")]),
    "12a": ("Leaves dark green, apex obtuse; plants growing on rocky hillsides", 6, [("leaf_surface_colour", "dark_green"), ("leaf_apex_shape", "obtuse"), ("habitat_terms", "rocky_hillsides")]),
    "12b": ("Leaves pale green, apex acute; plants growing along watercourses", 13, [("leaf_surface_colour", "pale_green"), ("leaf_apex_shape", "acute"), ("habitat_terms", "watercourses")]),
    "13a": ("Leaves 1.0-4.5 mm wide; fruits 2.0-3.5 mm across", M2, [("leaf_width", 1.0, 4.5, "mm"), ("fruit_width", 2.0, 3.5, "mm")]),
    "13b": ("Leaves 4.5-9.0 mm wide; fruits 3.5-5.0 mm across", M1, [("leaf_width", 4.5, 9.0, "mm"), ("fruit_width", 3.5, 5.0, "mm")]),
    "14a": ("Bark grey, scaly, closely adhering; fruits with a shallow dome, stigma >2 times width of style", 13,
            [("bark_colour", "grey"), ("bark_texture", "scaly"), ("bark_adherence", "closely_adhering"), ("fruit_dome", "shallow_dome"), ("stigma_to_style_width_class", ">2_times")]),
    "14b": ("Bark brown, papery, loosely adhering; fruits without a dome, stigma <2 times width of style", 15,
            [("bark_colour", "brown"), ("bark_texture", "papery"), ("bark_adherence", "loosely_adhering"), ("fruit_dome", "without_a_dome"), ("stigma_to_style_width_class", "<2_times")]),
    "15a": ("Leaves 8-15 times longer than wide; pedicels 2-4 mm long", 10, [("leaf_length_to_width_ratio", 8, 15, "ratio"), ("pedicel_length", 2, 4, "mm")]),
    "15b": ("Leaves 4-6 times longer than wide; pedicels 1.0-1.5 mm long", 9, [("leaf_length_to_width_ratio", 4, 6, "ratio"), ("pedicel_length", 1.0, 1.5, "mm")]),
    "16a": ("Leaves prickly", 17, [("leaf_apex_prickliness", "prickly")]),
    "16b": ("Leaves not prickly", 20, [("leaf_apex_prickliness", "not_prickly")]),
    "17a": ("Stem flanges present", 18, [("stem_flanges", "present")]),
    "17b": ("Stem flanges absent", 19, [("stem_flanges", "absent")]),
    "18a": ("Leaves 2-3 mm wide; fruits 5.0-6.5 mm long", 23, [("leaf_width", 2, 3, "mm"), ("fruit_length", 5.0, 6.5, "mm")]),
    "18b": ("Leaves 0.8-1.5 mm wide; fruits 3-4 mm long", 25, [("leaf_width", 0.8, 1.5, "mm"), ("fruit_length", 3, 4, "mm")]),
    "19a": ("Old leaves hairy; fruits 6.5-8.0 mm in diameter", 18, [("leaf_hairs_adult_leaves", "hairy"), ("fruit_width", 6.5, 8.0, "mm")]),
    "19b": ("Old leaves glabrous; fruits 3-4 mm in diameter", 11, [("leaf_hairs_adult_leaves", "glabrous"), ("fruit_width", 3, 4, "mm")]),
    "20a": ("Stem flanges present, conspicuous", 21, [("stem_flanges", "present"), ("stem_flange_prominence", "conspicuous")]),
    "20b": ("Stem flanges absent or scarcely visible", 30, [("stem_flanges", "absent_or_scarcely_visible")]),
    "21a": ("Largest leaves <6 mm long", 22, [("leaf_length_largest_leaves_class", "<6_mm")]),
    "21b": ("Largest leaves >6 mm long", 23, [("leaf_length_largest_leaves_class", ">6_mm")]),
    "22a": ("Leaves strongly recurved; margins of sepals hairy", 26, [("leaf_orientation", "strongly_recurved"), ("sepal_margin_hairs", "hairy")]),
    "22b": ("Leaves not recurved; sepals glabrous throughout", 24, [("leaf_orientation", "not_recurved"), ("sepal_hairs", "glabrous")]),
    "23a": ("Fruit not woody or domed, sepals persisting", 24, [("fruit_texture", "not_woody"), ("fruit_dome", "not_domed"), ("sepal_persistence", "persistent")]),
    "23b": ("Fruit woody and domed, sepals not persisting", 25, [("fruit_texture", "woody"), ("fruit_dome", "domed"), ("sepal_persistence", "not_persistent")]),
    "24a": ("Bark smooth; fruit 3-locular; leaves 2.5-4.0 mm wide", 5, [("bark_texture", "smooth"), ("fruit_locule_count", "3"), ("leaf_width", 2.5, 4.0, "mm")]),
    "24b": ("Bark rough; fruit 5-locular; leaves 1.5-3.0 mm wide", 14, [("bark_texture", "rough"), ("fruit_locule_count", "5"), ("leaf_width", 1.5, 3.0, "mm")]),
    "25a": ("Bark +/- smooth; papery layers shed from hypanthium of fruit", 31, [("bark_texture", "+/-_smooth"), ("fruit_hypanthium_surface", "papery_layers_shed")]),
    "25b": ("Bark rough; hypanthium of fruit without papery layers", 26, [("bark_texture", "rough"), ("fruit_hypanthium_surface", "without_papery_layers")]),
    "26a": ("Margins of sepals hairy at anthesis; leaves mostly >4 mm wide", 27, [("sepal_margin_hairs", "hairy"), ("leaf_width_class", "mostly_>4_mm")]),
    "26b": ("Sepals glabrous at anthesis; leaves <4 mm wide", 28, [("sepal_hairs", "glabrous"), ("leaf_width_class", "<4_mm")]),
    "27a": ("Leaves concolourous, strongly 3-veined, apex acute; floral hypanthium glabrous", 28,
            [("leaf_discolority", "concolourous"), ("leaf_main_vein_count", 3, 3, "count"), ("leaf_venation", "strongly_3-veined"), ("leaf_apex_shape", "acute"), ("hypanthium_hairs", "glabrous")]),
    "27b": ("Leaves discolourous, not markedly 3-veined, apex usually obtuse; floral hypanthium usually pubescent", 27,
            [("leaf_discolority", "discolourous"), ("leaf_venation", "not_markedly_3-veined"), ("leaf_apex_shape", "obtuse", "", "usually"), ("hypanthium_hairs", "pubescent", "", "usually")]),
    "28a": ("Base of open fruit almost flat; leaves 3.5-4.0 mm wide", 20, [("fruit_base_shape", "almost_flat", "open fruit"), ("leaf_width", 3.5, 4.0, "mm")]),
    "28b": ("Base of open fruit hemispherical to obconical; leaves 1-4 mm wide", 29, [("fruit_base_shape", "hemispherical obconical", "open fruit"), ("leaf_width_class", "1--4_mm")]),
    "29a": ("Bark papery or fibrous, loosely adhering; leaves 13-22 mm long; fruits 7-10 mm in diameter", 22,
            [("bark_texture", "papery fibrous"), ("bark_adherence", "loosely_adhering"), ("leaf_length", 13, 22, "mm"), ("fruit_width", 7, 10, "mm")]),
    "29b": ("Bark scaly, closely adhering; leaves 6-15 mm long; fruits 5-8 mm in diameter", 21,
            [("bark_texture", "scaly"), ("bark_adherence", "closely_adhering"), ("leaf_length", 6, 15, "mm"), ("fruit_width", 5, 8, "mm")]),
    "30a": ("Sepals not persisting on fruit; leaves lemon-scented", 19, [("sepal_persistence", "not_persistent"), ("leaf_scent", "lemon-scented")]),
    "30b": ("Sepals persisting on fruit; leaves not lemon-scented", 31, [("sepal_persistence", "persistent"), ("leaf_scent", "not_lemon-scented")]),
    "31a": ("Fruit succulent, becoming wrinkled after seed shed, sessile", 32, [("fruit_fleshiness", "succulent"), ("fruit_surface", "wrinkled_after_seed_shed"), ("fruit_attachment", "sessile")]),
    "31b": ("Fruit dry, thin-walled, not wrinkled after seed shed, pedicellate", 33, [("fruit_fleshiness", "dry"), ("fruit_texture", "thin-walled"), ("fruit_surface", "not_wrinkled_after_seed_shed"), ("fruit_attachment", "pedicellate")]),
    "32a": ("Leaves 3-4 mm wide; floral hypanthium 3.5-4.0 mm long; dehisced fruits 5-6 mm long, 5(6)-locular", 16,
            [("leaf_width", 3, 4, "mm"), ("flower_hypanthium_length", 3.5, 4.0, "mm"), ("fruit_length", 5, 6, "mm", "dehisced fruit"), ("fruit_locule_count", "5(6)")]),
    "32b": ("Leaves 1.5-3.0 mm wide; floral hypanthium 2.0-2.5 mm long; dehisced fruits 4-5 mm long, (3)4-5-locular", 17,
            [("leaf_width", 1.5, 3.0, "mm"), ("flower_hypanthium_length", 2.0, 2.5, "mm"), ("fruit_length", 4, 5, "mm", "dehisced fruit"), ("fruit_locule_count", "(3)4--5")]),
    "33a": ("Leaves 3-5 mm long", 15, [("leaf_length", 3, 5, "mm")]),
    "33b": ("Leaves 6-18 mm long", 34, [("leaf_length_class", "6--18_mm")]),
    "34a": ("Valves not projecting above rim of fruit; bark papery", 9, [("fruit_valve_position", "not_projecting_above_rim"), ("bark_texture", "papery")]),
    "34b": ("Valves projecting above rim of fruit; bark scaly", 35, [("fruit_valve_position", "projecting_above_rim"), ("bark_texture", "scaly")]),
    "35a": ("Leaves 6-10 mm long; floral bracts persisting to anthesis", 14, [("leaf_length", 6, 10, "mm"), ("bract_persistence", "persisting_to_anthesis")]),
    "35b": ("Leaves 10-18 mm long; floral bracts not persisting to anthesis", 36, [("leaf_length_class", "10--18_mm"), ("bract_persistence", "not_persisting_to_anthesis")]),
    "36a": ("Pedicels c. 1 mm long, fruit dome about half hypanthium length", 13, [("pedicel_length", 1, None, "mm"), ("fruit_dome_height_relative_to_hypanthium", "about_half")]),
    "36b": ("Pedicels 3-4 mm long, fruit dome about equal to hypanthium length", 12, [("pedicel_length", 3, 4, "mm"), ("fruit_dome_height_relative_to_hypanthium", "about_equal")]),
}
# Couplets 2-15 etc. name a terminal taxon only in the page's right-hand column; which leads are terminal is fixed here.
TERMINAL = {"4a", "4b", "5a", "7a", "7b", "8a", "11a", "11b", "12a", "13a", "13b", "14a", "15a", "15b", "18a", "18b", "19a", "19b",
            "22a", "22b", "24a", "24b", "25a", "27a", "27b", "28a", "29a", "29b", "30a", "32a", "32b", "33a", "34a", "35a", "36a", "36b"}
PAGE = lambda n: "p.657" if n <= 15 else ("p.658" if n <= 32 else "p.659")


def taxa_under(lead):
    """Terminal taxa a lead leads to, following the printed couplet numbers."""
    nxt = KEY[lead][1]
    if lead in TERMINAL:
        return [nxt]
    return taxa_under(f"{nxt}a") + taxa_under(f"{nxt}b")


TAXON_OF = {**{k: v for k, v in NAME.items()}, M1: MAD1, M2: MAD2}
key_rows = []
for lead, (text, _, specs) in KEY.items():
    terms = list(dict.fromkeys(taxa_under(lead)))
    names = [TAXON_OF[t] for t in terms]
    if MAD1 in names and MAD2 in names:
        names.append(MAD)
    group = len(names) > 1
    num = int(re.match(r"\d+", lead).group())
    sec = f"Dichotomous key, couplet {num}, {PAGE(num)}"
    for spec in specs:
        for nm in names:
            if isinstance(spec[1], str):
                ent = spec[2] if len(spec) > 2 else ""
                q = spec[3] if len(spec) > 3 else ""
                key_rows.append((nm, spec[0], text, spec[1], "mode", "", sec, q, ent, "species_group" if group else ""))
            else:
                _, lo, hi, unit = spec[:4]
                ent = spec[4] if len(spec) > 4 else ""
                if group:
                    raise SystemExit(f"numeric group lead {lead}: {spec}")
                if lo == hi or hi is None:
                    vt = "mode" if unit == "count" else "mean"
                    key_rows.append((nm, spec[0], text, lo, vt, unit, sec, "", ent, ""))
                else:
                    key_rows.append((nm, spec[0], text, lo, "minimum", unit, sec, "", ent, ""))
                    key_rows.append((nm, spec[0], text, hi, "maximum", unit, sec, "", ent, ""))

# a taxon reached by two paths gets one row per value, with both couplets named
merged = {}
for nm, trait, text, val, vt, unit, sec, q, ent, rank in key_rows:
    k = (nm, trait, str(val), vt, q, ent)
    if k in merged:
        m0 = merged[k]
        m0["texts"].append(text)
        m0["secs"].append(sec)
        m0["rank"] = m0["rank"] if not rank else m0["rank"]
        if not rank:
            m0["rank"] = ""
    else:
        merged[k] = dict(nm=nm, trait=trait, texts=[text], val=val, vt=vt, unit=unit, secs=[sec], q=q, ent=ent, rank=rank)
for m0 in merged.values():
    sec = m0["secs"][0]
    if len(m0["secs"]) > 1:
        nums = ", ".join(re.search(r"couplet (\d+)", s).group(1) for s in m0["secs"])
        sec = f"Dichotomous key, couplets {nums}"
    raw = " / ".join(dict.fromkeys(m0["texts"]))
    add(m0["nm"], m0["trait"], raw, m0["val"], m0["vt"], m0["unit"], sec, q=m0["q"], ent=m0["ent"], rank=m0["rank"])

# ------------------------------------------------------------------ Species -> subspecies, where the subspecies text is silent
sp_rows = [r0 for r0 in rows if r0["taxon_name"] == MAD and not r0["source_section"].startswith("Dichotomous")]
for sub in (MAD1, MAD2):
    have = {(r0["trait"], r0["entity_measured"]) for r0 in rows if r0["taxon_name"] == sub and not r0["source_section"].startswith("Dichotomous")}
    for r0 in sp_rows:
        if (r0["trait"], r0["entity_measured"]) not in have:
            r1 = dict(r0, taxon_name=sub, rank_scored_from="species")
            r1["notes"] = r0["notes"]
            rows.append(r1)

# ------------------------------------------------------------------ Genus statement (Introduction), to taxa whose own text is silent
sec = "Introduction, p.643"
raw = "Leptospermum is characterised within the family Myrtaceae by its flowers with a single whorl of free stamens which are shorter than the petals, its versatile anthers, capsular fruits and alternate leaves."
GENUS = [("stamen_whorls", "single_whorl"), ("stamen_fusion", "free"), ("stamen_length_relative_to_petals", "shorter_than_petals"),
         ("flower_anther_attachment", "versatile"), ("fruit_type", "capsular"), ("leaf_phyllotaxis", "alternate")]
own = {(r0["taxon_name"], r0["trait"]) for r0 in rows if not r0["source_section"].startswith(("Dichotomous", "Multi-access"))}
for tx in ALL_TAXA:
    for trait, val in GENUS:
        if (tx, trait) not in own:
            add(tx, trait, raw, val, "mode", "", sec, rank="genus")

# ------------------------------------------------------------------ Inferred typical values
sec = "Inferred"
for tx in ALL_TAXA:
    add(tx, "plant_photosynthetic_organ", "Leaves described; no leafless, phyllode or photosynthetic-stem wording", "leaf", "mode", "", sec, rank="inferred")
    add(tx, "parasitic", "Green-leaved plant; no parasitism or host named", "not_parasitic", "mode", "", sec, rank="inferred")
    if not any(r0["taxon_name"] == tx and r0["trait"] == "plant_growth_substrate" for r0 in rows):
        add(tx, "plant_growth_substrate", "No aquatic, epiphytic or lithophytic wording", "terrestrial", "mode", "", sec, rank="inferred")
    # broadleaf: a flat-lamina shape word, or leaves at least 3 mm wide
    shapes = {v for r0 in rows if r0["taxon_name"] == tx and r0["trait"] == "leaf_shape" and not r0["population_region"] for v in r0["trait_value"].split(" ")}
    widths = [float(r0["trait_value"]) for r0 in rows if r0["taxon_name"] == tx and r0["trait"] == "leaf_width" and r0["value_type"] == "minimum" and not r0["population_region"]]
    flat = shapes & {"narrow-lanceolate", "narrowly_elliptical", "broadly_elliptical", "elliptical", "obovate"}
    if flat or (widths and max(widths) >= 3):
        add(tx, "leaf_type", "Flat-lamina leaf shape, or leaves at least 3 mm wide", "broadleaf", "mode", "", sec, rank="inferred")

rows.sort(key=lambda r0: (ALL_TAXA.index(r0["taxon_name"]), ))

# ------------------------------------------------------------------ Treatment type and first-description year, per taxon
# Vocabulary of the family-taxon-table skill, plus "not described" (annotated list and keys only). Years only as cited in
# the paper's own nomenclature and synonymy.
ND, REV, NEW, NEWI = "not described", "revision", "new species", "new infra taxa"
AL = "Annotated list: type, distribution and habitat."
TAXA = [
    (NAME[1], NEW, 1992, "sp. nov., this paper", "The Species, p.645", ""),
    (MAD, REV, 1920, "earliest: replaced synonym Agonis longifolia C. White & Francis 1920", "The Species, p.645",
     "nom. nov., with description, for the illegitimate L. longifolium (C. White & Francis) S.T. Blake 1958 (later homonym of L. longifolium Cunn. 1841)."),
    (MAD1, NEWI, 1920, "autonym; holds the type of Agonis longifolia C. White & Francis 1920 (Endeavour River)", "The Species, p.646",
     "Autonym created with subsp. sativum; brief description."),
    (MAD2, NEWI, 1992, "subsp. nov., this paper", "The Species, p.646", "Brief description."),
    (NAME[3], ND, 1860, "earliest: basionym Kunzea brachyandra F. Muell. 1860", "The Species, p.647", AL),
    (NAME[4], REV, 1907, "Valeton 1907", "The Species, p.647", "Described in full; circumscription narrowed to New Guinea."),
    (NAME[5], ND, 1989, "J. Thompson 1989", "The Species, p.647", AL),
    (NAME[6], ND, 1900, "Bailey 1900", "The Species, p.648", AL),
    (NAME[7], ND, 1842, "Schauer 1842", "The Species, p.648", AL),
    (NAME[8], ND, 1920, "earliest: replaced synonym Agonis elliptica C. White & Francis 1920 (same type); L. whitei Cheel 1932", "The Species, p.648", AL),
    (NAME[9], ND, 1790, "earliest: basionym Melaleuca trinervia Smith 1790", "The Species, p.648", AL),
    (NAME[10], ND, 1989, "J. Thompson 1989", "The Species, p.648", AL),
    (NAME[11], ND, 1923, "Cheel 1923", "The Species, p.648", AL),
    (NAME[12], ND, 1855, "F. Muell. 1855", "The Species, p.649", AL),
    (NAME[13], ND, 1989, "J. Thompson 1989", "The Species, p.649", AL),
    (NAME[14], ND, 1848, "Lindley 1848", "The Species, p.649", AL),
    (NAME[15], ND, 1797, "Smith 1797", "The Species, p.649", AL),
    (NAME[16], NEW, 1992, "sp. nov., this paper", "The Species, p.649", ""),
    (NAME[17], ND, 1932, "Cheel 1932", "The Species, p.650", AL),
    (NAME[18], ND, 1788, "Gaertner 1788", "The Species, p.650", AL),
    (NAME[19], ND, 1906, "R. Baker & H.G. Smith 1906", "The Species, p.650", AL),
    (NAME[20], ND, 1989, "J. Thompson 1989", "The Species, p.650", AL),
    (NAME[21], ND, 1796, "Salisb. 1796 (synonym L. flavescens Smith 1797)", "The Species, p.651", AL),
    (NAME[22], ND, 1989, "J. Thompson 1989", "The Species, p.651", AL),
    (NAME[23], ND, 1989, "J. Thompson 1989", "The Species, p.651", AL),
    (NAME[24], ND, 1947, "C. White 1947", "The Species, p.651", AL),
    (NAME[25], ND, 1797, "Smith 1797", "The Species, p.651", AL),
    (NAME[26], ND, 1852, "J.D. Hook 1852", "The Species, p.652", AL + " Borneo and Sulawesi."),
    (NAME[27], REV, 1823, "earliest: heterotypic synonym Glaphyria nitida Jack 1823; L. javanicum Blume 1826", "The Species, p.652",
     "Described in full; circumscription narrowed. Malesia and Burma."),
    (NAME[28], ND, 1889, "Bailey 1889", "The Species, p.653", AL),
    (NAME[29], REV, 1826, "Blume 1826; synonyms L. annae Stein 1885, L. flavescens var. angustifolia Ridley 1922, L. petersonii subsp. lanceolatum J. Thompson 1989",
     "The Species, p.653", "Reinstated at species level, with description."),
    (NAME[30], ND, 1905, "Bailey 1905", "The Species, p.654", AL),
    (NAME[31], ND, 1989, "J. Thompson 1989", "The Species, p.654", AL),
]
assert sorted(t[0] for t in TAXA) == sorted(ALL_TAXA)
TCOLS = ["taxon_name", "treatment_type", "original_concept_year", "original_concept_basis", "source_section", "notes"]
with open(DS / "raw" / "Bean_1992_taxa.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(TCOLS)
    w.writerows(TAXA)
TT = {t[0]: (t[1], str(t[2]) if t[2] else "") for t in TAXA}
for r0 in rows:
    r0["treatment_type"], r0["original_concept_year"] = TT[r0["taxon_name"]]

COLS = ["taxon_name", "treatment_type", "original_concept_year", "entity_type", "rank_scored_from", "commonness_qualifier", "population_region", "entity_measured", "trait", "in_traits.yml",
        "trait_value_raw", "trait_value", "trait_value_clean", "value_type", "unit", "source_section", "notes"]
with open(DS / "raw" / "trait_data.csv", "w", newline="") as f:
    w = csv.DictWriter(f, COLS)
    w.writeheader()
    w.writerows(rows)

# data.csv = trait_data.csv + trait_scoring_method, from rank_scored_from
METHOD = {"genus": "inferred_from_genus", "species": "inferred_from_species", "species_group": "inferred_from_species_group", "inferred": "inferred"}
with open(DS / "data.csv", "w", newline="") as f:
    w = csv.DictWriter(f, COLS + ["trait_scoring_method"])
    w.writeheader()
    for r0 in rows:
        w.writerow(dict(r0, trait_scoring_method=METHOD.get(r0["rank_scored_from"], "")))

print(len(rows), "rows;", len({r0['taxon_name'] for r0 in rows}), "taxa;",
      len({r0['trait'] for r0 in rows if r0['in_traits.yml']}), "mapped traits;",
      len({r0['trait'] for r0 in rows if not r0['in_traits.yml']}), "candidates")
