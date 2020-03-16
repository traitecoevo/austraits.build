Definitions for traits in `AusTraits`
================
Daniel Falster
2020-03-16

  - [Introduction](#introduction)
  - [Traits](#traits)
      - [accessory\_cost\_mass](#accessory_cost_mass)
      - [accessory\_cost\_fraction](#accessory_cost_fraction)
      - [bark\_delta13C](#bark_delta13c)
      - [bark\_delta15N](#bark_delta15n)
      - [bark\_mass\_area](#bark_mass_area)
      - [bark\_C\_per\_dry\_mass](#bark_c_per_dry_mass)
      - [bark\_N\_per\_dry\_mass](#bark_n_per_dry_mass)
      - [bark\_thickness](#bark_thickness)
      - [branch\_mass\_fraction](#branch_mass_fraction)
      - [sprout\_depth](#sprout_depth)
      - [carotenoid\_per\_area](#carotenoid_per_area)
      - [carotenoid\_per\_dry\_mass](#carotenoid_per_dry_mass)
      - [cell\_epidermis\_Ca\_per\_fresh\_mass](#cell_epidermis_ca_per_fresh_mass)
      - [cell\_hypodermis\_Ca\_per\_fresh\_mass](#cell_hypodermis_ca_per_fresh_mass)
      - [cell\_internal\_parenchyma\_Ca\_per\_fresh\_mass](#cell_internal_parenchyma_ca_per_fresh_mass)
      - [cell\_palisade\_mesophyll\_Ca\_per\_fresh\_mass](#cell_palisade_mesophyll_ca_per_fresh_mass)
      - [cell\_sclerenchyma\_Ca\_per\_fresh\_mass](#cell_sclerenchyma_ca_per_fresh_mass)
      - [cell\_spongy\_mesophyll\_Ca\_per\_fresh\_mass](#cell_spongy_mesophyll_ca_per_fresh_mass)
      - [cell\_epidermis\_P\_per\_fresh\_mass](#cell_epidermis_p_per_fresh_mass)
      - [cell\_hypodermis\_P\_per\_fresh\_mass](#cell_hypodermis_p_per_fresh_mass)
      - [cell\_internal\_parenchyma\_P\_per\_fresh\_mass](#cell_internal_parenchyma_p_per_fresh_mass)
      - [cell\_palisade\_mesophyll\_P\_per\_fresh\_mass](#cell_palisade_mesophyll_p_per_fresh_mass)
      - [cell\_sclerenchyma\_P\_per\_fresh\_mass](#cell_sclerenchyma_p_per_fresh_mass)
      - [cell\_spongy\_mesophyll\_P\_per\_fresh\_mass](#cell_spongy_mesophyll_p_per_fresh_mass)
      - [chlorophyll\_per\_area](#chlorophyll_per_area)
      - [chlorophyll\_A\_per\_area](#chlorophyll_a_per_area)
      - [chlorophyll\_B\_per\_area](#chlorophyll_b_per_area)
      - [chlorophyll\_per\_dry\_mass](#chlorophyll_per_dry_mass)
      - [chlorophyll\_A\_per\_dry\_mass](#chlorophyll_a_per_dry_mass)
      - [chlorophyll\_B\_per\_dry\_mass](#chlorophyll_b_per_dry_mass)
      - [chlorophyll\_A\_B\_ratio](#chlorophyll_a_b_ratio)
      - [ca](#ca)
      - [cc](#cc)
      - [ci](#ci)
      - [ci\_at\_Amax](#ci_at_amax)
      - [ci\_at\_Asat](#ci_at_asat)
      - [ci\_over\_ca](#ci_over_ca)
      - [cotyledon\_position](#cotyledon_position)
      - [cotyledon\_type](#cotyledon_type)
      - [upper\_cuticle\_thickness](#upper_cuticle_thickness)
      - [lower\_cuticle\_thickness](#lower_cuticle_thickness)
      - [diaspore\_mass](#diaspore_mass)
      - [dispersal\_appendage](#dispersal_appendage)
      - [dispersal\_syndrome](#dispersal_syndrome)
      - [dispersers](#dispersers)
      - [dormancy\_type](#dormancy_type)
      - [embryo\_colour](#embryo_colour)
      - [upper\_epidermis\_thickness](#upper_epidermis_thickness)
      - [lower\_epidermis\_thickness](#lower_epidermis_thickness)
      - [epithelial\_cell\_density](#epithelial_cell_density)
      - [epithelial\_cell\_layer\_thickness](#epithelial_cell_layer_thickness)
      - [fire\_response](#fire_response)
      - [fire\_response\_detailed](#fire_response_detailed)
      - [fire\_cued\_seeding](#fire_cued_seeding)
      - [flower\_colour](#flower_colour)
      - [flower\_count\_maximum](#flower_count_maximum)
      - [flowering\_time](#flowering_time)
      - [fluorescence\_Jmax\_per\_mass](#fluorescence_jmax_per_mass)
      - [fluorescence\_Vcmax\_per\_mass](#fluorescence_vcmax_per_mass)
      - [fluorescence\_Jmax\_over\_Vcmax](#fluorescence_jmax_over_vcmax)
      - [fruit\_mass](#fruit_mass)
      - [fruit\_width](#fruit_width)
      - [fruit\_length](#fruit_length)
      - [fruit\_breadth](#fruit_breadth)
      - [fruit\_type](#fruit_type)
      - [fruit\_type\_botany](#fruit_type_botany)
      - [fruit\_type\_function](#fruit_type_function)
      - [fruiting\_time](#fruiting_time)
      - [fv\_over\_fm](#fv_over_fm)
      - [germination](#germination)
      - [germination\_treatment](#germination_treatment)
      - [genome\_size](#genome_size)
      - [glaucous](#glaucous)
      - [growth\_habit](#growth_habit)
      - [huber\_value](#huber_value)
      - [hypocotyl\_type](#hypocotyl_type)
      - [upper\_hypodermis\_thickness](#upper_hypodermis_thickness)
      - [lower\_hypodermis\_thickness](#lower_hypodermis_thickness)
      - [leaf\_Al\_per\_dry\_mass](#leaf_al_per_dry_mass)
      - [leaf\_angle](#leaf_angle)
      - [leaf\_area](#leaf_area)
      - [leaf\_area\_ratio](#leaf_area_ratio)
      - [leaf\_absorption](#leaf_absorption)
      - [leaf\_reflectance](#leaf_reflectance)
      - [leaf\_transmission](#leaf_transmission)
      - [leaf\_PRI](#leaf_pri)
      - [leaf\_arrangement](#leaf_arrangement)
      - [leaf\_B\_per\_dry\_mass](#leaf_b_per_dry_mass)
      - [leaf\_C\_per\_dry\_mass](#leaf_c_per_dry_mass)
      - [leaf\_Ca\_per\_dry\_mass](#leaf_ca_per_dry_mass)
      - [leaf\_cell\_wall\_fraction](#leaf_cell_wall_fraction)
      - [leaf\_cell\_wall\_N](#leaf_cell_wall_n)
      - [leaf\_cell\_wall\_N\_fraction](#leaf_cell_wall_n_fraction)
      - [leaf\_CN\_ratio](#leaf_cn_ratio)
      - [leaf\_compoundness](#leaf_compoundness)
      - [leaf\_Cu\_per\_dry\_mass](#leaf_cu_per_dry_mass)
      - [leaf\_dark\_respiration\_per\_area](#leaf_dark_respiration_per_area)
      - [leaf\_dark\_respiration\_per\_dry\_mass](#leaf_dark_respiration_per_dry_mass)
      - [leaf\_light\_respiration\_per\_area](#leaf_light_respiration_per_area)
      - [leaf\_delta13C](#leaf_delta13c)
      - [leaf\_delta15N](#leaf_delta15n)
      - [leaf\_density](#leaf_density)
      - [leaf\_dry\_mass](#leaf_dry_mass)
      - [leaf\_dry\_matter\_content](#leaf_dry_matter_content)
      - [leaf\_elastic\_modulus](#leaf_elastic_modulus)
      - [leaf\_Fe\_per\_dry\_mass](#leaf_fe_per_dry_mass)
      - [leaf\_fracture\_force](#leaf_fracture_force)
      - [leaf\_fresh\_mass](#leaf_fresh_mass)
      - [leaf\_fresh\_mass\_per\_area](#leaf_fresh_mass_per_area)
      - [leaf\_hairs\_adult](#leaf_hairs_adult)
      - [leaf\_hairs\_juvenile](#leaf_hairs_juvenile)
      - [leaf\_hydraulic\_conductivity](#leaf_hydraulic_conductivity)
      - [leaf\_hydraulic\_vulnerability](#leaf_hydraulic_vulnerability)
      - [leaf\_K\_per\_area](#leaf_k_per_area)
      - [leaf\_K\_per\_dry\_mass](#leaf_k_per_dry_mass)
      - [leaf\_length](#leaf_length)
      - [leaf\_lifespan](#leaf_lifespan)
      - [leaf\_lignin\_per\_dry\_mass](#leaf_lignin_per_dry_mass)
      - [leaf\_margin](#leaf_margin)
      - [leaf\_mass\_fraction](#leaf_mass_fraction)
      - [leaf\_mass\_to\_stem\_mass](#leaf_mass_to_stem_mass)
      - [leaf\_mesophyll\_conductance\_per\_area](#leaf_mesophyll_conductance_per_area)
      - [leaf\_mesophyll\_conductance\_per\_mass](#leaf_mesophyll_conductance_per_mass)
      - [leaf\_Mg\_per\_dry\_mass](#leaf_mg_per_dry_mass)
      - [leaf\_Mn\_per\_dry\_mass](#leaf_mn_per_dry_mass)
      - [leaf\_Mo\_per\_dry\_mass](#leaf_mo_per_dry_mass)
      - [leaf\_N\_per\_area](#leaf_n_per_area)
      - [leaf\_N\_per\_dry\_mass](#leaf_n_per_dry_mass)
      - [leaf\_Na\_per\_dry\_mass](#leaf_na_per_dry_mass)
      - [leaf\_Ni\_per\_dry\_mass](#leaf_ni_per_dry_mass)
      - [leaf\_P\_per\_area](#leaf_p_per_area)
      - [leaf\_P\_per\_dry\_mass](#leaf_p_per_dry_mass)
      - [leaf\_phenology](#leaf_phenology)
      - [leaf\_photosynthetic\_nitrogen\_use\_efficiency](#leaf_photosynthetic_nitrogen_use_efficiency)
      - [leaf\_photosynthetic\_nitrogen\_use\_efficiency\_maximum](#leaf_photosynthetic_nitrogen_use_efficiency_maximum)
      - [leaf\_photosynthetic\_nitrogen\_use\_efficiency\_saturated](#leaf_photosynthetic_nitrogen_use_efficiency_saturated)
      - [leaf\_photosynthetic\_phosphorus\_use\_efficiency](#leaf_photosynthetic_phosphorus_use_efficiency)
      - [leaf\_photosynthetic\_phosphorus\_use\_efficiency\_maximum](#leaf_photosynthetic_phosphorus_use_efficiency_maximum)
      - [leaf\_photosynthetic\_phosphorus\_use\_efficiency\_saturated](#leaf_photosynthetic_phosphorus_use_efficiency_saturated)
      - [leaf\_photosynthetic\_water\_use\_efficiency](#leaf_photosynthetic_water_use_efficiency)
      - [leaf\_S\_per\_dry\_mass](#leaf_s_per_dry_mass)
      - [leaf\_saturated\_water\_content\_per\_mass](#leaf_saturated_water_content_per_mass)
      - [stem\_saturated\_water\_content\_per\_mass](#stem_saturated_water_content_per_mass)
      - [bark\_saturated\_water\_content\_per\_mass](#bark_saturated_water_content_per_mass)
      - [leaf\_shape](#leaf_shape)
      - [leaf\_specific\_conductivity](#leaf_specific_conductivity)
      - [leaf\_spines\_adult](#leaf_spines_adult)
      - [leaf\_thickness](#leaf_thickness)
      - [leaf\_toughness](#leaf_toughness)
      - [leaf\_transpiration](#leaf_transpiration)
      - [leaf\_transpiration\_at\_Amax](#leaf_transpiration_at_amax)
      - [leaf\_transpiration\_at\_Asat](#leaf_transpiration_at_asat)
      - [leaf\_turgor\_loss\_point](#leaf_turgor_loss_point)
      - [leaf\_type](#leaf_type)
      - [leaf\_water\_content\_per\_area](#leaf_water_content_per_area)
      - [leaf\_water\_content\_per\_mass](#leaf_water_content_per_mass)
      - [leaf\_width](#leaf_width)
      - [leaf\_Zn\_per\_dry\_mass](#leaf_zn_per_dry_mass)
      - [life\_form](#life_form)
      - [life\_history](#life_history)
      - [lifespan](#lifespan)
      - [mating\_system](#mating_system)
      - [modified\_NDVI](#modified_ndvi)
      - [modulus\_of\_elasticity](#modulus_of_elasticity)
      - [modulus\_of\_rupture](#modulus_of_rupture)
      - [nitrogen\_fixing](#nitrogen_fixing)
      - [N\_to\_P\_ratio](#n_to_p_ratio)
      - [osmotic\_potential](#osmotic_potential)
      - [palisade\_cell\_length](#palisade_cell_length)
      - [lower\_palisade\_cell\_thickness](#lower_palisade_cell_thickness)
      - [upper\_palisade\_cell\_thickness](#upper_palisade_cell_thickness)
      - [palisade\_cell\_width](#palisade_cell_width)
      - [palisade\_layer\_number](#palisade_layer_number)
      - [photosynthetic\_pathway](#photosynthetic_pathway)
      - [photosynthetic\_rate\_per\_area](#photosynthetic_rate_per_area)
      - [photosynthetic\_rate\_per\_area\_maximum](#photosynthetic_rate_per_area_maximum)
      - [photosynthetic\_rate\_per\_area\_saturated](#photosynthetic_rate_per_area_saturated)
      - [photosynthetic\_rate\_per\_dry\_mass](#photosynthetic_rate_per_dry_mass)
      - [photosynthetic\_rate\_per\_dry\_mass\_maximum](#photosynthetic_rate_per_dry_mass_maximum)
      - [photosynthetic\_rate\_per\_dry\_mass\_saturated](#photosynthetic_rate_per_dry_mass_saturated)
      - [plant\_growth\_form](#plant_growth_form)
      - [plant\_height](#plant_height)
      - [plant\_width](#plant_width)
      - [plant\_breadth](#plant_breadth)
      - [pollination\_syndrome](#pollination_syndrome)
      - [regen\_strategy](#regen_strategy)
      - [resorption\_leaf\_N](#resorption_leaf_n)
      - [resorption\_leaf\_P](#resorption_leaf_p)
      - [root\_diameter](#root_diameter)
      - [root\_dry\_matter\_content](#root_dry_matter_content)
      - [root\_mass\_fraction](#root_mass_fraction)
      - [root\_morphology](#root_morphology)
      - [root\_shoot\_ratio](#root_shoot_ratio)
      - [root\_structure](#root_structure)
      - [root\_wood\_density](#root_wood_density)
      - [sapwood\_specific\_conductivity](#sapwood_specific_conductivity)
      - [seed\_breadth](#seed_breadth)
      - [seed\_count](#seed_count)
      - [seed\_length](#seed_length)
      - [seed\_mass](#seed_mass)
      - [seed\_mass\_reserve](#seed_mass_reserve)
      - [seed\_oil\_content](#seed_oil_content)
      - [seed\_P\_concentration](#seed_p_concentration)
      - [seed\_protein\_content](#seed_protein_content)
      - [seed\_shape](#seed_shape)
      - [seed\_storage\_location](#seed_storage_location)
      - [seed\_texture](#seed_texture)
      - [seed\_viability](#seed_viability)
      - [seed\_volume](#seed_volume)
      - [seed\_width](#seed_width)
      - [seedling\_first\_leaf](#seedling_first_leaf)
      - [seedling\_first\_node](#seedling_first_node)
      - [senesced\_leaf\_Ca\_per\_dry\_mass](#senesced_leaf_ca_per_dry_mass)
      - [senesced\_leaf\_Cu\_per\_dry\_mass](#senesced_leaf_cu_per_dry_mass)
      - [senesced\_leaf\_Fe\_per\_dry\_mass](#senesced_leaf_fe_per_dry_mass)
      - [senesced\_leaf\_K\_per\_dry\_mass](#senesced_leaf_k_per_dry_mass)
      - [senesced\_leaf\_Mg\_per\_dry\_mass](#senesced_leaf_mg_per_dry_mass)
      - [senesced\_leaf\_Mn\_per\_dry\_mass](#senesced_leaf_mn_per_dry_mass)
      - [senesced\_leaf\_Mo\_per\_dry\_mass](#senesced_leaf_mo_per_dry_mass)
      - [senesced\_leaf\_N\_per\_dry\_mass](#senesced_leaf_n_per_dry_mass)
      - [senesced\_leaf\_Na\_per\_dry\_mass](#senesced_leaf_na_per_dry_mass)
      - [senesced\_leaf\_Ni\_per\_dry\_mass](#senesced_leaf_ni_per_dry_mass)
      - [senesced\_leaf\_P\_per\_dry\_mass](#senesced_leaf_p_per_dry_mass)
      - [senesced\_leaf\_S\_per\_dry\_mass](#senesced_leaf_s_per_dry_mass)
      - [senesced\_leaf\_Zn\_per\_dry\_mass](#senesced_leaf_zn_per_dry_mass)
      - [serotiny](#serotiny)
      - [soil\_salinity\_tolerance](#soil_salinity_tolerance)
      - [soluable\_protein\_per\_area](#soluable_protein_per_area)
      - [insoluable\_protein\_per\_area](#insoluable_protein_per_area)
      - [soluable\_sugars\_per\_area](#soluable_sugars_per_area)
      - [specific\_leaf\_area](#specific_leaf_area)
      - [specific\_root\_area](#specific_root_area)
      - [specific\_root\_length](#specific_root_length)
      - [spongy\_mesophyll\_thickness](#spongy_mesophyll_thickness)
      - [starch\_per\_area](#starch_per_area)
      - [stem\_C\_per\_dry\_mass](#stem_c_per_dry_mass)
      - [stem\_count\_categorical](#stem_count_categorical)
      - [stem\_cross\_section\_area](#stem_cross_section_area)
      - [stem\_cross\_section\_area\_no\_bark](#stem_cross_section_area_no_bark)
      - [stem\_dry\_matter\_content](#stem_dry_matter_content)
      - [stem\_hydraulic\_conductivity](#stem_hydraulic_conductivity)
      - [stem\_N\_per\_dry\_mass](#stem_n_per_dry_mass)
      - [basal\_diameter](#basal_diameter)
      - [stomatal\_conductance\_per\_area](#stomatal_conductance_per_area)
      - [stomatal\_conductance\_per\_area\_at\_Amax](#stomatal_conductance_per_area_at_amax)
      - [stomatal\_conductance\_per\_area\_at\_Asat](#stomatal_conductance_per_area_at_asat)
      - [stomatal\_density](#stomatal_density)
      - [support\_fraction](#support_fraction)
      - [tap\_root](#tap_root)
      - [thickest\_root\_diameter](#thickest_root_diameter)
      - [twig\_area](#twig_area)
      - [twig\_length](#twig_length)
      - [vapor\_pressure\_deficit](#vapor_pressure_deficit)
      - [vegetative\_regeneration](#vegetative_regeneration)
      - [water\_band\_index](#water_band_index)
      - [water\_potential\_midday](#water_potential_midday)
      - [water\_potential\_predawn](#water_potential_predawn)
      - [water\_tolerance](#water_tolerance)
      - [water\_use\_efficiency](#water_use_efficiency)
      - [wood\_delta13C](#wood_delta13c)
      - [wood\_delta15N](#wood_delta15n)
      - [wood\_density](#wood_density)
      - [wood\_C\_per\_dry\_mass](#wood_c_per_dry_mass)
      - [wood\_N\_per\_dry\_mass](#wood_n_per_dry_mass)
      - [woodiness](#woodiness)

<!-- Traits_definitions.md is generated from Traits_definitions.Rmd. Please edit that file -->

## Introduction

Below is the full list of trait definitions used in AusTraits version
0.9.1.9000, as defined in the file configuration files
[`config/definitions.yml`](https://github.com/traitecoevo/austraits.build/blob/master/config/definitions.yml)).

Separate documents provide information on:

  - [Building Austraits](Building.md)
  - [Contributing](Contributing.md)
  - [Tips & Tricks for AusTraits developers](TipTricks.md), and
  - [Full list of trait definitions](Trait_definitions.md)

## Traits

### accessory\_cost\_mass

  - label: Mass of seed acessory costs
  - description: Mass of seed acessory costs, the proportion of a fruit
    that does not develop into a seed
  - type: numeric
  - units: mg
  - allowable range: 0.01 - 10000 mg

### accessory\_cost\_fraction

  - label: Fraction of total reproductive investment to non-seed tissues
  - description: The fraction of total reproductive investment required
    to mature a seed that is invested in non-seed tissues
  - type: numeric
  - units: mg/mg
  - allowable range: 0.01 - 1 mg/mg

### bark\_delta13C

  - label: Bark carbon (C) isotope signature (delta 13C)
  - description: Bark carbon stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -50 - 0 per mille

### bark\_delta15N

  - label: Bark nitrogen (N) isotope signature (delta 15N)
  - description: Bark nitrogen stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -25 - 75 per mille

### bark\_mass\_area

  - label: Bark mass per unit surface area of stem
  - description: Bark mass per unit surface area of stem
  - type: numeric
  - units: g/cm2
  - allowable range: 0.01 - 1 g/cm2

### bark\_C\_per\_dry\_mass

  - label: Bark carbon (C) content per unit bark dry mass
  - description: Bark carbon (C) content per unit bark dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 50 - 750 mg/g

### bark\_N\_per\_dry\_mass

  - label: Bark nitrogen (N) content per unit bark dry mass
  - description: Bark nitrogen (N) content per unit bark dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### bark\_thickness

  - label: Bark thickness
  - description: Thickness of the bark of the stem
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 50 mm

### branch\_mass\_fraction

  - label: Fraction of plant dry mass comprised of branch material
  - description: Fraction of plant dry mass comprised of branch material
  - type: numeric
  - units: mg/mg
  - allowable range: 0 - 1 mg/mg

### sprout\_depth

  - label: Depth below ground (negative number) or height above ground
    (positive number) from which buds emerge following a disturbance
    (i.e. fire)
  - description: Depth of resprouting shoots
  - type: numeric
  - units: mm
  - allowable range: -100 - 100 mm

### carotenoid\_per\_area

  - label: Leaf carotenoid content per unit leaf area
  - description: Leaf carotenoid content per unit leaf area
  - type: numeric
  - units: umol/m2
  - allowable range: 10 - 1000 umol/m2

### carotenoid\_per\_dry\_mass

  - label: Leaf carotenoid content per unit leaf dry mass
  - description: Leaf carotenoid content per unit leaf dry mass
  - type: numeric
  - units: mmol/kg
  - allowable range: 0.1 - 10 mmol/kg

### cell\_epidermis\_Ca\_per\_fresh\_mass

  - label: Ca content of epidermal cells
  - description: Ca content of epidermal cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 500 umol/g

### cell\_hypodermis\_Ca\_per\_fresh\_mass

  - label: Ca content of hypodermis cells
  - description: Ca content of hypodermis cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 500 umol/g

### cell\_internal\_parenchyma\_Ca\_per\_fresh\_mass

  - label: Ca content of internal parenchyma cells
  - description: Ca content of internal parenchyma cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 500 umol/g

### cell\_palisade\_mesophyll\_Ca\_per\_fresh\_mass

  - label: Ca content of palisade mesophyll cells
  - description: Ca content of palisade mesophyll cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 100 umol/g

### cell\_sclerenchyma\_Ca\_per\_fresh\_mass

  - label: Ca content of sclerenchyma cells
  - description: Ca content of sclerenchyma cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 100 umol/g

### cell\_spongy\_mesophyll\_Ca\_per\_fresh\_mass

  - label: Ca content of spongy mesophyll cells
  - description: Ca content of spongy mesophyll cells
  - type: numeric
  - units: umol/g
  - allowable range: 1 - 1000 umol/g

### cell\_epidermis\_P\_per\_fresh\_mass

  - label: P content of epidermal cells
  - description: P content of epidermal cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 10 umol/g

### cell\_hypodermis\_P\_per\_fresh\_mass

  - label: P content of hypodermis cells
  - description: P content of hypodermis cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 10 umol/g

### cell\_internal\_parenchyma\_P\_per\_fresh\_mass

  - label: P content of internal parenchyma cells
  - description: P content of internal parenchyma cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 10 umol/g

### cell\_palisade\_mesophyll\_P\_per\_fresh\_mass

  - label: P content of palisade mesophyll cells
  - description: P content of palisade mesophyll cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 30 umol/g

### cell\_sclerenchyma\_P\_per\_fresh\_mass

  - label: P content of sclerenchyma cells
  - description: P content of sclerenchyma cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 10 umol/g

### cell\_spongy\_mesophyll\_P\_per\_fresh\_mass

  - label: P content of spongy mesophyll cells
  - description: P content of spongy mesophyll cells
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 30 umol/g

### chlorophyll\_per\_area

  - label: Sum of chlorophyll A and B per leaf area
  - description: Sum of chlorophyll A and B per leaf area
  - type: numeric
  - units: umol/m2
  - allowable range: 50 - 2000 umol/m2

### chlorophyll\_A\_per\_area

  - label: Chlorophyll A content relative to leaf area
  - description: Chlorophyll A content relative to leaf area
  - type: numeric
  - units: umol/m2
  - allowable range: 50 - 2000 umol/m2

### chlorophyll\_B\_per\_area

  - label: Sum of Chlorophyll B content relative to leaf area
  - description: Chlorophyll B content relative to leaf area
  - type: numeric
  - units: umol/m2
  - allowable range: 10 - 1000 umol/m2

### chlorophyll\_per\_dry\_mass

  - label: Leaf chlorophyll content per unit leaf dry mass
  - description: Leaf chlorophyll content per unit leaf dry mass
  - type: numeric
  - units: mmol/kg
  - allowable range: 0.1 - 10 mmol/kg

### chlorophyll\_A\_per\_dry\_mass

  - label: Leaf chlorophyll A content per unit leaf dry mass
  - description: Leaf chlorophyll A content per unit leaf dry mass
  - type: numeric
  - units: mmol/kg
  - allowable range: 0.1 - 10 mmol/kg

### chlorophyll\_B\_per\_dry\_mass

  - label: Leaf chlorophyll B content per unit leaf dry mass
  - description: Leaf chlorophyll B content per unit leaf dry mass
  - type: numeric
  - units: mmol/kg
  - allowable range: 0.01 - 10 mmol/kg

### chlorophyll\_A\_B\_ratio

  - label: Ratio of chlorophyll A to chlorophyll B
  - description: Ratio of chlorophyll A to chlorophyll B
  - type: numeric
  - units: umol/umol
  - allowable range: 1 - 10 umol/umol

### ca

  - label: ambient CO2 concentration
  - description: ambient CO2 concentration (external CO2 concentration)
  - type: numeric
  - units: umol/mol
  - allowable range: 50 - 1000 umol/mol

### cc

  - label: CO2 concentration inside chloroplasts
  - description: CO2 concentration inside chloroplasts
  - type: numeric
  - units: umol/umol
  - allowable range: 50 - 1000 umol/umol

### ci

  - label: internal CO2 concentration under ambient conditions
  - description: CO2 concentration in interstitial spaces under ambient
    conditions
  - type: numeric
  - units: umol/mol
  - allowable range: 50 - 1000 umol/mol

### ci\_at\_Amax

  - label: internal CO2 concentration during Amax measurement
  - description: CO2 concentration in interstitial spaces during Amax
    measurement
  - type: numeric
  - units: umol/mol
  - allowable range: 50 - 1000 umol/mol

### ci\_at\_Asat

  - label: internal CO2 concentration during Asat measurement
  - description: CO2 concentration in interstitial spaces during Asat
    measurement
  - type: numeric
  - units: umol/mol
  - allowable range: 50 - 1000 umol/mol

### ci\_over\_ca

  - label: Ratio of internal to external CO2 concentrations
  - description: Ratio of internal to external CO2 concentrations
  - type: numeric
  - units: umol/umol
  - allowable range: 0 - 1 umol/umol

### cotyledon\_position

  - label: Cotyledon position at germination
  - description: Binary variable distinguishing between seedlings where
    the cotyledon remains within the seed coat versus emerges from the
    seed coat at germination.
  - type: categorical
  - allowable values:
      - *cryptocotylar*: A type of seed germination in which the
        cotyledons remain within the seed coat at germination.
      - *phanerocotylar*: A type of seed germination in which the
        cotyledons emerge from the seed coat.

### cotyledon\_type

  - label: Cotyledon type
  - description: Binary variable distinguishing between glabrous versus
    hairy cotyledons
  - type: categorical
  - allowable values:
      - *glabrous*: Cotyledon lacks hairs
      - *hairy*: Cotyledon has hairs

### upper\_cuticle\_thickness

  - label: Upper cuticle thickness
  - description: Thickness of the upper cuticle
  - type: numeric
  - units: um
  - allowable range: 0.1 - 100 um

### lower\_cuticle\_thickness

  - label: Lower cuticle thickness
  - description: Thickness of the lower cuticle
  - type: numeric
  - units: um
  - allowable range: 0.1 - 100 um

### diaspore\_mass

  - label: Mass of entire diaspore
  - description: Mass of seed including dispersal appendages
  - type: numeric
  - units: mg
  - allowable range: 0.001 - 10000 mg

### dispersal\_appendage

  - label: Appendage of propagule which facilitates dispersal
  - description: Type of dispersal appendage present
  - type: categorical
  - allowable values:
      - *aril*: Fleshy outgrowth of a seed, that often attracts animals
        like birds or ants
      - *awns*: A slender, bristle-like projection, e.g. from the back
        or tip of the glumes and lemmas in some grasses and on the fruit
        of some other species.
      - *awn\_bristle*: REPLACE WITH ‘awns bristles’
      - *barbs*: A rear-facing point, as in a fish hook that aid in seed
        and fruit dispersal. Includes “hooks”, a term used in Ranunculus
      - *beak*: Name of dispersal appendage in the genus Carex
      - *bladdery\_wings*: Maybe referring to the wings present on the
        pollen grains of some conifers to aid in their dispersal by wind
      - *bristles*: Bristle-like projections
      - *bract*: REPLACE WITH ‘bracts’
      - *bracts*: When the bracts below the inflorescence are the
        dispersal appendage. For the family Poaceae “bract” refers to
        the glumes, the modified membranous bracts surrounding the
        spikelet of a grass.
      - *caruncle*: A type of eliasome, term especially used in the
        family Euphorbiaceae; an outgrowth or appendage at or near the
        hilum of certain seeds that aids in dispersal. Includes
        “strophiole”
      - *curved\_awn*: A curved, slender awn
      - *dry\_dehiscent*: Dry fruit that dehisces in some manner
      - *drupe*: Fruit with membranous exocarp, fleshy mesocarp and hard
        endocarp
      - *elaiosome*: Fleshy (often fatty) appendage on seeds that
        attracts ants
      - *enclosing\_wing*: Wing-line seed extentions that enclose the
        seed
      - *exarillate*: A seed that lacks an aril
      - *feathery\_style*: When a feathery style aids in seed dispersal
      - *fleshy\_dehiscent\_capsule*: A fleshy fruit that splits open
        and releases seeds at maturity; fruit type
      - *fleshy\_fruit*: A fruit where part or all of the pericarp
        (fruit wall) is fleshy at maturity; fruit type
      - *fleshy\_wings\_capsule*: A fruit whose capsule or wings are
        fleshy
      - *floral\_parts*: When a plant’s floral parts, the petals and/or
        sepals, are persistent and aid in seed and fruit dispersal
      - *funicle*: The stalk that joins the seed to the pod frame; aids
        in dispersal when persistent
      - *glumes*: The modified membranous bracts surrounding the
        spikelet of a grass; also see ‘bract’
      - *hairs*: Modified hairs aid in seed dispersal
      - *indehiscent*: Fruit that does not open at maturity in a
        pre-defined way, but instead relies on predation or
        decomposition to release the seeds;fruit type
      - *inflated\_parts*: When some part of the seed, fruit, or
        associated tissues is inflated, aiding in seed or fruit
        dispersal
      - *none*: When a fruit and associated tissues lack any dispersal
        appendages; see also exarillate
      - *paddles*: unknown meaning; currently used only for “Brachyscome
        dentata”
      - *pappus*: The pappus is the modified calyx, the part of an
        individual floret, that surrounds the base of the corolla tube
        in flower heads of the plant family Asteraceae
      - *placental\_endocarp*: Exact dispersal mechanism unclear;
        currently used only for “Crowea saligna” (maybe others now)
      - *pseudo-wing*: UNKNOWN
      - *receptacle*: A winged achene, a type of fruit in which a
        flattened wing of fibrous, papery tissue develops from the ovary
        wall
      - *samara*: Fleshy seed coat; term especially used for cycads;
        dispersal appendage?
      - *sarcotesta*: When scales assist in seed or fruit dispersal
      - *scales*: Seed containing a sac of air
      - *seed\_airsac*: A seed wing that arises from one side; dispersal
        appendage
      - *seed\_unilaterally\_winged*: Seed wing absent
      - *seed\_wing\_obsolete*: Modified short hairs aid in seed
        dispersal
      - *short\_hairs*: Spines that aid in seed and fruit dispersal
      - *spines*: Outgrowth of the hilum region which restricts water
        movement into and out of some seeds, especially legumes
      - *strophiole*: REPLACE WITH ‘strophiole’
      - *strophiole\_on\_seed*: Cases where a species’ style, stigma or
        carpel are persistent and aid in seed or fruit dispersal
      - *style*: A fruit where some tissue layers have become flattened
        and papery to function as wings; includes reference to a winged
        nut and winged achene (samara); fruit type
      - *winged\_fruit*: Referring to wing-like seed extensions that aid
        in wind dispersal
      - *wings*: UNSURE; assume it refers to conifers, where woody
        bracts protect the seeds
      - *woody\_bract\_scales*: Fleshy outgrowth of a seed, that often
        attracts animals like birds or ants

### dispersal\_syndrome

  - label: Dispersal syndrome
  - description: Type of dispersal syndrome displayed by species,
    although the list includes many dispersal appendages and fruit
    types. Many definitions come from Kew Botanic Gardens website.
  - type: categorical
  - allowable values:
      - *adhesion*: Dispersal syndrome where fruit is transported by
        attaching itself to something, usually an animal; true dispersal
        syndrome; will overlap with exozoochory; also called epizoochory
      - *anemochory*: Dispersal by wind; true dispersal syndrome
      - *animal\_vector*: Seeds are dispersed by an animal; true
        dispersal syndrome; should be replaced with zoochory
      - *aril*: Fleshy outgrowth of a seed, that often attracts animals
        like birds or ants; dispersal appendage
      - *atelochory*: Dispersal is prevented; also called antitelochory
      - *ballistic*: Seeds are launched away from the plant by explosion
        as soon as the seed capsule opens; true dispersal syndrome
      - *chamaechory*: diaspore is rolled along ground surface by wind
      - *dyszoochory*: diaspore is eaten intentionally by an animal
      - *elaiosome*: Fleshy (often fatty) appendage on seeds that
        attracts ants; dispersal appendage
      - *endozoochory*: Diaspores that dispersed through ingestion by
        animals, either intentionally or unintentionally and are
        transported before dropping off; includes the term ingestion;
        true dispersal syndrome
      - *exozoochory*: When seeds are dispersed on the surface of an
        animal; will overlap significantly with the terms adhesion and
        animal\_vector
      - *fleshy\_fruit*: A fruit where part or all of the pericarp
        (fruit wall) is fleshy at maturity; fruit type
      - *fleshy\_wings\_capsule*: A fruit whose capsule or wings are
        fleshy
      - *hydrochory*: Dispersal on the surface of water; see also the
        term water; true dispersal syndrome
      - *mobile*: UNKNOWN
      - *myrmecochory*: Dispersules with elaiosomes (specialised
        nutritious appendages) that make them attractive for capture,
        transport and use by ants or related insects; true dispersal
        syndrome
      - *nautohydrochory*: water; e.g. floating/submerged in
        fresh/saltwater currents
      - *ombrohydrochory*: water; diaspore is propelled by action of
        rain on plant structure/wetting by rain or dew
      - *parent\_plant\_or\_diaspore*: methods originating from parent
        plant or diaspore
      - *synzoochory*: animal; diaspore is carried intentionally
      - *unassisted*: Seeds are dispersed without assistance; true
        dispersal syndrome; same as barochory; seeds drop to the ground
        close to or beneath the parent plant
      - *undefined*: UNKNOWN
      - *vertebrate*: Seeds are dispersed by a vertebrate species; true
        dispersal syndrome
      - *water*: Seeds dispersal depends in some way on water; should
        use hydrochory instead
      - *wind*: Seeds are dispersed by wind; true dispersal syndrome;
        should use anemochory instead
      - *xerochasywater*: wetting by rain or dew; also called hygrochasy

### dispersers

  - label: Types of animals dispersing fruit
  - description: Types of animals dispersing fruit
  - type: categorical
  - allowable values:
      - *aborigines*: aborigines
      - *ants*: amt
      - *birds*: birds
      - *flying\_foxes*: flying foxes
      - *mammals*: mammals

### dormancy\_type

  - label: Dormancy type
  - description: Classification for seed dormancy
  - type: categorical
  - allowable values:
      - *morphophysiological\_dormancy*: Seeds exhibit
        morphophysiological dormancy
      - *non\_dormant*: Seeds are non-dormant
      - *physical\_dormancy*: Seeds exhibit physical dormancy
      - *physiological\_dormancy*: Seeds exhibit physiological dormancy

### embryo\_colour

  - label: Embryo colour
  - description: Binary variable distinguishing between embryos that are
    green versus colourless
  - type: categorical
  - allowable values:
      - *colourless*: Colourless embryo
      - *green*: Green embryo

### upper\_epidermis\_thickness

  - label: Upper epidermis thickness
  - description: Thickness of the upper epidermis
  - type: numeric
  - units: um
  - allowable range: 1 - 100 um

### lower\_epidermis\_thickness

  - label: Lower epidermis thickness
  - description: Thickness of the lower epidermis
  - type: numeric
  - units: um
  - allowable range: 1 - 100 um

### epithelial\_cell\_density

  - label: Epithelial cell density on the leaf surface
  - description: Epithelial cell density on the leaf surface
  - type: numeric
  - units: count/mm2
  - allowable range: 100 - 10000 count/mm2

### epithelial\_cell\_layer\_thickness

  - label: Epithelial cell layer thickness on the leaf surface
  - description: Epithelial cell layer thickness on the leaf surface
  - type: numeric
  - units: um
  - allowable range: 1 - 100 um

### fire\_response

  - label: Resprouts or is killed by fire
  - description: Distinguishes between plants that are killed by fire
    and resprout following fire
  - type: categorical
  - allowable values:
      - *fire\_killed*: Plants killed by hot fires
      - *weak\_resprouting*: Plant shows weak resprouting following fire
      - *intermediate\_resprouting*: Plant shows intermediate
        resprouting following fire
      - *strong\_resprouting*: Plant shows strong resprouting following
        fire
      - *resprouts*: Plants resprout from underground storage organ
        following fire. (For studies that don’t differentiate between
        respouting strength)

### fire\_response\_detailed

  - label: Resprouts or is killed by fire
  - description: Detailed information distinguishing between plants that
    are killed by fire and resprout following fire
  - type: categorical
  - allowable values:
      - *fire\_killed*: Plants killed by hot fires
      - *weak\_resprouting*: Plant shows weak resprouting following fire
      - *intermediate\_resprouting*: Plant shows intermediate
        resprouting following fire
      - *strong\_resprouting*: Plant shows strong resprouting following
        fire

### fire\_cued\_seeding

  - label: Fire-cued seeding
  - description: Distinguishes between plants that do and do not have
    fire-cued seeding
  - type: categorical
  - allowable values:
      - *fire\_cued\_seeding*: Plants that germinate robustly following
        fire
      - *no\_fire\_cued\_seeding*: Plants that do not show increased
        seeding following fire

### flower\_colour

  - label: Flower colour
  - description: Flower colour, with six possible outcomes
  - type: categorical
  - allowable values:
      - *blue\_purple*: blue or purple
      - *green*: green flower
      - *pink*: pink flower
      - *red\_brown*: red or brown flower
      - *white\_cream*: white or cream flower
      - *yellow\_orange*: yellow orange flower

### flower\_count\_maximum

  - label: Asymptotice flower number
  - description: Maximum flower number produced
  - type: numeric
  - units: count
  - allowable range: 0 - 1000000 count

### flowering\_time

  - label: Range of flowering period
  - description: Months during which species is flowering; keyed as a
    sequences of 12 0s (not flowering) and 1s (flowering) starting with
    January
  - type: character
  - allowable values:
      - \*\*:

### fluorescence\_Jmax\_per\_mass

  - label: Capacity for photosynthetic electron transport, measured
    through chlorophyll fluorescence, on a per mass basis
  - description: Capacity for photosynthetic electron transport,
    measured through chlorophyll fluorescence, on a per mass basis
  - type: numeric
  - units: umol/g/s
  - allowable range: 0 - 3 umol/g/s

### fluorescence\_Vcmax\_per\_mass

  - label: Maximum carboxylase activity of ribulose 1,5-bisphosphate
    carboxylase/oxygenase (Rubisco), measured through chlorophyll
    fluorescence, on a per mass basis
  - description: Maximum carboxylase activity of ribulose
    1,5-bisphosphate carboxylase/oxygenase (Rubisco), measured through
    chlorophyll fluorescence, on a per mass basis
  - type: numeric
  - units: umol/g
  - allowable range: 0 - 1 umol/g

### fluorescence\_Jmax\_over\_Vcmax

  - label: Ratio of photosynthetic electron transport capacity to
    maximum Rubisco activity, measured through chlorophyll fluorescence
  - description: Ratio of photosynthetic electron transport capacity to
    maximum Rubisco activity, measured through chlorophyll fluorescence
  - type: numeric
  - units: 1/s
  - allowable range: 0 - 5 1/s

### fruit\_mass

  - label: Fruit mass
  - description: The weight of a fruit, including the seed
  - type: numeric
  - units: mg
  - allowable range: 1 - 10000 mg

### fruit\_width

  - label: Fruit diameter
  - description: The longest width dimension of a fruit; orthogonal to
    the length
  - type: numeric
  - units: mm
  - allowable range: 0.1 - 100 mm

### fruit\_length

  - label: Fruit length
  - description: Longest fruit dimension or if clearly recognizable the
    length from its base to its apex
  - type: numeric
  - units: mm
  - allowable range: 0.1 - 1000 mm

### fruit\_breadth

  - label: Fruit breadth
  - description: The shorter width dimension of a fruit; orthogonal to
    the length
  - type: numeric
  - units: mm
  - allowable range: 0.1 - 100 mm

### fruit\_type

  - label: Fruit type
  - description: Fruit types
  - type: categorical
  - allowable values:
      - *achene*: A small, dry, indehiscent one-seeded fruit with a thin
        wall
      - *anthocarp\_nut*: A false fruit consisting of the true fruit and
        the base of the floral whorls; instance where the true fruit is
        a nut
      - *anthocarp\_viscid*: UNCERTAIN what the viscid refers to (See
        anthocarp above)
      - *berry*: Fleshy, indehiscent fruit in which the seeds are
        generally more than 1 and are not encased in a stone
      - *berry\_inf*: UNCERTAIN how this differs from general definition
        for berry
      - *capsule*: Dry fruit from compound pistil, nearly always
        dehiscent
      - *capsule\_explosive*: Dry fruit from compound pistil, that
        releases its seeds in an explosive manner
      - *capsule\_aril*: Dry fruit from compound pistil, with a fleshy
        outgrowth (aril) to attract pollinators
      - *capsule\_wing*: Dry fruit from compound pistil, with a wing for
        dispersal by wind
      - *caryopsis*: a dry one-seeded fruit in which the ovary wall is
        united with the seed coat, typical of grasses and cereals
      - *cypsela*: An achene developed from an inferior bicarpellary
        ovary fused with the calyx tube (in Asteraceae)
      - *dehiscent\_pod*: Fleshy or pulpy, indehiscent, superficially
        berry-like fruit in which 1 seed is encased in a stone or more
        than 1 seed is encased in an equal number of free or variously
        fused stones
      - *drupe*: Fibrous or dry drupe; indehiscent, superficially
        berry-like fruit in which 1 seed is encased in a stone or more
        than 1 seed is encased in an equal number of free or variously
        fused stones
      - *drupe\_fibrous*: Dry fruit from a simple pistil, dehiscent on
        generally only one side, along a single suture
      - *fig*: Fruits from a legume; a dry or somewhat fleshy, 1- to
        many-seeded fruit from a simple pistil, typically dehiscent
        longitudinally along two sutures and splitting into halves that
        remain joined at the base
      - *follicle*: Fruits from a legume; a legume (see above) that is
        indehiscent
      - *indehiscent\_pod*: Mostly dry, sometimes fleshy or pulpy,
        usually indehiscent fruit in which a single seed is encased in a
        hard shell
      - *legume*: A nut that is enclosed in a winglike bract
      - *legume\_indehiscent*: Small, dry nut or nut-like fruit, usually
        several of which are produced by a single flower; especially an
        achene
      - *mericarp*: A legume or superficially similar fruit
      - *multiple\_fruit*: A winged achene, a type of fruit in which a
        flattened wing of fibrous, papery tissue develops from the ovary
        wall
      - *nut*: A dry fruit which splits into individual carpels
      - *nut\_winged*: A fleshy hollow receptacle that develops into a
        multiple fruit, as in the fig
      - *nutlet*: A multiple fruit consisting of several united fruits,
        originating from several originally free carpels, usually fleshy
      - *pod*: Small, bladderlike, thin-walled indehiscent fruit
      - *samara*: Small, bladderlike, thin-walled indehiscent fruit,
        that is partially fleshy
      - *schizocarp*: Small, bladderlike, thin-walled indehiscent fruit,
        that is spiny
      - *syconium*: A small, dry, indehiscent one-seeded fruit with a
        thin wall
      - *syncarp*: A false fruit consisting of the true fruit and the
        base of the floral whorls; instance where the true fruit is a
        nut
      - *utricle*: UNCERTAIN what the viscid refers to (See anthocarp
        above)
      - *utricle\_fleshy*: Fleshy, indehiscent fruit in which the seeds
        are generally more than 1 and are not encased in a stone
      - *utricle\_spiny*: UNCERTAIN how this differs from general
        definition for berry

### fruit\_type\_botany

  - label: Fruit type (fleshy / dry)
  - description: Binary variable, dividing fruits into ‘dry’ versus
    ‘fleshy’ based on their appearance
  - type: categorical
  - allowable values:
      - *dry*: fruit whose appearance is dry
      - *fleshy*: fruit whose appearance is fleshy

### fruit\_type\_function

  - label: Fruit type (fleshy / dry)
  - description: Binary variable, dividing fruits into ‘dry’ versus
    ‘fleshy’ based on their function
  - type: categorical
  - allowable values:
      - *dry*: fruit that are functionally dry
      - *fleshy*: fruit that are functionally fleshy

### fruiting\_time

  - label: Plant reproductive phenology timing (fruiting)
  - description: Months during which species is fruiting; keyed as a
    sequences of 12 0s (not flowering) and 1s (flowering) starting with
    January
  - type: character
  - allowable values:
      - \*\*:

### fv\_over\_fm

  - label: Fv/Fm
  - description: Chlorophyll fluorescence measurement that indicates
    whether plant stress affects photosystem II in a dark adapted state
  - type: numeric
  - units: x/x
  - allowable range: 0.2 - 1 x/x

### germination

  - label: Germination (proportion)
  - description: Proportion of seeds that germinate
  - type: numeric
  - units: n/n
  - allowable range: 0 - 1 n/n

### germination\_treatment

  - label: Germination treatment
  - description: Seed treatment required for germination
  - type: categorical
  - allowable values:
      - *apply\_chemical*: apply generic chemical treatment
      - *crack\_seed*: same as remove seed\_coat
      - *excise\_pericarp*: heating the seed
      - *heat*: soaked
      - *imbibed*: soaked in a mild bleach solution
      - *imbibed\_bleach*: soaked in a mild solution of calcium
        hypochlorite (is this always the same as standard bleach?)
      - *imbibed\_calcium\_hypochlorite*: soaked in water that has
        boiled and then cooled slightly
      - *imbibed\_cooled\_boiling*: soaked in distilled water
      - *imbibed\_distilled*: soaked in hot, but not booiling water
      - *imbibed\_hot\_water*: soaked in near-boiling water
      - *imbibed\_near\_boiling*: soaked and seed casing removed
      - *imbibed\_remove\_casing*: soaked to leach salts
      - *imbibed\_remove\_salt*: soaked in seed primer
      - *imbibed\_seed\_primer*: soaked, then seed is nicked
      - *imbibed\_then\_nicked*: make incision into pericarp
      - *incise\_pericarp*: remove bracts surrounding the seed
      - *remove\_bracts*: remove the mericarp plug
      - *remove\_mericarp\_plug*: same as excise pericarp
      - *remove\_seed\_casing*: rinse the seed (generic)
      - *remove\_seed\_coat*: Remove or partially remove a covering that
        is not the seed coat
      - *rinse*: Remove or partially remove a covering that is not the
        seed coat. Covering sufficiently tough a vice is required to
        break it.
      - *scarify\_file\_seed\_coat*: Chip, nick, or otherwise partially
        remove the seed coat
      - *scarify\_fruit*: Sand the seed coat
      - *scarify\_fruit\_vice*: Simulate smoke conditions for a period
        using the disk method.
      - *scarify\_nick\_seed\_coat*: Soak seed in smoked water
      - *scarify\_pierce\_seed\_coat*: Seed sterilised in calcium
        hydroxide (or similar)
      - *scarify\_remove\_aril*: Seed sterilised in 10% doemestos (or
        similar)
      - *scarify\_remove\_seed\_coat*: Cold treatment
      - *scarify\_sand\_seed\_coat*: apply generic chemical treatment
      - *scarify\_seed\_coat\_vice*: same as remove seed\_coat
      - *simulate\_smoke*: heating the seed
      - *smoked\_water*: soaked
      - *sterilise\_calcium\_hydroxide*: soaked in a mild bleach
        solution
      - *sterilise\_domestos*: soaked in a mild solution of calcium
        hypochlorite (is this always the same as standard bleach?)
      - *stratification*: soaked in water that has boiled and then
        cooled slightly

### genome\_size

  - label: Plant genome size
  - description: Mass of the plant’s genome
  - type: numeric
  - units: pg
  - allowable range: 1 - 10 pg

### glaucous

  - label: Glaucous
  - description: Variable indicating if a plant’s leaves are glaucous or
    not
  - type: categorical
  - allowable values:
      - *0*: not glaucous
      - *1*: glaucous

### growth\_habit

  - label: Growth habit (data actually describes plant vegetative
    reproduction capacity)
  - description: Variable that defines a combination of growth habit and
    plant vegetative reproductive potential
  - type: categorical
  - allowable values:
      - *prostrate*: Plants that lie flat on the ground
      - *rhizomatous*: Plants having modified underground stems
      - *stoloniferous*: Plants having horizontal branches from the base
        of the plant that produce new plants from buds at its tip or
        nodes
      - *tufted*: A dense clump attached at the base or growing close
        together; can refer to grasses, shrubs, or trees

### huber\_value

  - label: Huber value
  - description: Sapwood area to leaf area ratio
  - type: numeric
  - units: mm2\_sapwood/mm2\_leaf
  - allowable range: 1e-06 - 0.1 mm2\_sapwood/mm2\_leaf

### hypocotyl\_type

  - label: Hypocotyl type
  - description: Binary variable distinguishing between glabrous versus
    hairy hypocotyls (the embryonic axis to which the cotyeledons are
    attached).
  - type: categorical
  - allowable values:
      - *glabrous*: Hypocotyl lacks hairs
      - *hairy*: Hypocotyl has hairs

### upper\_hypodermis\_thickness

  - label: Upper hypodermis thickness
  - description: Thickness of the upper hypodermis
  - type: numeric
  - units: um
  - allowable range: 1 - 100 um

### lower\_hypodermis\_thickness

  - label: Lower hypodermis thickness
  - description: Thickness of the lower hypodermis
  - type: numeric
  - units: um
  - allowable range: 1 - 100 um

### leaf\_Al\_per\_dry\_mass

  - label: Leaf aluminum (Al) content per unit leaf dry mass
  - description: Leaf aluminum (Al) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0.01 - 10000 mg/kg

### leaf\_angle

  - label: Leaf angle, relative to horizontal
  - description: Leaf angle, relative to horizontal
  - type: numeric
  - units: degrees
  - allowable range: -90 - 90 degrees

### leaf\_area

  - label: Leaf area
  - description: Area of the leaf surface
  - type: numeric
  - units: mm2
  - allowable range: 0.1 - 1e+07 mm2

### leaf\_area\_ratio

  - label: Leaf area per unit plant dry mass
  - description: Ratio of leaf area to total plant dry mass
  - type: numeric
  - units: mm2/mg
  - allowable range: 1 - 100 mm2/mg

### leaf\_absorption

  - label: Proportion of incoming light that is absorbed by the leaf
  - description: Proportion of incoming light between 400-700 nm that is
    absorbed by the leaf
  - type: numeric
  - units: umol/umol
  - allowable range: 0.01 - 1 umol/umol

### leaf\_reflectance

  - label: Proportion of incoming light that is reflected by the leaf
  - description: Proportion of incoming light between 400-700 nm that is
    reflected by the leaf
  - type: numeric
  - units: umol/umol
  - allowable range: 0.001 - 1 umol/umol

### leaf\_transmission

  - label: Proportion of incoming light that is transmitted through the
    leaf
  - description: Proportion of incoming light between 400-700 nm that is
    transmitted through the leaf
  - type: numeric
  - units: umol/umol
  - allowable range: 0.001 - 1 umol/umol

### leaf\_PRI

  - label: Photochemical Reflectance Index
  - description: The photochemical reflectance index measures plant
    responses to stress, by indicating changes in carotenoid pigments in
    live foliage.
  - type: numeric
  - units: umol/umol
  - allowable range: -1 - 1 umol/umol

### leaf\_arrangement

  - label: Arrangement of leaves
  - description: Describes leaf arrangement on the stem
  - type: categorical
  - allowable values:
      - *alternate*: A single leaf is attached at each stem node
      - *opposite*: Leaves attach to the stem in pairs
      - *reduced\_to\_scales*: Plant lacks true leaves. Leaves are
        instead small scales
      - *whorled\_tufted\_crowded*: Three or more leaves are attached at
        each node

### leaf\_B\_per\_dry\_mass

  - label: Leaf boron (B) content per unit leaf dry mass
  - description: Leaf boron (B) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0.1 - 1000 mg/kg

### leaf\_C\_per\_dry\_mass

  - label: Leaf carbon (C) content per unit leaf dry mass
  - description: Leaf carbon (C) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 50 - 750 mg/g

### leaf\_Ca\_per\_dry\_mass

  - label: Leaf calcium (Ca) content per unit leaf dry mass
  - description: Leaf calcium (Ca) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.01 - 1000 mg/g

### leaf\_cell\_wall\_fraction

  - label: Leaf cell wall fraction (fraction of cell wall material
    recovered from total leaf biomass)
  - description: Fraction of total leaf biomass that is cell wall
    material
  - type: numeric
  - units: dimensionless
  - allowable range: 0.1 - 1 dimensionless

### leaf\_cell\_wall\_N

  - label: Leaf cell wall nitrogen concentration
  - description: Proportion of leaf cell wall material that is nitrogen
  - type: numeric
  - units: mmol/g
  - allowable range: 0.1 - 10 mmol/g

### leaf\_cell\_wall\_N\_fraction

  - label: Leaf cell wall nitrogen fraction
  - description: Proportion of all N in leaves that is found in the leaf
    cell walls
  - type: numeric
  - units: dimensionless
  - allowable range: 0.01 - 1 dimensionless

### leaf\_CN\_ratio

  - label: Leaf carbon/nitrogen (C/N) ratio
  - description: Leaf carbon/nitrogen (C/N) ratio
  - type: numeric
  - units: g/g
  - allowable range: 10 - 1000 g/g

### leaf\_compoundness

  - label: Leaf compoundness
  - description: Indicates whether or not a leaf is compound; different
    ‘simple’ terminology used by different studies
  - type: categorical
  - allowable values:
      - *compound*: A leaf that is divided into multiple leaflets
      - *leafless*: A stem that lacks leaves
      - *lobed*: A leaf that is lobed but not compound
      - *palmate*: A leaf that is palmately compound
      - *phyllode*: Stem segment that functions like a leaf
      - *simple*: A leaf with a single undivided blade
      - *trifoliate*: A leaf that is divided into three leaflets; a
        special category of compound
      - *unifoliate*: A leaf that consists of a single leaflet; a
        special category of compound

### leaf\_Cu\_per\_dry\_mass

  - label: Leaf copper (Cu) content per unit leaf dry mass
  - description: Leaf copper (Cu) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0.01 - 150 mg/kg

### leaf\_dark\_respiration\_per\_area

  - label: Leaf respiration rate per unit leaf area
  - description: Leaf respiration rate per unit leaf area
  - type: numeric
  - units: umolCO2/m2/s
  - allowable range: 1e-05 - 10 umolCO2/m2/s

### leaf\_dark\_respiration\_per\_dry\_mass

  - label: Leaf respiration rate per unit leaf dry mass
  - description: Leaf respiration rate per unit leaf dry mass
  - type: numeric
  - units: umolCO2/g/s
  - allowable range: 0.001 - 100 umolCO2/g/s

### leaf\_light\_respiration\_per\_area

  - label: Leaf respiration rate per unit leaf area under light
  - description: Leaf respiration rate per unit leaf area under light
  - type: numeric
  - units: umolCO2/m2/s
  - allowable range: 1e-05 - 10000 umolCO2/m2/s

### leaf\_delta13C

  - label: Leaf carbon (C) isotope signature (delta 13C)
  - description: Leaf carbon stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -50 - 0 per mille

### leaf\_delta15N

  - label: Leaf nitrogen (N) isotope signature (delta 15N)
  - description: Leaf nitrogen stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -25 - 75 per mille

### leaf\_density

  - label: Leaf tissue density
  - description: Leaf tissue density
  - type: numeric
  - units: mg/mm3
  - allowable range: 0.1 - 10 mg/mm3

### leaf\_dry\_mass

  - label: Leaf dry mass
  - description: Leaf dry mass
  - type: numeric
  - units: mg
  - allowable range: 0.1 - 15000 mg

### leaf\_dry\_matter\_content

  - label: Leaf dry mass per unit leaf fresh mass (Leaf dry matter
    content, LDMC)
  - description: Leaf dry mass per unit leaf fresh mass. (See also
    leaf\_water\_content\_per\_mass, the ratio of water content to leaf
    dry mass, recorded by some studies.)
  - type: numeric
  - units: g/g
  - allowable range: 0.001 - 20 g/g

### leaf\_elastic\_modulus

  - label: Leaf elastic modulus
  - description: The ratio of the change in cell turgor relative to the
    change in cell volume
  - type: numeric
  - units: mmol/m2/s
  - allowable range: 0.1 - 100 mmol/m2/s

### leaf\_Fe\_per\_dry\_mass

  - label: Leaf iron (Fe) content per unit leaf dry mass
  - description: Leaf iron (Fe) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 10000 mg/kg

### leaf\_fracture\_force

  - label: Leaf force of fracture
  - description: Measures of leaf mechanical resistant; how much force
    is required to shear, punch or rip a leaf
  - type: numeric
  - units: N
  - allowable range: 0.01 - 10 N

### leaf\_fresh\_mass

  - label: Leaf fresh mass
  - description: Leaf fresh mass
  - type: numeric
  - units: mg
  - allowable range: 0.1 - 25000 mg

### leaf\_fresh\_mass\_per\_area

  - label: Leaf fresh mass per leaf area
  - description: Leaf fresh mass per leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 200 - 2000 g/m2

### leaf\_hairs\_adult

  - label: Presense of hairs on adult leaves
  - description: Binary variable describing whether or not adult leaves
    have hairs
  - type: categorical
  - allowable values:
      - *0*: leaves on adult plants do not have hairs
      - *1*: leaves on adult plants do have hairs

### leaf\_hairs\_juvenile

  - label: Presense of hairs on juvenile leaves
  - description: Binary variable describing whether or not juvenile
    leaves have hairs
  - type: categorical
  - allowable values:
      - *0*: leaves on juvenile plants do not have hairs
      - *1*: leaves on juvenile plants do have hairs

### leaf\_hydraulic\_conductivity

  - label: Leaf hydraulic conductivity
  - description: Measure of how efficiently water is transported through
    the leaf, determined as the ratio of water flow rate through the
    leaf to the difference in water potential across the leaf,
    standardised to leaf area.
  - type: numeric
  - units: mmol/m2/s/MPa
  - allowable range: 0.1 - 100 mmol/m2/s/MPa

### leaf\_hydraulic\_vulnerability

  - label: Leaf hydraulic vulnerability
  - description: The leaf water potential value at which leaf hydraulic
    conductance have declined by 50% from the mean maximum rate
  - type: numeric
  - units: neg\_MPa
  - allowable range: 0.001 - 40 neg\_MPa

### leaf\_K\_per\_area

  - label: Leaf potassium (K) content per unit leaf area
  - description: Leaf potassium (K) content per unit leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.1 - 10 g/m2

### leaf\_K\_per\_dry\_mass

  - label: Leaf potassium (K) content per unit leaf dry mass
  - description: Leaf potassium (K) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### leaf\_length

  - label: Leaf length
  - description: Length of the leaf, including petiole and rachis in
    compound leaves
  - type: numeric
  - units: mm
  - allowable range: 0.1 - 1e+05 mm

### leaf\_lifespan

  - label: Leaf lifespan (longevity)
  - description: Leaf lifespan (longevity)
  - type: numeric
  - units: month
  - allowable range: 1 - 1000 month

### leaf\_lignin\_per\_dry\_mass

  - label: Leaf lignin per unit leaf dry mass
  - description: Leaf lignin per unit leaf dry mass
  - type: numeric
  - units: mg/mg
  - allowable range: 0.001 - 1 mg/mg

### leaf\_margin

  - label: Leaf margin
  - description: Description of leaf margin are lobed versus entire.
    Trait values could be expanded to include toothed as a separate
    category
  - type: categorical
  - allowable values:
      - *entire*: Leaf margin without lobes or toothed
      - *lobed*: Leaf margin that is either lobed or toothed

### leaf\_mass\_fraction

  - label: Leaf mass fraction
  - description: Ratio of leaf dry mass to total plant dry mass
  - type: numeric
  - units: g/g
  - allowable range: 0.1 - 1 g/g

### leaf\_mass\_to\_stem\_mass

  - label: Ratio of leaf dry mass to stem dry mass
  - description: Ratio of leaf dry mass to stem dry mass
  - type: numeric
  - units: g/g
  - allowable range: 0.1 - 10 g/g

### leaf\_mesophyll\_conductance\_per\_area

  - label: Mesophyll conductance per unit leaf area
  - description: Rate of CO2 movement between chloroplasts and
    substomatal cavities (intracellular space), per unit leaf area
  - type: numeric
  - units: mmolCO2/g/s
  - allowable range: 0.01 - 1 mmolCO2/g/s

### leaf\_mesophyll\_conductance\_per\_mass

  - label: Mesophyll conductance per unit leaf mass
  - description: Rate of CO2 movement between chloroplasts and
    substomatal cavities (intracellular space), per unit leaf mass
  - type: numeric
  - units: molCO2/m2/s
  - allowable range: 0.01 - 10 molCO2/m2/s

### leaf\_Mg\_per\_dry\_mass

  - label: Leaf magnesium (Mg) content per unit leaf dry mass
  - description: Leaf magnesium (Mg) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.01 - 100 mg/g

### leaf\_Mn\_per\_dry\_mass

  - label: Leaf manganese (Mn) content per unit leaf dry mass
  - description: Leaf manganese (Mn) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 10000 mg/kg

### leaf\_Mo\_per\_dry\_mass

  - label: Leaf molybdenum (Mo) content per unit leaf dry mass
  - description: Leaf molybdenum (Mo) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0 - 100 mg/kg

### leaf\_N\_per\_area

  - label: Leaf nitrogen (N) content per unit leaf area
  - description: Leaf nitrogen (N) content per unit leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.2 - 20 g/m2

### leaf\_N\_per\_dry\_mass

  - label: Leaf nitrogen (N) content per unit leaf dry mass
  - description: Leaf nitrogen (N) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### leaf\_Na\_per\_dry\_mass

  - label: Leaf sodium (Na) content per unit leaf dry mass
  - description: Leaf sodium (Na) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 20000 mg/kg

### leaf\_Ni\_per\_dry\_mass

  - label: Leaf nickel (Ni) content per unit leaf dry mass
  - description: Leaf nickel (Ni) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 10 - 1e+05 mg/kg

### leaf\_P\_per\_area

  - label: Leaf phosphorus (P) content per unit leaf area
  - description: Leaf phosphorus (P) content per unit leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.005 - 1 g/m2

### leaf\_P\_per\_dry\_mass

  - label: Leaf phosphorus (P) content per unit leaf dry mass
  - description: Leaf phosphorus (P) content per unit leaf dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.03 - 12 mg/g

### leaf\_phenology

  - label: Leaf phenology type (evergreen, deciduous, semideciduous)
  - description: Variable indicating whether a plant has deciduous
    versus evergreen leaves; different types of deciduousness included
    as trait values
  - type: categorical
  - allowable values:
      - *deciduous*: Plant where all leaves are shed yearly, either due
        to drought or cold
      - *evergreen*: Plant which retains its leaves year-round
      - *facultative\_drought\_deciduous*: Plant that sometimes sheds
        its years in response to drought
      - *semi\_deciduous*: Plant that sheds its years each year for just
        a very brief period of time

### leaf\_photosynthetic\_nitrogen\_use\_efficiency

  - label: Leaf photosynthesis rate per unit leaf nitrogen (N) content
    (photosynthetic nitrogen use efficiency, PNUE)
  - description: Ratio of photosynthesis (CO2 assimilation rate) to leaf
    nitrogen content
  - type: numeric
  - units: umolCO2/molN/s
  - allowable range: 0.1 - 800 umolCO2/molN/s

### leaf\_photosynthetic\_nitrogen\_use\_efficiency\_maximum

  - label: Leaf photosynthesis rate per unit leaf nitrogen (N) content
    (photosynthetic nitrogen use efficiency, PNUE) at saturating light
    and CO2 conditions
  - description: Ratio of photosynthesis (CO2 assimilation rate) to leaf
    nitrogen content at saturating light and CO2 conditions
  - type: numeric
  - units: umolCO2/molN/s
  - allowable range: 0.1 - 800 umolCO2/molN/s

### leaf\_photosynthetic\_nitrogen\_use\_efficiency\_saturated

  - label: Leaf photosynthesis rate per unit leaf nitrogen (N) content
    (photosynthetic nitrogen use efficiency, PNUE) at saturating light
    conditions but ambient CO2 conditions
  - description: Ratio of photosynthesis (CO2 assimilation rate) to leaf
    nitrogen content at saturating light conditions but ambient CO2
    conditions
  - type: numeric
  - units: umolCO2/molN/s
  - allowable range: 0.1 - 800 umolCO2/molN/s

### leaf\_photosynthetic\_phosphorus\_use\_efficiency

  - label: Photosynthetic phosphorus use efficiency
  - description: Ratio of photosynthetic rate to leaf phosphorus
    concentration
  - type: numeric
  - units: umolCO2/molP/s
  - allowable range: 500 - 50000 umolCO2/molP/s

### leaf\_photosynthetic\_phosphorus\_use\_efficiency\_maximum

  - label: Leaf photosynthesis rate per unit leaf phosphorus (P) content
    (photosynthetic phosphorus use efficiency, PPUE) at saturating light
    and CO2 conditions
  - description: Ratio of photosynthesis (CO2 assimilation rate) to leaf
    phosphorus content at saturating light and CO2 conditions
  - type: numeric
  - units: umolCO2/molP/s
  - allowable range: 500 - 50000 umolCO2/molP/s

### leaf\_photosynthetic\_phosphorus\_use\_efficiency\_saturated

  - label: Leaf photosynthesis rate per unit leaf phosphorus (P) content
    (photosynthetic phosphorus use efficiency, PPUE) at saturating light
    conditions but ambient CO2 conditions
  - description: Ratio of photosynthesis (CO2 assimilation rate) to leaf
    phosphorus content at saturating light conditions but ambient CO2
    conditions
  - type: numeric
  - units: umolCO2/molP/s
  - allowable range: 500 - 50000 umolCO2/molP/s

### leaf\_photosynthetic\_water\_use\_efficiency

  - label: Leaf photosynthesis rate per unit leaf transpiration (water
    use efficiency, WUE)
  - description: WUE; Ratio of photosynthesis (CO2 assimilation rate) to
    leaf transpiration (water loss)
  - type: numeric
  - units: umolCO2/mmolH2O
  - allowable range: 0.01 - 1 umolCO2/mmolH2O

### leaf\_S\_per\_dry\_mass

  - label: Leaf sulphur (S) content per unit leaf dry mass
  - description: Leaf sulphur (S) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 100 - 50000 mg/kg

### leaf\_saturated\_water\_content\_per\_mass

  - label: Water content per unit mass of saturated leaf
  - description: Ratio of water in a saturated leaf (maximal water
    holding capacity at full turgidity) to leaf dry mass
  - type: numeric
  - units: g/g
  - allowable range: 0.1 - 10 g/g

### stem\_saturated\_water\_content\_per\_mass

  - label: Water content per unit mass of saturated stem
  - description: Ratio of water in a saturated stem (maximal water
    holding capacity at full turgidity) to stem dry mass
  - type: numeric
  - units: g/g
  - allowable range: 0.1 - 10 g/g

### bark\_saturated\_water\_content\_per\_mass

  - label: Water content per unit mass of saturated bark
  - description: Ratio of water in a saturated bark (maximal water
    holding capacity at full turgidity) to bark dry mass
  - type: numeric
  - units: g/g
  - allowable range: 0.1 - 10 g/g

### leaf\_shape

  - label: Leaf shape
  - description: Leaf shape
  - type: categorical
  - allowable values:
      - *acicular*: Needle-shaped
      - *article*: Leaf comprised of jointed segments, as in family
        Casuarinaceae
      - *branchlets\_articles*: Branches with article leaves, as in
        family Casuarinaceae
      - *broadly\_lanceolate*: Like lanceolate, but broader
      - *broadly\_obovate*: Like obovate, but broader
      - *broadly\_ovate*: Like ovate, but broader
      - *circular*: Circular
      - *cladode*: A flattened stem or internode that resembles and
        functions as a leaf
      - *clavate*: Slender, broadening towards the tip; club-shaped,
        with the thickened part at the tip
      - *cordate*: Heart-shaped
      - *cuneate*: Triangular, wedge-shaped, stem attaches to point
      - *deltoid*: Triangular, stem attaches to side
      - *dimidate*: A leaf in which only one side is developed
      - *elliptical*: Oval shaped, without a distinct point
      - *elliptical-oblong*: Oval shaped, without a distinct point, but
        more elongated
      - *falcate*: Sickle-shaped
      - *filiform*: Thread-like or filament-shaped
      - *fusiform*: Spindle-shaped and tapering at both ends
      - *half\_terete*: Semi-circular in cross-section
      - *hastate*: Spear-shaped - pointed, with barbs, shaped like a
        spear point, with flaring pointed lobes at the base
      - *lanceolate*: Long, wider in the middle, shaped like a lance tip
      - *leafless*: No leaf present
      - *linear*: Long and very narrow like a blade of grass
      - *linear\_elliptical*: A particularly long, narrow elliptical
        leaf
      - *linear\_oblanceolate*: A particularly long, narrow oblanceolate
        leaf
      - *linear\_obovate*: A particularly long, narrow obovate leaf
      - *lobed*: Being divided by clefts, may be pinnately lobed or
        palmately lobed
      - *lorate*: Having the form of a strap
      - *narrowly\_cuneate*: like cuneate, but narrower
      - *narrowly\_elliptical*: like elliptical, but narrower
      - *narrowly\_oblong*: like oblong, but narrower
      - *narrowly\_obovate*: like obovate, but narrower
      - *narrowly\_obtrullate*: like obtrullate, but narrower
      - *narrowly\_ovate*: like ovate, but narrower
      - *narrowly\_subulate*: like subulate, but narrower
      - *narrowly\_rhomboidal*: like rhomboidal, but narrower
      - *narrowly\_triangular*: like triangular, but narrower
      - *needle*: Needle-shaped
      - *obcordate*: Heart-shaped, stem attaches at the tapering end
      - *oblanceolate*: Much longer than wide and with the widest
        portion near the tip, reversed lanceolate
      - *oblate*: An elongate leaf that is wider in the middle than at
        the two ends
      - *oblong*: Having an elongated form with slightly parallel sides,
        roughly rectangular
      - *obtriangular*: A triangular leaf where the stem attaches to the
        tapering end
      - *obtrullate*: In the form of a reversed trowel (with longer
        sides meeting at the base)
      - *obovate*: Teardrop-shaped, stem attaches to the tapering end;
        reversed ovate
      - *obovoid*: A 3-dimensional shape; teardrop-shaped, stem attaches
        to the tapering end
      - *orbicular*: Circular
      - *ovate*: Oval, egg-shaped, with a tapering point and the widest
        portion near the petiole
      - *palm*: UNCERTAIN if this refers to a leaf from a palm
        (Araceeae) or a palmately lobed leaf
      - *palmately\_lobed*: Palm-shaped, i.e., with lobes or leaflets
        stemming from the leaf base
      - *peltate*: A round leaf where the petiole attaches near the
        center
      - *phyllode*: Stem segment that functions like a leaf
      - *reniform*: Shaped like a kidney - an oval with an inward curve
        on one side
      - *rhomboidal*: Diamond-shaped
      - *scale*: Scale leaf
      - *spathulate*: Spoon-shaped; having a broad flat end which tapers
        to the base
      - *spine*: Leaf reduced to a spine
      - *straight*: Leaf with very straight sides
      - *subulate*: Slender and tapering to a point; awl-shaped.
      - *terete*: Circular in cross-section; more or less cylindrical
        without grooves or ridges
      - *terete\_compressed*: Like terete, but compressed
      - *triangular*: Triangular leaf where stem attaches to a side;
        similar to deltoid
      - *trigonous\_terete*: Terete, but triangular in cross section
      - *widely\_obovate*: like obovate, but wider
      - *widely\_obtrullate*: In the form of a reversed trowel (with
        longer sides meeting at the base), but wider

### leaf\_specific\_conductivity

  - label: Leaf specific hydraulic conductivity (Kl)
  - description: Kl; the ratio of leaf hydraulic conductivity to the
    leaf area distil to the segment
  - type: numeric
  - units: 10^4 x kg/m/s/MPa
  - allowable range: 0.1 - 100 10^4 x kg/m/s/MPa

### leaf\_spines\_adult

  - label: Presense of spines on adult leaves
  - description: Binary variable describing whether or not adult leaves
    have spines
  - type: categorical
  - allowable values:
      - *0*: leaves on adult plants do not have spines
      - *1*: leaves on adult plants do have spines

### leaf\_thickness

  - label: Leaf thickness
  - description: Thickness of the leaf lamina
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 51 mm

### leaf\_toughness

  - label: Leaf toughness
  - description: The work (energy) required to fracture a leaf by
    tearing, shearing or punching
  - type: numeric
  - units: N/m
  - allowable range: 100 - 10000 N/m

### leaf\_transpiration

  - label: Leaf transpiration under ambient conditions
  - description: Rate of water loss from leaf under ambient conditions
  - type: numeric
  - units: mmol/m2/s
  - allowable range: 1e-04 - 10000 mmol/m2/s

### leaf\_transpiration\_at\_Amax

  - label: Leaf transpiration during Amax measurement
  - description: Rate of water loss from leaf during Amax measurement
  - type: numeric
  - units: mmol/m2/s
  - allowable range: 1e-04 - 10000 mmol/m2/s

### leaf\_transpiration\_at\_Asat

  - label: Leaf transpiration during Asat measurement
  - description: Rate of water loss from leaf during Asat measurement
  - type: numeric
  - units: mmol/m2/s
  - allowable range: 1e-04 - 10000 mmol/m2/s

### leaf\_turgor\_loss\_point

  - label: Leaf turgor loss point
  - description: Water potential at which a leaf loses turgor
  - type: numeric
  - units: neg\_MPa
  - allowable range: 0.01 - 100 neg\_MPa

### leaf\_type

  - label: Leaf type
  - description: Broad definitions of leaf type
  - type: categorical
  - allowable values:
      - *article*: Leaf comprised of jointed segments
      - *broadleaf*: Flat leaf lamina
      - *leafless*: Lacks leaves
      - *needle*: Needle or awl-shaped leaf
      - *phyllode*: Winged leaf stalk which functions as a leaf
      - *scale\_leaf*: A small flat leaf resembling a scale

### leaf\_water\_content\_per\_area

  - label: Leaf water content per unit area
  - description: The ratio of the mass of water in a leaf relative to
    its surface area
  - type: numeric
  - units: g/m2
  - allowable range: 10 - 1000 g/m2

### leaf\_water\_content\_per\_mass

  - label: Leaf water content per unit mass
  - description: The ratio of the mass of water in a leaf relative to
    its dry mass. (See also leaf\_dry\_matter\_content, the ratio of a
    leaf’s dry mass to fresh mass, that is recorded by a greater number
    of studies.)
  - type: numeric
  - units: g/g
  - allowable range: 0.01 - 10 g/g

### leaf\_width

  - label: Leaf width
  - description: The longest width axis of a leaf; orthogonal to its
    length
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 1e+05 mm

### leaf\_Zn\_per\_dry\_mass

  - label: Leaf zinc (Zn) content per unit leaf dry mass
  - description: Leaf zinc (Zn) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0.1 - 1000 mg/kg

### life\_form

  - label: Life form
  - description: Raunkiaer classification; Categorical classification of
    plants according to shoot-apex or bud protection
  - type: categorical
  - allowable values:
      - *aerophytes*: Plants that obtain moisture and nutrients from the
        air and rain
      - *chamaephyte*: Plants with buds on persistent shoots near the
        soil surface
      - *cryptophytes*: plants have subterranean or under water resting
        buds
      - *epiphyte*: Plant that grows on the surface of another plant
      - *geophyte*: Plants with bulbs that rest in dry soil as a
        rhizome, bulb or corm
      - *helophytes*: Plants with bulbs that rest in marshy or wet soil
      - *hemicryptophyte*: Plants with buds at or near the soil surface
      - *hydrophytes*: Plants with bulbs that rest submerged under water
      - *phanerophyte*: Plants with dominant buds on branches which
        project freely into the air; normally woody perennials
      - *therophyte*: Annual plants that complete their lives rapidly in
        favourable conditions and survive the unfavourable cold or dry
        season in the form of seeds

### life\_history

  - label: Life history
  - description: Categorical description of plant’s life history
  - type: categorical
  - allowable values:
      - *annual*: Plant that lives for only a single year
      - *biennial*: Plant with a two-year lifespan
      - *ephemeral*: Very short-lived plant; the lifespan is usually
        only a few months
      - *indefinite*: Very long-lived plant, whose exact lifespan is
        unknown. Category also includes plants that reproduce
        vegetatively, such that their ‘genetic’ lifespan is indefinite
      - *long\_lived*: Very long-lived plant, whose exact lifespan is
        unknown
      - *perennial*: Plant that lives for 3 or more growing seasons
      - *short\_lived\_perennial*: A short-lived perennial
      - *short\_lived*: Unclear term that only suggests a plant is not
        long-lived - could be ephemeral, annual or biennial

### lifespan

  - label: Life span (years)
  - description: Broad categories of plant life span, in years
  - type: categorical
  - allowable values:
      - *\<1*: Plant that lives less than 1 year
      - *\<5*: Plant that lives less than 5 years
      - *\<10*: Plant that lives less than 10 years
      - *\<20*: Plant that lives less than 20 years
      - *\<50*: Plant that lives less than 50 years
      - *\<100*: Plant that lives less than 100 years
      - *\>100*: Plant that lives more than 100 years
      - *?*: Plant with unknown lifespan

### mating\_system

  - label: Plant mating system
  - description: Plant mating system
  - type: categorical
  - allowable values:
      - *andromonoecious*: Plants with hermaphrodite flowers and
        staminate flowers (producing just male gametes) on the same
        plant but no pistillate (female) flowers.
      - *dioecious*: Plants that produce male and female flowers on
        separate plants
      - *gynomonoecious*: Plants with hermaphrodite flowers and
        pistillate flowers (producing just female gametes) on the same
        plant but no staminate (male) flowers.
      - *hermaphrodite*: Flowers can produce both male and female
        gametes
      - *monoecious*: Plants that have both male and female flowers on
        the same plant, but an individual flower produces only male or
        female gamets.

### modified\_NDVI

  - label: modified normalized difference vegetation index (modified
    NDVI)
  - description: modified normalized difference vegetation index
    (modified NDVI), based on Landsat data
  - type: numeric
  - units: dimensionless
  - allowable range: 0.1 - 1 dimensionless

### modulus\_of\_elasticity

  - label: Bulk modulus of elasticity (Young’s modulus)
  - description: A measure of xylem’s resistance to being deformed
    elastically (i.e., non-permanently) when a stress is applied to it
  - type: numeric
  - units: Pa
  - allowable range: 1 - 100000 Pa

### modulus\_of\_rupture

  - label: Bulk modulus of rupture
  - description: A measure of the force required to rupture xylem vessel
  - type: numeric
  - units: Pa
  - allowable range: 1 - 100000 Pa

### nitrogen\_fixing

  - label: Plant nitrogen fixation capacity
  - description: Binary variable describing whether or not a plant hosts
    a nitrogen-fixing bacteria
  - type: categorical
  - allowable values:
      - *0*: plant does not exhibit nitrogen-fixation
      - *1*: plant does exhibit nitrogen-fixation

### N\_to\_P\_ratio

  - label: Ratio of N to P dry mass
  - description: Ratio of N to P dry mass
  - type: numeric
  - units: dimensionless
  - allowable range: 1 - 100 dimensionless

### osmotic\_potential

  - label: Osmotic potential
  - description: Potential for water to move across a semi-permeable
    membrane based on solute concentration
  - type: numeric
  - units: MPa
  - allowable range: -10 - 0 MPa

### palisade\_cell\_length

  - label: Length of individual palisade cells
  - description: Length of individual palisade cells
  - type: numeric
  - units: um
  - allowable range: 10 - 1000 um

### lower\_palisade\_cell\_thickness

  - label: Lower palisade cell thickness
  - description: Thickness (length) of lower palisade cells
  - type: numeric
  - units: um
  - allowable range: 10 - 1000 um

### upper\_palisade\_cell\_thickness

  - label: Upper palisade cell thickness
  - description: Thickness (length) of upper palisade cells
  - type: numeric
  - units: um
  - allowable range: 10 - 1000 um

### palisade\_cell\_width

  - label: Width of individual palisade cells
  - description: Width of individual palisade cells
  - type: numeric
  - units: um
  - allowable range: 1 - 50 um

### palisade\_layer\_number

  - label: Number of layers of palisade cells
  - description: Number of layers of palisade cells
  - type: numeric
  - units: count
  - allowable range: 1 - 10 count

### photosynthetic\_pathway

  - label: Photosynthetic pathway
  - description: Type of photosynthetic pathway displayed by plants
  - type: categorical
  - allowable values:
      - *c3*: Plant using the ‘standard’ photosynthetic pathway, where a
        3-carbon compound is produced after the first stage in the
        photosynthetic pathway
      - *c4*: Plants in which the photosynthetic light-dependent
        reactions and the Calvin cycle are physically separated to
        reduce photo respiration
      - *c3-c4*: Species that shift between c3 and c4 photosynthesis
      - *cam*: Plants which display crassulacean acid metabolism
      - *c3-cam*: Species that shift between c3 and cam photosynthesis
      - *c4-cam*: Species that shift between c4 and cam photosynthesis

### photosynthetic\_rate\_per\_area

  - label: Leaf photosynthesis rate per unit leaf area
  - description: Rate at which a plant consumes carbon dioxide through
    photosynthesis, normalised to leaf area
  - type: numeric
  - units: umolCO2/m2/s
  - allowable range: 0.25 - 50 umolCO2/m2/s

### photosynthetic\_rate\_per\_area\_maximum

  - label: Leaf photosynthesis rate per unit leaf area at saturating
    light and CO2 conditions
  - description: Rate at which a plant consumes carbon dioxide through
    photosynthesis at saturating light and CO2 conditions, normalised to
    leaf area
  - type: numeric
  - units: umolCO2/m2/s
  - allowable range: 0.25 - 85 umolCO2/m2/s

### photosynthetic\_rate\_per\_area\_saturated

  - label: Leaf photosynthesis rate per unit leaf area at saturating
    light conditions
  - description: Rate at which a plant consumes carbon dioxide through
    photosynthesis at saturating light conditions but ambient CO2
    conditions, normalised to leaf area
  - type: numeric
  - units: umolCO2/m2/s
  - allowable range: 0.25 - 85 umolCO2/m2/s

### photosynthetic\_rate\_per\_dry\_mass

  - label: Leaf photosynthesis rate per unit leaf dry mass
  - description: Maximum rate at which a plant consumes carbon dioxide
    through photosynthesis, normalised to leaf dry mass
  - type: numeric
  - units: umolCO2/g/s
  - allowable range: 0.01 - 1 umolCO2/g/s

### photosynthetic\_rate\_per\_dry\_mass\_maximum

  - label: Leaf photosynthesis rate per unit leaf dry mass at saturating
    light and CO2 conditions
  - description: Maximum rate at which a plant consumes carbon dioxide
    through photosynthesis at saturating light and CO2 conditions,
    normalised to leaf dry mass
  - type: numeric
  - units: umolCO2/g/s
  - allowable range: 0.01 - 1 umolCO2/g/s

### photosynthetic\_rate\_per\_dry\_mass\_saturated

  - label: Leaf photosynthesis rate per unit leaf dry mass at saturating
    light conditions
  - description: Maximum rate at which a plant consumes carbon dioxide
    through photosynthesis at saturating light conditions but ambient
    CO2 conditions, normalised to leaf dry mass
  - type: numeric
  - units: umolCO2/g/s
  - allowable range: 0.01 - 1 umolCO2/g/s

### plant\_growth\_form

  - label: Plant Growth Form
  - description: Different growth forms displayed by plants, including
    both standard plant growth form descriptors (tree, shrub, etc.) and
    specific plant characteristics (i.e. patristic)
  - type: categorical
  - allowable values:
      - *aquatic*: Grows primarily in water
      - *aquatic\_herb*: Herb that grows primarily in water
      - *climber*: Plant that climbs up another plant’s trunk, branches,
        rather than being able to support itself
      - *climber\_tree*: Tree that climbs up another plant
      - *climber\_herb*: Herbaceous plant that climbs up another plant
      - *climber\_shrub*: Shrubby plant that climbs up another plant
      - *creeper*: Creeper that roots along stems
      - *cycad*: Cycad
      - *epiphyte*: Plant that grows on top of another plant
      - *fern*: A group of vascular plants that reproduce via spores
      - *geophyte*: A perennial plant with an underground food storage
        organ, such as a bulb, tuber, corm, or rhizome
      - *graminoid*: Herbaceous plants with a grass-like morphology
      - *grass-tree*: Term specific refers to members of the family
        Xanthorrhoaceae, monocots with a tree-like growth-form
      - *hemi-epiphyte*: Plant that partially grows on top of another
        plant
      - *herb*: A seed-bearing plant which does not have a woody stem
        and dies down to the ground after flowering
      - *palm*: Flowering plants in the family Arecaceae, most with
        large, compound evergreen leaves referred to as fronds
      - *parasite*: A plant that derives some or all of its nutritional
        requirements from another living plant
      - *prostrate*: Plant that lies flat against the ground
      - *subshrub*: A dwarf shrub, especially one that is only woody at
        the base
      - *succulent*: A plant where some parts are thick and fleshy and
        usually involved in water storage
      - *shrub*: A woody plant that has multiple stems arising at or
        near ground level
      - *treelet*: A woody plant that is intermediate between a shrub
        and a tree
      - *tree*: A tall, woody, perennial plant, usually with a single
        main trunk

### plant\_height

  - label: Plant height
  - description: Vegetative plant height
  - type: numeric
  - units: m
  - allowable range: 0.001 - 130 m

### plant\_width

  - label: Plant width
  - description: The width of the plant canopy
  - type: numeric
  - units: m
  - allowable range: 0.001 - 20 m

### plant\_breadth

  - label: Plant breadth
  - description: The breadth of the plant canopy (smaller of two
    dimensions)
  - type: numeric
  - units: m
  - allowable range: 0.001 - 20 m

### pollination\_syndrome

  - label: Pollination syndrome
  - description: Pollination syndrome
  - type: categorical
  - allowable values:
      - *animal*: Generic cath-all for any form of animal pollination
      - *wind*: wind-pollinated

### regen\_strategy

  - label: Regeneration strategy
  - description: Different regeneration strategies displayed by plants.
    Trait values include both generic terms and quite specific ones. See
    Pausus, Lamont et al. 2018, doi.org/10.1111/nph.14982 for trait
    values used and detailed desciptions of recolonization ability and
    level of fire protection provided by each regeneration strategy.
    This trait includes terminology for storage organs and regeneration
    strategies following fire. The trait “fire\_response” is a binary
    trait distinguishing between fire-killed and regenerating species.
  - type: categorical
  - allowable values:
      - *aboveground*: Plants that resprout from an aboveground organ,
        but the type of organ isn’t specified.
      - *aboveground\_caudex*: Plant regenerates from above-ground
        apical meristems. Also termed a caudex. Term refers to palm-like
        plants with aerial buds that resprout, including grass trees,
        palms, tree ferns, and pandanus.
      - *basal*: Plant regenerates from the base of the stem (versus
        underground). Generally synonymous with “lignotuber”, especially
        among Australian plants.
      - *belowground\_caudex*: Plant regenerates from belowe-ground
        caudex.
      - *bud-bearing\_root*: Lateral root that produces vertical suckers
      - *bulb*: Below ground storage organ derived from stem (and
        sometimes leaf) tissue.
      - *corm*: Below ground storage organ derived from stem tissue.
      - *epicormic*: Plants that resprout from epicormic buds; these are
        buds beneath the bark on stems that survive fire
      - *fleshy\_underground\_organ*: Plants that use fleshy, generally
        underground organs (swellings) to regenerate; examples include
        corms, tubers, bulbs, and caudex
      - *lignotuber*: Swollen woody structures are located at the
        transition between the stem base and root crown of woody shrubs,
        mallees and small trees, and are formed from stem tissues. Buds
        are located over the entire structure. No colonisation ability.
      - *fire\_killed*: Plant that is fire-killed and regenerates only
        by seed. (An obligate re-seeder.)
      - *resprouts*: Plant reprouts following fire, but the mechanism is
        not specified.
      - *rhizome*: Plant has an underground stem; this stem may be woody
        or non-woody. Rhizomes are both a mechanism for vegetative
        spread and a strategy to survive fire.
      - *root\_crown*: Plant that produces a few vertical shoots from
        the root crown.
      - *root\_tuber*: Below ground storage organ derived from root
        tissue.
      - *seed*: Plant that regenerate only or primarily by seed. These
        are mostly fire-killed species, but the not always
      - *stem\_tuber*: Below ground storage organ derived from stem
        tissue.
      - *woody\_rootstock*: Plant that regenerate by resprouting from
        wood rootstock.
      - *woody\_rootstock\_or\_lignotuber*: Plant that regenerates by
        resprouting from either a woody rootstock or lignotuber.
      - *no\_storage\_organ*: No storage organ present
      - *storage\_organ*: Storage organ present, but the type is unknown
      - *underground*: Plants that resprout from an underground organ,
        but the type of organ isn’t specified.
      - *unknown*: Assume this trait value means it is unknown if a
        storage organ is present or a plant can regenerate, not that the
        type is unknown

### resorption\_leaf\_N

  - label: Nitrogen resorption from leaves
  - description: Nitrogen resorption from leaves
  - type: numeric
  - units: dimensionless
  - allowable range: -1.1 - 1 dimensionless

### resorption\_leaf\_P

  - label: Phosphorus resorption from leaves
  - description: Phosphorus resorption from leaves
  - type: numeric
  - units: dimensionless
  - allowable range: -1.1 - 1 dimensionless

### root\_diameter

  - label: Root diameter
  - description: Root diameter
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 1000 mm

### root\_dry\_matter\_content

  - label: Root dry mass per unit root fresh mass (Root dry matter
    content)
  - description: Root dry mass per unit root fresh mass
  - type: numeric
  - units: mg/g
  - allowable range: 10 - 1000 mg/g

### root\_mass\_fraction

  - label: Fraction of plant dry mass comprised of root material
  - description: Fraction of plant dry mass comprised of root material
  - type: numeric
  - units: mg/mg
  - allowable range: 0.01 - 1 mg/mg

### root\_morphology

  - label: Categorical root descriptions, sense Cannon 1949
  - description: Categorical root descriptions sensu Cannon 1949, A
    Tentative Classification of Root Systems, Ecology,
    doi.org/10.2307/1932458
  - type: categorical
  - allowable values:
      - *type\_i*: The primary root is fibrous or fleshy and is
        relatively long; mesophytic
      - *type\_ii*: The primary root is relatively short and is fibrous.
        Roots are well branched in a dry habitat but branch little in
        one that is wet.
      - *type\_iii*: The primary root is usually long and fibrous. The
        first order lateral roots are well branched and are usually
        short; they are rather evenly distributed along the primary
        root; xerophytic
      - *type\_iv*: The primary root is usually long and slender like
        that of Type II. The first order lateral roots are well
        branched. Those closest to the surface of the ground, or not far
        below, are relatively long; xerophytic and mesophytic
      - *type\_v*: The primary root is divided-forked or branched-and
        fibrous, and with few first order lateral roots, or, in age,
        apparently with none; xerophytic
      - *type\_vi*: The primary root is usually as in Type V. The first
        order lateral roots are mostly on the upper portion of the
        primary root and are thus relatively superficial; xerophytic or
        in well aerated sandy soil where the rainfall is not light.
      - *type\_vii*: Adventitious roots that are in one group and arise
        on a short horizontal, or verti- cal, axis of the shoot. They
        are fibrous or fleshly; mesophytic and hydrophytic
      - *type\_viii*: Adventitious roots that are in one group and are
        fibrous and fleshy, or fibrous and thick. The fibrous
        adventitious roots are absorbing-anchoring roots and are usually
        with laterals to the second order; meophytic
      - *type\_ix*: Adventitious roots that arise on the aerial stem and
        branches of some herbaceous species, shrubs, and trees; on
        creepers, vines, and runners; and on subterranean rhizomes and
        stolons

### root\_shoot\_ratio

  - label: Root to shoot ratio
  - description: Ratio of root mass to shoot mass
  - type: numeric
  - units: mg/mg
  - allowable range: 0.01 - 1 mg/mg

### root\_structure

  - label: Root structure
  - description: Specific specialized types of root structures and root
    symbioses. <https://www.mycorrhizas.info/> provides detailed
    information for types of mycorrhizal associations.
  - type: categorical
  - allowable values:
      - *arbuscular\_mycorrhizal*: Symbiosis in which the mycorrhizal
        fungi does penetrate the cortex cells of the plant roots and its
        hyphae form arbuscules and vesicles inside the plant root;
        endomycorrhizal, AM, VAM
      - *carnivorous*: non-mycorrhizal carnivorous root
      - *cluster\_roots*: cluster roots in non-proteoid species
      - *coiling\_vam*: Associations that spread predominantly by
        intracellular hyphal coils within roots; a sub-category of
        arbuscular mycorrhizal association
      - *dauciform\_root*: A type of cluster root found in the family
        Cyperaceae
      - *ectomycorrhizal*: Symbiosis in which the mycorrhizal fungi does
        not penetrate the cortex cells of the plant roots
      - *ericoid\_mycorrhizal*: A form of arbuscular mycorrhizal
        relationship only found on plants in the order Ericales. The
        fungi’s hyphae penetrate the plant root but do not form
        arbuscules
      - *fine\_roots*: Particularly fine roots for nutrient uptake
      - *hemiparasitic\_root*: Partially parasitic root
      - *long\_root\_hairs*: Specialised long root hairs
      - *orchid\_mycorrhizal*: A mycorrhizal relationship specific to
        orchid species, and for most orchids, essential for seedlings to
        establish
      - *non\_mycorrhizal*: Plants lacking a mycorrhizal symbiont
      - *parasitic\_root*: Parasitic root
      - *proteoid\_root*: Also known as cluster roots, are plant roots
        that form clusters of closely spaced short lateral rootlets and
        aid in nutrient upake in nutrient-poor soils; common in members
        of the family Proteaceae, but present in other families as well
      - *root\_hairs*: Specialised root hairs
      - *sand-binding*: Persistent sheaths of sand that form around the
        roots of species in the families Restionaceae, Cyperaceae,
        Haemodoraceae, and Lomandraceae in western Australia. Assumed to
        function in nutrient uptake.
      - *subepidermal\_mycorrhizal*: Mycorrhizae where hyphae grow in a
        cavity under epidermal cells. Only in the genus Thysanotus (
        family Laxmaniaceae).

### root\_wood\_density

  - label: Root wood density
  - description: For root wood, the ratio of mass to volume, yielding
    density
  - type: numeric
  - units: mg/mm3
  - allowable range: 0.1 - 10 mg/mm3

### sapwood\_specific\_conductivity

  - label: Sapwood specific conductivity (Ks)
  - description: Ks; Describes the flow rate of water (kg/s) along a
    stem for a given drop in pressure (1/MPa), normalised to the length
    of the segment (1/m). Calculated as leaf hydraulic conductivity
    divided by the sapwood cross-sectional area where the measurement is
    taken.
  - type: numeric
  - units: kg/m/s/MPa
  - allowable range: 0.05 - 50 kg/m/s/MPa

### seed\_breadth

  - label: Seed breadth
  - description: The shorter width axis of a seed; orthogonal to its
    length
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 100 mm

### seed\_count

  - label: Seed count
  - description: Number of seeds produced by a plant
  - type: numeric
  - units: n
  - allowable range: 0 - 1e+05 n

### seed\_length

  - label: Seed length
  - description: Longest seed dimension
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 1000 mm

### seed\_mass

  - label: Seed dry mass
  - description: Seed carbon dry mass
  - type: numeric
  - units: mg
  - allowable range: 1e-04 - 1e+05 mg

### seed\_mass\_reserve

  - label: Seed dry mass reserve
  - description: Energy reserves stored in seeds that are mobilized at
    the time of germination; on a carbon dry mass basis
  - type: numeric
  - units: mg
  - allowable range: 1e-04 - 1000 mg

### seed\_oil\_content

  - label: Seed oil content
  - description: Seed oil content as a fraction of total seed weight,
    usually on a dry weight basis
  - type: numeric
  - units: g/g
  - allowable range: 0.001 - 1 g/g

### seed\_P\_concentration

  - label: Seed phosphorus concentration
  - description: Seed phosphorus concentration on a weight basis
  - type: numeric
  - units: mg/g
  - allowable range: 1 - 100 mg/g

### seed\_protein\_content

  - label: Seed protein content
  - description: Seed protein content as a fraction of total seed weight
  - type: numeric
  - units: g/g
  - allowable range: 1e-05 - 1 g/g

### seed\_shape

  - label: Seed shape
  - description: Possible seed shapes. Note that some terms currently
    used refer to 2-dimensional shapes, not 3-dimensional shapes.
  - type: categorical
  - allowable values:
      - *angular*: Doesn’t refer to a specific shape, but simply that
        instead of being smoothly rounded, it is angular
      - *broadly\_ellipsoid*: Like ellipsoid, but broader
      - *broadly\_obovoid*: A 3-dimensional shape; like obovoid, but
        particularly broad in the middle
      - *circular*: Both width dimensions are similar; rounded; REPLACE
        WITH ‘globoid’ - maybe
      - *comma\_shaped*: Seed shaped liked a comma; see also ‘falcate’
      - *compressed*: Seed flatter
      - *compressed\_cylinder*: Like cylinder, but a flatter
        3-dimensional shape
      - *compressed\_ellipsoid*: Like ellipsoid, but a flatter
        3-dimensional shape
      - *compressed\_globoid*: Like globoid, but a flatter 3-dimensional
        shape
      - *compressed\_lachrimiform*: Like lachrimiform, but a flatter
        3-dimensional shape
      - *compressed\_obovoid*: Like obovoid, but a flatter 3-dimensional
        shape
      - *compressed\_ovoid*: Like ovoid, but a flatter 3-dimensional
        shape
      - *compressed\_sectoroid*: Like sectoroid, but a flatter
        3-dimensional shape
      - *conical*: Shaped like a cone; the opposite of turbinate
      - *cordiform*: Heart-shaped
      - *cylindrical*: A 3-dimensional shape; shaped like a cylinder;
        see also ‘terete’ which refers to particular thin cylindrical
        shape; should maybe be cylinder?
      - *discoid*: Resembling a disc, with two convex faces; similar to
        plano-convex and lenticular, flatter
      - *discoid\_two\_lobed*: Resembling a disc, but with two lobes
      - *ellipsoid*: A 3-dimensional shape; elliptic in outline and with
        a length:breadth ratio between 3:2 and 2:1
      - *elongated\_lachrimiform*: Like lachrimiform, but extended along
        the longest axis
      - *elongated\_ovoid*: Like ovoid, but extended along the longest
        axis
      - *elongated\_pyramidal*: Like pyramidal, but extended along the
        longest axis
      - *elongated\_reniform*: Like reniform, but extended along the
        longest axis
      - *elongated\_sectoroid*: Like sectoroid, but extended along the
        longest axis
      - *falcate*: Sickle-shaped; see also ‘comma-shaped’
      - *flat*: Suggests a quite flattened seed, but no information
        about shape
      - *fusiform*: Spindle-shaped, cigar-shaped, circular in
        cross-section and tapering at both ends
      - *globoid*: Globe-shaped; includes ‘globular’ and ‘globose’
      - *inflated\_pyramidal*: Like pyramidal, but inflated
      - *inflated\_trigonal\_pyramidal*: Like trigonal\_pyramidal, but
        inflated
      - *irregular*: Irregular shape
      - *irregular\_lachrimiform*: Like lachrimiform, but somewhat
        irregular
      - *irregular\_ovoid*: Like ovoid, but somewhat irregular
      - *lachrimiform*: A 3-dimensional shape; tear-shaped
      - *lanceolate*: A 2-dimensional shape; much longer than wide, with
        the widest point below the middle; in reference to a seed,
        assume a rounded 2-D shape; REPLACE WITH OVOID
      - *lenticular*: A 3-dimensional shape; shaped like a biconvex
        lens. Similar to plano-convex??
      - *muricate*: has numerous short hard outgrowths
      - *obclavate*: A 2 or 3-dimensional shape; slender, broadening
        towards the base; club-shaped, with the thickened part at the
        base
      - *obloid*: A 3-dimensional shape, where the length is a few times
        greater than the width, with sides almost parallel and ends
        rounded.
      - *obovoid*: A 3-dimensional shape; similar to ovoid, but attached
        at the narrower end and with a length:breadth ratio between 3:2
        and 2:1
      - *orbicular*: Fairly flat, but round in 2-dimensional outline;
        see also compressed\_globoid
      - *ovoid*: Egg-shaped (i.e., widest below the middle)
      - *pointed\_ovoid*: Like ovoid, but with one pointier end
      - *pointed\_samara*: A type of fruit in which a flattened wing of
        fibrous, papery tissue develops from the ovary wall; variant
        that is more pointed; not truly a shape
      - *pyramidal*: Pyramid; cross-section unknown
      - *pyriform*: A 3-dimensional shape; resembling a pear, attached
        at the broader end
      - *rectangular*: REPLACE WITH ‘rectangular\_prism’
      - *rectangular\_prism*: Prism that is rectangular in cross=section
      - *reniform*: Having a circular or roughly circular shape with a
        notch, like a kidney
      - *rhomboid*: Diamond shaped in outline with the broadest axis in
        the middle and with a length:breadth ratio between 3:2 and 2:1
      - *round*: Seed with round outline; seems to be a subcategory of
        samara
      - *samara*: A type of fruit in which a flattened wing of fibrous,
        papery tissue develops from the ovary wall; not truly a shape
      - *sectoroid*: Shaped like an orange segment
      - *semigloboid*: Semi-circular in cross-section
      - *sinusoidal*: S-shaped; includes ‘sigmoidal’
      - *spathulate*: Spoon-shaped
      - *spherical*: A 3-dimensional shape; round with all dimensions
        approximately equal
      - *spheroid*: A 3-dimensional shape; nearly, but imperfectly
        spherical
      - *square\_prism*: Prism that is square in cross=section
      - *square\_pyramidal*: Pyramid that is square in cross-section
      - *subcylindrical*: Nearly or almost cylindrical in shape
      - *subgloboid*: Nearly or almost globoid or spheroid in shape
      - *suborbicular*: Nearly or almost orbicular in shape
      - *subtriangular*: Nearly or almost ‘trigonal\_prism’ in shape
      - *terete*: Circular in cross-section; more or less cylindrical
        without grooves or ridges
      - *triangular*: subset of samara possibly; 3-d structure should be
        trigonal\_prism or trigonal\_pyramidal
      - *trigonal\_prism*: Prism that is triangular in cross-section
      - *trigonal\_pyramidal*: Pyramid that is triangular in
        cross-section
      - *turbinate*: A 3-dimensional shape; top shaped; inversely
        conical
      - *wedge\_shaped*: A 2 or 3-dimensional shape; tapers to a point
        at one end; includes the 2-D classificatiion “cuneate” and
        possibly plants that are more conical in outline

### seed\_storage\_location

  - label: Location where seeds are stored at maturity
  - description: Location where seeds are stored at maturity
  - type: categorical
  - allowable values:
      - *canopy*: Seeds stored in the canopy, usually still within a
        woody cone
      - *soil*: Seeds stored in the soil

### seed\_texture

  - label: Seed texture
  - description: The texture of a seed
  - type: categorical
  - allowable values:
      - *acutely\_reticulate*: seed surface with a thickened linear
        ridge
      - *bumpy*: seed surface deeply wrinkled
      - *chartaceous*: seed surface finely wrinkled
      - *ciliate*: seed surface with a fine-scaled pattern
      - *costate*: seed surface hairy
      - *deeply\_reticulate*: seed surface covered with short blunt
        protruberances
      - *deeply\_rugose*: seed surface pitted
      - *dimpled*: seed surface rough
      - *finely\_reticulate*: wrinkled
      - *finely\_rugose*: rough (synonym seed)
      - *finely\_textured*: seed surface with a shallow linear ridge
      - *fluted*: seed surface with shallow pits
      - *hairs*: seed surface resembles a shallow net or network
      - *lineolate*: seed surface with shallow wrinkles
      - *micronate-tubercular*: seed surface slightly wrinkled
      - *multi-faceted*: seed surface smooth
      - *papillate*: seed surface covered with hairs with radiating
        papillae
      - *papillose*: seed surface marked with more or less parallel
        longitudinal ridges
      - *pitted*: seed surface covered with densely matted woolly hairs
      - *plumose*: seed surface covered with small rounded warts,
        protuberances or nodules
      - *raised\_edge*: seed surface wavy
      - *reticulate*: seed surface covered with short, dense, silky,
        upright hairs; soft and velvety
      - *ribbed*: seed surface very rough
      - *rough*: seed surface with a thickened linear ridge
      - *rugose*: seed surface deeply wrinkled
      - *scabrous*: seed surface finely wrinkled
      - *scalar*: seed surface with a fine-scaled pattern
      - *scaly*: seed surface hairy
      - *shallowly\_costate*: seed surface covered with short blunt
        protruberances
      - *shallowly\_pitted*: seed surface pitted
      - *shallowly\_reticulate*: seed surface rough
      - *shallowly\_rugose*: wrinkled
      - *slightly\_rugose*: rough (synonym seed)
      - *smooth*: seed surface with a shallow linear ridge
      - *stellate*: seed surface with shallow pits
      - *striate*: seed surface resembles a shallow net or network
      - *tomentose*: seed surface with shallow wrinkles
      - *tuberculate*: seed surface slightly wrinkled
      - *undulate*: seed surface smooth
      - *velutinous*: seed surface covered with hairs with radiating
        papillae
      - *very\_rough*: seed surface marked with more or less parallel
        longitudinal ridges

### seed\_viability

  - label: Seed viability
  - description: Proportion of seeds that are viable
  - type: numeric
  - units: n/n
  - allowable range: 0 - 1 n/n

### seed\_volume

  - label: Seed volume
  - description: Volume of a seed
  - type: numeric
  - units: mm3
  - allowable range: 0.001 - 1e+05 mm3

### seed\_width

  - label: Seed width
  - description: The longest width dimension of a seed; orthogonal to
    the length
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 100 mm

### seedling\_first\_leaf

  - label: First leaf style
  - description: Binary variable distinguishing between seedlings where
    the first leaf is scale-like (cataphyll) versus leaf-like.
  - type: categorical
  - allowable values:
      - *scale-like*: When the first leaf at germination is scale-like,
        also referred to as a cataphyll
      - *leaf-like*: When the first leaf at germination is leaf-like

### seedling\_first\_node

  - label: Leaf count at first node
  - description: Binary variable distinguishing between seedlings where
    the leaves at the first node are single versus paired.
  - type: categorical
  - allowable values:
      - *single*: When there is a single leaf at the first node
      - *paired*: When there is a pair of leaves at the first node

### senesced\_leaf\_Ca\_per\_dry\_mass

  - label: Senesced leaf calcium (Ca) content per unit leaf dry mass
  - description: Senesced leaf calcium (Ca) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.01 - 1000 mg/g

### senesced\_leaf\_Cu\_per\_dry\_mass

  - label: Senesced leaf copper (Cu) content per unit leaf dry mass
  - description: Senesced leaf copper (Cu) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0.01 - 100 mg/kg

### senesced\_leaf\_Fe\_per\_dry\_mass

  - label: Senesced leaf iron (Fe) content per unit leaf dry mass
  - description: Senesced leaf iron (Fe) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 10000 mg/kg

### senesced\_leaf\_K\_per\_dry\_mass

  - label: Senesced leaf potassium (K) content per unit leaf dry mass
  - description: Senesced leaf potassium (K) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### senesced\_leaf\_Mg\_per\_dry\_mass

  - label: Senesced leaf magnesium (Mg) content per unit leaf dry mass
  - description: Senesced leaf magnesium (Mg) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.01 - 100 mg/g

### senesced\_leaf\_Mn\_per\_dry\_mass

  - label: Senesced leaf manganese (Mn) content per unit leaf dry mass
  - description: Senesced leaf manganese (Mn) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 10000 mg/kg

### senesced\_leaf\_Mo\_per\_dry\_mass

  - label: Senesced leaf molybdenum (Mo) content per unit leaf dry mass
  - description: Senesced leaf molybdenum (Mo) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0 - 30 mg/kg

### senesced\_leaf\_N\_per\_dry\_mass

  - label: Senesced leaf nitrogen (N) content per unit leaf dry mass
  - description: Senesced leaf nitrogen (N) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### senesced\_leaf\_Na\_per\_dry\_mass

  - label: Senesced leaf sodium (Na) content per unit leaf dry mass
  - description: Senesced leaf sodium (Na) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 1 - 20000 mg/kg

### senesced\_leaf\_Ni\_per\_dry\_mass

  - label: Senesced leaf nickel (Ni) content per unit leaf dry mass
  - description: Senesced leaf nickel (Ni) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 10 - 1e+05 mg/kg

### senesced\_leaf\_P\_per\_dry\_mass

  - label: Senesced leaf phosphorus (P) content per unit leaf dry mass
  - description: Senesced leaf phosphorus (P) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.01 - 12 mg/g

### senesced\_leaf\_S\_per\_dry\_mass

  - label: Senesced leaf sulphur (S) content per unit leaf dry mass
  - description: Senesced leaf sulphur (S) content per unit leaf dry
    mass
  - type: numeric
  - units: mg/kg
  - allowable range: 100 - 50000 mg/kg

### senesced\_leaf\_Zn\_per\_dry\_mass

  - label: Senesced leaf zinc (Zn) content per unit leaf dry mass
  - description: Senesced leaf zinc (Zn) content per unit leaf dry mass
  - type: numeric
  - units: mg/kg
  - allowable range: 0 - 100 mg/kg

### serotiny

  - label: Serotiny
  - description: When a fruit or cone only releases its seeds following
    an environmental trigger, often fire
  - type: categorical
  - allowable values:
      - *not\_serotinous*: Plant does NOT display serotiny
      - *serotinous*: Plant does display serotiny
      - *serotiny\_low*: Plant displays a low level of serotiny
      - *serotiny\_moderate*: Plant displays a moderate level of
        serotiny

### soil\_salinity\_tolerance

  - label: Salt tolerance
  - description: Maximum salinity tolerated by a species, reported as
    the conductivitiy of the soil
  - type: numeric
  - units: dS/m
  - allowable range: 0.1 - 100 dS/m

### soluable\_protein\_per\_area

  - label: Soluble protein per leaf area
  - description: Mass of soluble protein per leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.1 - 20 g/m2

### insoluable\_protein\_per\_area

  - label: Insoluble protein per leaf area
  - description: Mass of insoluble protein per leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.1 - 20 g/m2

### soluable\_sugars\_per\_area

  - label: Soluble sugars per leaf area
  - description: Mass of soluble sugars per leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 1 - 100 g/m2

### specific\_leaf\_area

  - label: Leaf area per unit leaf dry mass (specific leaf area, SLA)
  - description: Leaf area per unit leaf dry mass; SLA
  - type: numeric
  - units: mm2/mg
  - allowable range: 0.4 - 500 mm2/mg

### specific\_root\_area

  - label: Root area per unit root dry mass (specific root area)
  - description: Root area per unit root dry mass
  - type: numeric
  - units: mm2/mg
  - allowable range: 0.5 - 500 mm2/mg

### specific\_root\_length

  - label: Root length per unit root dry mass (specific root length,
    SRL)
  - description: Root length per unit root dry mass; SRL
  - type: numeric
  - units: m/g
  - allowable range: 0.5 - 500 m/g

### spongy\_mesophyll\_thickness

  - label: Spongy mesophyll cell thickness
  - description: Thickness of the spongy mesophyll cells
  - type: numeric
  - units: um
  - allowable range: 10 - 1000 um

### starch\_per\_area

  - label: Starch per leaf area
  - description: Mass of atarch per leaf area
  - type: numeric
  - units: g/m2
  - allowable range: 0.1 - 50 g/m2

### stem\_C\_per\_dry\_mass

  - label: Stem carbon (C) content per unit stem dry mass
  - description: Stem carbon (C) content per unit stem dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 100 - 1000 mg/g

### stem\_count\_categorical

  - label: Stem Count, categorical
  - description: Number of stems present, expressed in groups, where
    categories were 1=1; 2-3=2; 4-10=3; 11-30=4; and \>30=5. Used by
    Peter Vesk.
  - type: numeric
  - units: count
  - allowable range: 0 - 5 count

### stem\_cross\_section\_area

  - label: Stem cross-sectional area
  - description: Cross-sectional area of the stem
  - type: numeric
  - units: mm2
  - allowable range: 1 - 1000 mm2

### stem\_cross\_section\_area\_no\_bark

  - label: Stem cross-sectional area without bark
  - description: Cross-sectional area of the stem once bark is removed
  - type: numeric
  - units: mm2
  - allowable range: 1 - 1000 mm2

### stem\_dry\_matter\_content

  - label: Stem dry mass per unit stem fresh mass (Stem dry matter
    content)
  - description: Stem dry mass per unit stem fresh mass
  - type: numeric
  - units: mg/g
  - allowable range: 10 - 1000 mg/g

### stem\_hydraulic\_conductivity

  - label: Hydraulic conductivity (Kh)
  - description: Kh; Measure of how efficiently water is transported
    through the leaf, determined as the ratio of water flow rate through
    the leaf to the difference in water potential across the leaf,
    standardised to leaf area.
  - type: numeric
  - units: kg\*m/s/MPa
  - allowable range: 1e-08 - 1e-04 kg\*m/s/MPa

### stem\_N\_per\_dry\_mass

  - label: Stem carbon (N) content per unit stem dry mass
  - description: Stem carbon (N) content per unit stem dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 1000 mg/g

### basal\_diameter

  - label: Stem diameter
  - description: Diameter at the base of the plant, standardly “DBH”
    except in short plants
  - type: numeric
  - units: mm
  - allowable range: 0.1 - 1000 mm

### stomatal\_conductance\_per\_area

  - label: Stomatal conductance per unit leaf area under ambient
    conditions
  - description: Rate of water loss through stomata under ambient
    conditions, per unit leaf area
  - type: numeric
  - units: mmolH2O/m2/s
  - allowable range: -0.1 - 2000 mmolH2O/m2/s

### stomatal\_conductance\_per\_area\_at\_Amax

  - label: Stomatal conductance per unit leaf area under saturated light
    and CO2 conditions
  - description: Rate of water loss through stomata, per unit leaf area
    under saturated light and CO2 conditions
  - type: numeric
  - units: mmolH2O/m2/s
  - allowable range: -0.1 - 2000 mmolH2O/m2/s

### stomatal\_conductance\_per\_area\_at\_Asat

  - label: Stomatal conductance per unit leaf area under saturated light
    conditions
  - description: Rate of water loss through stomata, per unit leaf area
    under saturated light conditions
  - type: numeric
  - units: mmolH2O/m2/s
  - allowable range: -0.1 - 2000 mmolH2O/m2/s

### stomatal\_density

  - label: Stomatal density on the leaf surface
  - description: Stomatal density on the leaf surface
  - type: numeric
  - units: count/mm2
  - allowable range: 1 - 1000 count/mm2

### support\_fraction

  - label: Proportion of shoot dry mass that is stems (versus leaves)
  - description: Proportion of shoot dry mass that is stems (versus
    leaves)
  - type: numeric
  - units: g/g
  - allowable range: 0 - 1 g/g

### tap\_root

  - label: Tap root
  - description: Binary variable describing whether or not a plant has a
    tap root present
  - type: categorical
  - allowable values:
      - *0*: tap root not present
      - *1*: tap root present

### thickest\_root\_diameter

  - label: Thickest root diameter
  - description: Diameter of the thickest root
  - type: numeric
  - units: mm
  - allowable range: 0.01 - 1000 mm

### twig\_area

  - label: Terminal twig cross-sectional area
  - description: The cross-sectional area of the terminal twig
  - type: numeric
  - units: mm2
  - allowable range: 0.05 - 50 mm2

### twig\_length

  - label: Terminal twig length
  - description: The length of the terminal twig
  - type: numeric
  - units: mm
  - allowable range: 1 - 1000 mm

### vapor\_pressure\_deficit

  - label: Vapor pressure deficit
  - description: Vapor pressure deficit across the leaf surface
  - type: numeric
  - units: kPa
  - allowable range: 0 - 5 kPa

### vegetative\_regeneration

  - label: Ability to regerenate and spread through the growth and
    division of vegetative material.
  - description: Ability to regerenate and spread through the growth and
    division of vegetative material.
  - type: categorical
  - allowable values:
      - *not\_vegetative*: Plant that regenerate and spread only by seed
      - *vegetative*: Generic term including all plants that can
        regenerate and spread through the growth and division of
        vegetative material, not just from seed; examples include
        runners, rhizomes, and bulbs

### water\_band\_index

  - label: Water band index
  - description: Water band index, the ratio of the reflectance at 970
    nm / 900 nm, recorded from the spectroradiometer.
  - type: numeric
  - units: dimensionless
  - allowable range: 1 - 1.1 dimensionless

### water\_potential\_midday

  - label: Midday water potential
  - description: A plant’s water potential during the heat of the day
  - type: numeric
  - units: MPa
  - allowable range: -10 - 0 MPa

### water\_potential\_predawn

  - label: Pre-dawn water potential
  - description: A plant’s water potential just before sunrise
  - type: numeric
  - units: MPa
  - allowable range: -6 - 0 MPa

### water\_tolerance

  - label: Water (and salt) tolerance strategies
  - description: Water-tolerance categories
  - type: categorical
  - allowable values:
      - *glycophyte*: Plants that cannot survive even one tenth the salt
        concentration found in seawater
      - *halophyte*: Plants that complete their life cycle in a salty
        environment; many survive in seawater or even higher
        concentrations of salt
      - *hydrohalophyte*: Tidal swamp or ‘mangrove’ and coastal lagoon
        elements and temperate-zone salt-marsh species are classed
        together.
      - *phreatophyte*: Deep-rooted plants—usually trees—obtaining water
        from a deep underground source that may or may not be saline.
      - *psammophile*: Sand-loving plants commonly found in littoral
        strand or inland sandfields
      - *weedy*: This inexact term is meant to indicate a high degree of
        adaptability to sites, and proven tendency toward aggressive
        colonization of either dry or wet sites, or rarely, both. In
        general, weeds invade and colonize highly disturbed sites or
        areas.
      - *xerohalophyte*: Plants adapted to inland salt desert and saline
        habitats
      - *xerophyte*: Drought-tolerant and drought-adapted plants

### water\_use\_efficiency

  - label: Water use efficiency
  - description: Rate of carbon dioxide uptake relative to water loss,
    per unit leaf area
  - type: numeric
  - units: mmolCO2/molH2O/m2/s
  - allowable range: 1 - 10 mmolCO2/molH2O/m2/s

### wood\_delta13C

  - label: Wood carbon (C) isotope signature (delta 13C)
  - description: Wood carbon stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -50 - 0 per mille

### wood\_delta15N

  - label: Wood nitrogen (N) isotope signature (delta 15N)
  - description: Wood nitrogen stable isotope signature
  - type: numeric
  - units: per mille
  - allowable range: -25 - 75 per mille

### wood\_density

  - label: Stem dry mass per unit fresh stem volume
  - description: Stem dry mass per unit stem fresh volume (stem specific
    density or SSD or wood density)
  - type: numeric
  - units: mg/mm3
  - allowable range: 0.1 - 1.3 mg/mm3

### wood\_C\_per\_dry\_mass

  - label: Wood carbon (C) content per unit wood dry mass
  - description: Wood carbon (C) content per unit wood dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 50 - 750 mg/g

### wood\_N\_per\_dry\_mass

  - label: Wood nitrogen (N) content per unit wood dry mass
  - description: Wood nitrogen (N) content per unit wood dry mass
  - type: numeric
  - units: mg/g
  - allowable range: 0.1 - 100 mg/g

### woodiness

  - label: Woodiness
  - description: A plant’s degree of lignification in stems
  - type: categorical
  - allowable values:
      - *herbaceous*: Plant with non-lignified stems
      - *semi\_woody*: Plant with partially lignified stems
      - *woody*: Plant that produces secondary xylem, have lignin
