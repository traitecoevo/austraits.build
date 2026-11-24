# Builds raw/trait_data.csv (long format) and data.csv from raw/table4_transcription.csv,
# a verbatim transcription of Table 4 of Dadswell (1972) CSIRO Div. Forest Products
# Technological Paper 66, plus a handful of species-specific statements from the text.
# Run from the repository root: Rscript data/Dadswell_1972/raw/build_trait_data_Dadswell_1972.R
#
# Cell notation in table4_transcription.csv (as printed):
#   "a-b(m)"  range of specimen means and species mean
#   "a-b"     range only (three specimens, or ray percentages / multiseriate heights)
#   "x & y"   individual results for two specimens
#   letters   categories defined in section 4 of the text; "a-c" = a, b and c
#   "*a"      asterisk above/before a letter = specimens generally in that class
#   "a*"      asterisk below/after a letter = specimens only occasionally (1-3) in that class
#   "†"       basic density determined on air-dry material by Dadswell; densities without †
#             were tabulated from Kingston & Risdon (1961)

suppressMessages(library(dplyr))

dir <- "data/Dadswell_1972"
t4 <- read.csv(file.path(dir, "raw/table4_transcription.csv"), colClasses = "character",
               na.strings = character(0), check.names = FALSE)

# ---- taxon names ----------------------------------------------------------------------
taxon_from_verbatim <- function(x) {
  epithet <- sub("^E[.:] ([a-z]+).*$", "\\1", x)
  infra <- ifelse(grepl("ssp\\. ([a-z]+)", x), sub(".*ssp\\. ([a-z]+).*", " subsp. \\1", x), "")
  paste0("Eucalyptus ", epithet, infra)
}
t4$taxon_name <- taxon_from_verbatim(t4$name_verbatim)

# ---- Kingston_1961 overlap for basic density ---------------------------------------
kingston <- read.csv("data/Kingston_1961/data.csv")$scientific_name
kingston_alias <- c("Eucalyptus bauerana" = "Eucalyptus baueriana",
                    "Eucalyptus blaxlandii" = "Eucalyptus blaxlandi",
                    "Eucalyptus jacksonii" = "Eucalyptus jacksoni",
                    "Eucalyptus bancroftii" = "Eucalyptus bancrofti",
                    "Eucalyptus mannifera subsp. maculosa" = "Eucalyptus mannifera",
                    "Eucalyptus dalrympleana subsp. dalrympleana" = "Eucalyptus dalrympleana",
                    "Eucalyptus muellerana" = "Eucalyptus muelleriana",
                    "Eucalyptus macrorhyncha" = "Eucalyptus macrorrhyncha",
                    "Eucalyptus tectifica" = "Eucalyptus spenceriana")

# ---- category vocabularies -------------------------------------------------------------
amount4 <- c(a = "abundant", b = "moderately_abundant", c = "sparse", d = "absent")
vocab <- list(
  t4.2 = c(a = "straw_light_yellow_brown_or_pale_brown", b = "brown", c = "pink_brown",
           d = "red_brown", e = "dark_red_brown_dark_brown_or_chocolate_brown"),
  t4.5 = c(a = "very_distinctly_marked", b = "well_marked", c = "evident",
           d = "vaguely_indicated", e = "absent"),
  t4.8 = c(a = "very_common", b = "fairly_common_to_common", c = "not_common", d = "rare"),
  t4.9 = c(a = "radial_multiples_common", b = "radial_multiples_few", c = "radial_multiples_absent"),
  t4.12 = amount4[1:3], t4.13 = amount4, t4.14 = amount4[1:3], t4.17 = amount4,
  t4.18 = amount4, t4.25 = amount4,
  t4.15 = c("+" = "aliform_to_confluent", "-" = "not_aliform_to_confluent"),
  t4.16 = c("+" = "diffuse_apotracheal", "-" = "diffuse_apotracheal_absent_or_rare"),
  t4.26 = c(a = "twice_wall_thickness_greater_or_equal_lumen",
            b = "twice_wall_thickness_less_or_equal_lumen",
            c = "twice_wall_thickness_less_equal_and_greater_than_lumen"),
  t4.29 = c(a = "pit_borders_indistinct", b = "pit_borders_moderately_distinct",
            c = "pit_borders_distinct"),
  t4.30 = c(a = "5_or_more_per_25_fibres", b = "3_to_4_per_25_fibres", c = "1_to_2_per_25_fibres")
)
# AusTraits values (trait_value_clean) for the categorical traits already in config/traits.yml
clean <- list(
  t4.5 = c(very_distinctly_marked = "distinct", well_marked = "distinct", evident = "moderately_distinct",
           vaguely_indicated = "indistinct", absent = "absent"),
  t4.9 = c(radial_multiples_common = "vessel_radial_multiples_few vessel_radial_multiples_many",
           radial_multiples_few = "vessel_solitary vessel_radial_multiples_few",
           radial_multiples_absent = "vessel_solitary"),
  t4.15 = c(aliform_to_confluent = "vasicentric aliform confluent",
            not_aliform_to_confluent = "scanty_paratracheal vasicentric"),
  t4.16 = c(diffuse_apotracheal = "diffuse", diffuse_apotracheal_absent_or_rare = ""),
  t4.26 = c(twice_wall_thickness_greater_or_equal_lumen = "thick-walled",
            twice_wall_thickness_less_or_equal_lumen = "thin-walled",
            twice_wall_thickness_less_equal_and_greater_than_lumen = "thin-walled thick-walled"),
  t4.29 = c(pit_borders_indistinct = "simple_to_minutely_bordered",
            pit_borders_moderately_distinct = "moderately_distinctly_bordered",
            pit_borders_distinct = "distinctly_bordered")
)
burn_vocab <- c("Ch." = "charcoal", "P.Ash" = "partial_ash", "Ash" = "full_ash")

# column -> trait name, unit, source section label
cols <- tribble(
  ~col, ~trait, ~type, ~unit, ~section, ~context,
  "t4.2_colour", "heartwood_colour", "cat", "", "Table 4, col. 4.2 (colour)", "",
  "t4.3_basic_density_lb_ft3", "wood_density", "num", "lb/ft3", "Table 4, col. 4.3 (basic density)", "",
  "t4.4_burning_splinter", "wood_splinter_burning", "burn", "", "Table 4, col. 4.4 (burning splinter test)", "",
  "t4.5_growth_rings", "wood_growth_ring_distinctiveness", "cat", "", "Table 4, col. 4.5 (growth rings)", "",
  "t4.6_vessel_avg_tangential_diameter_um", "stem_vessel_diameter", "num", "um", "Table 4, col. 4.6 (vessel average tangential diameter)", "",
  "t4.7_vessel_max_tangential_diameter_um", "stem_vessel_diameter", "num", "um", "Table 4, col. 4.7 (vessel maximum tangential diameter)", "",
  "t4.8_vessel_oblique_alignment", "wood_vessel_diagonal_pattern", "cat", "", "Table 4, col. 4.8 (oblique alignment of vessels)", "",
  "t4.9_vessel_radial_multiples", "stem_vessel_grouping", "cat", "", "Table 4, col. 4.9 (radial multiples)", "",
  "t4.10_vessel_number_per_mm2", "stem_vessel_density", "num", "{count}/mm2", "Table 4, col. 4.10 (vessel number)", "",
  "t4.11_vessel_member_length_mm", "stem_vessel_element_length", "num", "mm", "Table 4, col. 4.11 (vessel member length)", "",
  "t4.12_tyloses", "wood_vessel_tyloses", "cat", "", "Table 4, col. 4.12 (tyloses in vessels)", "",
  "t4.13_vessel_kino_like_deposits", "wood_deposit_frequency", "cat", "", "Table 4, col. 4.13 (\"kino-like\" deposits in vessels)", "vessels",
  "t4.14_axial_parenchyma_amount", "wood_axial_parenchyma_fraction_categorical", "cat", "", "Table 4, col. 4.14 (axial parenchyma amount)", "",
  "t4.15_aliform_to_confluent", "wood_axial_parenchyma_arrangement", "cat", "", "Table 4, col. 4.15 (paratracheal parenchyma aliform to confluent)", "",
  "t4.16_diffuse", "wood_axial_parenchyma_arrangement", "cat", "", "Table 4, col. 4.16 (apotracheal parenchyma diffuse)", "",
  "t4.17_crystals", "wood_crystals", "cat", "", "Table 4, col. 4.17 (crystals in axial parenchyma)", "axial parenchyma",
  "t4.18_axial_parenchyma_kino_like_deposits", "wood_deposit_frequency", "cat", "", "Table 4, col. 4.18 (\"kino-like\" deposits in axial parenchyma)", "axial parenchyma",
  "t4.19_rays_per_mm", "wood_ray_density", "num", "{count}/mm", "Table 4, col. 4.19 (ray number)", "",
  "t4.20_ray_avg_max_height_cells", "wood_ray_cell_longitudinal_count", "num", "{count}", "Table 4, col. 4.20 (ray average maximum height)", "",
  "t4.21_rays_pct_biseriate", "wood_ray_biseriate_fraction", "num", "%", "Table 4, col. 4.21 (percentage biseriate rays)", "",
  "t4.22_rays_pct_triseriate", "wood_ray_triseriate_fraction", "num", "%", "Table 4, col. 4.22 (percentage triseriate rays)", "",
  "t4.23_rays_pct_multiseriate", "wood_ray_multiseriate_fraction", "num", "%", "Table 4, col. 4.23 (percentage multiseriate rays)", "",
  "t4.24_multiseriate_portion_max_height_cells", "wood_ray_multiseriate_portion_height", "num", "{count}", "Table 4, col. 4.24 (maximum height of multiseriate portion)", "",
  "t4.25_ray_kino_like_deposits", "wood_deposit_frequency", "cat", "", "Table 4, col. 4.25 (\"kino-like\" deposits in rays)", "rays",
  "t4.26_fibre_wall_thickness_lumen", "wood_fibre_wall_thickness_categorical", "cat", "", "Table 4, col. 4.26 (fibre wall thickness / lumen diameter)", "",
  "t4.27_fibre_length_mm", "wood_fibre_length", "num", "mm", "Table 4, col. 4.27 (fibre length)", "",
  "t4.28_fibre_diameter_um", "wood_fibre_diameter", "num", "um", "Table 4, col. 4.28 (fibre diameter)", "",
  "t4.29_fibre_pitting", "wood_fibre_pit_type", "cat", "", "Table 4, col. 4.29 (fibre pitting)", "",
  "t4.30_vasicentric_tracheids", "wood_vasicentric_tracheids", "cat", "", "Table 4, col. 4.30 (vasicentric tracheids)", ""
)
existing <- c("wood_density", "wood_growth_ring_distinctiveness", "stem_vessel_diameter", "stem_vessel_grouping",
              "stem_vessel_density", "wood_axial_parenchyma_arrangement", "wood_fibre_wall_thickness_categorical",
              "stem_vessel_perforation_plate_type", "wood_ray_cell_type", "stem_vessel_element_length",
              "wood_ray_density", "wood_fibre_length", "wood_fibre_diameter",
              "wood_fibre_pit_type", "wood_deposit_frequency", "wood_ray_cell_longitudinal_count")
candidate_note <- "Candidate trait, not in config/traits.yml (checked 2026-10)."

# elements measured per specimen (replicates for single-specimen values), from section 4
per_specimen_n <- c(t4.6 = 50, t4.7 = 10, t4.11 = 20, t4.20 = 10, t4.27 = 50, t4.28 = 100)

# ---- parsers ---------------------------------------------------------------------------
parse_letters <- function(cell, key) {
  # returns list(terms, generally, occasionally)
  x <- gsub(" ", "", cell)
  gen <- regmatches(x, gregexpr("\\*[a-e+-]", x))[[1]]
  occ <- regmatches(x, gregexpr("[a-e+-]\\*", x))[[1]]
  x <- gsub("\\*", "", x)
  parts <- strsplit(x, "[,&]")[[1]]
  out <- unlist(lapply(parts, function(p) {
    if (grepl("^[a-e]-[a-e]$", p)) letters[match(substr(p, 1, 1), letters):match(substr(p, 3, 3), letters)] else p
  }))
  v <- vocab[[key]]
  list(terms = unname(v[unique(out)]),
       generally = unname(v[gsub("\\*", "", gen)]),
       occasionally = unname(v[gsub("\\*", "", occ)]))
}
parse_burn <- function(cell) {
  x <- gsub(" ", "", cell)
  parts <- strsplit(x, "[,&]")[[1]]
  gen <- burn_vocab[gsub("\\*", "", parts[grepl("^\\*", parts)])]
  occ <- burn_vocab[gsub("\\*", "", parts[grepl("\\*$", parts)])]
  list(terms = unname(burn_vocab[gsub("\\*", "", parts)]), generally = unname(gen), occasionally = unname(occ))
}
qualifier_note <- function(p) {
  s <- c(if (length(p$generally)) paste0("specimens generally ", paste(p$generally, collapse = ", ")),
         if (length(p$occasionally)) paste0("only occasionally (1-3 specimens) ", paste(p$occasionally, collapse = ", ")))
  if (length(s)) paste0("Table 4 asterisk: ", paste(s, collapse = "; "), ".") else ""
}

rows <- list()
add <- function(...) rows[[length(rows) + 1]] <<- tibble(...)

for (i in seq_len(nrow(t4))) {
  r <- t4[i, ]
  n_spec <- as.integer(r$t4.1_no_specimens)
  for (j in seq_len(nrow(cols))) {
    cc <- cols[j, ]
    key <- sub("^(t4\\.[0-9]+)_.*", "\\1", cc$col)
    cell <- r[[cc$col]]
    is_density <- cc$trait == "wood_density"
    base_note <- if (cc$trait %in% existing) "" else candidate_note
    dens_method <- if (is_density) ifelse(r$t4.3_density_dagger == "†", "air-dry material", "") else ""
    omit <- ""
    src <- if (is_density && r$t4.3_density_dagger != "†") "Kingston_1961" else "Dadswell_1972"
    if (is_density && r$t4.3_density_dagger != "†") {
      k_name <- ifelse(r$taxon_name %in% names(kingston_alias), kingston_alias[r$taxon_name], r$taxon_name)
      omit <- if (k_name %in% kingston) "Kingston_1961" else ""
    }
    n_pop <- if (is_density) r$t4.3_no_trees_density else as.character(n_spec)
    common <- list(taxon_name = r$taxon_name, original_name = r$name_verbatim,
                   blakely_no = r$blakely_no, blakely_series = r$blakely_series,
                   density_method = dens_method, source_id = src, omit_duplicate = omit,
                   wood_tissue = cc$context, trait = cc$trait, unit = cc$unit,
                   source_section = cc$section)
    gap_note <- function(why) do.call(add, c(common, list(entity_type = "population", trait_value_raw = cell,
                   trait_value = "", trait_value_clean = "", value_type = "", n = "",
                   notes = trimws(paste(c(base_note, why), collapse = " ")))))
    if (cell == "" || (cell == "-" && (cc$type == "num" || key %in% c("t4.2", "t4.4", "t4.12")))) {
      why <- if (r$blakely_no == "15" && key == "t4.12") "Shown as '-' in Table 4; all E. tetraptera samples were sapwood (section 2.1)."
             else if (r$blakely_no == "15") "Not recorded: all E. tetraptera samples were sapwood; heartwood features (e.g. colour, burning splinter test) not recorded (section 2.1)."
             else if (cell == "") "Cell blank in Table 4; source gives no explanation."
             else "Shown as '-' (not determined) in Table 4; source gives no explanation."
      gap_note(why)
      next
    }
    if (cc$type %in% c("cat", "burn")) {
      p <- if (cc$type == "burn") parse_burn(cell) else parse_letters(cell, key)
      cl <- if (!is.null(clean[[key]])) paste(unique(unlist(strsplit(clean[[key]][p$terms], " "))), collapse = " ") else ""
      do.call(add, c(common, list(entity_type = "population", trait_value_raw = cell,
                 trait_value = paste(p$terms, collapse = " "), trait_value_clean = cl,
                 value_type = "mode", n = as.character(n_spec),
                 notes = paste(c(base_note, qualifier_note(p)), collapse = " ") |> trimws())))
      next
    }
    # numeric
    if (grepl("^[0-9.]+-[0-9.]+\\([0-9.]+\\)$", cell)) {
      v <- as.numeric(regmatches(cell, gregexpr("[0-9.]+", cell))[[1]])
      for (k in 1:3) {
        note <- base_note
        val <- v[k]
        if (is_density && k < 3) note <- paste(note, "Range is the 95 per cent probability range of the species (section 4.3), not the observed minimum/maximum.")
        if (k == 1 && v[1] > v[3]) { note <- paste(note, sprintf("Printed range minimum (%s) exceeds the printed mean (%s); probable misprint, value not used.", v[1], v[3])); val <- NA }
        do.call(add, c(common, list(entity_type = "population", trait_value_raw = cell,
                   trait_value = ifelse(is.na(val), "", format(val)), trait_value_clean = ifelse(is.na(val), "", format(val)),
                   value_type = c("minimum", "maximum", "mean")[k], n = n_pop, notes = trimws(note))))
      }
    } else if (grepl("^[0-9.]+-[0-9.]+$", cell)) {
      v <- as.numeric(strsplit(cell, "-")[[1]])
      for (k in 1:2) do.call(add, c(common, list(entity_type = "population", trait_value_raw = cell,
                 trait_value = format(v[k]), trait_value_clean = format(v[k]),
                 value_type = c("minimum", "maximum")[k], n = n_pop,
                 notes = trimws(paste(base_note, "Range of specimen results; no species mean given (section 2.2).")))))
    } else if (grepl("&", cell)) {
      v <- sort(as.numeric(trimws(strsplit(cell, "&")[[1]])))
      for (k in 1:2) do.call(add, c(common, list(entity_type = "population", trait_value_raw = cell,
                 trait_value = format(v[k]), trait_value_clean = format(v[k]),
                 value_type = c("minimum", "maximum")[k], n = n_pop,
                 notes = trimws(paste(base_note, "Individual results for two specimens (section 2.2), recorded as minimum and maximum because the source does not link specimens across columns.")))))
    } else {
      v <- as.numeric(cell)
      single_specimen <- if (is_density) r$t4.3_no_trees_density == "1" else n_spec == 1
      vt <- if (key == "t4.24") "maximum" else if (key %in% c("t4.21", "t4.22", "t4.23") && single_specimen) "raw" else "mean"
      note <- base_note
      if (!single_specimen && !is_density) note <- paste(note, "Single value printed for a species with more than one specimen; specimens gave the same result.")
      if (is_density && !single_specimen) note <- paste(note, "Fewer than eight specimens / three trees, so only the species mean is given (section 4.3).")
      do.call(add, c(common, list(entity_type = ifelse(single_specimen, "individual", "population"),
                 trait_value_raw = cell, trait_value = format(v), trait_value_clean = format(v), value_type = vt,
                 n = ifelse(single_specimen, ifelse(is.na(per_specimen_n[key]), "", as.character(per_specimen_n[key])), n_pop),
                 notes = trimws(note))))
    }
  }
}

# ---- statements from the text (section 1.3 / 4) ---------------------------------------
not_corymbosae <- !grepl("CORYMBOSAE", t4$blakely_series)
for (i in which(not_corymbosae)) {
  add(taxon_name = t4$taxon_name[i], original_name = t4$name_verbatim[i], blakely_no = t4$blakely_no[i],
      blakely_series = t4$blakely_series[i], density_method = "", source_id = "Dadswell_1972", omit_duplicate = "", wood_tissue = "",
      trait = "stem_vessel_perforation_plate_type", unit = "", source_section = "Section 1.3 (Vessels)",
      entity_type = "species", trait_value_raw = "Perforation plates are exclusively simple and mainly horizontal",
      trait_value = "simple", trait_value_clean = "simple", value_type = "mode", n = "",
      notes = "Stated for all species examined outside the Corymbosae series (section 1.3).")
  add(taxon_name = t4$taxon_name[i], original_name = t4$name_verbatim[i], blakely_no = t4$blakely_no[i],
      blakely_series = t4$blakely_series[i], density_method = "", source_id = "Dadswell_1972", omit_duplicate = "", wood_tissue = "",
      trait = "wood_ray_cell_type", unit = "", source_section = "Section 1.3 (Rays)",
      entity_type = "species", trait_value_raw = "They are weakly heterocellular with one to several rows of marginal procumbent cells",
      trait_value = "weakly_heterocellular", trait_value_clean = "heterocellular", value_type = "mode", n = "",
      notes = "Stated for all species examined outside the Corymbosae series (section 1.3).")
}
text_rows <- tribble(
  ~blakely_no, ~trait, ~raw, ~value, ~section,
  "577", "heartwood_colour", "Four species distinctive for their purple tints", "purple_tints", "Section 4.2 (Colour)",
  "304", "heartwood_colour", "Four species distinctive for their purple tints", "purple_tints", "Section 4.2 (Colour)",
  "14",  "heartwood_colour", "Four species distinctive for their purple tints", "purple_tints", "Section 4.2 (Colour)",
  "17",  "heartwood_colour", "Four species distinctive for their purple tints ... E. papuana (17) in some cases", "purple_tints", "Section 4.2 (Colour)",
  "32",  "heartwood_colour", "E. calophylla (32), pink and/or grey [tints]", "pink_tints grey_tints", "Section 4.2 (Colour)",
  "225", "heartwood_colour", "E. bridgesiana (225), pink, yellow, orange [tints], sometimes varying between specimens", "pink_tints yellow_tints orange_tints", "Section 4.2 (Colour)",
  "507,507B", "heartwood_colour", "E. microtheca (507) and E. citriodora (53) were the only species with some specimens recording chocolate-brown", "chocolate_brown", "Section 4.2 (Colour)",
  "53",  "heartwood_colour", "E. microtheca (507) and E. citriodora (53) were the only species with some specimens recording chocolate-brown", "chocolate_brown", "Section 4.2 (Colour)",
  "19a", "heartwood_colour", "samples of E. confertiflora (19a) were streaked with the latter colour [chocolate-brown]", "chocolate_brown_streaks", "Section 4.2 (Colour)",
  "437", "wood_vessel_tyloses", "Tyloses were recorded as absent for only a few samples", "absent", "Section 4.12 (Tyloses)",
  "370", "wood_vessel_tyloses", "Tyloses were recorded as absent for only a few samples", "absent", "Section 4.12 (Tyloses)",
  "381", "wood_vessel_tyloses", "Tyloses were recorded as absent for only a few samples", "absent", "Section 4.12 (Tyloses)",
  "263", "wood_vessel_tyloses", "Tyloses were recorded as absent for only a few samples", "absent", "Section 4.12 (Tyloses)",
  "369", "wood_vessel_tyloses", "Tyloses were recorded as absent for only a few samples", "absent", "Section 4.12 (Tyloses)",
  "3",   "wood_vessel_tyloses", "showed one sample with no tyloses", "absent", "Section 4.12 (Tyloses)",
  "2",   "wood_vessel_tyloses", "showed one sample with no tyloses", "absent", "Section 4.12 (Tyloses)"
)
for (k in seq_len(nrow(text_rows))) {
  i <- match(text_rows$blakely_no[k], t4$blakely_no)
  add(taxon_name = t4$taxon_name[i], original_name = t4$name_verbatim[i], blakely_no = t4$blakely_no[i],
      blakely_series = t4$blakely_series[i], density_method = "", source_id = "Dadswell_1972", omit_duplicate = "", wood_tissue = "",
      trait = text_rows$trait[k], unit = "", source_section = text_rows$section[k],
      entity_type = "population", trait_value_raw = text_rows$raw[k], trait_value = text_rows$value[k],
      trait_value_clean = "", value_type = "mode", n = t4$t4.1_no_specimens[i],
      notes = paste(candidate_note, "From the text, supplementing the Table 4 category for this species."))
}

# Column 4.7 (mean of the 10 widest of 50 vessels per specimen) is stem_vessel_diameter with
# value_type maximum: the species mean of 4.7 (or, without a mean, the upper end of its range)
# becomes the maximum. The other 4.7 range ends, and the upper end of the 4.6 range (largest
# specimen mean), would duplicate that maximum and are kept but not mapped. Column 4.20
# (average height of the 10 tallest rays, in cells) is treated the same way as
# wood_ray_cell_longitudinal_count with value_type maximum.
rows <- bind_rows(rows) |>
  mutate(not_mapped_reason = "") |>
  group_by(taxon_name) |>
  mutate(
    is47 = grepl("col. 4.7", source_section),
    has47mean = any(is47 & value_type == "mean"),
    is420 = grepl("col. 4.20", source_section),
    has420mean = any(is420 & value_type == "mean"),
    not_mapped_reason = case_when(
      grepl("col. 4.6", source_section) & value_type == "maximum" ~
        "Upper end of the range of specimen means; the maximum vessel diameter is taken from col. 4.7.",
      is47 & value_type == "minimum" ~
        "Lower end of the range of specimen means of the 10 widest vessels; not a maximum.",
      is47 & value_type == "maximum" & has47mean ~
        "Upper end of the range of specimen means of the 10 widest vessels; the species mean of col. 4.7 is used as the maximum.",
      is420 & value_type == "minimum" ~
        "Lower end of the range of specimen means of the 10 tallest rays; not a maximum.",
      is420 & value_type == "maximum" & has420mean ~
        "Upper end of the range of specimen means of the 10 tallest rays; the species mean of col. 4.20 is used as the maximum.",
      TRUE ~ not_mapped_reason
    ),
    value_type = if_else((is47 | is420) & value_type == "mean", "maximum", value_type)
  ) |>
  ungroup() |>
  select(-is47, -has47mean, -is420, -has420mean)

# Columns 4.15 (paratracheal parenchyma aliform to confluent) and 4.16 (apotracheal parenchyma
# diffuse) both describe wood_axial_parenchyma_arrangement, so they are combined into one row per
# taxon. Section 4.16 notes that diffuse parenchyma in E. bancroftii (196) tends to link up into
# short tangential lines of a few cells, i.e. diffuse-in-aggregates.
arrangement <- rows |>
  filter(trait == "wood_axial_parenchyma_arrangement") |>
  group_by(taxon_name, original_name, blakely_no, blakely_series, entity_type, n) |>
  summarise(
    trait_value_raw = paste0("4.15: ", trait_value_raw[grepl("4.15", source_section)],
                             "; 4.16: ", trait_value_raw[grepl("4.16", source_section)]),
    trait_value = paste(setdiff(unlist(strsplit(trait_value, " ")), "diffuse_apotracheal_absent_or_rare"), collapse = " "),
    trait_value_clean = paste(unique(setdiff(unlist(strsplit(trait_value_clean, " ")), "")), collapse = " "),
    notes = paste(unique(setdiff(notes, "")), collapse = " "),
    .groups = "drop"
  ) |>
  mutate(
    source_section = "Table 4, cols 4.15 (paratracheal parenchyma aliform to confluent) and 4.16 (apotracheal parenchyma diffuse)",
    trait = "wood_axial_parenchyma_arrangement", value_type = "mode",
    density_method = "", source_id = "Dadswell_1972", omit_duplicate = "", wood_tissue = "",
    unit = "", not_mapped_reason = "",
    bancroftii = blakely_no == "196",
    trait_value = if_else(bancroftii, paste(trait_value, "diffuse_tending_to_short_tangential_lines"), trait_value),
    trait_value_clean = if_else(bancroftii, paste(trait_value_clean, "diffuse-in-aggregates"), trait_value_clean),
    source_section = if_else(bancroftii, paste0(source_section, "; section 4.16 text"), source_section),
    notes = if_else(bancroftii, trimws(paste(notes, "Section 4.16: diffuse parenchyma abundantly present, with frequently a tendency to link up and form short tangential lines of a few cells.")), notes)
  ) |>
  select(-bancroftii)
rows <- bind_rows(filter(rows, trait != "wood_axial_parenchyma_arrangement"), arrangement)

out <- rows |>
  mutate(entity_context = "", location_name = "", error_type = "", error_value = "") |>
  select(taxon_name, original_name, blakely_no, blakely_series, entity_type, entity_context,
         location_name, density_method, source_id, omit_duplicate, not_mapped_reason, wood_tissue, trait, trait_value_raw,
         trait_value, trait_value_clean, value_type, unit, error_type, error_value, n,
         source_section, notes)

write.csv(out, file.path(dir, "raw/trait_data.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(out, file.path(dir, "data.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(nrow(out), "rows,", length(unique(out$taxon_name)), "taxa,", length(unique(out$trait)), "traits\n")
