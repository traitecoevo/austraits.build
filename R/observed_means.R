# Observed means: one value per taxon and trait, computed directly from the
# records of a built database (the "simple" species means).
#
# No build runs this. `post_export()` in `R/post_export.R` sources this file
# and writes the table into the release directory, so it is made once per
# release, from the same flattened table as the flattened exports.
#
# The method is the geometric method developed in traitecoevo/austraits-means
# (its `docs/observed-means-geometric-method.md` and `terminology.md`), moved
# here so that the observed means are part of each AusTraits release. The
# modelled means and the community layers stay in austraits-means and build on
# these observed means, so a change of method here changes them too.
#
# In brief, for a numeric trait:
#
#   - records with `value_type` raw, mean or median are used, whatever their
#     basis of record or life stage, grouped by dataset x location; the group
#     value is the geometric mean of its records;
#   - minimum and maximum records from preserved specimens and the literature
#     (floras) are grouped by observation; the group value is the geometric
#     midpoint, sqrt(minimum x maximum), or the lone bound if only one is given;
#   - datasets in `observed_means_value_type_rules()` use the value types
#     listed there instead;
#   - `value` is the geometric mean of the group values, each group counting
#     once whatever its number of records; `arithmetic_mean` is the arithmetic
#     mean of the same groups, kept in its own column;
#   - records with a zero or negative value are not used (a geometric mean
#     needs positive values).
#
# For a categorical trait: one row per taxon x trait, with the most frequent
# value (`value_mode`) and the count of every value (`value_count`).
#
# Subspecies, varieties and forms are rolled up to their species; genus-level
# names keep their own rows.

#' Traits the observed means are computed for.
#'
#' The data portal's core traits, plus `leaf_N_per_area`, `leaf_P_per_area`
#' and `woodiness`, which austraits-means needs. A starting list rather than a
#' settled scope: add a trait here to give it an observed mean. Every name
#' must be a trait in `config/traits.yml` (`check_observed_means_traits()`, so
#' a renamed trait stops the release rather than silently dropping out); a
#' trait with no usable records simply has no rows.
observed_means_traits <- function() {
  c(
    "bud_bank_location", "clonal_spread_mechanism",
    "dispersal_syndrome", "dispersal_unit", "dispersers",
    "fire_time_from_fire_to_flowering", "flower_colour", "flower_diameter",
    "flowering_time", "fruit_colour", "fruit_dehiscence", "fruit_dry_mass",
    "fruit_fleshiness", "fruit_height", "fruit_length", "fruit_type",
    "fruit_width", "huber_value", "leaf_area", "leaf_area_ratio",
    "leaf_arrangement", "leaf_axil_angle", "leaf_base_shape",
    "leaf_compoundness", "leaf_delta13C", "leaf_dry_mass",
    "leaf_dry_matter_content", "leaf_glaucousness", "leaf_hairs_adult_leaves",
    "leaf_hairs_immature_leaves", "leaf_hairs_juvenile_leaves",
    "leaf_inclination_angle", "leaf_lamina_division", "leaf_lamina_posture",
    "leaf_length", "leaf_lobation", "leaf_margin", "leaf_margin_posture",
    "leaf_mass_fraction", "leaf_mass_per_area", "leaf_N_per_area",
    "leaf_N_per_dry_mass", "leaf_P_per_area", "leaf_P_per_dry_mass",
    "leaf_phenology", "leaf_photosynthetic_rate_per_area_maximum",
    "leaf_photosynthetic_rate_per_dry_mass_maximum", "leaf_phyllotaxis",
    "leaf_shape", "leaf_stomatal_conductance_per_area_at_Amax",
    "leaf_stomatal_density_abaxial", "leaf_stomatal_density_adaxial",
    "leaf_type", "leaf_water_content_per_dry_mass", "leaf_width",
    "leaflet_area", "life_form", "life_history", "lifespan", "nitrogen_fixing",
    "parasitic", "petiole_length", "photosynthetic_pathway",
    "plant_climbing_mechanism", "plant_growth_form", "plant_growth_substrate",
    "plant_height", "plant_height_climbing_plant", "plant_photosynthetic_organ",
    "plant_physical_defence_structures", "plant_succulence",
    "pollination_syndrome", "post_fire_flowering",
    "post_fire_recruitment", "reproductive_maturity", "resprouting_capacity",
    "root_structure", "seed_dormancy_class", "seed_dry_mass", "seed_length",
    "seed_shape", "seed_viability", "seed_width", "seedbank_location",
    "seedbank_longevity_class", "serotiny", "sex_type", "stem_density",
    "stem_growth_habit", "stem_mass_fraction", "stem_vessel_density",
    "stem_vessel_diameter", "stem_vessel_lumen_fraction", "storage_organ",
    "vegetative_reproduction_ability", "wood_density", "woodiness",
    "woodiness_detailed"
  )
}

#' Per-dataset exceptions to the value types used.
#'
#' A dataset listed here enters the observed means with the value types given
#' here, as observations, instead of the default raw, mean and median.
#'
#' `AVH_2026` (heights from herbarium specimen labels) carries one bound per
#' specimen, a minimum or a maximum, and no raw values, so the default would
#' drop all of it. Each bound is used as one observation.
#'
#' @return tibble(dataset_id, value_type), one row per allowed combination.
observed_means_value_type_rules <- function() {
  tibble::tribble(
    ~dataset_id, ~value_type,
    "AVH_2026",  "minimum",
    "AVH_2026",  "maximum"
  )
}

#' Stop if a listed trait is not defined in `config/traits.yml`.
#'
#' The check is against the file rather than the database's `definitions`,
#' which hold only the traits some dataset maps: a listed trait without data
#' yet is fine, a name the dictionary no longer has is not.
#'
#' @param traits trait names to check.
#' @param file the trait definitions.
check_observed_means_traits <- function(traits = observed_means_traits(),
                                        file = "config/traits.yml") {
  unknown <- setdiff(traits, names(yaml::read_yaml(file)$traits$elements))
  if (length(unknown) > 0) {
    stop(
      sprintf("Traits listed for the observed means are not in `%s`: ", file),
      paste(unknown, collapse = ", "),
      ". Update `observed_means_traits()` in `R/observed_means.R`.",
      call. = FALSE
    )
  }
  invisible(traits)
}

#' Observed means of a database.
#'
#' @param database a built traits.build database.
#' @param flattened `traits.use::flatten_database(database)`, if the caller
#'   already has it (flattening takes longer than everything else here).
#' @param traits trait names to compute means for.
#' @param rules per-dataset value-type rules.
#' @return one row per taxon x trait (x unit, for numeric traits), with the
#'   columns `observed_means_dictionary()` defines.
observed_means <- function(database,
                           flattened = traits.use::flatten_database(database),
                           traits = observed_means_traits(),
                           rules = observed_means_value_type_rules()) {

  records <- observed_means_records(flattened, database$taxa, traits)

  estimate_observed_means(records, rules) |>
    dplyr::mutate(estimate_type = "observed_mean", method = "geometric") |>
    dplyr::relocate("estimate_type", "method", .after = "unit")
}

#' Records of the given traits, with infraspecific taxa rolled up to species.
#'
#' Records of a subspecies, variety or form are relabelled with their species
#' (the taxa table's `binomial`). Taxon details (`taxon_rank`, `genus`,
#' `family`, `establishment_means`, `taxon_id`) then come from the species'
#' own row of the taxa table; a species that has no row of its own gets rank
#' "species" and keeps its records' genus and family.
#'
#' @param flattened a flattened database.
#' @param taxa the database's taxa table.
#' @param traits trait names to keep.
observed_means_records <- function(flattened, taxa, traits) {

  species_of <- taxa |>
    dplyr::filter(
      .data$taxon_rank %in% c("subspecies", "variety", "form"),
      !is.na(.data$binomial),
      .data$binomial != .data$taxon_name
    ) |>
    dplyr::distinct(.data$taxon_name, .species = .data$binomial)

  flattened |>
    dplyr::filter(.data$trait_name %in% traits) |>
    dplyr::select(
      "dataset_id", "taxon_name", "observation_id", "trait_name", "value",
      "unit", "value_type", "basis_of_record", "location_id", "original_name",
      "taxon_rank", "genus", "family"
    ) |>
    dplyr::left_join(species_of, by = "taxon_name") |>
    dplyr::mutate(
      taxon_rank = dplyr::if_else(is.na(.data$.species), .data$taxon_rank, "species"),
      taxon_name = dplyr::coalesce(.data$.species, .data$taxon_name)
    ) |>
    dplyr::select(-".species") |>
    dplyr::left_join(
      dplyr::distinct(
        dplyr::select(taxa, "taxon_name", "establishment_means", "taxon_id"),
        .data$taxon_name, .keep_all = TRUE
      ),
      by = "taxon_name"
    )
}

#' Observed means from prepared records.
#'
#' @param records output of `observed_means_records()`.
#' @param rules per-dataset value-type rules.
estimate_observed_means <- function(records, rules = observed_means_value_type_rules()) {

  numeric <- dplyr::filter(records, !is.na(.data$unit))
  categorical <- dplyr::filter(records, is.na(.data$unit))

  taxa <- records |>
    dplyr::distinct(
      .data$taxon_name, .data$taxon_rank, .data$genus, .data$family,
      .data$establishment_means, .data$taxon_id
    ) |>
    dplyr::distinct(.data$taxon_name, .keep_all = TRUE)

  dplyr::bind_rows(
    observed_means_numeric(numeric, rules) |> dplyr::mutate(type = "numerical"),
    observed_means_categorical(categorical) |> dplyr::mutate(type = "categorical")
  ) |>
    dplyr::left_join(taxa, by = "taxon_name") |>
    dplyr::arrange(.data$taxon_name, .data$trait_name)
}

observed_means_numeric <- function(records, rules) {

  records <- records |>
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) |>
    dplyr::filter(!is.na(.data$value), .data$value > 0)

  ruled <- paste(rules$dataset_id, rules$value_type)
  is_ruled_dataset <- records$dataset_id %in% rules$dataset_id

  # Records summarised per dataset x location: raw, mean and median records,
  # and whatever the rules allow for the datasets they name
  location_records <- records[
    (is_ruled_dataset & paste(records$dataset_id, records$value_type) %in% ruled) |
      (!is_ruled_dataset & records$value_type %in% c("raw", "mean", "median")), ]

  # Bounds from floras, summarised per observation
  flora_records <- records[
    !is_ruled_dataset &
      records$value_type %in% c("minimum", "maximum") &
      records$basis_of_record %in% c("preserved_specimen", "literature"), ]

  # `min(x, Inf)` and `max(x, -Inf)` are `min(x)` and `max(x)` without the
  # warning for no values: with no records of a kind, dplyr still evaluates
  # each summary once, on nothing
  group_summary <- function(data) {
    dplyr::summarise(
      data,
      .groups = "drop",
      log_mean = mean(log(.data$value)),
      arithmetic = mean(.data$value),
      value_min = min(.data$value, Inf),
      value_max = max(.data$value, -Inf),
      n_records = dplyr::n()
    )
  }

  record_stats <- dplyr::bind_rows(
    dplyr::mutate(location_records, flora = FALSE),
    dplyr::mutate(flora_records, flora = TRUE)
  ) |>
    dplyr::group_by(.data$taxon_name, .data$trait_name, .data$unit) |>
    dplyr::summarise(
      .groups = "drop",
      n_flora = sum(.data$flora),
      n_field = sum(.data$basis_of_record %in% "field"),
      sd_log = if (dplyr::n() > 1) stats::sd(log(.data$value)) else NA_real_
    )

  locations <- location_records |>
    dplyr::group_by(
      .data$taxon_name, .data$trait_name, .data$unit, .data$dataset_id, .data$location_id
    ) |>
    group_summary() |>
    dplyr::mutate(kind = "location")

  floras <- flora_records |>
    dplyr::group_by(
      .data$taxon_name, .data$trait_name, .data$unit, .data$dataset_id,
      .data$observation_id, .data$original_name
    ) |>
    group_summary() |>
    dplyr::mutate(kind = "flora")

  dplyr::bind_rows(locations, floras) |>
    dplyr::group_by(.data$taxon_name, .data$trait_name, .data$unit) |>
    dplyr::summarise(
      .groups = "drop",
      log_value = mean(.data$log_mean),
      arithmetic_mean = mean(.data$arithmetic),
      value_min = min(.data$value_min, Inf),
      value_max = max(.data$value_max, -Inf),
      n_records = sum(.data$n_records),
      n_locations = sum(.data$kind == "location"),
      n_datasets = dplyr::n_distinct(.data$dataset_id),
      dataset_id = paste(sort(unique(.data$dataset_id)), collapse = "; ")
    ) |>
    dplyr::mutate(value = exp(.data$log_value), .after = "unit") |>
    dplyr::left_join(record_stats, by = c("taxon_name", "trait_name", "unit")) |>
    dplyr::relocate("n_flora", "n_field", "sd_log", .after = "n_datasets") |>
    dplyr::mutate(
      reliability = observed_means_reliability(.data$n_datasets, .data$n_records),
      .after = "sd_log"
    )
}

# Categorical traits: one row per taxon x trait, with value counts and the
# modal value (ties joined by a space, in sorted order). A record listing
# several space-separated values counts once for each.
observed_means_categorical <- function(records) {

  counts <- records |>
    dplyr::mutate(value = stringr::str_split(.data$value, " ")) |>
    tidyr::unnest_longer("value") |>
    dplyr::filter(!is.na(.data$value), .data$value != "") |>
    dplyr::count(.data$taxon_name, .data$trait_name, .data$value, name = "n")

  modes <- counts |>
    dplyr::group_by(.data$taxon_name, .data$trait_name) |>
    dplyr::arrange(.data$value, .by_group = TRUE) |>
    dplyr::summarise(
      .groups = "drop",
      value_mode = paste(.data$value[.data$n == max(.data$n, 0L)], collapse = " "),
      value_count = paste0(.data$value, " (", .data$n, ")", collapse = "; ")
    )

  records |>
    dplyr::group_by(.data$taxon_name, .data$trait_name) |>
    dplyr::summarise(
      .groups = "drop",
      n_records = dplyr::n(),
      n_datasets = dplyr::n_distinct(.data$dataset_id),
      n_field = sum(.data$basis_of_record %in% "field"),
      dataset_id = paste(sort(unique(.data$dataset_id)), collapse = "; ")
    ) |>
    dplyr::left_join(modes, by = c("taxon_name", "trait_name")) |>
    dplyr::mutate(
      reliability = observed_means_reliability(.data$n_datasets, .data$n_records)
    )
}

#' Provisional reliability class, from the evidence behind a value.
observed_means_reliability <- function(n_datasets, n_records) {
  dplyr::case_when(
    n_datasets >= 3 & n_records >= 10 ~ "high",
    n_datasets >= 2 | n_records >= 5 ~ "medium",
    TRUE ~ "low"
  )
}

#' Column dictionary for the observed means, in column order.
observed_means_dictionary <- function() {
  tibble::tribble(
    ~column, ~definition,
    "taxon_name", "Taxon name as aligned in the AusTraits build, with subspecies, varieties and forms rolled up to their species. Genus-level names are kept as their own rows.",
    "trait_name", "AusTraits trait name (APD).",
    "unit", "Unit of the numeric trait values; empty for categorical traits.",
    "estimate_type", "Always observed_mean: computed directly from the records, with no model.",
    "method", "Always geometric: the geometric method.",
    "value", "Numeric traits: the observed mean, a geometric mean. Records are averaged on the log scale within each dataset x location, flora minimum and maximum bounds give a geometric midpoint per observation, and the value is exp(mean) over those groups, each group counting once. Records with zero or negative values are not used.",
    "log_value", "Natural log of value.",
    "arithmetic_mean", "Arithmetic mean over the same groups and records (each group's arithmetic mean or midpoint, then their mean). Not the observed mean; given for comparison.",
    "value_min", "Smallest record value used.",
    "value_max", "Largest record value used.",
    "n_records", "Number of records used.",
    "n_locations", "Numeric traits: number of distinct dataset x location groups.",
    "n_datasets", "Number of AusTraits datasets contributing records.",
    "n_flora", "Numeric traits: number of minimum and maximum records from preserved specimens or the literature (floras) used.",
    "n_field", "Number of records used whose basis of record is field.",
    "sd_log", "Numeric traits: standard deviation of the natural log of the record values used; empty with a single record.",
    "reliability", "Provisional class from the evidence behind the value: high (at least 3 datasets and 10 records), medium (at least 2 datasets or 5 records), low (otherwise). Thresholds are provisional.",
    "dataset_id", "Datasets contributing, sorted, joined by '; '. Each dataset's reference is in the database's sources table under the same key.",
    "type", "numerical or categorical.",
    "value_mode", "Categorical traits: the most frequent value; tied values are joined by a space.",
    "value_count", "Categorical traits: each value with the number of records carrying it, as 'value (n)' joined by '; '. A record listing several space-separated values counts once for each.",
    "taxon_rank", "Taxonomic rank of taxon_name; empty for a name matched to neither the APC nor the APNI.",
    "genus", "Genus.",
    "family", "Family.",
    "establishment_means", "Native, naturalised or both, as given by APC.",
    "taxon_id", "APC taxon identifier."
  )
}

#' Write the observed means and their dictionary.
#'
#' Writes `<stem>.parquet`, `<stem>.csv.gz` and `<stem>-dictionary.csv`. The
#' dictionary must define every column, in order, and stops the release if a
#' column was added or renamed without it.
#'
#' @param means output of `observed_means()`.
#' @param path directory to write into.
#' @param version_number the database version, used in the file names.
#' @return the three file paths, invisibly.
write_observed_means <- function(means, path, version_number) {

  dictionary <- observed_means_dictionary()
  if (!identical(names(means), dictionary$column)) {
    stop(
      "The observed means' columns do not match `observed_means_dictionary()`: ",
      paste(union(setdiff(names(means), dictionary$column),
                  setdiff(dictionary$column, names(means))), collapse = ", "),
      if (setequal(names(means), dictionary$column)) "the order differs",
      call. = FALSE
    )
  }
  dictionary$type <- vapply(means, function(x) class(x)[1], character(1))
  dictionary <- dictionary[c("column", "type", "definition")]

  stem <- file.path(path, sprintf("austraits-%s-observed-means", version_number))
  files <- paste0(stem, c(".parquet", ".csv.gz", "-dictionary.csv"))

  arrow::write_parquet(means, files[1])
  readr::write_csv(means, files[2], na = "")
  readr::write_csv(dictionary, files[3])

  invisible(files)
}
