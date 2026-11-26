# Observed means: one value per taxon and trait, computed directly from the
# records of a built database (the "simple" species means).
#
# No build runs this. `post_export()` in `R/post_export.R` sources this file
# and writes the table into the release directory, so it is made once per
# release.
#
# The calculation is `traits.use::summarise_trait_means()`, which any
# traits.build database can use; its help page describes the method. In
# brief: for a numeric trait, the geometric mean of dataset x location means
# and flora range midpoints, each counting once (an arithmetic mean for a
# trait whose values can be negative); for a categorical trait, the
# most frequent value and the count of every value; for every trait in the
# database, with subspecies, varieties and forms rolled up to their species.
#
# What is specific to AusTraits is here: the taxa published (APC-accepted
# species only), the value-type rule below, and the files the release carries. The modelled means and the community layers are
# built in traitecoevo/austraits-means, on top of these observed means, so a
# change of method changes them too.

#' Per-dataset exceptions to the value types used.
#'
#' A dataset listed here enters the observed means with the value types given
#' here, as observations, instead of the default raw, mean and median.
#'
#' `AVH_2026` (heights from herbarium specimen labels) records a minimum or a
#' maximum per specimen, and a few specimens both, with `basis_of_record`
#' `field` and no raw values. The default uses bounds only from preserved
#' specimens and the literature, so it would drop all of them. With this rule
#' each bound is a record, averaged with the other AVH records at its location
#' (one specimen per location, mostly) like a raw value.
#'
#' @return tibble(dataset_id, value_type), one row per allowed combination.
observed_means_value_type_rules <- function() {
  tibble::tribble(
    ~dataset_id, ~value_type,
    "AVH_2026",  "minimum",
    "AVH_2026",  "maximum"
  )
}

#' Is a name a hybrid formula or intergrade?
#'
#' "Acacia ampliceps x Acacia bivenosa", "(A x B) x C" and "Apium prostratum
#' var. filiforme - Apium prostratum var. prostratum" are: " x " or " - " is
#' followed by a genus, the name's own, or by a bracket. Named nothospecies
#' and nothovarieties ("Acacia x grayana", "Eucalyptus pauciflora var. x
#' cylindrocarpa") are not, nor are phrase names ("Populus x Loch Valley
#' (I.C.Clarke 2861)").
#'
#' @param taxon_name character vector.
#' @return a logical vector.
observed_means_hybrid_formula <- function(taxon_name) {
  genus <- sub("^\\(?([^ ]+).*$", "\\1", taxon_name)
  genus <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", genus)
  out <- mapply(function(name, genus) grepl(paste0("\\s(x|-)\\s(\\(|", genus, "\\s)"), name),
                taxon_name, genus, USE.NAMES = FALSE)
  as.logical(out) %in% TRUE
}

#' Which rows of a taxa table the APC accepts, at species rank or below,
#' other than hybrid formulae.
#'
#' @param taxa a data frame with columns `taxon_name`, `taxonomic_dataset`,
#'   `taxonomic_status` and `taxon_rank`: a database's taxa table, or a flat
#'   table.
#' @return a logical vector, one per row of `taxa`.
observed_means_accepted <- function(taxa) {
  needed <- c("taxon_name", "taxonomic_dataset", "taxonomic_status", "taxon_rank")
  missing <- setdiff(needed, names(taxa))
  if (length(missing) > 0) {
    stop("The observed means are published for APC-accepted species only, which needs the taxonomy: ",
         "the table lacks ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  !is.na(taxa$taxon_name) &
    taxa$taxonomic_dataset %in% "APC" &
    taxa$taxonomic_status %in% "accepted" &
    taxa$taxon_rank %in% c("species", "subspecies", "variety", "form") &
    !observed_means_hybrid_formula(taxa$taxon_name)
}

#' The species AusTraits publishes observed means for (#922).
#'
#' The species the APC accepts, and the species of the subspecies, varieties
#' and forms it accepts, other than hybrid formulae. Named nothospecies, such
#' as "Acacia x grayana", are APC-accepted species. Genus- and family-level
#' names, names APCalign could not align below genus, APNI names the APC does
#' not place, excluded names and synonyms are not published as taxa of their
#' own. The records of a subspecies, variety or form count towards its
#' species whatever its own status, as `summarise_trait_means()` rolls them
#' up: the records of "Aloe vera var. vera", an autonym the APC does not
#' place, are records of "Aloe vera", which it accepts.
#'
#' @inheritParams observed_means_accepted
#' @return a character vector of species names.
observed_means_species <- function(taxa) {
  accepted <- observed_means_accepted(taxa)
  rank_marker <- "\\s(subsp|var|f|forma|nothosubsp|nothovar)\\.\\s.*$"
  species <- unique(sub(rank_marker, "", taxa$taxon_name[accepted]))
  # a species whose own row the APC does not accept stays out
  setdiff(species, taxa$taxon_name[!accepted])
}

#' Observed means of a database, as AusTraits publishes them: for
#' APC-accepted species only (`observed_means_species()`).
#'
#' @param database a built traits.build database, or a flat table from one
#'   that carries the taxonomy columns.
#' @return one row per taxon x trait (x unit, for numeric traits), with the
#'   columns `traits.use::trait_means_dictionary()` defines.
observed_means <- function(database) {
  published <- observed_means_species(if (is.data.frame(database)) database else database$taxa)
  means <- traits.use::summarise_trait_means(
    database,
    value_type_rules = observed_means_value_type_rules()
  )
  means[means$taxon_name %in% published, ]
}

#' Write the observed means and their dictionary.
#'
#' Writes `<stem>.parquet`, `<stem>.rds`, `<stem>.csv.gz` and
#' `<stem>-dictionary.csv`. The dictionary must define every column, in order,
#' and stops the release if the two have drifted apart.
#'
#' The csv is gzipped, not zipped: it is one file, gzip streams, and most csv
#' readers (readr, data.table, arrow, duckdb, polars, pandas) open it directly.
#' traits.use's and austraits' `load_*(means = TRUE)` find it by that name.
#'
#' @param means output of `observed_means()`.
#' @param path directory to write into.
#' @param version_number the database version, used in the file names.
#' @return the four file paths, invisibly.
write_observed_means <- function(means, path, version_number) {

  if (length(version_number) != 1 || is.na(version_number) || !nzchar(version_number)) {
    stop("The database records no version (`build_info$version`), which names the files; ",
         "build it with `build_add_version()`, as `build.R` does.", call. = FALSE)
  }

  # A taxon column is in the table only if the database's taxa table has it
  dictionary <- traits.use::trait_means_dictionary()
  # AusTraits publishes APC-accepted species only (`observed_means_species()`)
  dictionary$definition[dictionary$column == "taxon_name"] <- paste(
    "Taxon name: a species the Australian Plant Census (APC) accepts, as aligned in AusTraits.",
    "Records of its subspecies, varieties and forms count towards it.",
    "Genus-level names, names not aligned to an accepted species, and hybrid formulae are not published."
  )
  dictionary <- dictionary[dictionary$column %in% names(means) |
                             !dictionary$column %in% c("taxon_rank", "genus", "family", "establishment_means", "taxon_id"), ]
  if (!identical(names(means), dictionary$column)) {
    stop(
      "The observed means' columns do not match `traits.use::trait_means_dictionary()`: ",
      paste(union(setdiff(names(means), dictionary$column),
                  setdiff(dictionary$column, names(means))), collapse = ", "),
      if (setequal(names(means), dictionary$column)) "the order differs",
      call. = FALSE
    )
  }
  dictionary$type <- vapply(means, function(x) class(x)[1], character(1))
  dictionary <- dictionary[c("column", "type", "definition")]

  stem <- file.path(path, sprintf("austraits-%s-observed-means", version_number))
  files <- paste0(stem, c(".parquet", ".rds", ".csv.gz", "-dictionary.csv"))

  arrow::write_parquet(means, files[1])
  saveRDS(means, files[2])
  readr::write_csv(means, files[3], na = "")
  readr::write_csv(dictionary, files[4])

  invisible(files)
}
