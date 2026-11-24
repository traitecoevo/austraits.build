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
# What is specific to AusTraits is here: the value-type rule below, and the
# files the release carries. The modelled means and the community layers are
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

#' Observed means of a database, as AusTraits publishes them.
#'
#' @param database a built traits.build database.
#' @return one row per taxon x trait (x unit, for numeric traits), with the
#'   columns `traits.use::trait_means_dictionary()` defines.
observed_means <- function(database) {
  traits.use::summarise_trait_means(
    database,
    value_type_rules = observed_means_value_type_rules()
  )
}

#' Write the observed means and their dictionary.
#'
#' Writes `<stem>.parquet`, `<stem>.csv.gz` and `<stem>-dictionary.csv`. The
#' dictionary must define every column, in order, and stops the release if
#' the two have drifted apart.
#'
#' @param means output of `observed_means()`.
#' @param path directory to write into.
#' @param version_number the database version, used in the file names.
#' @return the three file paths, invisibly.
write_observed_means <- function(means, path, version_number) {

  if (length(version_number) != 1 || is.na(version_number) || !nzchar(version_number)) {
    stop("The database records no version (`build_info$version`), which names the files; ",
         "build it with `build_add_version()`, as `build.R` does.", call. = FALSE)
  }

  # A taxon column is in the table only if the database's taxa table has it
  dictionary <- traits.use::trait_means_dictionary()
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
  files <- paste0(stem, c(".parquet", ".csv.gz", "-dictionary.csv"))

  arrow::write_parquet(means, files[1])
  readr::write_csv(means, files[2], na = "")
  readr::write_csv(dictionary, files[3])

  invisible(files)
}
