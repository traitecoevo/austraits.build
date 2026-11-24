# AusTraits' export step.
#
# No build runs this. `create_release()` in `R/create_release.R` calls
# `traits.build::build_post_export(austraits, export_dir)`, which sources this
# file and runs `post_export()` on the built database, writing into the
# release directory:
#
#   austraits-<version>-flattened.rds      one wide table, from
#                                          `traits.use::flatten_database()`
#   austraits-<version>-flattened.parquet  the same table as parquet
#
#   austraits-<version>-observed-means.parquet         one value per taxon and
#   austraits-<version>-observed-means.csv.gz          trait, computed from the
#   austraits-<version>-observed-means-dictionary.csv  records, by
#                                    `traits.use::summarise_trait_means()`;
#                                    AusTraits' settings for it are in
#                                    `R/observed_means.R`
#
# They are written here, at release, rather than in a `post_build()` step,
# because a `post_build()` runs on every build: these take about two minutes
# (roughly 25 seconds to flatten, 50 to save the rds, 25 for parquet, and 10
# for the observed means). Parquet is an AusTraits release output
# rather than a traits.build one (traits.build#201), which is why it is
# written here. A release needs `arrow` installed.
#
# `build_post_export()` sources this file into an environment of its own, from
# the repository root; the observed-means functions are sourced into that same
# environment, so `post_export()` finds them and nothing reaches the global
# environment.

source("R/observed_means.R", local = TRUE)

post_export <- function(database, path) {

  version_number <- database$build_info$version
  if (length(version_number) != 1 || is.na(version_number) || !nzchar(version_number)) {
    stop("The database records no version (`build_info$version`), which names the release files; ",
         "build it with `build_add_version()`, as `build.R` does.", call. = FALSE)
  }

  flattened <- traits.use::flatten_database(database)

  saveRDS(flattened, file.path(path, sprintf("austraits-%s-flattened.rds", version_number)))
  arrow::write_parquet(flattened, file.path(path, sprintf("austraits-%s-flattened.parquet", version_number)))

  write_observed_means(observed_means(database), path, version_number)
}
