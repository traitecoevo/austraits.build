# AusTraits' export step.
#
# No build runs this. `create_release()` in `R/create_release.R` calls
# `traits.build::build_post_export(austraits, export_dir)`, which sources this
# file and runs `post_export()` on the built database, writing the flattened
# exports into the release directory:
#
#   austraits-<version>-flattened.rds      one wide table, from
#                                          `traits.use::flatten_database()`
#   austraits-<version>-flattened.parquet  the same table as parquet
#
# They are written here, at release, rather than in a `post_build()` step,
# because a `post_build()` runs on every build: these take about 90 seconds
# (13 to flatten, 48 to save the rds, 26 for parquet). Parquet is an AusTraits
# release output rather than a traits.build one (traits.build#201), which is
# why it is written here. A release needs `arrow` installed.

post_export <- function(database, path) {

  version_number <- database$build_info$version

  flattened <- traits.use::flatten_database(database)

  saveRDS(flattened, file.path(path, sprintf("austraits-%s-flattened.rds", version_number)))
  arrow::write_parquet(flattened, file.path(path, sprintf("austraits-%s-flattened.parquet", version_number)))
}
