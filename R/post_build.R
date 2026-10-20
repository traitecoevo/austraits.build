# AusTraits' post-build step.
#
# `build.R` calls `traits.build::build_post_process()` after
# `build_add_version()`, which sources this file and runs `post_build()` on the
# compiled database before `build.R` saves it as `export/data/curr/austraits.rds`.
#
# It writes the exports AusTraits releases beside that file, and returns the
# database unchanged:
#
#   export/data/curr/austraits-flattened.rds      one wide table, from
#                                                 `austraits::flatten_database()`
#   export/data/curr/austraits-flattened.parquet  the same table as parquet;
#                                                 skipped if `arrow` cannot load
#
# `create_release()` in `scripts/release.Rmd` copies these into a release rather
# than recomputing them. Parquet is an AusTraits release output rather than a
# traits.build one (traits.build#201), which is why it is written here.
#
# On a full build this step takes about 90 seconds: 13 to flatten, 48 to save
# the flattened rds (gzip of a 2.3 million row table) and 26 for parquet. The
# plaintext csv export and its zip stay in `create_release()`. Writing the csv
# files takes under a second, so a release loses nothing by recomputing them,
# whereas writing them on every build would put another 550 MB in
# `export/data/curr`; the zip takes 11 seconds and is needed only for a release.

post_build <- function(database) {

  path <- file.path("export", "data", "curr")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  flattened <- austraits::flatten_database(database)
  saveRDS(flattened, file.path(path, "austraits-flattened.rds"))

  write_flattened_parquet(flattened, file.path(path, "austraits-flattened.parquet"))

  database
}

# `arrow` is in Suggests, and on some machines its compiled library breaks when
# the system Arrow is upgraded, in which case it is installed but will not load.
# Neither should stop a build, so the parquet file is skipped with a message
# saying why. A copy left by an earlier build is removed, so that
# `export/data/curr` never holds a parquet file that disagrees with the rds.
write_flattened_parquet <- function(flattened, file) {

  loaded <- tryCatch(
    {
      loadNamespace("arrow")
      TRUE
    },
    error = function(e) conditionMessage(e)
  )

  if (!isTRUE(loaded)) {
    unlink(file)
    # The first two lines say whether arrow is missing or installed but broken
    message(sprintf(
      "Skipping `%s`: package `arrow` cannot be loaded.\n  %s",
      file, paste(utils::head(strsplit(loaded, "\n")[[1]], 2), collapse = "\n  ")
    ))
    return(invisible(NULL))
  }

  arrow::write_parquet(flattened, file)
}
