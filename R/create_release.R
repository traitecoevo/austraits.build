#' Create release
#'
#' Builds various files to go with the release of a new AusTraits version. Including
#' austraits.rds, a readme file, dictionary.rmd file, plain text files with csv, and
#' a NEWS.md file.
#'
#' The steps around this function are in `RELEASING.md`.
#'
#' @param austraits AusTraits database object
#' @param v_prev previous release version (e.g. "7.0.0"). If given, `scripts/news.Rmd` is
#'   rendered into the export folder, comparing against
#'   `export/data/austraits-<v_prev>/austraits-<v_prev>.rds`, which must already exist.
#'   Default = NULL, no NEWS generated.
#' @export
create_release <- function(austraits, v_prev= NULL) {
  version_number <- austraits$build_info$version

  export_dir <- sprintf("export/data/austraits-%s", version_number)
  unlink(export_dir, recursive = TRUE)
  dir.create(export_dir, FALSE, TRUE)

  # RDS file
  filename <- sprintf(I("%s/austraits-%s.rds"), export_dir, version_number)
  saveRDS(austraits, filename)

  # dictionary_target:
  rmarkdown::render(
    "scripts/dictionary.Rmd",
    output_format = "all",
    output_dir = export_dir,
    params = list(austraits = austraits),
  )

  # plaintext_target:
  path <- sprintf("%s/austraits-%s", export_dir, version_number)
  traits.build::write_plaintext(austraits, path)

  # News
  # Written to the export folder only. The root NEWS.md holds every past release,
  # so the new section is merged into it by hand rather than copied over it.
  if (!is.null(v_prev)) {
    rmarkdown::render(
      "scripts/news.Rmd",
      output_format = rmarkdown::github_document(html_preview = FALSE),
      output_file = "NEWS.md",
      output_dir = export_dir,
      params = list(v_prev = v_prev, v_curr = version_number),
    )
  }

  # flattened database
  austraits_flattened <-
    austraits |>
    austraits::flatten_database()

  filename <- sprintf("%s/austraits-%s-flattened.rds", export_dir, version_number)

  austraits_flattened |>
    saveRDS(filename)

  filename <- sprintf("%s/austraits-%s-flattened.parquet", export_dir, version_number)

  austraits_flattened |>
    arrow::write_parquet(filename)

  # Go to directory and zip
  # remove existing file
  wd <- setwd(dirname(path))
  on.exit(setwd(wd))

  filename <- sprintf("%s.zip", basename(path))
  unlink(filename)
  utils::zip(filename, basename(path))
  unlink(basename(path), recursive = TRUE)

  message("Export created at ", export_dir)
}
