
#' Create release
#'
#' Builds various files to go with the release of a new AusTraits version. Including
#' austraits.rds, a readme file, dictionary.rmd file, plain text files with csv, and
#' a NEWS.md file.
#'
#' @param austraits AusTraits database object
#' @param v_prev specify whether to update the NEWS.md file, default = NULL
#' @export
create_release <- function(austraits, v_prev= NULL) {
  version_number <- austraits$build_info$version

  export_dir <- sprintf("export/data/austraits-%s", version_number)
  unlink(export_dir, recursive = TRUE)
  dir.create(export_dir, FALSE, TRUE)

  # RDS file
  filename <- sprintf(I("%s/austraits-%s.rds"), export_dir, version_number)
  saveRDS(austraits, filename)

  # readme
  rmarkdown::render(
    "scripts/README.Rmd", 
    output_format = "all",
    output_dir = export_dir, 
    params = list(version_number = version_number)
  )

  # dictionary_target:
  rmarkdown::render(
    "scripts/dictionary.Rmd", 
    output_format = "all",
    output_dir = export_dir,
    params = list(austraits = austraits), 
  )

  # plaintext_target:
  path <- sprintf("%s/austraits-%s", export_dir, version_number)
  write_plaintext(austraits, path)

  # News
  if (!is.null(v_prev)) {
    rmarkdown::render(
      "scripts/news.Rmd",
      output_format = "all",
      output_dir = export_dir,
      params = list(v_prev = v_prev, v_curr = version_number),
    )

  file.copy(sprintf("%s/NEWS.md", export_dir), "NEWS.md")
  }

  # Go to directory and zip
  # remove existing file
  wd <- setwd(dirname(path))
  on.exit(setwd(wd))

  filename <- sprintf("%s.zip", basename(path))
  unlink(filename)
  utils::zip(filename, basename(path))
  unlink(path, recursive = TRUE)

  message("Export created at ", export_dir)

}
