
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
  knitr::knit("scripts/README.Rmd", output = sprintf("%s/readme.txt", export_dir))

  # dictionary_target:
  rmarkdown::render("scripts/dictionary.Rmd", params = list(austraits = austraits), output_file = sprintf("../%s/dictionary.html", export_dir))

  # plaintext_target:
  path <- sprintf("%s/austraits-%s", export_dir, version_number)
  write_plaintext(austraits, path)

  # News
  if (!is.null(v_prev)) {
    rmarkdown::render("scripts/news.Rmd",
      params = list(v_prev = v_prev, v_curr = version_number),
      output_file = "tmp_news.md"
    )

    f1 <- readLines("scripts/tmp_news.md")
    f2 <- readLines("NEWS.md")

    writeLines(c(f1, "\n", f2), "NEWS.md")
  }

  file.copy("NEWS.md", sprintf("%s/NEWS.md", export_dir))

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
