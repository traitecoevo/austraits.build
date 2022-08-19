
#' Build reports for listed datasets
#'
#' Builds a detailed report for every dataset with a unique `dataset_id`, based on the template Rmd file provided.  The reports are
#' rendered as html files and saved in the specified output folder.
#'
#' @param dataset_id name of specific study/dataset
#' @param austraits compiled austraits database
#' @param overwrite logical value to determine whether to overwrite existing report,
#' @param output_path location where rendered report will be saved
#' @param input_file report script (.Rmd) file to build study report
#' @param quiet An option to suppress printing during rendering from knitr, pandoc command line and others.
#' @param keep keep intermediate Rmd file used?
#'
#' @rdname dataset_report
#' @return html file of the rendered report located in the specified output folder.
#' @export
dataset_report <- function(dataset_id, austraits, overwrite=FALSE, 
                                    output_path = "export/reports", 
                                    input_file = system.file("support", "report_dataset.Rmd", package = "austraits.build"),
                                    quiet=TRUE, keep =FALSE) {
  
  for(d in dataset_id)
    dataset_report_worker(
      dataset_id = d, 
      austraits = austraits,
      overwrite = overwrite,
      output_path = output_path,
      input_file = input_file,
      quiet = quiet, 
      keep=keep
    )
}

dataset_report_worker <- function(dataset_id, austraits, overwrite=FALSE, 
                                           output_path = "export/reports", 
                                           input_file = system.file("support", "report_dataset.Rmd", package = "austraits.build"),
                                           quiet=TRUE, keep=FALSE) {
  
  if(!file.exists(output_path)) {
    dir.create(output_path, FALSE, TRUE)
  }
  
  # filenames
  input_Rmd <- sprintf("tmp_%s_report.Rmd", dataset_id)
  output_html <- sprintf("%s/%s.html", output_path, dataset_id)
  
  if(overwrite | !file.exists(output_html)) {
    
    cat(sprintf("Building report for %s ", dataset_id))
    
    # Create a new Rmd file with name embedded in title
    x <- readLines(input_file)
    x[2] <- sprintf("title: Report on study `%s` from", dataset_id)
    writeLines(x, input_Rmd)
    
    # knit and render. Note, call render directly
    # in preference to knit, then render, as leaflet widget 
    # requires this to work
    result <- try(
      rmarkdown::render(
        input_Rmd, 
        output_file = output_html, 
        quiet = quiet,
        params = list(
                  dataset_id = dataset_id,
                  austraits = austraits
          )
      )
    )

    # remove temporary Rmd
    if(!keep)
      unlink(input_Rmd)
    cat(" -> ", output_html, "\n")
  } else{
    cat(sprintf("Report for %s already exists -> %s\n", dataset_id, output_html))
  }
  
}

#' Format table with kable and default styling for html
#'
#' @param ... arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
util_kable_styling_html <- function(...) {
    txt <- 
      kableExtra::kable(...) %>%
      kableExtra::kable_styling(..., 
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE, 
                  position = "left"
                  ) 
    
    # hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', txt)
}
