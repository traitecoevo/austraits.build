
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
#' @rdname dataset_generate_report
#' @return html file of the rendered report located in the specified output folder.
#' @export
dataset_generate_report <- function(dataset_id, austraits, overwrite=FALSE, 
                                    output_path = "export/reports", 
                                    input_file = system.file("support", "report_dataset.Rmd", package = "austraits.build"),
                                    quiet=TRUE, keep =FALSE) {
  
  for(d in dataset_id)
    dataset_generate_report_worker(
      dataset_id = d, 
      austraits = austraits,
      overwrite = overwrite,
      output_path = output_path,
      input_file = input_file,
      quiet = quiet, 
      keep=keep
    )
}

dataset_generate_report_worker <- function(dataset_id, austraits, overwrite=FALSE, 
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
        params= list(
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

#' Get SHA string from Github repository for latest commit
#' 
#' Get SHA string for the latest commit on Github for the repository. SHA is the
#' abbreviated SHA-1 40 digit hexadecimal number which Github uses as the 
#' Commit ID to track changes made to a repo
#'
#' @param path root directory where a specified file is located, default file name
#' is the remake.yml file
#'
#' @return 40-digit SHA character string for the latest commit to the repository 
#' @export
get_SHA <- function(path = ".") {
  git2r::sha(git2r::last_commit(git2r::repository(path)))
}

#' Format table with kable and default styling for html
#'
#' @param ... arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
my_kable_styling_html <- function(...) {
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




#' Format a tree structure from a vector 
#' 
#' `create_tree_branch()` is used to create a tree structure to show how things
#' are related. In AusTraits, this is used in the vignettes to show the file 
#' structure of the repository and also to show the different components of the 
#' AusTraits database 
#'
#' @param x vector of terms
#' @param title name of branch
#' @param prefix specifies the amount of indentation
#'
#' @return vector of character strings for the tree structure
create_tree_branch <- function(x, title, prefix="") {
  c(
    sprintf("%s%s", prefix,title), 
    sprintf("%s%s %s", prefix,
            c(rep("\u251c\u2500\u2500", length(x) -1), "\u2514\u2500\u2500"),
            x)
  )
}
