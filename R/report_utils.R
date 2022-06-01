#' Build reports for all studies
#' 
#' Build reports for all studies using the `build_dataset_report()` function. This
#' function builds a study report for every study with a unique `dataset_id` that
#' has been loaded into AusTraits using `build_dataset_report()`. The reports are 
#' rendered as html files and saved in the "export/reports" folder.  
#'
#' @param dataset_ids names of studies/ datasets, default is NULL
#' @param ... arguments passed to build_dataset_report()
#'
#' @return html files of the study report for all studies 
#' @export
build_dataset_reports <- function(dataset_ids=NULL, ...) {

  # define if does not already exist, 
  # for all studies with suitable metadata file
  if(length(dataset_ids) ==0 | is.null(dataset_ids) | any(is.na(dataset_ids)))
    dataset_ids <- list.files("data")
  
  for(dataset_id in dataset_ids)
    build_dataset_report(dataset_id, ...)
}

#' Build report for a specific study
#' 
#' Builds a report for a specified study using `dataset_id`. The information for 
#' the report is stored in an Rmd file and the final report is rendered as an html.  
#' The report is generated from the `report_study.Rmd` file in the scripts folder. 
#' Existing reports can be overwritten by setting overwrite = TRUE
#'
#' @param dataset_id name of specific study/dataset
#' @param overwrite logical value to determine whether to overwrite existing report,
#' default = FALSE, if report exists already set to TRUE to overwrite
#' @param output_path location where rendered report will be saved
#' @param input_file report script (.Rmd) file to build study report
#'
#' @return html file of the rendered report located in the "export/reports" folder
#' @export
build_dataset_report <- function(dataset_id, overwrite=FALSE, output_path = "export/reports", 
                                 input_file = system.file("support", "report_dataset.Rmd", package = "austraits.build")) {

  if(!file.exists(output_path)) {
    dir.create(output_path, FALSE, TRUE)
  }
  
  cat(sprintf("Building report for %s: ", dataset_id))
  
  # filenames
  input_Rmd <- sprintf("%s/%s.Rmd", output_path, dataset_id)
  output_html <- sprintf("%s/%s.html", output_path, dataset_id)
  
  if(overwrite | !file.exists(output_html)) {
    
    # Create a new Rmd file with name embedded in title
    x <- readLines(input_file)
    x <- gsub("title: Report on study from",  sprintf("title: Report on study `%s` from", dataset_id), x)
    writeLines(x, input_Rmd)
    
    # knit and render. Note, call render directly
    # in preference to knit, then render, as leaflet widget 
    # requires this to work
    try(rmarkdown::render(input_Rmd, quiet=TRUE))
    
    # remove temporary Rmd
    unlink(input_Rmd)
  }
  cat(" -> done\n")
}

#' Get SHA link from Github
#' 
#' Get SHA link using the get_SHA() function. The link generated leads to the latest
#' commit for the Github repository. SHA is the abbreviated SHA-1 40 digit
#' hexadecimal number which Github uses to track commits and changes made to a repository. 
#' 
#' @param ... arguments passed to the get_SHA()
#'
#' @return SHA link to a github commit as a character string formatted using markdown syntax
#' @export
get_SHA_link <- function(...) {
  sha <- get_SHA(...)
  sprintf("[%s](https://github.com/traitecoevo/austraits/tree/%s)",   sha, sha)
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
get_SHA <- function(path = rprojroot::find_root("remake.yml")) {
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


#' Generate hyperlink for markdown and html
#' 
#' Generate hyperlink for markdown and html files
#'
#' @param link character string for the url link
#' @param text character string for the text to display
#' @param type file type, default is markdown "md" otherwise html
#'
#' @return character string with the text and link formatted for md and html
#' @export
#'
#' @examples as_link("www.austraits.org", "austraits")
as_link <- function(link, text, type="md") {
  if(type=="md")
    sprintf('[%s](%s)', text, link)
  else
    sprintf("<a href='%s'> %s </a>", link, text)
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
