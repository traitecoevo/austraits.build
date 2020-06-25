#' Title
#'
#' @param dataset_ids 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
build_study_reports <- function(dataset_ids=NULL, ...) {

  # define if does not already exist, 
  # for all studies with suitable metadata file
  if(length(dataset_ids) ==0 | is.null(dataset_ids) | any(is.na(dataset_ids)))
    dataset_ids <- austraits$trait$dataset_id %>% unique()
  
  for(dataset_id in dataset_ids)
    build_study_report(dataset_id, ...)
}

#' Title
#'
#' @param dataset_id 
#' @param overwrite 
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
build_study_report <- function(dataset_id, overwrite=FALSE, path = "export/reports") {
  
  if(!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
  
  cat(sprintf("Building report for %s: ", dataset_id))
  
  # filenames
  input_Rmd <- sprintf("%s/%s.Rmd", path, dataset_id)
  output_html <- sprintf("%s/%s.html", path, dataset_id)
  
  if(overwrite | !file.exists(output_html)) {
    
    # Create a new Rmd file with name embedded in title
    x <- readLines("scripts/report_study.Rmd")
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

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
get_SHA_link <- function(...) {
  sha <- get_SHA(...)
  sprintf("[%s](https://github.com/traitecoevo/austraits/tree/%s)",   sha, sha)
}

#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
get_SHA <- function(path = rprojroot::find_root("remake.yml")) {
  git2r::branch_target(git2r::repository_head(git2r::repository(path)))
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
my_kable_styling_html <- function(...) {
    kableExtra::kable(...) %>%
    kableExtra::kable_styling(..., 
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE, 
                  position = "left"
                  ) %>%
    # hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', .)
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
my_kable_styling_pdf <- function(...) {
    kableExtra::kable(...)
}

## format a table with kable and default styling
#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
my_kable_styling_markdown <- function(...) {
  kableExtra::kable(...)
}

#' Title
#'
#' @param link 
#' @param text 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
as_link <- function(link, text, type="md") {
  if(type=="md")
    sprintf('[%s](%s)', text, link)
  else
    sprintf("<a href='%s'> %s </a>", link, text)
}


## Function to format a tree structure from a vector
## X is a vector of terms
## title is name of master branch
## prefix sepcifies amount of indentation
#' Title
#'
#' @param x 
#' @param title 
#' @param prefix 
#'
#' @return
#' @export
#'
#' @examples
create_tree_branch <- function(x, title, prefix="") {
  c(
    sprintf("%s%s", prefix,title), 
    sprintf("%s%s %s", prefix,
            c(rep("├──", length(x) -1), "└──"),
            x)
  )
}


