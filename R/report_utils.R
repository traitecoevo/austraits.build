library(dplyr)

build_study_reports <- function(dataset_ids=NULL, ...) {
  
  # define if does not already exist, 
  # for all studies with suitable metadata file
  if(length(dataset_ids) ==0 | is.null(dataset_ids) | any(is.na(dataset_ids)))
    dataset_ids <- austraits$data$dataset_id %>% unique()
  
  for(dataset_id in dataset_ids)
    build_study_report(dataset_id, ...)
}

build_study_report <- function(dataset_id, overwrite=FALSE, path = "export/reports") {
  
  cat(sprintf("Building report for %s: ", dataset_id))
  
  # filenames
  input_Rmd <- sprintf("export/reports/%s.Rmd", dataset_id)
  output_html <- sprintf("export/reports/%s.html", dataset_id)
  
  if(overwrite | !file.exists(output_html)) {
    
    # Create a new Rmd file with name embedded in title
    x <- readLines("vignettes/report_study.Rmd")
    x <- gsub("title: Report on study from",  sprintf("title: Report on study `%s` from", dataset_id), x)
    writeLines(x, input_Rmd)
    
    # knit and render. Note, call render directly
    # in preference to knit, then render, as leaflet widget 
    # requires this to work
    rmarkdown::render(input_Rmd, quiet=TRUE)
    
    # remove temporary Rmd
    unlink(input_Rmd)
  }
  cat(" -> done\n")
}

get_SHA_link <- function(...) {
  sha <- get_SHA(...)
  sprintf("[%s](https://github.com/traitecoevo/austraits/tree/%s)",   sha, sha)
}

get_SHA <- function(path = rprojroot::find_root("remake.yml")) {
  git2r::branch_target(git2r::repository_head(git2r::repository(path)))
}

pallette1 <- function(){
  c('red', 'seagreen3', 'steelblue3', 'yellow2')
}

## format a table with kable and default styling
my_kable_styling <- function(...) {
  kableExtra::kable(...) %>%
  kableExtra::kable_styling(..., 
                bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left"
                ) %>%
  # hack to add margin to plot
  gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', .)
}

as_link <- function(link, text, type="md") {
  if(type=="md")
    sprintf('[%s](%s)', text, link)
  else
    sprintf("<a href='%s'> %s </a>", link, text)
}

as_link_tpl <- function(ID) {
  sprintf("http://www.theplantlist.org/tpl1.1/record/%s", ID) %>%
  as_link(ID)
}

as_link_apc <- function(ID) {
  sprintf("https://biodiversity.org.au/nsl/services/node/apc/%s", ID) %>%
  as_link(ID)
}

as_link_apni <- function(ID) {
  sprintf("http://id.biodiversity.org.au/node/apni/%s", ID) %>%
  as_link(ID)
}

# calculates coefficient of variation
CV <- function(x){
  sqrt(var(x))/mean(x)
}

md_link <- function(text, link) {
  sprintf("[%s](%s)", text, link)
}

md_link_doi <- function(doi) {
  md_link(doi, paste0("http://doi.org/", doi))
}

## Function to format a tree structure from a vector
## X is a vector of terms
## title is name of master branch
## prefix sepcifies amount of indentation
create_tree_branch <- function(x, title, prefix="") {
  c(
    sprintf("%s%s", prefix,title), 
    sprintf("%s%s %s", prefix,
            c(rep("├──", length(x) -1), "└──"),
            x)
  )
}

