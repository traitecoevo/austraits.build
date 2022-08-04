
#' Load schema for an austraits.build data compilation (excluding traits)
#'
#' @param path path to schema file. By default loads version included with the package
#' @param subsection section to load
#'
#' @return a list
#' @export
#'
#' @examples{
#' 
#' schema <- load_schema()
#' }
load_schema <- function(path=system.file("support", "austraits.build_schema.yml", package = "austraits.build"), subsection=NULL){
  
  schema <- yaml::read_yaml(path)
  
  if(!is.null(subsection)) {
    schema <-  schema[[subsection]]
  }
  
  schema
}

#' Retrieve version for compilation from definitions
#'
#' @param path path to traits definitions
#'
#' @return a string
#' @export
get_version <- function(path =  "config/traits.yml") {
  load_schema(path)$build$version
}
