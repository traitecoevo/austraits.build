
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
#' schema <- get_schema()
#' }
get_schema <- function(path=system.file("support", "austraits.build_schema.yml", package = "austraits.build"), subsection=NULL){
  
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
util_get_version <- function(path =  "config/metadata.yml") {
  get_schema(path)$metadata$version
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
util_get_SHA <- function(path = ".") {
  git2r::sha(git2r::last_commit(git2r::repository(path)))
}
