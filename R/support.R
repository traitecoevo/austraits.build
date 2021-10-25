#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Read in a csv as a tibble with column types as characters
#'
#' Reads in a csv file using the read_csv function from readr
#' with columns as characters
#'
#' @param ... arguments passed to the read_csv()
#'
#' @return tibble
#' @export
read_csv_char <- function(...){
  readr::read_csv(..., col_types = cols(.default = "c"), progress=FALSE)
}

#' Convert NULL values to a different value
#'
#' null_as converts NULL values in a vector to a different value. Default is
#' converting NuLL to NA
#'
#' @param x a vector containing null values
#' @param val specify what the null value should be returned as, default is NA
#'
#' @return a vector with null values replaced
#'
#' @export
#' @examples null_as(NULL)
null_as <- function(x, val=NA){
  if(is.null(x)) return(val)
  x
}

#' Extract a trait element from the definitions$traits$elements
#'
#' @param i a value within the definitions$traits$elements list which refers to types of traits
#' @param my_list the list that contains the element we're interested in (i.e. definitions$traits$elements)
#' @param var the type of variable of a trait
#'
#' @return the element/properties of a trait
#'
#' @export
#' @examples
#' \dontrun{
#' extract_list_element(1, definitions$traits$elements, "units")
#' }
extract_list_element <- function(i, my_list, var) {
  i %>% lapply(function(x) my_list[[x]][[var]]) %>% lapply(null_as) %>% unlist()
}

#' Rename columns
#'
#' @param obj a  tibble with multiple columns
#' @param from a vector of the initial column names
#' @param to  a vector of the new column names
#'
#' @return a  tibble with new column names
rename_columns <- function(obj, from, to) {
  names(obj)[match(from, names(obj))] <- to
  obj
}

#'  Split and sort cells with multiple values
#'
#'  split_then_sort: For a vector x in which individual cell may have
#'  multiple values (separated by 'sep'), sort records within each cell alphabetically
#'
#' @param x an individual cell with multiple values
#' @param sep a separator, a whitespace is the default
#'
#' @return a vector of alphabetically sorted records
#'
#' @export
#' @examples split_then_sort("z y x")
split_then_sort <- function(x, sep=" ") {

  # find cells with multiple values, indicated by presence of sep
  i <- grep(sep, x)
  # for those cells, split, sort then combine
  x[i] <- x[i] %>%
      stringr::str_split(" ") %>%
      lapply(function(xi) xi %>% sort() %>% paste(collapse=" ")) %>%
      unlist()
  x
}

#'  Convert dataframe to list
#'
#'  Convert a dataframe to a named list, useful when converting to yaml
#'
#' @param df a dataframe
#' @return a (yaml) list
#'
#' @export
#' @examples df_to_list(iris)
df_to_list <- function(df) {
  attr(df, "out.attrs") <- NULL
  unname(lapply(split(df, seq_len(nrow(df))), as.list))
}

#' Convert a list of lists to dataframe
#'
#' Convert a list of lists to dataframe requires that every list have same named elements
#'
#' @param my_list a list of lists to dataframe
#' @param as_character logical:  indicating whether the values are read as character
#' @param on_empty value to return if my_list is NULL, NA or is length == 0, default = NA
#'
#' @export
#' @examples list_to_df(df_to_list(iris))
list_to_df <- function(my_list, as_character= TRUE, on_empty=NA) {

  if(is.null(my_list) || any(is.na(my_list)) || length(my_list) ==0)
    return(on_empty)

  if(as_character)
    my_list <- lapply(my_list, lapply, as.character)

  dplyr::bind_rows(lapply(my_list, tibble::as_tibble))
}

#' Convert a list with single entries to dataframe
#'
#' @param my_list a list with single entries
#' @return a tibble with two columns
#' @export
#' @examples list1_to_df(as.list(iris)[2])
list1_to_df <- function(my_list) {

  for(f in names(my_list)) {
    if(is.null(my_list[[f]]))
      my_list[[f]] <- NA
  }

  tibble::tibble(key = names(my_list), value = unlist(my_list))
}

#' Add an item to the end of a list
#'
#' @param my_list a list
#' @param to_append a list
#'
#' @return a list merged with an added item at the end
#' @export
#' @examples  append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3,4)])
append_to_list <- function(my_list, to_append) {
  my_list[[length(my_list)+1]] <-  to_append
  my_list
}

#' Read in a metadata.yml file for a study
#'
#' @param path location of the metadata file
#'
#' @export
read_metadata <- function(path) {

  data <- yaml::read_yaml(path)

  # We want to preserve formatting in custom R code
  # but read_yaml looses it. So read in as text, if not empty
  if(!is.na(data$config$custom_R_code)) {
    # Read in again, extracting custom R code

    data2 <- readLines(path)

    code_start <- grep("  custom_R_code:", data2, fixed = TRUE)
    code_end <- which(data2 == "traits:")-1

    data$config$custom_R_code <-
      data2[code_start:code_end] %>%
      gsub("  custom_R_code:", "", ., fixed = TRUE) %>%
      paste(collapse = "\n")
  }

  data
}

#' Write metadata.yml for a study
#' 
#' write metadata.yml file with custom R code formatted to allow line breaks
#'
#' @param data austraits metadata object (a list)
#' @param path location where the metadata file is to be written to
#' @param style_code  should the R code be styled?
#'
#' @rdname write_metadata
#' @export
#' @examples
#' \dontrun{
#' f <- "data/Falster_2003/metadata.yml"
#' data <- read_metadata(f)
#' write_metadata(data, f)
#' }
write_metadata <- function(data, path, style_code=FALSE) {

  y <- data
  y$config$custom_R_code <- NA

  txt <- yaml::as.yaml(y, column.major = FALSE, indent=2) %>%
    gsub(": ~",":", ., fixed=TRUE)

  #reinsert custom R code
  if(!is.na(data$config$custom_R_code)) {

    code <- data$config$custom_R_code

    if(style_code)
      code <- code %>% suppressWarnings(styler::style_text(transformers = .data$tidyverse_style(strict = TRUE)))

    txt <- gsub("custom_R_code: .na", code %>% paste(collapse = "\n") %>%
                  paste0("custom_R_code:", .), txt, fixed = TRUE)
  }

  file <- file(path, "w", encoding = "UTF-8")
  on.exit(close(file))
  cat(txt, file=file)
}

##' Read yaml (from package yaml)
##' @importFrom yaml read_yaml
##' @name read_yaml
##' @rdname read_yaml
##' @export
NULL

##' write yaml (from package yaml)
##' @importFrom yaml write_yaml
##' @name write_yaml
##' @rdname write_yaml
##' @export
NULL

#' Build website
#'
#' Build website using the build_site() function from `pkgdown`
#'
build_website <- function() {
  devtools::document()
  pkgdown::build_site()
}
