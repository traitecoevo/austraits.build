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

#' <brief desc>
#'
#' <full description>
#'
#' @param ... <what param does>
#'
#' @export
#' @return
read_csv_char <- function(...){
  readr::read_csv(..., col_types = cols(.default = "c"), progress=FALSE)
}


#' Swap a null vale to something else
#'
#' @param x 
#' @param val 
#'
#' @return
#'
#' @export
#' @examples
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
#' @examples extract_list_element(1, definitions$traits$elements, "units")
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

#' split_then_sort: For a vector x in which individual cell may have multiple values (separated by 'sep'), sort records within each cell  alphabetically
#'
#' @param x an individual cell with multiple values 
#' @param sep a separator, a whitespace is the default
#'
#' @return a vector of alphabetically sorted records
#' @examples split_then_sort("z y x")
split_then_sort <- function(x, sep=" ") {

  # find cells with multiple values, indicated by presence of sep
  i <- grep(sep, x)
  # for those cells, split, sort then combine
  x[i] <- x[i] %>% 
      str_split(" ") %>%  
      lapply(function(xi) xi %>% sort() %>% paste(collapse=" ")) %>%
      unlist()
  x
}


#' Convert a dataframe to a named list, useful when converting to yaml
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

# Convert a list of lists to dataframe
# requires that every list have same named elements

#' Convert a list of lists to dataframe requires that every list have same named elements
#' @param Convert a list of lists to dataframe 
#' @param as_character logical:  indicating whether the values are read as character
#' @param on_empty 

#' @export
#' @examples list_to_df(df_to_list(iris))
list_to_df <- function(my_list, as_character= TRUE, on_empty=NA) {
  
  if(is.null(my_list) || is.na(my_list) || length(my_list) ==0)
    return(on_empty)

  if(as_character)
    my_list <- lapply(my_list, lapply, as.character)

  dplyr::bind_rows(lapply(my_list, as_tibble))
}

#' Convert a list with single entries to dataframe
#' @param my_list a list with single entries
#' @return a tibble with two columns
#' @export
#' @examples list1_to_df(as.list(iris)[2])
#' 
list1_to_df <- function(my_list) {

  for(f in names(my_list)) {
    if(is.null(my_list[[f]])) 
      my_list[[f]] <- NA
  }
  
  tibble(key = names(my_list), value = unlist(my_list))
}

#' Add an item to the end of a list
#' @param my_list a list 
#' @param to_append a list
#'
#' @return a list merged with an added item at the end
#' @examples  append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3,4)])
append_to_list <- function(my_list, to_append) {
  my_list[[length(my_list)+1]] <-  to_append
  my_list
}

# Provide an easier entry point for
# parsing a YAML file
read_yaml <- yaml::yaml.load_file

#' Write yaml to filename with preferred defaults, Designed so that read_yaml(write_yaml(y)) == y
#' @param y a (yaml) list or a data frame
#' @param filename  a character string for naming a file
#'
#' @export
#' @examples write_yaml(iris, "iris.yaml")
write_yaml <- function(y, filename) {
  txt <- yaml::as.yaml(y, column.major = FALSE, indent=2)
  txt <- gsub(": ~",":", txt, fixed=TRUE)
  writeLines(txt, filename)
}


build_website <- function() {
  devtools::document()
  pkgdown::build_site()
}
