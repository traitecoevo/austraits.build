#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
#' @usage lhs \%>\% rhs
NULL

#' Read yaml (from package yaml)
#' @importFrom yaml read_yaml
#' @name read_yaml
#' @rdname read_yaml
NULL

#' write yaml (from package yaml)
#' @importFrom yaml write_yaml
#' @name write_yaml
#' @rdname write_yaml
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
#' util_replace_null converts NULL values in a vector to a different value. Default is
#' converting NuLL to NA
#'
#' @param x a vector containing null values
#' @param val specify what the null value should be returned as, default is NA
#'
#' @return a vector with null values replaced
#' @examples \dontrun{
#' util_replace_null(NULL)
#' }
util_replace_null <- function(x, val=NA){
  if(is.null(x)) return(val)
  x
}


#' Convert all columns in data frame to character

#' @param df a dataframe
#' 
#' @return a dataframe
#' 
#' @examples lapply(austraits.build:::util_df_convert_character(iris), class) 
util_df_convert_character <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
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
#' util_extract_list_element(1, definitions$traits$elements, "units")
#' }
util_extract_list_element <- function(i, my_list, var) {
  i %>% lapply(function(x) my_list[[x]][[var]]) %>% lapply(util_replace_null) %>% unlist()
}



#'  Split and sort cells with multiple values
#'
#'  util_separate_and_sort: For a vector x in which individual cell may have
#'  multiple values (separated by 'sep'), sort records within each cell alphabetically
#'
#' @param x an individual cell with multiple values
#' @param sep a separator, a whitespace is the default
#'
#' @return a vector of alphabetically sorted records
#'
#' @examples \dontrun{util_separate_and_sort("z y x")}
util_separate_and_sort <- function(x, sep=" ") {

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
#' @export 
#' @examples util_df_to_list(iris)
util_df_to_list <- function(df) {
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
#' @examples util_list_to_df2(util_df_to_list(iris))
util_list_to_df2 <- function(my_list, as_character= TRUE, on_empty=NA) {

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
#' @examples \dontrun{
#' util_list_to_df1(as.list(iris)[2])
#' }
util_list_to_df1 <- function(my_list) {

  for(f in names(my_list)) {
    if(is.null(my_list[[f]]))
      my_list[[f]] <- NA
  }

  tibble::tibble(key = names(my_list), value = unlist(my_list))
}


#' Convert BibEntry object to a list
#'
#' @param bib BibEntry object
#'
#' @return list
util_bib_to_list <- function(bib) {

  # Read in file, convert to list, set key
  bib <- bib %>% unlist()

  if (!is.null(bib$author)) {
    bib$author <- paste(bib$author, collapse = " and ")
  }
  if (!is.null(bib$editor)) {
    bib$editor <- paste(bib$editor, collapse = " and ")
  }

  bib
}

#' Add an item to the end of a list
#'
#' @param my_list a list
#' @param to_append a list
#'
#' @return a list merged with an added item at the end
#' @examples  \dontrun{
#' util_append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3,4)])
#' }
util_append_to_list <- function(my_list, to_append) {
  my_list[[length(my_list)+1]] <-  to_append
  my_list
}

#' Read in a metadata.yml file for a study
#'
#' @param path location of the metadata file
#' @importFrom rlang .data
#'
#' @export
read_metadata <- function(path) {

  data <- yaml::read_yaml(path)

  # We want to preserve formatting in custom R code
  # but read_yaml looses it. So read in as text, if not empty
  if(!is.na(data$dataset$custom_R_code)) {
    # Read in again, extracting custom R code

    data2 <- readLines(path)

    code_start <- grep("  custom_R_code:", data2, fixed = TRUE)
    code_end <- grep("  collection_date:", data2, fixed=TRUE)-1

    data$dataset$custom_R_code <-
      data2[code_start:code_end] %>%
      gsub("custom_R_code:", "", ., fixed = TRUE) %>%
      paste(collapse = "\n")
  }

  data
}


#' Read the `metadata.yml` file for specified `dataset_id`
#'
#' @inheritParams metadata_path_dataset_id
#'
#' @return A list with contents of metadata for specified `dataset_id`
read_metadata_dataset <- function(dataset_id) {
  dataset_id %>%
    metadata_path_dataset_id() %>%
    read_metadata()
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
#' @importFrom rlang .data
#' @importFrom styler style_text
#' @export
#' @examples
#' \dontrun{
#' f <- "data/Falster_2003/metadata.yml"
#' data <- read_metadata(f)
#' write_metadata(data, f)
#' }
write_metadata <- function(data, path, style_code=FALSE) {

  y <- data
  y$dataset$custom_R_code <- NA
  
  txt <- yaml::as.yaml(y, column.major = FALSE, indent=2) %>%
    gsub(": ~",":", ., fixed=TRUE)
  
  #reinsert custom R code
  if(!is.na(data$dataset$custom_R_code)) {
    
    code <- data$dataset$custom_R_code
    
    if(style_code)
      code <- code %>% suppressWarnings(styler::style_text(transformers = .data$tidyverse_style(strict = TRUE)))
    
    txt <- gsub("custom_R_code: .na", code %>% paste(collapse = "\n") %>%
                  paste0("custom_R_code:", .), txt, fixed = TRUE)
  }
  
  if(!stringr::str_sub(txt, nchar(txt)) == "\n")
    txt <- c(txt, "\n")
  
  file <- file(path, "w", encoding = "UTF-8")
  on.exit(close(file))
  cat(txt, file=file)
}


#' Write the YAML representation of metadata.yml for specified `dataset_id` to
#' file \code{data/dataset_id/metadata.yml}
#'
#' @inheritParams metadata_path_dataset_id
#' @param metadata metadata file
#'
#' @return a yml file
write_metadata_dataset <- function(metadata, dataset_id) {
  write_metadata(metadata, dataset_id %>% metadata_path_dataset_id())
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
create_tree_branch <- function(x, title, prefix = "") {
  c(
    sprintf("%s%s", prefix, title),
    sprintf(
      "%s%s %s", prefix,
      c(rep("\u251c\u2500\u2500", length(x) - 1), "\u2514\u2500\u2500"),
      x
    )
  )
}

#' Strip scientific names of formatting and abbreviations
#' 
#' Enables better fuzzy matching of scientific names by 
#' reducing the number of characters that could differ
#' 
#' @param x vector of names to clean
#' @return vector of cleaned names
#' @importFrom stringr fixed
#' @export 
#' @examples c("Bankisa_serrata", "bankisa  serrata", "Banksia Seratta") %>% util_strip_taxon_names()
util_strip_taxon_names <- function(x) {
  x %>%
    stringr::str_remove_all(fixed(" subsp."))  %>%
    stringr::str_remove_all(fixed(" aff.")) %>%
    stringr::str_remove_all(fixed(" var.")) %>%
    stringr::str_remove_all(fixed(" ser.")) %>%
    stringr::str_remove_all(fixed(" f.")) %>%
    stringr::str_remove_all(fixed(" s.l.")) %>%
    stringr::str_remove_all(fixed(" s.s.")) %>%
    stringr::str_replace_all("[-._()]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}
