# Swap a null vale to something else
null_as <- function(x, val=NA){
  if(is.null(x)) return(val)
  x
}

extract_list_element <- function(i, my_list, var) {
  i %>% lapply(function(x) my_list[[x]][[var]]) %>% lapply(null_as) %>% unlist()
}


read_csv_char <- function(...){
  read_csv(..., col_types = cols(.default = "c"))
}

rename_columns <- function(obj, from, to) {
  names(obj)[match(from, names(obj))] <- to
  obj
}

# For a vector x in which individual cell may have multiple values (separated by `sep`), sort records within each cell  alphabetically
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


# Convert a dataframe to a named list
# Useful when converting to yaml
df_to_list <- function(df) {
  attr(df, "out.attrs") <- NULL
  unname(lapply(split(df, seq_len(nrow(df))), as.list))
}

# Convert a list of lists to dataframe
# requires that every list have same named elements
list_to_df <- function(my_list, as_character= TRUE, on_empty=NA) {
  
  if(is.null(my_list) || is.na(my_list) || length(my_list) ==0)
    return(on_empty)

  if(as_character)
    my_list <- lapply(my_list, lapply, as.character)

  dplyr::bind_rows(lapply(my_list, as.tibble))
}

# Convert a list with single entries to dataframe
list1_to_df <- function(my_list) {

  for(f in names(my_list)) {
    if(is.null(my_list[[f]])) 
      my_list[[f]] <- NA
  }
  
  tibble(key = names(my_list), value = unlist(my_list))
}


## Add an item to the end of a list
append_to_list <- function(my_list, to_append) {
  my_list[[length(my_list)+1]] <-  to_append
  my_list
}

# Provide an easier entry point for
# parsing a YAML file
read_yaml <- yaml::yaml.load_file

# Write yaml to filename with preferred defaults
# Designed so that read_yaml(write_yaml(y)) == y
write_yaml <- function(y, filename) {
  txt <- yaml::as.yaml(y, column.major = FALSE, indent=2)
  txt <- gsub(": ~",":", txt, fixed=TRUE)
  writeLines(txt, filename)
}
