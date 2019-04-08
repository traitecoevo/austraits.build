# core packages used in build
# pacman loads and if necessary installs
pacman::p_load(tidyverse, yaml, stringr, RefManageR)

# Additional packages used in build, setup or reports
# pacman checks installed, and installs if necessary, but does not load
for(v in 
      # extra packages used in build
    c( "git2r",
      # extra packages used for reports
      "knitr", "rmarkdown", "crayon", "ggbeeswarm", "scales",
      "gridExtra", "kableExtra", "leaflet",  "rprojroot", 
      # extra packages used during setup
      "devtools", "rcrossref", "Taxonstand", 
                "testthat", "whisker") 
    ){
  if(!pacman::p_isinstalled(v))
    pacman::p_install(v)
}

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

na_type <- function(type){
  list(character=NA_character_, numeric=NA_real_)[[type]]
}

na_vector <- function(type, n) {
  rep(list(character=NA_character_, numeric=NA_real_)[[type]], n)
}

rename_columns <- function(obj, from, to) {
  names(obj)[match(from, names(obj))] <- to
  obj
}

ensure_as_character <- function(data) {
  for(v  in names(data))
    data[[v]] <- as.character(data[[v]])
  data
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

last <- function(x) {
  x[[length(x)]]
}

## Make colours semitransparent:
make_transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out = n)
    col <- rep(col, length.out = n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1, ], tmp[2, ], tmp[3, ], alpha = opacity)
  }
}

capitalize <- function (string) {
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  string
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
