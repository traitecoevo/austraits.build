`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

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


## Hack work around to change key in bib entry (bibtex entry
## redefines '[' and/or '[[' in ways that cause nothing but grief)
set_bib_key <- function(bib, key) {
  bib_plain <- unclass(bib)
  attr(bib_plain[[1]], "key") <- key
  class(bib_plain) <- "bibentry"
  bib_plain
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

get_citation <- function(bibentry) {
  if (is.null(bibentry$doi)) {
    doi <- ""
    url <- if (is.null(bibentry$url[[1]])) "" else bibentry$url[[1]]
  } else {
    doi <- bibentry$doi[[1]]
    url <- paste0("http://doi.org/", doi)
  }

  data.frame(doi=doi, url=url, citation=format_citation(bibentry),
             stringsAsFactors=FALSE)
}

## This is a hack that would be better done with a style.  See
## ?bibentry for directions to that rabbithole.
format_citation <- function(bibentry) {
  citation <- format(bibentry, "text")
  find <- c("<URL.+>", "<.+?>", " , .", ", .", "\n", "*", "_", "“", "”", "..",
            ",.", ". .", "'''.'", "''.")
  replace <- c("", "", ".", ".", " ", "", "", "'", "'", ".", ".", ".", "", "")
  fixed <- rep(TRUE, length(find))
  fixed[c(1, 2)] <- FALSE
  for (i in seq_along(find)) {
    citation <- gsub(find[i], replace[i], citation, fixed = fixed[i])
  }
  citation
}
