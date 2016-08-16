rename_columns <- function(obj, from, to) {
  names(obj)[match(from, names(obj))] <- to
  obj
}

ensure_as_character <- function(data) {
  for(v  in names(data))
    data[[v]] <- as.character(data[[v]])
  data
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

# Paste together list of var_names and their values, used for
# aggregating varnames into 'grouping' variable NOTE: Used in
# dataManipulate.R
makeGroups <- function(data, var_names) {
  apply(cbind(data[, var_names]), 1, function(x)
        ## jsonlite::toJSON(as.list(x), auto_unbox=TRUE)
        paste(var_names, "=", x, collapse = "; "))
}

# Convert a dataframe to a named list
# Useful when converting to yaml
df_to_list <- function(df) {
  attr(df, "out.attrs") <- NULL
  unname(lapply(split(df, seq_len(nrow(df))), as.list))
}

# Convert a list of lists to dataframe
# requires that every list have same named elements
list_to_df <- function(my_list) {
  bind_rows(lapply(my_list, data.frame, stringsAsFactors = FALSE))
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