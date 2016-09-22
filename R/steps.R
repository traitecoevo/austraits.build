load_study <- function(filename_data_raw,
                       filename_context,
                       filename_metadata,
                       definitions_data,
                       definitions_traits,
                       definitions_context,
                       categorical_trait_constraints,
                       unit_conversion_functions
                       ) {

  # read metadata
  metadata <- read_yaml(filename_metadata)

  data <- read_data_study(filename_data_raw,
                          metadata,
                          definitions_data,
                          definitions_traits,
                          categorical_trait_constraints,
                          unit_conversion_functions
                          )

  # Now that we're done with them, drop config parts of metadata
  for(v in c("config", "traits", "substitutions")) {
    metadata[[v]] <- NULL
  }

  key <- basename(dirname(filename_data_raw))

  # read context data
  context <- read_csv(filename_context, col_types = cols())
  if(nrow(context) > 0) {
    context <- data.frame(dataset_id = key, context)
  }
  context <- add_all_columns(context, definitions_context)
  context <- fix_types(context, definitions_context)

  # bibentry <- set_bib_key(bibtex::read.bib(filename_bib), key)

  list(key        = key,
       data       = data,
       context    = context,
       metadata   = metadata
       )
}

## These are the cleaning steps:
read_data_study <- function(filename_data_raw,
                            metadata,
                            definitions_data,
                            definitions_traits,
                            categorical_trait_constraints,
                            unit_conversion_functions
                            ) {

  dataset_id <- basename(dirname(filename_data_raw))

  # data processing
  data <- read_csv(filename_data_raw, col_types = cols())
  data <- custom_manipulation(metadata[["config"]][["custom_R_code"]])(data)
  data <- parse_data(dataset_id, data, metadata)
  data <- add_all_columns(data, definitions_data)
  data <- drop_unsupported(data, definitions_traits, categorical_trait_constraints)
  data <- convert_units(data, definitions_traits, unit_conversion_functions)
  # data <- post_process(data)
  data
}


## Creates a function that applies custom data manipulations as needed
## If the metadata field custom_R_code is not empty, apply code
## specified there. Otherwise we apply the identity function to
## indicate no manipulations will be  done.
## The code custom_R_code assumes a single input -- a  data.frame
## called `data` and returns a data.frame
custom_manipulation <- function(txt) {
  if (!is.null(txt) && !is.na(txt)  && nchar(txt) > 0) {
    function(data) {eval(parse(text=txt), env=new.env())}
  } else {
    identity
  }
}

## Remove any disallowed traits or values and provide a warning
drop_unsupported <- function(data, definitions_traits, categorical_trait_constraints) {

  # Remove any traits not listed in config
  i <- data[["trait_name"]] %in% definitions_traits[["trait_name"]]

  if(any(!i)) {
    message(sprintf("unsupported trait dropped: %s", paste(unique(data[["trait_name"]][!i]), collapse=", ")))
  }
  data <- data[i,]

  # Remove any values of categorical traits not listed in definitions
  i <- data[["trait_name"]] %in% names(categorical_trait_constraints)
  if(any(i)) {
    for(v in unique(data[["trait_name"]][i])) {
      ii <-  !(data[["trait_name"]] == v & !data[["value"]] %in%  categorical_trait_constraints[[v]])

      if(any(!ii)) {
        message(sprintf("unsupported value of %s dropped: %s", v, paste(unique(data[["value"]][!ii]), collapse=", ")))
      }
      data <- data[ii,]
    }
  }

  data
}

make_unit_conversion_functions <- function(filename) {
  x <- read_csv(filename, col_types = cols())

  # make functions from text
  fs <- lapply(x[["function"]], function(x) {
                                  my_f <- function(x) {}
                                  body(my_f) <- parse(text = x)
                                  my_f})
  names(fs) <- unit_conversion_name(x[["unit_from"]], x[["unit_to"]])
  fs
}

unit_conversion_name <- function(from, to) {sprintf("%s-%s", from, to)}

## Convert units to desired type
convert_units <- function(data, info, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- mutate(data,
      i = match(trait_name, info[["trait_name"]]),
      to = info[["units"]][i],
      ucn = unit_conversion_name(unit, to),
      type = info[["type"]][i],
      to_convert =  type == "numeric" & unit != to)

  # Mark anything problematic as not for conversion, wipe values and set as NA
  # TODO: should these rows just be dropped? Could do that but also not that much of
  # a problem, as will get culled during review.
  j <- is.na(data[["to_convert"]]) |
        data[["to_convert"]] & !data[["ucn"]] %in% names(unit_conversion_functions)

  if(any(j)) {
    message(sprintf("records set to NA because of missing unit conversions: %s", paste(
      unique(data[["ucn"]][j]), collapse=", ")))
    data[["to_convert"]][j] <- FALSE
    data[["value"]][j] <- NA
    data[["unit"]][j] <- NA
  }

  f <- function(value, name) {
    as.character(unit_conversion_functions[[name]](as.numeric(value)))
  }

  # Split by unique unit conversions, to allow for as few calls as possible
  data %>%
    group_by(ucn, to_convert) %>%
    mutate(
      value = ifelse(to_convert, f(value, ucn[1]), value),
      unit = ifelse(to_convert, to, unit)) %>%
    ungroup() %>%
    select(one_of(vars))
}

## Standardise data columns to match standard template.
##
## May add or remove columns of data as needed so that all sets have
## the same columns.
add_all_columns <- function(data, info) {
  na_vector <- function(type, n) {
    rep(list(character=NA_character_, numeric=NA_real_)[[type]], n)
  }

  missing <- setdiff(info[["variable"]], names(data))
  if (length(missing) != 0) {
    extra <- as.data.frame(lapply(info$type[info[["variable"]] %in% missing], na_vector, nrow(data)),
                           stringsAsFactors = FALSE)
    names(extra) <- missing
    data <- cbind(data[names(data) %in% info[["variable"]]], extra)
  } else {
    data <- data[names(data) %in% info[["variable"]]]
  }
  data[info[["variable"]]]
}

## Ensures variables have correct type
fix_types <- function(data, variable_definitions) {
  var_def <- variable_definitions
  for (i in seq_along(var_def$variable)) {
    v <- var_def$variable[i]
    data[[v]] <- match.fun(paste0("as.", var_def$type[i]))(data[[v]])
  }
  data
}

# processes a single dataset
parse_data <- function(dataset_id, data, metadata) {

  # get config data for dataset
  dataset_vert <- metadata[["config"]][["is_vertical"]]

  # Step 1. create dataframe with data for vars that we want to keep, and set to correct names
  # all names in "variable_match" must exist in dataset, if not then we need to stop and fix the problem
  var_in <- unlist(metadata[["config"]][["variable_match"]])
  var_out <- names(metadata[["config"]][["variable_match"]])
  if (any(!var_in %in% colnames(data))) {
    stop(paste0("\nVariable '", setdiff(var_in, colnames(data)), "' from data.csv not found in configVarnames"))
  }
  df <- data %>% select(one_of(var_in)) %>%
      rename_columns(var_in, var_out)

  # Step 2. Add trait information, with correct names

  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  cfgChar <- list_to_df(metadata[["traits"]])
  if (dataset_vert == FALSE & any(! cfgChar[["var_in"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(cfgChar[["var_in"]], colnames(data))))
  }

  ## if needed, change from wide to long style
  if (dataset_vert == FALSE) {
    # if the dataset is "wide" then process each variable in turn, to create the "long" dataset -
    # say the original dataset has 20 rows of data and 5 traits, then we will end up with 100 rows
    out <- list()
    for (i in seq_len(nrow(cfgChar))) {
      # create a temporary dataframe which is a copy of df
      # df is our data frame containing all the columns we want EXCEPT for the trait data itself
      out[[i]] <- df
      # to x we append columns of data for trait_name, unit and value (the latter is retrieved from the data)
      out[[i]][["trait_name"]] <- cfgChar[["var_in"]][i]
      out[[i]][["value"]] <- as.character(data[[cfgChar[["var_in"]][i]]])
    }
    out <- dplyr::bind_rows(out)
  } else {
    out <- df
    out[["value"]] <- as.character(out[["value"]])
  }

  # Add information on trait type, precision, metholdogy_ids, if not already present
  vars <- c("value_type", "replicates", "precision", "methodology_ids")
  i <- match(out[["trait_name"]], cfgChar[["var_in"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    for(v in vars) {
      out[[v]] <- NA
      out[[v]][j] <- cfgChar[[v]][i[j]]
    }
  }

  # Now process any name changes as per metadata[["traits"]]
  out[["unit"]] <- NA_character_
  i <- match(out[["trait_name"]], cfgChar[["var_in"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    out[["unit"]][j] <- cfgChar[["unit_in"]][i[j]]
    out[["trait_name"]][j] <- cfgChar[["trait_name"]][i[j]]
  }

  # Implement any value changes as per substitutions
  if(!is.na(metadata[["substitutions"]][1])) {
    cfgLookup <-  list_to_df(metadata[["substitutions"]])
    for(i in seq_len(nrow(cfgLookup))) {
      j <- which(out[["trait_name"]] == cfgLookup[["trait_name"]][i] &
                  out[["value"]] == cfgLookup[["find"]][i])
      if( length(j) > 0 ) {
        out[["value"]][j] <- cfgLookup[["replace"]][i]
      }
    }
  }

  # Drop any NA trait or values
  out <- dplyr::filter(out, !is.na(trait_name) & !is.na(value))

  out[["study"]] = dataset_id
  out
}


combine_austraits <- function(..., d=list(...), variable_definitions, compiler_contacts) {
  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "key")
  ret <- list(data=combine("data", d),
              context=combine("context", d),
              metadata=lapply(d, "[[", "metadata")
              )

#  ret$bibtex <- do.call("c", unname(lapply(d, "[[", "bibtex")))
#  ret$dictionary <- variable_definitions
  ret
}

## Functions for extracting bits from austraits.  Works around some of the
## limitations in how I wrote remake.
extract_austraits_data <- function(austraits) {
  austraits$data
}
