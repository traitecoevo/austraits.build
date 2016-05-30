load_study <- function(filename_data_raw,
                       filename_data_config,
                       filename_configVarnames,
                       filename_configPlantCharacters,
                       filename_configLookups,
                       filename_metadata,
                       definitions_data,
                       definitions_traits,
                       unit_conversion_functions
                       ) {

  # read metadata
  metadata <- read_csv(filename_metadata)
  for(v  in names(metadata))
    metadata[[v]] <- as.character(metadata[[v]])

  data <- read_data_study(filename_data_raw,
                          filename_data_config,
                          filename_configVarnames,
                          filename_configPlantCharacters,
                          filename_configLookups,
                          metadata,
                          definitions_data,
                          definitions_traits,
                          unit_conversion_functions
                          )

  key <- basename(dirname(filename_data_raw))
  # bibentry <- set_bib_key(bibtex::read.bib(filename_bib), key)

  # methods  <- read_methods(filename_columns, definitions_data)
  # contacts <- read_csv(filename_contact)
  # metadata <- read_csv(filename_metadata)

  list(key        = key,
       data       = data,
       metadata   = metadata)
       # methods    = methods,
       # bibtex     = bibentry,
       # contacts   = contacts,
       # references = get_citation(bibentry))
}

## These are the cleaning steps:
read_data_study <- function(filename_data_raw,
                            filename_data_config,
                            filename_configVarnames,
                            filename_configPlantCharacters,
                            filename_configLookups,
                            metadata,
                            definitions_data,
                            definitions_traits,
                            unit_conversion_functions
                            ) {

  dataset_id <- basename(dirname(filename_data_raw))

  # Load configuration files
  cfgDataset <- read_csv(filename_data_config)
  cfgVarNames <- read_csv(filename_configVarnames)
  cfgChar <- read_csv(filename_configPlantCharacters)
  cfgLookup <- read_csv(filename_configLookups)

  # data processing
  data <- read_csv(filename_data_raw)
  data <- parse_data(dataset_id, data, cfgDataset, cfgVarNames, cfgChar, cfgLookup, metadata$dataset_num[1])
  data <- add_all_columns(data, definitions_data)
  data <- drop_unsupported(data, definitions_traits)
  data <- convert_units(data, definitions_traits, unit_conversion_functions)

  # # get character lookup values where necessary
  # out$lookup <- mapply(function(x, y) {
  #   # for each value of each character: get lookup corresponding to the value
  #   as.character(cfgLookup$value[cfgLookup$character == x & cfgLookup$lookup == y][1])
  # }, out$character, out$value, SIMPLIFY = TRUE)

  # data <- add_new_data(data, filename_new_data)
  # data <- post_process(data)
  data
}


## Remove any traits that are not correctly defined and provide a warning
drop_unsupported <- function(data, definitions_traits) {
  i <- data$character %in% definitions_traits$variable

  if(any(!i)) {
    message(sprintf("unsupported variable dropped: %s", paste(unique(data$character[!i]), collapse=", ")))
  }
  data[i,]
}

make_unit_conversion_functions <- function(filename) {
  x <- read_csv(filename)

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
      i = match(character, info[["variable"]]),
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
    group_by(ucn) %>%
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

  missing <- setdiff(info$variable, names(data))
  if (length(missing) != 0) {
    extra <- as.data.frame(lapply(info$type[info$variable==missing], na_vector, nrow(data)),
                           stringsAsFactors = FALSE)
    names(extra) <- missing
    data <- cbind(data[names(data) %in% info$variable], extra)
  } else {
    data <- data[names(data) %in% info$variable]
  }
  data[info$variable]
}

## Modifies data by adding new values from table studyName/dataNew.csv
##
## Within the column given by `newVariable`, replace values that match
## `lookupValue` within column `lookupVariable` with the value
## `newValue`.  If `lookupVariable` is `NA`, then replace all elements
## of `newVariable` with the value `newValue`. Note that
## lookupVariable can be the same as newVariable.
add_new_data <- function(data, filename) {
  import <- read_csv(filename)
  if (nrow(import) > 0) {
    import$lookupVariable[import$lookupVariable == ""] <- NA
  }

  if (!is.null(import)) {
    for (i in seq_len(nrow(import))) {
      col_to <- import$newVariable[i]
      col_from <- import$lookupVariable[i]
      if (is.na(col_from)) {
        # apply to whole column
        data[col_to] <- import$newValue[i]
      } else {
        ## apply to subset
        rows <- data[[col_from]] == import$lookupValue[i]
        data[rows, col_to] <- import$newValue[i]
      }
    }
  }

  data
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

# gets config value for given key value, use this for configDataset only (use for configDataset.csv only)
# cfg: dataframe containing config data
# key: the key for which we want the value to be returned
get_config <- function(cfg, key) {
  cfg$value[cfg$key == key]
}

# processes a single dataset
parse_data <- function(dataset_id, data, cfgDataset, cfgVarNames, cfgChar, cfgLookup, dataset_num) {

  # get config data for dataset
  dataset_header <- get_config(cfgDataset, "header")
  dataset_skip <- as.numeric(get_config(cfgDataset, "skip"))
  dataset_vert <- get_config(cfgDataset, "plant_char_vertical")

  # remove metadata_id and project_source_id, if they exist
  for(v in  intersect(c("metadata_id", "primary_source_id"), colnames(data))) {
    data[[v]] <- NULL
  }
  # skip (remove) rows from top of dataset as specified in dataset config
  if (dataset_skip > 0)
    data <- data[-dataset_skip, ]

  # all cfgVarNames$var_in must exist in dataset, if not then we need to stop and fix the problem
  if (any(!cfgVarNames$var_in %in% colnames(data))) {
    stop(paste0("\nVariable '", setdiff(cfgVarNames$var_in, colnames(data)), "' from data.csv not found in configVarnames"))
  }

  # create dataframe with data for vars that we want to keep, and set to correct varnames
  df <- rename_columns(data[, cfgVarNames$var_in, drop=FALSE], cfgVarNames$var_in, cfgVarNames$var_out)

  # correct site names if necessary, remove dataset_id appended to end (e.g. "Agnes Banks_2")
  # in some cases these numbers were added manually by RVG to site names in the raw data
  if ("site_name" %in% colnames(df)) {

    # get old dataset number - need this to trim the number from the end of site names in some cases (was added previously by RVG)
    regex <- paste0("_", dataset_num, "$")

    df$site_name <- sapply(df$site_name, function(x) {
      # get position of, e.g., "_2" in site name
      # NOTE: this throws a warning for some reason, saying the pattern has length > 1. Not sure why, doesn't seem to matter.
      pos <- suppressWarnings(unlist(regexpr(regex, x)))
      # if "_2" (or whatever) doesn't appear in site name then just return the site name
      # otherwise return the corrected site name with "_2" (or whatever) removed
      if (is.na(pos) | pos == -1) {
        return(x)
      } else {
        return(substr(x, 1, pos - 1))
      }
    })
  }

  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  if (dataset_vert == FALSE & any(! cfgChar[["var_name"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(cfgChar[["var_name"]], colnames(data))))
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
      # to x we append columns of data for character name, unit and value (the latter is retrieved from the data)
      out[[i]][["character"]] <- cfgChar[["var_name"]][i]
      out[[i]][["value"]] <- as.character(data[[cfgChar[["var_name"]][i]]])
    }
    out <- dplyr::bind_rows(out)
  } else {
    out <- df
    out[["value"]] <- as.character(out[["value"]])
  }

  # Now process any name changes as per configPlantCharacters
  out[["unit"]] <- NA_character_
  i <- match(out[["character"]], cfgChar[["var_name"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    out[["unit"]][j] <- cfgChar[["unit"]][i[j]]
    out[["character"]][j] <- cfgChar[["character"]][i[j]]
  }

  out[["study"]] = dataset_id
  out
}


combine_austraits <- function(..., d=list(...), variable_definitions, compiler_contacts) {
  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  names(d) <- sapply(d, "[[", "key")

  ret <- list(data=combine("data", d),
              # methods=combine("methods", d),
              # contacts=rbind.fill(combine("contacts", d),
              #   data.frame(studyName="austraits_construction", compiler_contacts)),
              # references=combine("references", d),
              metadata=combine("metadata", d)
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
