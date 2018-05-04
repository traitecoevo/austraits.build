load_study <- function(filename_data_raw,
                       filename_context,
                       filename_metadata,
                       definitions,
                       unit_conversion_functions,
                       species_list_known
                       ) {

  dataset_id <- basename(dirname(filename_data_raw))

  # read metadata
  metadata <- read_yaml(filename_metadata)

  # load and clean trait data
  data <- read_csv(filename_data_raw, col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    add_all_columns(definitions, "data") %>%
    drop_unsupported_traits(definitions) %>%
    convert_units(definitions, unit_conversion_functions) %>%
    drop_unsupported_values(definitions) %>%
    update_taxonomy(metadata)

  # Now that we're done with them, drop config parts of metadata
  for(v in c("config", "traits", "substitutions")) {
    metadata[[v]] <- NULL
  }

  # read context data
  context <- read_csv(filename_context, col_types = cols(.default = "c"))
  if(nrow(context) > 0) {
    context <- mutate(context, dataset_id = dataset_id)
  }
  context <- add_all_columns(context, definitions, "context")

  species_list <- tibble(species_name =  unique(data$species_name)) %>%
                  left_join(species_list_known, by = "species_name") %>%
                  arrange(species_name)

  list(dataset_id = dataset_id,
       data       = data,
       context    = context,
       species_list = species_list,
       metadata   = metadata
       )
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

## Remove any disallowed traits
drop_unsupported_traits <- function(data, definitions) {

  # Remove any traits not listed in config
  i <- data$trait_name %in% names(definitions$traits$values)

  if(any(!i)) {
    message(sprintf("unsupported trait dropped: %s", paste(unique(data[["trait_name"]][!i]), collapse=", ")))
  }

 filter(data, i)
}


## Remove any disallowed traits
drop_unsupported_values <- function(data, definitions) {

  # Remove any values of categorical traits not listed in definitions
  i <- trait_is_categorical(data[["trait_name"]], definitions)

  if(any(i)) {

    for(v in unique(data[["trait_name"]][i])) {
      ii <-  data[["trait_name"]] == v & !is.null(definitions$traits$values[[v]]$values) & data[["value"]] %notin% names(definitions$traits$values[[v]]$values)

      if(any(ii)) {
        message(sprintf("unsupported value of %s dropped: %s", v, paste(data[["value"]][ii] %>% unique() %>% sort(), collapse=", ")))
        data <- filter(data, !ii)
      }
    }
  }

  # # Remove any values of numerical traits out of range
  i <- trait_is_numeric(data[["trait_name"]], definitions)

  if(any(i)) {
    for(v in unique(data[["trait_name"]][i])) {
      x <- suppressWarnings(as.numeric(data[["value"]]))
      ii <-  data[["trait_name"]] == v & !is.na(x) &
        (x < definitions$traits$values[[v]]$values$minimum | x > definitions$traits$values[[v]]$values$maximum)

      if(any(ii)) {
        message(sprintf("unsupported value of %s dropped: %s", v, paste(x[ii] %>% unique() %>% sort(), collapse=", ")))
        data <- filter(data, !ii)
      }
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
convert_units <- function(data, definitions, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- data %>%
    mutate(
      i = match(trait_name, names(definitions$traits$values)),
      to = extract_list_element(i, definitions$traits$values, "units"),
      ucn = unit_conversion_name(unit, to),
      type = extract_list_element(i, definitions$traits$values, "type"),
      to_convert =  type == "numeric" & unit != to)

  # Identify anything problematic in conversions and drop
  j <- is.na(data[["to_convert"]]) |
        data[["to_convert"]] & !data[["ucn"]] %in% names(unit_conversion_functions)

  if(any(j)) {
    message(sprintf("records dropped because of missing unit conversions: %s", paste(
      unique(data[["ucn"]][j]), collapse=", ")))
    data <- filter(data, !j)
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

# Add or remove columns of data as needed so that all sets have
# the same columns.
add_all_columns <- function(data, definitions, group) {

  vars <- names(definitions[[group]][["columns"]])
  missing <- setdiff(vars, names(data))

  for(v in missing)
    data <- mutate(data, !!v := NA)

  select(data, one_of(vars))
}

# processes a single dataset
parse_data <- function(data, dataset_id, metadata) {

  # get config data for dataset
  dataset_vert <- metadata[["config"]][["is_vertical"]]

  # Step 1. create dataframe with data for vars that we want to keep, and set to correct names
  # all names in "variable_match" must exist in dataset, if not then we need to stop and fix the problem
  var_in <- unlist(metadata[["config"]][["variable_match"]])
  var_out <- names(metadata[["config"]][["variable_match"]])
  if (any(!var_in %in% colnames(data))) {
    stop(paste0("\nVariable '", setdiff(var_in, colnames(data)), "' from data.csv not found in configVarnames"))
  }
  df <- data %>%
        select(one_of(var_in)) %>%
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

  out[["dataset_id"]] = dataset_id
  out
}


## Enforce some standards on species names
standardise_names <- function(x) {

  f <- function(x, find, replace) {
    gsub(find, replace, x, perl=TRUE)
  }

  x %>%
    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%

    ## sp. not sp or spp
    f("\\ssp(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%

    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)", " subsp.\\1") %>%
    f("\\sssp.(\\s|$)", " subsp.\\1") %>%
    f("\\ssubsp(\\s|$)", " subsp.\\1") %>%
    f("\\ssub sp.(\\s|$)", " subsp.\\1") %>%

    ## lower case after subsp.
    f("\\ssubsp.\\s([A-Z])", " subsp. \\L\\1") %>%

    ## var. not var
    f("\\svar(\\s|$)", " var.\\1") %>%

    ## aff. not affin, aff, affn
    f("\\saffin(\\s|$)", " aff.\\1") %>%
    f("\\saff(\\s|$)", " aff.\\1") %>%
    f("\\saffn(\\s|$)", " aff.\\1") %>%

    ## remove double space
    f("[\\s]+", " ")
}

update_taxonomy  <- function(study_data, metadata){

  out <- study_data

  # copy original species name to a new column
  out[["original_name"]] = out[["species_name"]]

  # Now make any replacements specified in metadata yaml
  ## Read metadata table, quit if empty
  cfgLookup <-  list_to_df(metadata[["taxonomic_updates"]])
  if(nrow(cfgLookup) == 0) {
    return(out)
  }

  ## Makes replacements, row by row
  for(i in seq_len(nrow(cfgLookup))) {
    j <- which(out[["species_name"]] == cfgLookup[["find"]][i])
    if( length(j) > 0 )
      out[["species_name"]][j] <- cfgLookup[["replace"]][i]
  }

  out[["species_name"]] <- standardise_names(out[["species_name"]])

  ## Return updated table
  out
}

combine_austraits <- function(..., d=list(...)) {
  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "dataset_id")
  ret <- list(data=combine("data", d),
              context=combine("context", d),
              species_list=combine("species_list", d) %>% 
                              arrange(species_name) %>% 
                              filter(!duplicated(.)),
              metadata=lapply(d, "[[", "metadata")
              )

#  ret$bibtex <- do.call("c", unname(lapply(d, "[[", "bibtex")))
#  ret$dictionary <- variable_definitions
  ret
}

