#' Configure AusTraits database object (needs review)
#' 
#' Used in the remake::make process to configure individual studies mapping the 
#' individual traits found in that study along with any relevant unit conversions 
#' and trait definitions. `subset_config` and `load_study` are applied to every study
#' in the remake.yml file
#'
#' @param filename_metadata metadata yaml file for a given study
#' @param definitions definitions read in from the definitions.yml file in the config folder
#' @param unit_conversion_functions unit_conversion.csv file read in from the config folder
#'
#' @return list
#' @export
#'
#' @examples
subset_config <- function(
  filename_metadata,
  definitions, 
  unit_conversion_functions) {
  
  dataset_id <- basename(dirname(filename_metadata))
  
  # read metadata
  metadata <- read_yaml(filename_metadata)
  
  # table of trait_mapping
  trait_mapping <- 
    metadata[["traits"]] %>%
    list_to_df() %>%
    filter(!is.na(trait_name)) %>% 
    # determine unit conversions
    mutate(
      i = match(trait_name, names(definitions$traits$elements)),
      to = map_chr(i, ~extract_list_element(.x, definitions$traits$elements, "units")),
      conversion = unit_conversion_name(unit_in, to)
    )
  
  unit_conversion_functions_sub <- unit_conversion_functions[trait_mapping %>% filter(unit_in!=to) %>% pull(conversion) %>% unique()]
  
  # subset of trait definitions
  traits <-
    definitions$traits$elements[names(definitions$traits$elements) %in% trait_mapping$trait_name]
  
  value_type <- definitions$value_type
  
  list(dataset_id = dataset_id,
       metadata = metadata, 
       definitions = list(
         traits = traits,
         value_type = value_type,
         columns_traits = names(definitions[["austraits"]][["elements"]][["traits"]][["elements"]]),
         columns_sites = names(definitions[["austraits"]][["elements"]][["sites"]][["elements"]]),
         columns_contexts = names(definitions[["austraits"]][["elements"]][["contexts"]][["elements"]])
       ),
       unit_conversion_functions = unit_conversion_functions_sub) 
}

#' Load Study
#' 
#' load_study is used to load individual studies using the config file generated 
#' from `subset_config()`. `subset_config` and `load_study` are applied to every 
#' study in the remake.yml file
#'
#' @param filename_data_raw raw data.csv file for any given study
#' @param config_for_dataset config settings generated from subset_config()
#'
#' @return list, AusTraits database object
#' @export
#'
#' @examples
load_study <- function(filename_data_raw, 
                       config_for_dataset) {
    
  dataset_id <- config_for_dataset$dataset_id
  metadata <- config_for_dataset$metadata
  definitions <- config_for_dataset$definitions
    
  unit_conversion_functions <- config_for_dataset$unit_conversion_functions
    
  # load and clean trait data
  traits <- read_csv(filename_data_raw, col_types = cols(), guess_max = 100000, progress=FALSE) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    add_all_columns(definitions$columns_traits) %>%
    flag_unsupported_traits(definitions) %>%
    flag_excluded_observations(metadata) %>%
    convert_units(definitions, unit_conversion_functions) %>%
    flag_unsupported_values(definitions) %>%
    apply_taxonomic_updates(metadata) %>%
    mutate(
      # For cells with multiple values (separated by a space), sort these alphabetically
      value =  ifelse(is.na(error), split_then_sort(value),value),
      value_type = factor(value_type, levels = names(definitions$value_type$values)),
      #ensure dates are converted back to character
      date = as.character(date)
      ) %>% 
    arrange(observation_id, trait_name, value_type) 

############ Remove this function as it appears to be redundant. Code is the same as `format_sites` #######  
  
  # read site data
  # format_sites_contexts <- function(v, my_list, context = FALSE) {
  #   tmp <- 
  #     my_list[[v]] %>%
  #     list1_to_df()
  #   if(!context){
  #     tmp %>% rename(site_property="key") %>%
  #     mutate(dataset_id=dataset_id, site_name = v)
  #   } else {
  #     tmp %>% rename(context_property="key") %>%
  #     mutate(dataset_id=dataset_id, context_name = v)
  #   }    
  # }

################################################################################  
  
  # extract site data from metadata
  sites <- 
    metadata$sites %>%  
    format_sites(dataset_id) %>% 
    add_all_columns(definitions$columns_sites) %>% 
    select(-error) %>% 
    # reorder so type, description come first, if present
    mutate(i = case_when(site_property == "description" ~ 1, site_property == "latitude (deg)" ~ 2,  site_property == "longitude (deg)" ~ 3, TRUE ~ 4)) %>%
    arrange(site_name, i, site_property) %>% 
    select(-i)

  # read contextual data
  contexts <- 
    metadata$contexts %>% 
    format_sites(dataset_id, context = TRUE) %>% 
    add_all_columns(definitions$columns_contexts) %>% 
    select(-error) %>% 
    # reorder so type, description come first, if present
    mutate(i = case_when(context_property == "type" ~ 1, context_property == "description" ~ 2, TRUE ~ 3)) %>%
    arrange(context_name, i, context_property) %>% 
    select(-i)

  # record contributors
  contributors <- 
    metadata$people %>%
    list_to_df() %>% 
    mutate(dataset_id = dataset_id) %>% 
    select(dataset_id = dataset_id, everything()) %>% 
    filter(!is.na(name))

  # record methods on study from metadata
  sources <- metadata$source %>% 
            lapply(convert_list_to_bib) %>% reduce(c)  
  source_primary_key <- metadata$source$primary$key
  source_secondary_keys <- setdiff(names(sources), source_primary_key)

  methods <-   
    full_join( by = "dataset_id",
      # methods used to collect each trait  
      metadata[["traits"]] %>%
        list_to_df() %>%
        filter(!is.na(trait_name)) %>% 
        mutate(dataset_id = dataset_id) %>%
        select(dataset_id, trait_name, methods) 
      ,
      # study methods
      metadata$dataset %>% 
        list1_to_df() %>% 
        spread(key, value) %>%
        select(one_of(names(metadata$dataset))) %>% 
          mutate(dataset_id = dataset_id) %>%
          select(-original_file, -notes)
      )  %>%
      full_join( by = "dataset_id",
        #references
        tibble(
          dataset_id = dataset_id,
          source_primary_key = source_primary_key,
          source_primary_citation = bib_print(sources[[source_primary_key]]),
          source_secondary_key = source_secondary_keys %>% paste(collapse = "; "),
          source_secondary_citation = ifelse(length(source_secondary_keys) == 0, NA_character_,
            map_chr(sources[source_secondary_keys], bib_print) %>% paste(collapse = "; ") %>% str_replace_all(".;", ";")
          )
        )
      )

  # Retrieve taxonomic details for known species
  taxonomic_updates <-  
    traits %>% 
    select(dataset_id, original_name, cleaned_name = taxon_name) %>% 
    distinct() %>% 
    arrange(cleaned_name)

  list(dataset_id = dataset_id,
       traits       = traits %>% filter(is.na(error)) %>% select(-error),
       sites    = sites,
       contexts    = contexts,
       methods    = methods,
       excluded_data = traits %>% filter(!is.na(error)) %>% select(error, everything()),
       taxonomic_updates = taxonomic_updates,
       taxa = taxonomic_updates %>% select(taxon_name = cleaned_name) %>% distinct(),
       definitions = definitions,
       contributors = contributors,
       sources =  sources
       )
}

## Creates a function that applies custom data manipulations as needed
## If the metadata field custom_R_code is not empty, apply code
## specified there. Otherwise we apply the identity function to
## indicate no manipulations will be  done.
## The code custom_R_code assumes a single input -- a  data.frame
## called `data` and returns a data.frame
#' Apply custom data manipulations
#' 
#' Applies custom data manipulations if the metadata field custom_R_code is not empty
#' Otherwise no manipulations will be done. 
#'
#' @param txt character text within custom_R_code
#'
#' @return character text containing custom_R_code if custom_R_code is not empty,
#' otherwise no changes are made 
#' @export
#'
#' @examples
custom_manipulation <- function(txt) {
  if (!is.null(txt) && !is.na(txt)  && nchar(txt) > 0) {
    function(data) {eval(parse(text=txt), env=new.env())}
  } else {
    identity
  }
}


#' Format site and context data from list to tibble
#' 
#' Format site data read in from the metadata.yml file. Converts from list to tibble.
#' Includes context information if available by specifying context = TRUE
#'
#' @param my_list list of input information
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param context logical, adds context information if available, default = FALSE
#'
#' @return tibble with site and context details if available
#' @export
#'
#' @examples
#' \dontrun{
#' format_sites(yaml::read_yaml("data/Falster_2003/metadata.yml")$sites, "Falster_2003")
#' format_sites(yaml::read_yaml("data/Apgaua_2017/metadata.yml")$context, "Apgaua_2017", context = TRUE)
#' }
format_sites <- function(my_list, dataset_id, context = FALSE) {

  f_helper <- function(v, a_list, context = FALSE) {
    tmp <- 
      a_list[[v]] %>%
      list1_to_df()
    if(!context){
      tmp %>% rename(site_property="key") %>%
      mutate(site_name = v)
    } else {
      tmp %>% rename(context_property="key") %>%
      mutate(context_name = v)
    }    
  }

  # if length 1 then it's an "na"
  if(length(unlist(my_list)) > 1){
    out <- 
      my_list %>%   
      lapply(lapply, as.character) %>%
      lapply(names(.), f_helper, ., context = context) %>%
      dplyr::bind_rows() %>% 
      mutate(dataset_id=dataset_id)
  } else {
    out <- tibble(dataset_id = character())
  }
  out
}

#' Flag any unrecognised traits 
#' 
#' Flag any unrecognised traits, as defined in the definitions.yml file
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the definitions.yml file in the config folder
#'
#' @return tibble with unrecognised traits flagged as "Unsupported trait" in the "error" column
#' @export
flag_unsupported_traits <- function(data, definitions) {
  
  # create error column if not already present
  if(is.null(data[["error"]]))
    data[["error"]] <- NA_character_

  # exclude traits not in definitions
  i <- data$trait_name %in% names(definitions$traits)
  dplyr::mutate(data, error = ifelse(!i, "Unsupported trait", error))
}


#' Flag any excluded observations
#' 
#' Checks the metadata yaml file for any excluded observations. If there are none, 
#' returns the original data. If there are excluded observations returns the mutated data 
#' with excluded observations flagged in a new column
#'
#' @param data tibble or dataframe containing the study data
#' @param metadata metadata yaml file for any given study
#'
#' @return dataframe with flagged excluded observations if there are any
#' @export
#'
#' @examples
flag_excluded_observations <- function(data, metadata) {
  
  if(length(metadata$exclude_observations)==1 && is.na(metadata$exclude_observations)) return(data)
  
  fix <- 
    metadata$exclude_observations %>% 
    list_to_df() %>% 
    separate_rows(find, sep=", ") %>% 
    mutate(find = str_squish(find))
  
  if(nrow(fix) == 0) return(data)

  fix <- split(fix, fix$variable)
  
  for(v in names(fix))
    data <- 
      mutate(data, error = ifelse(data[[v]] %in% fix[[v]]$find, "Observation excluded in metadata", error))

  data
}

# checks if values in vector x are in y
# values in x may contain multiple values separated by `sep`
# so first split these
#' Check values in one vector against values in another vector
#'
#' @param x vector
#' @param y vector
#' @param sep amount of space separating values to be split, default = " " (a single space)
#'
#' @return vector of logical values
#' @export
#'
#' @examples
check_all_values_in <- function(x, y, sep=" "){
  x %>% str_split(sep) %>% sapply(function(xi) all(xi %in% y))
}


# formats bibentry according to desired style using RefManageR
# not using print.BibEntry as this message to screen
#' Format BibEntry using RefManageR
#' 
#' Format BibEntry object according to desired style using RefManageR
#'
#' @param bib BibEntry object
#' @param .opts list of parameters for formatting style
#'
#' @return 
#' @export
#'
#' @examples
bib_print <- function(bib, .opts = list(first.inits = TRUE, max.names = 1000, style="markdown") ) {

  # set format
  oldopts <- RefManageR::BibOptions(.opts)
  on.exit(RefManageR::BibOptions(oldopts))

  bib %>% 
    RefManageR:::format.BibEntry(.sort = F) %>%
    # HACK: remove some of formatting introduced in line above
    # would be nicer if we could apply csl style
    gsub("[] ", "", ., fixed = TRUE) %>% 
    gsub("\\n", " ", .) %>% 
    gsub("  ", " ", .) %>% 
    gsub("DOI:", " doi: ", ., fixed = TRUE) %>% 
    gsub("URL:", " url: ", ., fixed = TRUE) %>% 
    ifelse(tolower(bib$bibtype) == "article",  gsub("In:", " ", .), .)
}

# convert a list of elements into a valid bibEntry
#' Convert a list of elements into a BibEntry object
#'
#' @param ref list of elements for a reference
#'
#' @return BibEntry object
#' @export
#'
#' @examples
convert_list_to_bib <- function(ref) {
  if(is.null(ref)) return(NULL)

  if(is.na(ref[1])) return(NULL)


  # Replace , with and to get correct handling of authors
  ref$author <- gsub(",", " and ", ref$author)

  # Ensures capitalisation of title retained as is
  if(!is.null(ref$title))
    ref$title = sprintf("{%s}", ref$title)

  RefManageR::as.BibEntry(ref)
}

#' Convert BibEntry object to a list
#'
#' @param bib BibEntry object
#'
#' @return list
#' @export
#'
#' @examples
convert_bib_to_list <- function(bib) {

  # Read in file, convert to list, set key
    bib <- bib %>% unlist()
  
    if(!is.null(bib$author))
      bib$author <- paste(bib$author, collapse=" and ")
    if(!is.null(bib$editor))
      bib$editor <- paste(bib$editor, collapse=" and ")

    bib
}

#' Flag values outside of allowable range
#' 
#' Flags any values that are outside the allowable range defined in the 
#' definitions.yml file. NA values are flagged as errors. 
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the definitions.yml file in the config folder
#'
#' @return
#' @export
#'
#' @examples
flag_unsupported_values <- function(data, definitions) {

  # NA values
  data <- data %>% 
    mutate(
      error = ifelse(is.na(value), "Missing value", error),
      error = ifelse(is.na(taxon_name), "Missing species name", error),
    )

  # only check traits not already flagged as errors
  traits <- (filter(data, is.na(error)) %>% pull(trait_name) %>% unique())

  for(trait in traits ) {
   
    # General categorical traits
    if(definitions$traits[[trait]]$type == "categorical") {

      i <-  is.na(data[["error"]]) &
            data[["trait_name"]] == trait &
            !is.null(definitions$traits[[trait]]$values) &
            !check_all_values_in(data$value, names(definitions$traits[[trait]]$values))
      data <- mutate(data, error = ifelse(i, "Unsupported trait value", error))
    }

    # specific tests for flowering, fruiting time
    if(trait %in% c("flowering_time", "fruiting_time") ) {

      ii <- data[["trait_name"]] == trait

      # Only Y,N
      i <-  ii & is.na(data[["error"]]) & grepl("[YN]+", data[["value"]])
      data <- mutate(data, error = ifelse(i, "Time can only contain 0 & 1s", error))

      # Must be length 12
      i <-  ii & is.na(data[["error"]]) & str_length(data[["value"]]) != 12
      data <- mutate(data, error = ifelse(i, "Times must be length 12", error))
    }

    # Numerical traits out of range
    if(definitions$traits[[trait]]$type == "numeric") {

      x <- suppressWarnings(as.numeric(data[["value"]]))
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait & is.na(x)
      data <- mutate(data, error = ifelse(i, "Value does not convert to numeric", error))
 
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait &
        (x < definitions$traits[[trait]]$values$minimum | x > definitions$traits[[trait]]$values$maximum)
      data <- mutate(data, error = ifelse(i, "Value out of allowable range", error))
    }
  }

  data
}

#' Make unit conversion functions
#'
#' @param filename name of file containing unit conversions
#'
#' @return list of conversion functions
#' @export
#'
#' @examples
make_unit_conversion_functions <- function(filename) {
  x <- read_csv(filename, col_types = cols(), progress=FALSE)

  # make functions from text
  fs <- lapply(x[["function"]], function(x) {
                                  my_f <- function(x) {}
                                  body(my_f) <- parse(text = x)
                                  my_f})
  names(fs) <- unit_conversion_name(x[["unit_from"]], x[["unit_to"]])
  fs
}

#' Generate unit conversion name
#' 
#' creates the unit conversion name based on the original units and the units to
#' converted to
#'
#' @param from character of original units
#' @param to character of units to be converted to
#'
#' @return character string containing the name what units are being converted to
#' @export
#'
#' @examples
unit_conversion_name <- function(from, to) {
  sprintf("%s-%s", from, to)
}

#' Convert units to desired type
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the definitions.yml file in the config folder
#' @param unit_conversion_functions unit_conversions.csv file stored in the config folder
#'
#' @return
#' @export
#'
#' @examples
convert_units <- function(data, definitions, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- data %>%
    mutate(
      i = match(trait_name, names(definitions$traits)),
      to = extract_list_element(i, definitions$traits, "units"),
      ucn = unit_conversion_name(unit, to),
      type = extract_list_element(i, definitions$traits, "type"),
      to_convert =  ifelse(is.na(error), (type == "numeric" & unit != to ), FALSE))

  # Identify anything problematic in conversions and drop
  j <- is.na(data[["to_convert"]]) |
        data[["to_convert"]] & !data[["ucn"]] %in% names(unit_conversion_functions)

  data <- mutate(data, 
            error = ifelse(j, "Missing unit conversion", error),
            to_convert = ifelse(j, FALSE, to_convert))

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
#' Add or remove columns of data
#' 
#' Add or remove columns of data as needed so that all sets
#' have the same columns. Used for adding extra columns for the trait, sites
#' and context tibbles
#'
#' @param data tibble or dataframe containing the study data
#' @param vars additional data to be added
#'
#' @return list or tibble with additional data added  
#' @export
#'
#' @examples
#' 
add_all_columns <- function(data, vars) {

  missing <- setdiff(vars, names(data))

  for(v in missing)
    data <- mutate(data, !!v := NA_character_)
  
  data %>%
    select(one_of(vars)) %>%
    mutate(error = NA_character_)
}

#' Process a single dataset
#' 
#' Processes a single dataset using the original dataframe and the associated 
#' metadata.yml file based on `dataset_id`. Builds a tibble with the study data 
#' and adds the corrected trait names, assigns a unique observation_id and implements
#' any categorical trait value substituions. `parse data` is used in the core 
#' workflow pipeline (such as in `load study`).
#'
#' @param data tibble or dataframe containing the study data
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param metadata yaml file with metadata
#'
#' @return tibble
#' @export
parse_data <- function(data, dataset_id, metadata) {

  # get config data for dataset
  data_is_long_format <- metadata[["config"]][["data_is_long_format"]]

  # Step 1. create dataframe with data for vars that we want to keep, and set to correct names
  # all names in "variable_match" must exist in dataset, if not then we need to stop and fix the problem
  var_in <- unlist(metadata[["config"]][["variable_match"]])
  var_out <- names(metadata[["config"]][["variable_match"]])
  if (any(!var_in %in% colnames(data))) {
    stop(paste0("\nVariable '", setdiff(var_in, colnames(data)), "' from data.csv not found in configVarnames"))
  }

  df <- data %>%
        select(one_of(var_in)) %>%
        rename_columns(var_in, var_out) %>% 
        mutate(dataset_id = dataset_id)

  # Add unique observation ids 
  # function builds id -- determine number of 00s needed based on number of records
  make_id <- function(n, dataset_id) 
              sprintf(paste0("%s_%0", ceiling(log10(n)), "d"), 
                              dataset_id, seq_len(n))

  if(!data_is_long_format) {
    # For wide datasets rows are assumed to be natural grouping
    df <- df %>% 
            mutate(observation_id = make_id(nrow(.), dataset_id))
  } else {
    
    # For long datasets, create unique identifier from taxon_name, site, and observation_id (if specified)
    df[["observation_id_tmp"]] <- gsub(" ", "-", df[["taxon_name"]])
      
    if(!is.null(df[["site_name"]][1]))
      df[["observation_id_tmp"]] <- paste0(df[["observation_id_tmp"]],"_", df[["site_name"]])

    if(!is.null(df[["observation_id"]])) {
      df[["observation_id_tmp"]] <- paste0(df[["observation_id_tmp"]],"_", df[["observation_id"]])
      df[["observation_id"]] <- NULL
    }

    df <- df %>%
              left_join(by = "observation_id_tmp",
                        tibble(observation_id_tmp = df[["observation_id_tmp"]] %>% unique() %>% sort(), 
                               observation_id = make_id(length(observation_id_tmp), dataset_id))
                        ) %>%
              select(-observation_id_tmp)
  }
  
  # Step 2. Add trait information, with correct names

  cfgChar <-
    metadata[["traits"]] %>%
    list_to_df() %>%
    filter(!is.na(trait_name))  # remove any rows without a matching trait record
   
  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  if (data_is_long_format == FALSE & any(! cfgChar[["var_in"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(cfgChar[["var_in"]], colnames(data))))
  }

  ## if needed, change from wide to long style
  if (data_is_long_format == FALSE) {
    # if the dataset is "wide" then process each variable in turn, to create the "long" dataset -
    # say the original dataset has 20 rows of data and 5 traits, then we will end up with 100 rows
    out <- list()
    for (i in seq_len(nrow(cfgChar))) {
      # create a temporary dataframe which is a copy of df
      # df is our data frame containing all the columns we want EXCEPT for the trait data itself
      out[[i]] <- df
      # to x we append columns of data for trait_name, unit and value (the latter is retrieved from the data)
      out[[i]][["trait_name"]] <- cfgChar[["var_in"]][i]
      out[[i]][["value"]] <- data[[cfgChar[["var_in"]][i]]] %>% as.character()
    }
    out <- dplyr::bind_rows(out)
  } else {
    out <- df
    out[["value"]] <- out[["value"]] %>%  as.character()
  }

  # Ensure all lower case
  out[["value"]] <- tolower(out[["value"]])

  # Add information on trait type, precision, if not already present
  vars <- c("value_type", "replicates")
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
    cfgLookup <-  list_to_df(metadata[["substitutions"]]) %>%
      mutate(
             find=tolower(find),
             replace=tolower(replace)
             )

    for(i in seq_len(nrow(cfgLookup))) {
      j <- which(out[["trait_name"]] == cfgLookup[["trait_name"]][i] &
                  out[["value"]] == cfgLookup[["find"]][i])
      if( length(j) > 0 ) {
        out[["value"]][j] <- cfgLookup[["replace"]][i]
      }
    }
  }

  out
}


## Enforce some standards on species names
#' Standardise species names
#'
#' @param x vector, dataframe or list containing original species names 
#'
#' @return vector with standardised species names
standardise_names <- function(x) {

  f <- function(x, find, replace) {
    gsub(find, replace, x, perl=TRUE)
  }

  x %>%
    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%

    ## sp. not sp or spp
    f("\\ssp(\\s|$)", " sp.\\1") %>%
    f("\\sspp.(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%

    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)", " subsp.\\1") %>%
    f("\\sssp.(\\s|$)", " subsp.\\1") %>%
    f("\\ssubsp(\\s|$)", " subsp.\\1") %>%
    f("\\ssub sp.(\\s|$)", " subsp.\\1") %>%

    ## var. not var
    f("\\svar(\\s|$)", " var.\\1") %>%

    ## aff. not affin, aff, affn
    f("\\saffin(\\s|$)", " aff.\\1") %>%
    f("\\saff(\\s|$)", " aff.\\1") %>%
    f("\\saffn(\\s|$)", " aff.\\1") %>%

    ## f. not forma
    f("\\sforma(\\s|$)", " f.\\1") %>%

    ## remove " ms" if present
    f("\\sms(\\s|$)", "\\1") %>%

    ## remove " s.l" or " s.s." if present
    f("\\ssl(\\s|$)", "\\1") %>%
    f("\\ss\\.l\\.(\\s|$)", "\\1") %>%
    f("\\sss(\\s|$)", "") %>%
    f("\\ss\\.s\\.(\\s|$)", "\\1") %>%

    ## clean white space
    #f("[\\s]+", " ") %>%
    str_squish()

}

#' Apply taxonomic updates
#' 
#' Applies taxonomic updates to the study data from the metadata.yml file
#'
#' @param data tibble or dataframe containing the study data
#' @param metadata yaml file containing the metadata
#'
#' @return tibble with the taxonomic updates applied
apply_taxonomic_updates  <- function(data, metadata){

  out <- data

  # copy original species name to a new column
  out[["original_name"]] <- out[["taxon_name"]]

  # Now make any replacements specified in metadata yaml
  ## Read metadata table, quit if empty
  cfgLookup <-  list_to_df(metadata[["taxonomic_updates"]])
  if(is.na(cfgLookup) || nrow(cfgLookup) == 0) {
    return(out)
  }

  to_update <- rep(TRUE, nrow(out))
  
  ## Makes replacements, row by row
  for(i in seq_len(nrow(cfgLookup))) {
    j <- which(out[["taxon_name"]] == cfgLookup[["find"]][i])
    if( length(j) > 0 ){
      out[["taxon_name"]][j] <- cfgLookup[["replace"]][i]
      to_update[j] <- FALSE
    }
  }

  # for any that haven't been updated, run script to standardize names
  out[["taxon_name"]][to_update] <- standardise_names(out[["taxon_name"]][to_update])

  ## Return updated table
  out
}

#' Combine all the AusTraits studies into the compiled AusTraits database
#'
#' `combine_austraits` compiles all the loaded studies into a single AusTraits 
#' database object as a large list
#'
#' @param ... arguments passed to other functions
#' @param d list of all the AusTraits studies
#' @param definitions definitions read in from the definitions.yml file in the config folder
#'
#' @return AusTraits compilation database as a large list
#' @export
combine_austraits <- function(..., d=list(...), definitions) {

  
  map_lgl(d, ~.x$site_name %>% is.logical())
  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # combine sources and remove duplicates
  sources <- d %>% lapply("[[", "sources") 
  keys <- sources %>% lapply(names)  %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% reduce(c) %>% .[keys]
  
  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "dataset_id")

  # taxonomy 
  taxonomic_updates <- 
    combine("taxonomic_updates", d) %>%
    group_by(original_name, cleaned_name) %>%
    mutate(dataset_id = paste(dataset_id, collapse = " ")) %>% 
    ungroup() %>% 
    distinct() %>% 
    arrange(original_name, cleaned_name)

  traits <- combine("traits", d)

  ret <- list(traits=traits,
              sites=combine("sites", d),
              contexts=combine("contexts", d),
              methods=combine("methods", d),
              excluded_data = combine("excluded_data", d),
              taxonomic_updates=taxonomic_updates,
              taxa = taxonomic_updates %>% select(taxon_name = cleaned_name) %>% distinct(),
              definitions = definitions,
              contributors=combine("contributors", d),
              sources = sources,
              build_info = list(
                      session_info = sessionInfo()
                      )
              )
  ret
}


#' Apply taxonomic updates to austraits_raw
#' 
#' Applies taxonomic updates to austraits_raw
#'
#' @param austraits_raw AusTraits compiled data as a large list without taxonomic updates applied
#' @param taxa taxon list
#'
#' @return list AusTraits compiled data with taxonomic updates applied
#' @export
update_taxonomy <- function(austraits_raw, taxa) {
  
  austraits_raw$taxonomic_updates <- 
    austraits_raw$taxonomic_updates %>% 
    left_join(by = "cleaned_name", 
              taxa %>% select(cleaned_name, taxonIDClean, taxonomicStatusClean, 
                                      alternativeTaxonomicStatusClean, acceptedNameUsageID, taxon_name)
              ) %>% 
    distinct() %>% 
    arrange(cleaned_name)
  
  austraits_raw$traits <- 
    austraits_raw$traits %>% 
    rename(cleaned_name = taxon_name) %>% 
    left_join(by = "cleaned_name", 
              taxa %>% select(cleaned_name, taxon_name)
              ) %>% 
    select(dataset_id, taxon_name, everything()) %>% 
    mutate(taxon_name = ifelse(is.na(taxon_name), cleaned_name, taxon_name)) %>% 
    select(-cleaned_name)
  
  species_tmp <-
    austraits_raw$traits %>% 
    select(taxon_name) %>% 
    distinct() %>% 
    left_join(by = "taxon_name",
      taxa %>% select(-contains("clean")) %>% distinct()
    ) %>%
    # extract genus as this is useful
    mutate(
      genus = taxon_name  %>% stringr::str_split(" " ) %>% map_chr(1),
      genus = ifelse(genus %in% taxa$taxon_name, genus, NA)
    )  %>% 
    arrange(taxon_name) %>% 
    mutate(
      taxonomicStatus = ifelse(is.na(taxonomicStatus) & !is.na(genus), "genus_known", taxonomicStatus),
      taxonomicStatus = ifelse(is.na(taxonomicStatus) & is.na(genus), "unknown", taxonomicStatus),
    ) %>% 
    split(.$taxonomicStatus)

  # retrieve families from list of known genera - prioritise genera with accepted names
   species_tmp[["genus_known"]] <- 
     species_tmp[["genus_known"]] %>% 
     select(-family) %>% 
     left_join(by="genus", 
               taxa %>% filter(taxonRank == "Genus") %>% select(genus = taxon_name, family) %>% distinct()
     )

  austraits_raw$taxa <-
    species_tmp %>% 
    bind_rows() %>% 
    arrange(taxon_name)

  austraits_raw
}

#' Add version information to AusTraits
#'
#' @param austraits AusTraits database object
#' @param version version number
#' @param git_sha Git SHA 
#'
#' @return AusTraits database object with version information added 
add_version_info <- function(austraits, version, git_sha) {
  
  austraits$build_info <- list(
    version=version,
    git_SHA=git_sha,
    session_info = austraits$build_info$session_info
  )

  austraits
}


#' Export AusTraits version as plain text
#'
#' @param austraits AusTraits database object
#' @param path pathway to save file
#'
#' @return csv files of tibbles containing traits, sites, contexts, methods, excluded_data,
#' taxonomic updates, taxa, contributors
export_version_plaintext <- function(austraits, path) {

  unlink(path, TRUE)
  dir.create(path, FALSE, TRUE)

  # Capture information on build into text
  build_info <- capture.output(print(austraits$build_info))
  writeLines(build_info, sprintf("%s/build_info.md", path))

  # Save definitions
  write_yaml(austraits[["definitions"]], sprintf("%s/definitions.yml", path))

  # Save references
  RefManageR::WriteBib(austraits$sources, sprintf("%s/sources", path))

  # Save tables
  for (v in c("traits", "sites", "contexts", "methods", "excluded_data", "taxonomic_updates", "taxa", "contributors")) {
    readr::write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  }
}

#' Create release
#' 
#' Builds various files to go with the release of a new AusTraits version. Including 
#' austraits.rds, a readme file, dictionary.rmd file, plain text files with csv, and
#' a NEWS.md file.
#'
#' @param austraits AusTraits database object
#' @param v_prev specify whether to update the NEWS.md file, default = NULL
create_release <- function(austraits, v_prev= NULL) {
  version_number <- austraits$build_info$version

  export_dir <- sprintf("export/data/austraits-%s", version_number)
  unlink(export_dir, recursive = TRUE)
  dir.create(export_dir, FALSE, TRUE)

  # RDS file
  filename <- sprintf(I("%s/austraits-%s.rds"), export_dir, version_number)
  saveRDS(austraits, filename)

  # readme
  knitr::knit("scripts/README.Rmd", output = sprintf("%s/readme.txt", export_dir))

  # dictionary_target:
  rmarkdown::render("scripts/dictionary.Rmd", params = list(austraits = austraits), output_file = sprintf("../%s/dictionary.html", export_dir))

  # plaintext_target:
  path <- sprintf("%s/austraits-%s", export_dir, version_number)
  export_version_plaintext(austraits, path)

  # News
  if (!is.null(v_prev)) {
    rmarkdown::render("scripts/news.Rmd",
      params = list(v_prev = v_prev, v_curr = version_number),
      output_file = "tmp_news.md"
    )

    f1 <- readLines("scripts/tmp_news.md")
    f2 <- readLines("NEWS.md")

    writeLines(c(f1, "\n", f2), "NEWS.md")
  }

  file.copy("NEWS.md", sprintf("%s/NEWS.md", export_dir))
  
  # Go to directory and zip
  # remove existing file
  wd <- setwd(dirname(path))
  on.exit(setwd(wd))

  filename <- sprintf("%s.zip", basename(path))
  unlink(filename)
  zip(filename, basename(path))
  unlink(path, recursive = TRUE)

  message("Export created at ", export_dir)

}
