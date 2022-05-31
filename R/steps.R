#' Configure AusTraits database object
#'
#' Creates the config object which gets passed onto `load_dataset`. The config list contains
#' the subset of trait definitions and unit conversions for those traits for a each study.
#' `subset_config` is used in the remake::make process to configure individual studies mapping the
#' individual traits found in that study along with any relevant unit conversions
#' and trait definitions. `subset_config` and `load_dataset` are applied to every study
#' in the remake.yml file
#'
#' @param filename_metadata metadata yaml file for a given study
#' @param trait_definitions definitions read in from the traits.yml
#' @param unit_conversion_functions unit_conversion.csv file read in from the config folder
#'
#' @return list with dataset_id, metadata, trait_definitions and unit_conversion_functions
#' @importFrom purrr map_chr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom lubridate dmy
#' @export
#'
#' @examples
#' \dontrun{
#' subset_config("data/Falster_2003/metadata.yml", read_yaml("config/traits.yml"),
#' make_unit_conversion_functions("config/unit_conversions.csv"))
#' }
subset_config <- function(
  filename_metadata,
  trait_definitions,
  unit_conversion_functions) {

  dataset_id <- basename(dirname(filename_metadata))

  # read metadata
  metadata <- read_metadata(filename_metadata)

  # table of trait_mapping
  trait_mapping <-
    metadata[["traits"]] %>%
    list_to_df() %>%
    filter(!is.na(.data$trait_name)) %>%
    # determine unit conversions
    dplyr::mutate(
      i = match(.data$trait_name, names(trait_definitions$elements)),
      to = map_chr(.data$i, ~extract_list_element(.x, trait_definitions$elements, "units")),
      conversion = unit_conversion_name(.data$unit_in, .data$to)
    )

  unit_conversion_functions_sub <-
    unit_conversion_functions[trait_mapping %>%
    filter(.data$unit_in!=.data$to) %>% dplyr::pull(.data$conversion) %>% unique()]

  # subset of trait definitions
  trait_definitions <-
    trait_definitions$elements[names(trait_definitions$elements) %in% trait_mapping$trait_name]

  list(dataset_id = dataset_id,
       metadata = metadata,
       trait_definitions = trait_definitions,
       unit_conversion_functions = unit_conversion_functions_sub)
}

#' Load Study
#'
#' load_dataset is used to load individual studies using the config file generated
#' from `subset_config()`. `subset_config` and `load_dataset` are applied to every
#' study in the remake.yml file
#'
#' @param filename_data_raw raw data.csv file for any given study
#' @param config_for_dataset config settings generated from `subset_config()`
#' @param filter_missing_values default filters missing values from the excluded data table. 
#' Change to false to see the rows with missing values.
#'
#' @return list, AusTraits database object
#' @export
#' @importFrom dplyr select mutate filter arrange distinct case_when full_join everything any_of
#' @importFrom tidyr spread
#' @importFrom purrr reduce map_chr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' load_dataset("data/Falster_2003/data.csv", subset_config("data/Falster_2003/metadata.yml",
#' read_yaml("config/traits.yml"), make_unit_conversion_functions("config/unit_conversions.csv"))
#' }
load_dataset <- function(filename_data_raw, 
                       config_for_dataset, 
                       schema,
                       filter_missing_values = TRUE){

  dataset_id <- config_for_dataset$dataset_id
  metadata <- config_for_dataset$metadata
  trait_definitions <- config_for_dataset$trait_definitions

  unit_conversion_functions <- config_for_dataset$unit_conversion_functions

  # load and clean trait data
  traits <- readr::read_csv(filename_data_raw, col_types = cols(), guess_max = 100000, progress=FALSE) %>%
    custom_manipulation(metadata[["dataset"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    add_all_columns(names(schema[["austraits"]][["elements"]][["traits"]][["elements"]])) %>%
    flag_unsupported_traits(trait_definitions) %>%
    flag_excluded_observations(metadata) %>%
    convert_units(trait_definitions, unit_conversion_functions) %>%
    flag_unsupported_values(trait_definitions) %>%
    apply_taxonomic_updates(metadata) %>%
    dplyr::mutate(
      # For cells with multiple values (separated by a space), sort these alphabetically
      value = ifelse(is.na(.data$error), split_then_sort(.data$value), .data$value),
      value_type = factor(.data$value_type, levels = names(schema$value_type$values)),
      #ensure dates are converted back to character
      collection_date = as.character(.data$collection_date)
      ) %>%
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)

  # extract site data from metadata
  sites <-
    metadata$sites %>%
    format_sites(dataset_id) %>%
    add_all_columns(names(schema[["austraits"]][["elements"]][["sites"]][["elements"]])) %>%
    dplyr::select(-.data$error) %>%
    # reorder so type, description come first, if present
    dplyr::mutate(i = case_when(.data$site_property == "description" ~ 1, .data$site_property == "latitude (deg)" ~ 2,
                                .data$site_property == "longitude (deg)" ~ 3, TRUE ~ 4)) %>%
    dplyr::arrange(.data$site_name, .data$i, .data$site_property) %>%
    dplyr::select(-.data$i)

  # read contextual data
  contexts <-
    metadata$contexts %>%
    format_sites(dataset_id, context = TRUE) %>%
    add_all_columns(names(schema[["austraits"]][["elements"]][["contexts"]][["elements"]])) %>%
    dplyr::select(-.data$error) %>%
    # reorder so type, description come first, if present
    dplyr::arrange(.data$context_name, .data$context_property)

  # record contributors
  if (length(unlist(metadata$contributors$data_collectors)) >1 ){
  contributors <-
    metadata$contributors$data_collectors %>%
    list_to_df() %>%
    dplyr::mutate(dataset_id = dataset_id) %>%
    filter(!is.na(.data$last_name))
  } else {
   contributors <- tibble::tibble(dataset_id = character())
  }

  contributors <-
    contributors %>%
    add_all_columns(names(schema[["austraits"]][["elements"]][["contributors"]][["elements"]]), add_error_column = FALSE)

  # record methods on study from metadata
  sources <- metadata$source %>%
            lapply(convert_list_to_bib) %>% reduce(c)
  source_primary_key <- metadata$source$primary$key
  source_secondary_keys <- setdiff(names(sources), source_primary_key)

  # combine collectors to add into the methods table
  collectors_tmp <-
    stringr::str_c(contributors$given_name, " ",
                   contributors$last_name,
                   ifelse(!is.na(contributors$additional_role),
                          paste0(" (", contributors$additional_role, ")"),
                          ""))  %>% paste(collapse = ", ")

  methods <-
    full_join( by = "dataset_id",
      # methods used to collect each trait
      metadata[["traits"]] %>%
        list_to_df() %>%
        filter(!is.na(.data$trait_name)) %>%
        dplyr::mutate(dataset_id = dataset_id) %>%
        dplyr::select(dataset_id, .data$trait_name, .data$methods)
      ,
      # study methods
      metadata$dataset %>%
        list1_to_df() %>%
        spread(.data$key, .data$value) %>%
        dplyr::select(dplyr::any_of(names(metadata$dataset))) %>%
          dplyr::mutate(dataset_id = dataset_id) %>%
          dplyr::select(-dplyr::any_of(c("original_file", "notes", "data_is_long_format", "taxon_name",
                                         "trait_name", "observation_id", "context_name", "site_name",
                                         "date", "collection_date", "custom_R_code",
                                         "taxon_name", "collection_type", "sample_age_class")))
      )  %>%
      full_join( by = "dataset_id",
        #references
        tibble::tibble(
          dataset_id = dataset_id,
          source_primary_key = source_primary_key,
          source_primary_citation = bib_print(sources[[source_primary_key]]),
          source_secondary_key = source_secondary_keys %>% paste(collapse = "; "),
          source_secondary_citation = ifelse(length(source_secondary_keys) == 0, NA_character_,
          map_chr(sources[source_secondary_keys], bib_print) %>% paste(collapse = "; ") %>%
          stringr::str_replace_all(".;", ";")
          )
        )
      ) %>%
    dplyr::mutate(data_collectors = collectors_tmp,
                  assistants = ifelse(is.null(metadata$contributors$assistants), NA_character_,
                                      metadata$contributors$assistants
                                      ),
                  austraits_curators = metadata$contributors$austraits_curators
                  )

  # Where missing, fill variables with values from sites
  vars <- c("collection_type", "sample_age_class", "collection_date", "measurement_remarks",
                  "value_type", "replicates")

  for(v in vars){
    # merge in to traits from site level
    if(v %in% sites$site_property){
      traits_tmp <- traits %>%
        dplyr::left_join(by = "site_name",
                         sites %>% tidyr::pivot_wider(names_from = "site_property", values_from = "value") %>%
                           dplyr::select(.data$site_name, col_tmp = dplyr::any_of(v)))
     ## Use site level value if present
     traits[[v]] <- ifelse(!is.na(traits_tmp[["col_tmp"]]), traits_tmp[["col_tmp"]], traits[[v]])
    }
  }

  # Remove any values included to map into traits table
  sites <- sites %>% dplyr::filter(!(.data$site_property %in% vars))

  # Retrieve taxonomic details for known species
  taxonomic_updates <-
    traits %>%
    dplyr::select(dataset_id, .data$original_name, cleaned_name = .data$taxon_name) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cleaned_name)

  list(dataset_id = dataset_id,
       traits     = traits %>% filter(is.na(.data$error)) %>% dplyr::select(-.data$error),
       sites      = sites,
       contexts   = contexts,
       methods    = methods,
       excluded_data = 
         if(filter_missing_values == TRUE){
           excluded_data = traits %>% dplyr::filter(!is.na(.data$error)) %>% dplyr::filter(error != "Missing value") %>%
             dplyr::select(.data$error, everything())
           } else {
             excluded_data = traits %>% filter(!is.na(.data$error)) %>% dplyr::select(.data$error, everything())
             },
       taxonomic_updates = taxonomic_updates,
       taxa       = taxonomic_updates %>% dplyr::select(taxon_name = .data$cleaned_name) %>% dplyr::distinct(),
       contributors = contributors,
       sources    = sources,
       trait_definitions = trait_definitions,
       schema=schema
  )
}

#' Apply custom data manipulations
#'
#' Applies custom data manipulations if the metadata field custom_R_code is not empty
#' Otherwise no manipulations will be done by applying the `identity` function.
#' The code custom_R_code assumes a single input.
#'
#' @param txt character text within custom_R_code of a metadata.yml file
#'
#' @return character text containing custom_R_code if custom_R_code is not empty,
#' otherwise no changes are made
#' @export
custom_manipulation <- function(txt) {
  if (!is.null(txt) && !is.na(txt)  && nchar(txt) > 0) {

    txt2 <-
      # Trim white space, quotes, new line from front and back
      txt %>% stringi::stri_trim_both("[^'\"\\ \\n]", negate=FALSE) %>%
      # Squish internal white space, also removes new line characters
      stringr::str_replace_all("\\s+", " ")
    # test: txt <-" '' \n Total of 23.5 bitcoins. "

    function(data) {eval(parse(text=txt2), envir=new.env())}
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
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' format_sites(yaml::read_yaml("data/Falster_2003/metadata.yml")$sites, "Falster_2003")
#' format_sites(yaml::read_yaml("data/Apgaua_2017/metadata.yml")$context,
#' "Apgaua_2017", context = TRUE)
#' }
format_sites <- function(my_list, dataset_id, context = FALSE) {

  # default, if length 1 then it's an "na"
  if (length(unlist(my_list)) == 1) {
    return(tibble::tibble(dataset_id = character()))
  }

  out <-
    my_list %>%
    lapply(lapply, as.character) %>%
    purrr::map_df(list1_to_df, .id = "name") %>%
    dplyr::mutate(dataset_id = dataset_id)

  if (!context) {
    out <- out %>%
      dplyr::rename(site_property = "key", site_name = "name")
  } else {
    out <- out %>%
      dplyr::rename(context_property = "key", context_name = "name")
  }

  out
}

#' Flag any unrecognised traits
#'
#' Flag any unrecognised traits, as defined in the traits.yml file
#'
#' @param data tibble or dataframe containing the study data
#' @param trait_definitions definitions read in from the traits.yml file in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with unrecognised traits flagged as "Unsupported trait" in the "error" column
#' @export
flag_unsupported_traits <- function(data, trait_definitions) {

  # create error column if not already present
  if(is.null(data[["error"]]))
    data[["error"]] <- NA_character_

  # exclude traits not in definitions
  i <- data$trait_name %in% names(trait_definitions)
  data %>%
    dplyr::mutate(error = ifelse(!i, "Unsupported trait", .data$error))

  data
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
#' @importFrom stringr str_squish
#' @importFrom rlang .data
#' @return dataframe with flagged excluded observations if there are any
#' @export
flag_excluded_observations <- function(data, metadata) {

  if(length(metadata$exclude_observations)==1 && is.na(metadata$exclude_observations)) return(data)

  fix <-
    metadata$exclude_observations %>%
    list_to_df() %>%
    tidyr::separate_rows(.data$find, sep=", ") %>%
    dplyr::mutate(find = str_squish(.data$find))

  if(nrow(fix) == 0) return(data)

  fix <- split(fix, fix$variable)

  for(v in names(fix))
    data <- data %>%
      dplyr::mutate(error = ifelse(data[[v]] %in% fix[[v]]$find, "Observation excluded in metadata", .data$error))

  data
}

#' Check values in one vector against values in another vector
#'
#' `check_all_values_in` checks if values in vector x are in y. Values in x may
#' contain multiple values separated by `sep` so these are split first using `str_split`.
#'
#' @param x vector
#' @param y vector
#' @param sep amount of space separating values to be split, default = " " (a single space)
#'
#' @return vector of logical values
#' @export
check_all_values_in <- function(x, y, sep=" "){
  x %>% stringr::str_split(sep) %>% sapply(function(xi) all(xi %in% y))
}

#' Format BibEntry using RefManageR
#'
#' Format BibEntry object according to desired style using RefManageR
#'
#' @param bib BibEntry object
#' @param .opts list of parameters for formatting style
#'
#' @importFrom rlang .data
#' @return character string of formatted reference
bib_print <- function(bib, .opts = list(first.inits = TRUE, max.names = 1000, style="markdown") ) {

  format.BibEntry <- utils::getFromNamespace("format.BibEntry", "RefManageR")
  # set format
  oldopts <- RefManageR::BibOptions(.opts)
  on.exit(RefManageR::BibOptions(oldopts))

  bib %>%
    format.BibEntry(.sort = F) %>%
    # HACK: remove some of formatting introduced in line above
    # would be nicer if we could apply csl style
    gsub("[] ", "", ., fixed = TRUE) %>%
    gsub("\\n", " ", .) %>%
    gsub("  ", " ", .) %>%
    gsub("DOI:", " doi: ", ., fixed = TRUE) %>%
    gsub("URL:", " url: ", ., fixed = TRUE) %>%
    ifelse(tolower(bib$bibtype) == "article",  gsub("In:", " ", .), .)
}

#' Convert a list of elements into a BibEntry object
#'
#' @param ref list of elements for a reference
#'
#' @return BibEntry object
#' @export
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
#' traits.yml file. NA values are flagged as errors.
#'
#' @param data tibble or dataframe containing the study data
#' @param trait_definitions definitions read in from the traits.yml file in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with flagged values outside of allowable range, unsupported categorical
#' trait values or missing values
#' @export
flag_unsupported_values <- function(data, trait_definitions) {

  # NA values
  data <- data %>%
    dplyr::mutate(
      error = ifelse(is.na(.data$value), "Missing value", .data$error),
      error = ifelse(is.na(.data$taxon_name), "Missing species name", .data$error),
    )

  # only check traits not already flagged as errors
  traits <- data %>%
    dplyr::filter(is.na(.data$error)) %>% dplyr::pull(.data$trait_name) %>% unique()

  for(trait in traits ) {

    # General categorical traits
    if(trait_definitions[[trait]]$type == "categorical") {

      i <-  is.na(data[["error"]]) &
            data[["trait_name"]] == trait &
            !is.null(trait_definitions[[trait]]$values) &
            !check_all_values_in(data$value, names(trait_definitions[[trait]]$values))
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Unsupported trait value", .data$error))
    }

    # specific tests for flowering, fruiting time
    if(trait %in% c("flowering_time", "fruiting_time") ) {

      ii <- data[["trait_name"]] == trait

      # Only Y,N
      i <-  ii & is.na(data[["error"]]) & grepl("[YN]+", data[["value"]])
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Time can only contain 0 & 1s", .data$error))

      # Must be length 12
      i <-  ii & is.na(data[["error"]]) & stringr::str_length(data[["value"]]) != 12
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Times must be length 12", .data$error))
    }

    # Numerical traits out of range
    if(trait_definitions[[trait]]$type == "numeric") {

      x <- suppressWarnings(as.numeric(data[["value"]]))
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait & is.na(x)
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Value does not convert to numeric", .data$error))

      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait &
        (x < trait_definitions[[trait]]$values$minimum | x > trait_definitions[[trait]]$values$maximum)
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Value out of allowable range", .data$error))
    }
  }

  data
}

#' Make unit conversion functions
#'
#' @param filename name of file containing unit conversions
#'
#' @return list of conversion functions
#' @importFrom readr cols read_csv
#' @export
#'
#' @examples
#' \dontrun{
#' make_unit_conversion_functions("config/unit_conversions.csv")
#' }
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
unit_conversion_name <- function(from, to) {
  sprintf("%s-%s", from, to)
}

#' Convert units to desired type
#'
#' @param data tibble or dataframe containing the study data
#' @param trait_definitions definitions read in from the traits.yml file in the config folder
#' @param unit_conversion_functions unit_conversions.csv file stored in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with converted units
#' @export
convert_units <- function(data, trait_definitions, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- data %>%
    dplyr::mutate(
      i = match(.data$trait_name, names(trait_definitions)),
      to = extract_list_element(.data$i, trait_definitions, "units"),
      ucn = unit_conversion_name(.data$unit, .data$to),
      type = extract_list_element(.data$i, trait_definitions, "type"),
      to_convert =  ifelse(is.na(.data$error), (.data$type == "numeric" & .data$unit != .data$to), FALSE))

  # Identify anything problematic in conversions and drop
  j <- is.na(data[["to_convert"]]) |
        data[["to_convert"]] & !data[["ucn"]] %in% names(unit_conversion_functions)

  data <- data %>%
    dplyr::mutate(error = ifelse(j, "Missing unit conversion", .data$error),
                  to_convert = ifelse(j, FALSE, .data$to_convert))

  f <- function(value, name) {
    as.character(unit_conversion_functions[[name]](as.numeric(value)))
  }

  # Split by unique unit conversions, to allow for as few calls as possible
  data %>%
    dplyr::group_by(.data$ucn, .data$to_convert) %>%
    dplyr::mutate(
      value = ifelse(.data$to_convert, f(.data$value, .data$ucn[1]), .data$value),
      unit = ifelse(.data$to_convert, .data$to, .data$unit)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(vars))
}

#' Add or remove columns of data
#'
#' Add or remove columns of data as needed so that all datasets
#' have the same columns. Also adds in an error column.
#'
#' @param data dataframe containing study data read in as a csv file
#' @param vars vector of variable columns names to be included in the final formatted tibble
#' @param add_error_column adds an extra column called error if TRUE
#'
#' @return tibble with the correct selection of columns including an error column
#' @importFrom rlang :=
#' @importFrom dplyr select mutate filter arrange distinct any_of
#' @export
add_all_columns <- function(data, vars, add_error_column = TRUE) {

  missing <- setdiff(vars, names(data))

  for(v in missing)
    data <- data%>%
      dplyr::mutate(!!v := NA_character_)

  data <- data %>%
    dplyr::select(dplyr::any_of(vars))

  if(add_error_column){
    data <- data %>%
      dplyr::mutate(error = NA_character_)
  }

  data
}

#' Process a single dataset
#'
#' Process a single dataset with `dataset_id` using the associated `data.csv` and
#' `metadata.yml` files. Adds a unique observation id for each row of observation,
#' trait names are formatted using AusTraits accepted names and trait substitutions
#' are added. `parse data` is used in the core workflow pipeline (i.e. in `load study`).
#'
#' @param data tibble or dataframe containing the study data
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param metadata yaml file with metadata
#' @return tibble in long format with AusTraits formatted trait names, trait
#' substitutions and unique observation id added
#' @importFrom dplyr select mutate filter arrange distinct case_when full_join everything any_of bind_cols
#' @importFrom rlang .data
#' @export
parse_data <- function(data, dataset_id, metadata) {

  # get config data for dataset
  data_is_long_format <- metadata[["dataset"]][["data_is_long_format"]]

  # Step 1a. create dataframe with data for vars that we want to keep, and set to correct names
  var_in <- unlist(metadata[["dataset"]])
  i <- var_in %in% names(data)

  df <- data %>%
        # next step selects and renames columns based on named vector
        dplyr::select(any_of(var_in[i])) %>%
        dplyr::mutate(dataset_id = dataset_id)

  # Step 1b. import any values that aren't columns of data
  vars <- c("value_type", "replicates", "collection_date",
            "collection_type", "sample_age_class", "measurement_remarks")

  df <-
    df %>%
    bind_cols(
      metadata[["dataset"]][names(metadata[["dataset"]]) %in% vars[!vars %in% names(df)]] %>% tibble::as_tibble()
    )

  # Add unique observation ids
  # function builds id -- determine number of 00s needed based on number of records
  make_id <- function(n, dataset_id)
              sprintf(paste0("%s_%0", ceiling(log10(n)), "d"),
                              dataset_id, seq_len(n))

  if(!data_is_long_format) {
    # For wide datasets rows are assumed to be natural grouping
    df <- df %>%
            dplyr::mutate(observation_id = make_id(nrow(.), dataset_id))
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
              dplyr::left_join(by = "observation_id_tmp",
                        tibble::tibble(observation_id_tmp = df[["observation_id_tmp"]] %>% unique() %>% sort(),
                               observation_id = make_id(length(.data$observation_id_tmp), dataset_id))
                        ) %>%
              dplyr::select(-.data$observation_id_tmp)
  }

  # Step 2. Add trait information, with correct names
  cfgChar <-
    metadata[["traits"]] %>%
    list_to_df() %>%
    dplyr::filter(!is.na(.data$trait_name))  # remove any rows without a matching trait record

  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  if (data_is_long_format == FALSE & any(! cfgChar[["var_in"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(cfgChar[["var_in"]], colnames(data))))
  }


  ## if needed, change from wide to long format
  if (!data_is_long_format) {

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

      # Pull in additional information for each trait as specified in traits part of metadata, here represented as cfgChar
      # Values in table can specify a column in the original data OR a value to use

      vars_to_check <- vars[vars%in% names(cfgChar)]
      # For each column in cfgChar
      for(v in vars_to_check) {
        # get value
        value <- cfgChar[i,v, drop=TRUE]

        # Check if it is a column in data or not and process accordingly
        if(!is.na(value)) {
          if(!is.null(data[[value]])) {
            out[[i]][[v]] <- data[[value]] %>% as.character()
          } else {
            out[[i]][[v]] <- value
          }
        }
      }
    }
    out <- dplyr::bind_rows(out)
  } else {
    out <- df %>% filter(.data$trait_name %in% cfgChar$var_in)
    out[["value"]] <- out[["value"]] %>%  as.character()

    # Pull in additional information for each trait as specified in traits part of metadata, here represented as cfgChar
    # Values in table can specify a column in the original data OR a value to use

    vars_to_check <- vars[vars%in% names(cfgChar)]
    # For each column in cfgChar
    for (i in seq_len(nrow(cfgChar))) {
      for(v in vars_to_check) {
        value <- cfgChar[i,v, drop=TRUE]
        if(!is.na(value)) {
          out[[v]][out$trait_name == cfgChar[["var_in"]][i]] <- value
        }
      }
    }
  }

  # Ensure all lower case
  out[["value"]] <- tolower(out[["value"]])

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
      dplyr::mutate(
             find = tolower(.data$find),
             replace = tolower(.data$replace)
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

#' Standardise species names
#'
#' Enforces some standards on species names
#'
#' @param x vector, dataframe or list containing original species names
#'
#' @importFrom stringr str_squish
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
  if(any(is.na(cfgLookup)) || nrow(cfgLookup) == 0) {
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
#' `combine_datasets` compiles all the loaded studies into a single AusTraits
#' database object as a large list
#'
#' @param ... arguments passed to other functions
#' @param d list of all the AusTraits studies
#'
#' @return AusTraits compilation database as a large list
#' @importFrom rlang .data
#' @export
combine_datasets <- function(..., d=list(...)) {

  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # combine sources and remove duplicates
  sources <- d %>% lapply("[[", "sources")
  keys <- sources %>% lapply(names)  %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% purrr::reduce(c)
  sources <- sources[keys]

  trait_definitions <- d %>% lapply("[[", "trait_definitions") %>% purrr::reduce(c)
  trait_definitions <- trait_definitions[!duplicated(names(trait_definitions))]
  trait_definitions <- trait_definitions[sort(names(trait_definitions))]
  
  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "dataset_id")

  # taxonomy
  taxonomic_updates <-
    combine("taxonomic_updates", d) %>%
    dplyr::group_by(.data$original_name, .data$cleaned_name) %>%
    dplyr::mutate(dataset_id = paste(.data$dataset_id, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$original_name, .data$cleaned_name)

  traits <- combine("traits", d)

  ret <- list(traits = traits,
              sites = combine("sites", d),
              contexts = combine("contexts", d),
              methods = combine("methods", d),
              excluded_data = combine("excluded_data", d),
              taxonomic_updates = taxonomic_updates,
              taxa = taxonomic_updates %>% dplyr::select(taxon_name = .data$cleaned_name) %>% dplyr::distinct(),
              contributors = combine("contributors", d),
              sources = sources,
              trait_definitions = trait_definitions,
              schema = d[[1]][["schema"]],
              build_info = list(
                      session_info = utils::sessionInfo()
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
#' @importFrom dplyr contains
#' @importFrom rlang .data
#'
#' @export
update_taxonomy <- function(austraits_raw, taxa) {

  austraits_raw$taxonomic_updates <-
    austraits_raw$taxonomic_updates %>%
    dplyr::left_join(by = "cleaned_name",
              taxa %>% dplyr::select(.data$cleaned_name, .data$taxonIDClean, .data$taxonomicStatusClean,
                                      .data$alternativeTaxonomicStatusClean, .data$acceptedNameUsageID, .data$taxon_name)
              ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cleaned_name)

  austraits_raw$traits <-
    austraits_raw$traits %>%
    dplyr::rename(cleaned_name = .data$taxon_name) %>%
    dplyr::left_join(by = "cleaned_name",
              taxa %>% dplyr::select(.data$cleaned_name, .data$taxon_name)
              ) %>%
    dplyr::select(.data$dataset_id, .data$taxon_name, everything()) %>%
    dplyr::mutate(taxon_name = ifelse(is.na(.data$taxon_name), .data$cleaned_name, .data$taxon_name)) %>%
    dplyr::select(-.data$cleaned_name)

  species_tmp <-
    austraits_raw$traits %>%
    dplyr::select(.data$taxon_name) %>%
    dplyr::distinct() %>%
    dplyr::left_join(by = "taxon_name",
      taxa %>% dplyr::select(-contains("clean")) %>% dplyr::distinct()
    ) %>%
    # extract genus as this is useful
    dplyr::mutate(
      genus = .data$taxon_name  %>% stringr::str_split(" ") %>% map_chr(1),
      genus = ifelse(.data$genus %in% taxa$taxon_name, .data$genus, NA)
    )  %>%
    dplyr::arrange(.data$taxon_name) %>%
    dplyr::mutate(
      taxonomicStatus = ifelse(is.na(.data$taxonomicStatus) & !is.na(.data$genus), "genus_known", .data$taxonomicStatus),
      taxonomicStatus = ifelse(is.na(.data$taxonomicStatus) & is.na(.data$genus), "unknown", .data$taxonomicStatus),
    ) %>%
    split(.$taxonomicStatus)

  # retrieve families from list of known genera - prioritise genera with accepted names
   species_tmp[["genus_known"]] <-
     species_tmp[["genus_known"]] %>%
     dplyr::select(-.data$family) %>%
     dplyr::left_join(by="genus",
               taxa %>% dplyr::filter(.data$taxonRank == "Genus") %>%
                 dplyr::select(genus = .data$taxon_name, .data$family) %>% dplyr::distinct()
     )

  austraits_raw$taxa <-
    species_tmp %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$taxon_name)

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
  build_info <- utils::capture.output(print(austraits$build_info))
  writeLines(build_info, sprintf("%s/build_info.md", path))

  # Save definitions
  yaml::write_yaml(austraits[["trait_definitions"]], sprintf("%s/trait_definitions.yml", path))
  yaml::write_yaml(austraits[["schema"]], sprintf("%s/schema.yml", path))

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
  utils::zip(filename, basename(path))
  unlink(path, recursive = TRUE)

  message("Export created at ", export_dir)

}
