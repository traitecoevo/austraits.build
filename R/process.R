#' Configure AusTraits database object
#'
#' Creates the config object which gets passed onto `dataset_process`. The config list contains
#' the subset of definitions and unit conversions for those traits for a each study.
#' `dataset_configure` is used in the remake::make process to configure individual studies mapping the
#' individual traits found in that study along with any relevant unit conversions
#' and definitions. `dataset_configure` and `dataset_process` are applied to every study
#' in the remake.yml file
#'
#' @param filename_metadata metadata yaml file for a given study
#' @param definitions definitions read in from the traits.yml
#' @param unit_conversion_functions unit_conversion.csv file read in from the config folder
#'
#' @return list with dataset_id, metadata, definitions and unit_conversion_functions
#' @importFrom purrr map_chr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom lubridate dmy
#' @export
#'
#' @examples
#' \dontrun{
#' dataset_configure("data/Falster_2003/metadata.yml", read_yaml("config/traits.yml"),
#' get_unit_conversions("config/unit_conversions.csv"))
#' }
dataset_configure <- function(
  filename_metadata,
  definitions,
  unit_conversion_functions) {

  dataset_id <- basename(dirname(filename_metadata))

  # read metadata
  metadata <- read_metadata(filename_metadata)

  # table of trait_mapping
  trait_mapping <-
    metadata[["traits"]] %>%
    util_list_to_df2() %>%
    dplyr::filter(!is.na(.data$trait_name)) %>%
    # determine unit conversions
    dplyr::mutate(
      i = match(.data$trait_name, names(definitions$elements)),
      to = purrr::map_chr(.data$i, ~util_extract_list_element(.x, definitions$elements, "units")),
      conversion = process_unit_conversion_name(.data$unit_in, .data$to)
    )

  unit_conversion_functions_sub <-
    unit_conversion_functions[trait_mapping %>%
    dplyr::filter(.data$unit_in!=.data$to) %>% dplyr::pull(.data$conversion) %>% unique()]

  # subset of definitions
  definitions <-
    definitions$elements[names(definitions$elements) %in% trait_mapping$trait_name]

  list(dataset_id = dataset_id,
       metadata = metadata,
       definitions = definitions,
       unit_conversion_functions = unit_conversion_functions_sub) 
}

#' Load Dataset
#'
#' dataset_process is used to load individual studies using the config file generated
#' from `dataset_configure()`. `dataset_configure` and `dataset_process` are applied to every
#' study in the remake.yml file
#'
#' @param filename_data_raw raw data.csv file for any given study
#' @param config_for_dataset config settings generated from `dataset_configure()`
#' @param schema schema for austraits.build
#' @param filter_missing_values default filters missing values from the excluded data table. 
#' Change to false to see the rows with missing values.
#'
#' @return list, AusTraits database object
#' @export
#' @importFrom dplyr select mutate filter arrange distinct full_join everything any_of
#' @importFrom tidyr spread
#' @importFrom purrr reduce map_chr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' dataset_process("data/Falster_2003/data.csv", dataset_configure("data/Falster_2003/metadata.yml",
#' read_yaml("config/traits.yml"), get_unit_conversions("config/unit_conversions.csv")),
#' get_schema())
#' }
dataset_process <- function(filename_data_raw, 
                       config_for_dataset, 
                       schema,
                       resource_metadata,
                       filter_missing_values = TRUE){

  dataset_id <- config_for_dataset$dataset_id
  metadata <- config_for_dataset$metadata
  definitions <- config_for_dataset$definitions

  unit_conversion_functions <- config_for_dataset$unit_conversion_functions

  # load and process contextual data
  contexts <- 
    metadata$contexts %>% 
    process_format_contexts(dataset_id)
 
  # load and clean trait data
  traits <- 
    readr::read_csv(filename_data_raw, col_types = cols(), guess_max = 100000, progress=FALSE) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])() %>%
    process_parse_data(dataset_id, metadata, contexts)

  # context ids needed to continue processing
  context_ids <- traits$context_ids
  
  locations <-
    metadata$locations %>%
    process_format_locations(dataset_id, schema)

  traits <- 
    traits$traits %>%
    process_add_all_columns(
      c(names(schema[["austraits"]][["elements"]][["traits"]][["elements"]]),
        "parsing_id","location_name", "taxonomic_resolution")
    ) %>%
    process_flag_unsupported_traits(definitions) %>%
    process_flag_excluded_observations(metadata) %>%
    process_convert_units(definitions, unit_conversion_functions) %>%
    process_flag_unsupported_values(definitions) %>%
    process_create_observation_id() %>%
    process_taxonomic_updates(metadata) %>%
    # Sorting of data
    dplyr::mutate(
      # For cells with multiple values (separated by a space), sort these alphabetically
      value = ifelse(is.na(.data$error), util_separate_and_sort(.data$value), .data$value),
      value_type = factor(.data$value_type, levels = names(schema$value_type$values))
      ) %>%
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type) %>%
    # ensure everything converted to character type
    util_df_convert_character()

  # extract location data from metadata
  locations <-
    metadata$locations %>%
    process_format_locations(dataset_id, schema)

  # replace location_name with a location_id
  if( nrow(locations) > 0 ) {
    traits <- 
      traits %>% 
      dplyr::select(-.data$location_id) %>%
      dplyr::left_join(by = c("location_name"), locations %>% dplyr::select(.data$location_name, .data$location_id) %>% dplyr::distinct())
  }

  # Where missing, fill variables in traits table with values from locations
  if( nrow(locations) > 0 ) {
    vars <- c("basis_of_record", "life_stage", "collection_date", "measurement_remarks", "entity_type",
              "value_type", "basis_of_value", "replicates", "population_id", "individual_id")
    
    for( v in vars ) {
      # merge into traits from location level
      if( v %in% unique(locations$location_property) ) {
        traits_tmp <- traits %>%
          dplyr::left_join(by = "location_id",
                            locations %>% 
                            tidyr::pivot_wider(names_from = "location_property", values_from = "value") %>%
                            dplyr::select(.data$location_id, col_tmp = dplyr::any_of(v)) %>%
                            stats::na.omit()
                          )
      # Use location level value if present
      traits[[v]] <- ifelse(!is.na(traits_tmp[["col_tmp"]]), traits_tmp[["col_tmp"]], traits[[v]])
      }
    }

    # Remove any values included to map into traits table
    locations <- locations %>% dplyr::filter(!(.data$location_property %in% vars))
  }

  # record contributors
  contributors <- 
    metadata$contributors %>% 
    process_format_contributors(dataset_id, schema)

  # record sources
  sources <- metadata$source %>%
            lapply(util_list_to_bib) %>% purrr::reduce(c)

  # record methods
  methods <- process_format_methods(metadata, dataset_id, sources, contributors)
 
  # Retrieve taxonomic details for known species
  taxonomic_updates <-
    traits %>%
    dplyr::select(dataset_id, .data$original_name, cleaned_name = .data$taxon_name, taxonomic_resolution = .data$taxonomic_resolution) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cleaned_name)

  # ensure correct order of columns in traits table
  # at this point, need to retain `taxonomic_resolution`, because taxa table & taxonomic_updates not yet assembled.
  traits <-
    traits %>%
    dplyr::select(dplyr::all_of(c(names(schema[["austraits"]][["elements"]][["traits"]][["elements"]]), "error", "taxonomic_resolution")))

  # Remove missing values is specified
  if( filter_missing_values == TRUE ) {
    traits <- 
      traits %>% dplyr::filter(!(!is.na(.data$error) & (.data$error == "Missing value")))
  }

  # Todo - resource_metadata
  # - Add contributors

  # combine for final output
  list(
       traits     = traits %>% dplyr::filter(is.na(.data$error)) %>% dplyr::select(-.data$error),
       locations  = locations,
       contexts   = context_ids$contexts %>% dplyr::select(-.data$var_in, -.data$find),
       methods    = methods,
       excluded_data = traits %>% dplyr::filter(!is.na(.data$error)) %>% 
              dplyr::select(.data$error, everything()),
       taxonomic_updates = taxonomic_updates,
       taxa       = taxonomic_updates %>% dplyr::select(taxon_name = .data$cleaned_name) %>% dplyr::distinct(),
       contributors = contributors,
       sources    = sources,
       definitions = definitions,
       schema = schema,
       metadata = resource_metadata,
       build_info = list(session_info = utils::sessionInfo())
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
process_custom_code <- function(txt) {
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

#' Create entity id
#' 
#' Creates 3-part entity id codes that combine a segment for species, population, 
#' and, when applicable, individual
#' This depends upon a parsing_id being established when the data.csv file is first read in
#'
#' @param data the traits table at the point where this function is called 
#'
#' @return character string 
process_create_observation_id <- function(data) {
  
  # Create three IDs: population_id, individual_id, observation_id

  # Create population_id

  # `population_id`'s are numbers assigned to unique combinations of 
  #                location_name, treatment_id and plot_id
  # their purpose is to allow `population_level` measurements to be
  # easily mapped to individuals within the given population
    if(
      !all(is.na(data[["location_name"]]))|
      !all(is.na(data[["plot_id"]]))|
      !all(is.na(data[["treatment_id"]]))
        ) {
      data <- data %>% 
        dplyr::mutate(
          population_id = paste(.data$location_name, .data$plot_id, .data$treatment_id, sep = "")
        )
    } else {
      data <- data %>% 
        dplyr::mutate(
          population_id = NA_character_
        )
    }

  data <- data %>%
    dplyr::mutate(
              pop_id_segment = ifelse(
                (!is.na(.data$location_name)|!is.na(.data$treatment_id)|!is.na(.data$plot_id)) & .data$entity_type %in% c("individual", "population"),
                process_generate_id(.data$population_id, "", sort = TRUE), 
                NA),
              pop_id_segment = ifelse(is.na(.data$pop_id_segment)  & 
                                        .data$entity_type %in% c("individual", "population"), 
                                        "pop_unk", .data$pop_id_segment),
              population_id = .data$pop_id_segment
            )

  ## Create individual_id

  # For datasets where there are individual-level measurements
  # (i.e. entity_type = `individual`), the `parsing_id` values
  # that were created in the `process_parse_data` function are
  # `individual_id`s.

  # There are 3 circumstances:

  # 1. There is a `individual_id` column read in through metadata$data
  #     and `parsing_id` is equivalent to `individual_id`
  # 2. There is only a single observation for each individual,
  #     and therefore parsing_id values assigned based upon row number
  #     correctly identifies an individual. This includes instances where
  #     there is a `temporal context`, but different individuals were
  #     measured each time.
  # 3. There are multiple observations for each individual, but these are
  #     presented in the data.csv file as multiple columns and therefore
  #     row number correctly identifies an individual.

  # For datasets where an individual_id is not assigned via metadata$dataset
  if( all(is.na(data[["individual_id"]])) ) {

  # check which rows of data include individual-level measurements
  # (based on entity type)
    has_ind_value <-
      data %>%
        dplyr::filter(!is.na(.data$value)) %>%
        dplyr::group_by(.data$parsing_id) %>%
        dplyr::summarise(check_for_ind = any(stringr::str_detect(.data$entity_type, "individual"))) %>%
        dplyr::ungroup()

    # If yes, `individual_id` is copied from `parsing_id`
    # If no, `individual_id` is set to NA
    # This step is required so that for the final `individual_id`
    # only rows of data containing some individual-level data are numbered,
    # to avoid missing numbers in the `individual_id` sequence

    data <- 
      data %>% 
      dplyr::left_join(has_ind_value, by = "parsing_id") %>%
      dplyr::mutate(individual_id = ifelse(.data$check_for_ind == TRUE, .data$parsing_id, NA))
  }

  # create final individual_id within each species and population

  # (as identified by their segment numbers),
  # The function `process_generate_id` ensures that values with the same
  # parsing_id/individual_id are given the same value.
  data <- 
    data %>% 
    dplyr::group_by(.data$taxon_name, .data$population_id) %>%
    dplyr::mutate(
      ind_id_segment = ifelse(!is.na(.data$individual_id) & .data$entity_type == "individual", process_generate_id(.data$individual_id,""), NA),
      #individual_id = row_number(),
      ind_id_segment = ifelse(is.na(.data$ind_id_segment) & is.na(.data$entity_type), process_generate_id(.data$individual_id,"entity_unk"),
                              .data$ind_id_segment)
          ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(individual_id = .data$ind_id_segment, check_for_ind = NA)


  ## Create observation_id  for a collection of measurements made on an entity
  #  (where an entity can be an individual, population, or taxon)
  #   at a single point in time.
  
  i <- !is.na(data$value)
  data[i,] <- 
    data[i,] %>%
    dplyr::group_by(.data$dataset_id) %>%
    dplyr::mutate(
      observation_id = 
        paste(.data$taxon_name, .data$population_id, .data$individual_id, .data$temporal_id, .data$entity_type, sep="-") %>%  
        process_generate_id("", sort = TRUE)
    ) %>%
    dplyr::ungroup() 
  
  data %>%
    dplyr::select(-.data$check_for_ind)
}

#' Function to generate seuqnece of integer ids from vector of names
#' Determines number of 00s needed based on number of records
#' @param x vector of text to convert
#' @param prefix text to put before id integer
#' @param sort logical to indicate whether x should be sorted before ids are generated
#' @return vector of ids
process_generate_id <- function(x, prefix, sort = FALSE) {
  
  make_id_segment <- function(n, prefix) {
    sprintf(paste0("%s%0", max(2, ceiling(log10(n))), "d"), prefix, seq_len(n))
  }

  d <- x %>%
    unique() %>%
    subset(., !is.na(.))

  if (sort) d <- sort(d, na.last = TRUE)
  
  id <- make_id_segment(length(d), prefix)
  
  id[match(x, d)]
}


#' Format context data from list to tibble
#'
#' Format context data read in from the metadata.yml file. Converts from list to tibble.
#'
#' @param my_list list of input information
#' @param dataset_id XXX
#' @return tibble with context details if available
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' process_format_contexts(read_metadata("data/Apgaua_2017/metadata.yml")$context)
#' }
process_format_contexts <- function(my_list, dataset_id) {
   f <- function(x) {
     tibble::tibble(context_property = x$context_property, category = x$category, var_in = x$var_in, util_list_to_df2(x$values))
   }

   if ( !is.na(my_list[1]) ) {
     contexts <-
       my_list %>%
       purrr::map_df(f) %>%
       dplyr::mutate(dataset_id = dataset_id) %>%
       dplyr::select(
         .data$dataset_id, .data$context_property, .data$category, .data$var_in,
         dplyr::any_of(c("find", "value", "description"))
       )

     if(is.null(contexts[["description"]]) ) {
      contexts[["description"]] <- NA_character_
     }
     # keep values from find column if a replacement isn't specified
     if ( is.null(contexts[["find"]]) ) {
       contexts[["find"]] <- NA_character_
     } else {
       contexts[["find"]] <- ifelse(is.na(contexts$find), contexts$value, contexts$find)
     }
   } else {
     contexts <-
       tibble::tibble(dataset_id = character(), var_in = character())
   }
  
  contexts
}

process_create_context_ids <- function(data, contexts) {
  # select context_cols
  tmp <- contexts %>%
    dplyr::select(.data$context_property, .data$var_in) %>%
    dplyr::distinct()

  # Extract context columns
  context_cols <- data %>% dplyr::select(tmp$var_in)
  names(context_cols) <- tmp$context_property

  # Find and replace values for each context property
  for (v in unique(contexts$context_property)) {

    ## first filter to each property
    xx <- contexts %>%
      dplyr::filter(.data$context_property == v)

    ## only do if find column is present and has non NA values
    if (!is.null(xx[["find"]])) {
      xx <- dplyr::filter(xx, is.na(.data$find))
      if (nrow(xx) > 0) {
        ## create named vector
        xxx <- stats::setNames(xx$value, xx$value)
        ## use named vector for find and value
        context_cols[[v]] <- xxx[context_cols[[v]]]
      }
    }
  }

  # group_by category and create ids
  tmp <-
    contexts %>%
    #  dplyr::filter(category == v) %>%
    dplyr::select(.data$context_property, .data$category, .data$value) %>%
    dplyr::distinct()

  categories <- c("plot", "treatment", "entity_context", "temporal", "method") %>% subset(., . %in% tmp$category)

  ids <- dplyr::tibble(.rows = nrow(context_cols))

  id_link <- list()

  for (w in categories) {
    xx <- contexts %>%
      dplyr::filter(.data$category == w)

    vars <- unique(xx[["context_property"]])

    make_id <- function(x) {
      sprintf(paste0("%0", max(2, ceiling(log10(x)), na.rm = TRUE), "d"), x)
    }
    # Below, unite function turns NAs into text, we need to convert back
    # Need to modify structure of NA, depending on the number of
    # variables in vars so two NAs will be NA_NA
    # only treat rows where everything is NA as NA
    NAs <- paste(rep("NA", length(vars)), collapse = "_")

    xxx <-
      context_cols %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      tidyr::unite("combined", remove = FALSE) %>%
      dplyr::mutate(
        combined = ifelse(.data$combined == NAs, NA, .data$combined),  
        id = .data$combined %>%
          as.factor() %>% as.integer() %>% make_id()
      ) %>%
      dplyr::select(-.data$combined)

    ## store ids
    ids[[paste0(w, "_id")]] <- xxx[["id"]]

    ## create link values
    for (v in vars) {
      id_link[[v]] <-
        xxx %>%
        dplyr::select(dplyr::all_of(v), .data$id) %>%
        dplyr::filter(!is.na(.data$id)) %>%
        dplyr::distinct() %>%
        dplyr::rename(find = v) %>%
        util_df_convert_character() %>%
        dplyr::group_by(.data$find) %>%
        dplyr::summarise(
          context_property = v,
          category = w,
          link_id = paste0(w, "_id"),
          link_vals = paste(.data$id, collapse = ", ")
        )
    }
  }

  contexts_finished <-
    contexts %>%
    dplyr::mutate(find = ifelse(is.na(.data$find), as.character(.data$value), as.character(.data$find))) %>%
    dplyr::left_join(
      id_link %>% dplyr::bind_rows(),
      by = c("context_property", "category", "find")
    )

  list(
    contexts = contexts_finished %>% util_df_convert_character(),
    ids = ids %>% util_df_convert_character()
  )
}

#' Format location data from list to tibble
#'
#' Format location data read in from the metadata.yml file. Converts from list to tibble.
#'
#' @param my_list list of input information
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param schema XXX
#' 
#' @return tibble with location details if available
#' @importFrom rlang .data
#' @importFrom dplyr select mutate filter arrange distinct case_when full_join everything any_of bind_cols

#'
#' @examples
#' \dontrun{
#' process_format_locations(read_metadata("data/Falster_2003/metadata.yml")$locations, "Falster_2003")
#' }
process_format_locations <- function(my_list, dataset_id, schema) {

  # default, if length 1 then it's an "na"
  if ( length(unlist(my_list)) == 1 ) {
    return(tibble::tibble(dataset_id = character()))
  }

  out <-
    my_list %>%
    lapply(lapply, as.character) %>%
    purrr::map_df(util_list_to_df1, .id = "name") %>%
    dplyr::mutate(dataset_id = dataset_id) %>%
    dplyr::rename(location_property = "key", location_name = "name") %>%
    process_add_all_columns(names(schema[["austraits"]][["elements"]][["locations"]][["elements"]]), add_error_column = FALSE) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      location_id = process_generate_id(location_name, "", sort = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    # reorder so type, description come first, if present
    dplyr::mutate(
      i = dplyr::case_when(
        .data$location_property == "description" ~ 1, 
        .data$location_property == "latitude (deg)" ~ 2,
        .data$location_property == "longitude (deg)" ~ 3, 
        TRUE ~ 4)
    ) %>%
    dplyr::arrange(.data$location_id, .data$location_name, .data$i, .data$location_property) %>%
    dplyr::select(-.data$i)
  
  out
}

#' Flag any unrecognised traits
#'
#' Flag any unrecognised traits, as defined in the traits.yml file
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the traits.yml file in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with unrecognised traits flagged as "Unsupported trait" in the "error" column
process_flag_unsupported_traits <- function(data, definitions) {

  # create error column if not already present
  if(is.null(data[["error"]]))
    data[["error"]] <- NA_character_

  # exclude traits not in definitions
  i <- data$trait_name %in% names(definitions)
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
process_flag_excluded_observations <- function(data, metadata) {

  if(length(metadata$exclude_observations)==1 && is.na(metadata$exclude_observations)) return(data)

  fix <-
    metadata$exclude_observations %>%
    util_list_to_df2() %>%
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
#' `util_check_all_values_in` checks if values in vector x are in y. Values in x may
#' contain multiple values separated by `sep` so these are split first using `str_split`.
#'
#' @param x vector
#' @param y vector
#' @param sep amount of space separating values to be split, default = " " (a single space)
#'
#' @return vector of logical values
util_check_all_values_in <- function(x, y, sep=" "){
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
util_list_to_bib <- function(ref) {
  if(is.null(ref)) return(NULL)

  if(is.na(ref[1])) return(NULL)


  # Replace , with and to get correct handling of authors
  ref$author <- gsub(",", " and ", ref$author)

  # Ensures capitalisation of title retained as is
  if(!is.null(ref$title))
    ref$title = sprintf("{%s}", ref$title)

  RefManageR::as.BibEntry(ref)
}

#' Flag values outside of allowable range
#'
#' Flags any values that are outside the allowable range defined in the 
#' traits.yml file. NA values are flagged as errors.
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the traits.yml file in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with flagged values outside of allowable range, unsupported categorical
#' trait values or missing values
process_flag_unsupported_values <- function(data, definitions) {

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
    if(definitions[[trait]]$type == "categorical") {

      i <-  is.na(data[["error"]]) &
            data[["trait_name"]] == trait &
            !is.null(definitions[[trait]]$allowed_values_levels) &
            !util_check_all_values_in(data$value, names(definitions[[trait]]$allowed_values_levels))
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
    if(definitions[[trait]]$type == "numeric") {

      x <- suppressWarnings(as.numeric(data[["value"]]))
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait & is.na(x) &  !(data[["value_type"]] %in% c("range", "bin"))
      
      data <- data %>%
        dplyr::mutate(error = ifelse(i, "Value does not convert to numeric", .data$error))
     
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait & !(data[["value_type"]] %in% c("range", "bin")) &
        (x < definitions[[trait]]$allowed_values_min | x > definitions[[trait]]$allowed_values_max)

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
#' get_unit_conversions("config/unit_conversions.csv")
#' }
get_unit_conversions <- function(filename) {
  x <- read_csv(filename, col_types = cols(), progress=FALSE)

  # make functions from text
  fs <- lapply(x[["function"]], function(x) {
                                  my_f <- function(x) {}
                                  body(my_f) <- parse(text = x)
                                  my_f})
  names(fs) <- process_unit_conversion_name(x[["unit_from"]], x[["unit_to"]])
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
process_unit_conversion_name <- function(from, to) {
  sprintf("%s-%s", from, to)
}

#' Convert units to desired type
#'
#' @param data tibble or dataframe containing the study data
#' @param definitions definitions read in from the traits.yml file in the config folder
#' @param unit_conversion_functions unit_conversions.csv file stored in the config folder
#'
#' @importFrom rlang .data
#' @return tibble with converted units
process_convert_units <- function(data, definitions, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- data %>%
    dplyr::mutate(
      i = match(.data$trait_name, names(definitions)),
      to = util_extract_list_element(.data$i, definitions, "units"),
      ucn = process_unit_conversion_name(.data$unit, .data$to),
      type = util_extract_list_element(.data$i, definitions, "type"),
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
process_add_all_columns <- function(data, vars, add_error_column = TRUE) {

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
#' @param contexts dataframe of contexts for this study
#' @return tibble in long format with AusTraits formatted trait names, trait
#' substitutions and unique observation id added
#' @importFrom dplyr select mutate filter arrange distinct case_when full_join everything any_of bind_cols
#' @importFrom rlang .data
process_parse_data <- function(data, dataset_id, metadata, contexts) {

  # get config data for dataset
  data_is_long_format <- metadata[["dataset"]][["data_is_long_format"]]

  # Step 1a. create dataframe with data for vars that we want to keep, and set to correct names
  var_in <- unlist(metadata[["dataset"]])
  i <- var_in %in% names(data)

  df <- data %>%
        # next step selects and renames columns based on named vector
        dplyr::select(any_of(c(var_in[i], "entity_context_id", "plot_id", "treatment_id", "temporal_id", "method_id", contexts$var_in))) %>%
        dplyr::mutate(dataset_id = dataset_id)
  
  # Step 1b. import any values that aren't columns of data
  vars <- c( "entity_type", "value_type", "basis_of_value", 
            "replicates", "collection_date",
            "basis_of_record", "life_stage",
            "measurement_remarks", "source_id")

  df <-
    df %>%
    dplyr::bind_cols(
      metadata[["dataset"]][names(metadata[["dataset"]]) %in% vars[!vars %in% names(df)]] %>% tibble::as_tibble()
    )

  # MAKE PARSING_ID

  # This section makes a temporary parsing_id
  # that will be replaced by a collection of id's in the final traits table.

  # It is required here to correctly connect rows of data as being collected
  # on the same individual or are part of the same observation

    if(!data_is_long_format) {

  # If an `individual_id` column  IS read in through metadata$dataset,
  # it is used to correctly cluster and identify individuals.

  # For a file where `individual_id` is specified in the metadata, 
  # if there are rows of data where the `individual_id`` is `NA`,
  # these are filled in with the row_number(),
  # just as would occur if no `individual_id` column is specified.
    
      if(!is.null(df[["individual_id"]])) {
        df[["parsing_id"]] <- df[["individual_id"]]
        
        prefix <- dataset_id

        df <- df %>%
                  dplyr::mutate(
                    parsing_id = ifelse(is.na(.data$parsing_id),
                                paste("temp", dplyr::row_number(), sep = "_"),.data$parsing_id) %>%
                                as.character() %>% 
                                process_generate_id(prefix)
                  )
      
      # If an `individual_id` column  IS NOT read in through metadata$dataset, row_numbers are assumed to represent unique `entities`
      # and `parsing_id` values are based on row numbers
                
      } else {

        df <- df %>%
                  dplyr::mutate(
                          row_numbers = dplyr::row_number(),
                          parsing_id = process_generate_id(row_numbers, dataset_id)
                        )
      }

  } else {

    # For long datasets, create unique identifier from taxon_name, location, and individual_id (if specified)
    
    df[["parsing_id_tmp"]] <- gsub(" ", "-", df[["taxon_name"]])

    if(!is.null(df[["location_name"]][1]))
      df[["parsing_id_tmp"]] <- paste0(df[["parsing_id_tmp"]],"_", df[["location_name"]])

    if(!is.null(df[["individual_id"]])) {
      df[["parsing_id_tmp"]] <- paste0(df[["parsing_id_tmp"]],"_", df[["individual_id"]])
    }
    
    prefix <- dataset_id
    
    df <- df %>%
      dplyr::mutate(
                parsing_id = parsing_id_tmp %>% as.character() %>% 
                process_generate_id(prefix)
                    ) %>%
              dplyr::select(-.data$parsing_id_tmp)
  }


  df <- df %>%
            dplyr::mutate(
              parsing_id = as.character(.data$parsing_id)
            )


  # Step 2. Add trait information, with correct names
  traits_table <-
    metadata[["traits"]] %>%
    util_list_to_df2() %>%
    dplyr::filter(!is.na(.data$trait_name))  # remove any rows without a matching trait record

  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  if (data_is_long_format == FALSE & any(! traits_table[["var_in"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(traits_table[["var_in"]], colnames(data))))
  }

  vars_traits <- c(vars, contexts$var_in)    

  ## if needed, change from wide to long format
  if (!data_is_long_format) {

    # if the dataset is `wide` then process each variable in turn, to create the `long` dataset -
    # say the original dataset has 20 rows of data and 5 traits, then we will end up with 100 rows

    out <- list()
    for (i in seq_len(nrow(traits_table))) {
      # create a temporary dataframe which is a copy of df
      # df is our data frame containing all the columns we want EXCEPT for the trait data itself
      out[[i]] <- df
      # to x we append columns of data for trait_name, unit and value (the latter is retrieved from the data)
      out[[i]][["trait_name"]] <- traits_table[["var_in"]][i]
      out[[i]][["value"]] <- data[[traits_table[["var_in"]][i]]] %>% as.character()

      # Pull in additional information for each trait as specified in traits part of metadata, here represented as traits_table
      # Values in table can specify a column in the original data OR a value to use

      vars_to_check <- vars_traits[vars_traits %in% names(traits_table)]
      # For each column in traits_table
      for(v in vars_to_check) {
        # get value
        value <- traits_table[i,v, drop=TRUE]
        # Check if it is a column in data or not and process accordingly

        # Question: Why can't `entity_type`, `basis_of_value` come in as column of data?

        if(!is.na(value)) {
          if(!is.null(data[[value]]) && !(v %in% c("entity_type", "basis_of_value")) ) {
            out[[i]][[v]] <- data[[value]] %>% as.character()
          } else {
            out[[i]][[v]] <- value %>% as.character()
          }
        }
      }
    }
    out <- dplyr::bind_rows(out)
  } else {

    out <- df %>% dplyr::filter(.data$trait_name %in% traits_table$var_in)
    out[["value"]] <- out[["value"]] %>%  as.character()

    # Pull in additional information for each trait as specified in traits part of metadata, here represented as traits_table
    # (column option not implemented) Values in table can specify a column in the original data OR a value to use

    vars_to_check <- vars_traits[vars_traits %in% names(traits_table)]
    # For each column in traits_table
    for (i in seq_len(nrow(traits_table))) {
      for(v in vars_to_check) {
        value <- traits_table[i,v, drop=TRUE]
        if(!is.na(value)) {
          out[[v]][out$trait_name == traits_table[["var_in"]][i]] <- value
        }
      }
    }
  }

  # Ensure all lower case
  out[["value"]] <- tolower(out[["value"]])

  # Now create context ids
  if (nrow(contexts) == 0) {
    context_ids <- 
      list(
        contexts = contexts,
        ids = tibble::tibble()
        )
    context_ids$contexts <- context_ids$contexts %>%
      dplyr::mutate(
        context_property = NA_character_,
        category = NA_character_,
        find = NA_character_,
        value = NA_character_,
        description = NA_character_,
        link_id = NA_character_,
        link_vals = NA_character_
      )

# XXXX Why doesn't this work? process_add_all_columns(names(schema[["austraits"]][["elements"]][["contexts"]][["elements"]]))      
      
  } else {
    context_ids <- process_create_context_ids(out, contexts)

    out <- 
      dplyr::bind_cols(out, context_ids$ids) %>% 
      dplyr::select(-any_of(unique(contexts$var_in)))
  }

  # Now process any name changes as per metadata[["traits"]]
  out[["unit"]] <- NA_character_
  i <- match(out[["trait_name"]], traits_table[["var_in"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    out[["unit"]][j] <- traits_table[["unit_in"]][i[j]]
    out[["trait_name"]][j] <- traits_table[["trait_name"]][i[j]]
  }

  # Implement any value changes as per substitutions
  if(!is.na(metadata[["substitutions"]][1])) {
    substitutions_table <-  util_list_to_df2(metadata[["substitutions"]]) %>%
      dplyr::mutate(
             find = tolower(.data$find),
             replace = tolower(.data$replace)
             )

    for(i in seq_len(nrow(substitutions_table))) {
      j <- which(out[["trait_name"]] == substitutions_table[["trait_name"]][i] &
                  out[["value"]] == substitutions_table[["find"]][i])
      if( length(j) > 0 ) {
        out[["value"]][j] <- substitutions_table[["replace"]][i]
      }
    }
  }
  
  list(
    traits = out,
    context_ids = context_ids
  )
}


#' Format contributors from list into tibble
#'
#' Format contributors, read in from the metadata.yml file. Converts from list to tibble.
#'
#' @param my_list list of input information
#' @param dataset_id XXX
#' @param schema XXX
#'
#' @return tibble with details of contributors
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' process_format_contributors(read_metadata("data/Falster_2003/metadata.yml")$contributors)
#' }
process_format_contributors <- function(my_list, dataset_id, schema) {

    if (length(unlist(my_list$data_collectors)) > 1) {
      contributors <-
        my_list$data_collectors %>%
        util_list_to_df2() %>%
        dplyr::mutate(dataset_id = dataset_id) %>%
        dplyr::filter(!is.na(.data$last_name))
    } else {
      contributors <- tibble::tibble(dataset_id = character())
    }

    contributors <-
      contributors %>%
      process_add_all_columns(names(schema[["austraits"]][["elements"]][["contributors"]][["elements"]]), add_error_column = FALSE)
  
  contributors
}

process_format_methods <- function(metadata, dataset_id, sources, contributors) {

  # identify sources as being `primary`, `secondary` or `original`
  # secondary datasets are additional publications associated with the primary citation
  # original dataset keys are used for compilations indicating the original data sources
  citation_types <- 
    tibble::tibble(
      source_key = names(metadata$source),
      type = str_replace_all(.data$source_key, "_[:digit:]+", ""),
      source_id = metadata$source %>%
        util_list_to_df2() %>%
        purrr::pluck("key")
    )

  source_primary_key <- metadata$source$primary$key
  source_secondary_keys <- citation_types %>%
    dplyr::filter(.data$type == "secondary") %>%
    purrr::pluck("source_id")

  source_original_dataset_keys <- citation_types %>% dplyr::filter(.data$type == "original") %>% purrr::pluck("source_id")
  
  # combine collectors to add into the methods table
  collectors_tmp <-
    stringr::str_c(contributors$given_name, " ",
                   contributors$last_name,
                   ifelse(!is.na(contributors$additional_role),
                          paste0(" (", contributors$additional_role, ")"),
                          ""))  %>% paste(collapse = ", ")

  methods <-
    dplyr::full_join( by = "dataset_id",
      # methods used to collect each trait
      metadata[["traits"]] %>%
        util_list_to_df2() %>%
        dplyr::filter(!is.na(.data$trait_name)) %>%
        dplyr::mutate(dataset_id = dataset_id) %>%
        dplyr::select(dataset_id, .data$trait_name, .data$methods)
      ,
      # study methods
      metadata$dataset %>%
        util_list_to_df1() %>%
        tidyr::spread(.data$key, .data$value) %>%
        dplyr::select(dplyr::any_of(names(metadata$dataset))) %>%
        dplyr::mutate(dataset_id = dataset_id) %>%
        dplyr::select(-dplyr::any_of(c("original_file", "notes", "data_is_long_format", "taxon_name", 
                                         "trait_name", "population_id", "individual_id",
                                         "location_name", "source_id", "value", "entity_type", 
                                         "collection_date", "custom_R_code", "replicates", "measurement_remarks",
                                         "taxon_name", "basis_of_value", "basis_of_record", "life_stage")))
      )  %>%
      full_join( by = "dataset_id",
      # references
        tibble::tibble(
          dataset_id = dataset_id,
          source_primary_key = source_primary_key,
          source_primary_citation = bib_print(sources[[source_primary_key]]),
          source_secondary_key = source_secondary_keys %>% paste(collapse = "; "),
          source_secondary_citation = ifelse(length(source_secondary_keys) == 0, NA_character_,
            purrr::map_chr(source_secondary_keys, ~sources[[.x]] %>% bib_print) %>% paste(collapse = "; ") %>%
              stringr::str_replace_all("\\.;", ";")
            ),                    
          source_original_dataset_key = source_original_dataset_keys %>% paste(collapse = "; "),
          source_original_dataset_citation = ifelse(length(source_original_dataset_keys) == 0, NA_character_,
            purrr::map_chr(source_original_dataset_keys, ~sources[[.x]] %>% bib_print) %>% paste(collapse = "; ") %>%
            stringr::str_replace_all("\\.;", ";")
          )
        )
      ) %>%
    dplyr::mutate(
      data_collectors = collectors_tmp,
      assistants = ifelse(is.null(metadata$contributors$assistants), NA_character_,
                                      metadata$contributors$assistants),
      austraits_curators = metadata$contributors$austraits_curators
      )

  methods
}

#' Standardise species names
#'
#' Enforces some standards on species names
#'
#' @param x vector, dataframe or list containing original species names
#'
#' @importFrom stringr str_squish
#' @return vector with standardised species names
process_standardise_names <- function(x) {

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
    stringr::str_squish()

}

#' Apply taxonomic updates
#'
#' Applies taxonomic updates to the study data from the metadata.yml file
#'
#' @param data tibble or dataframe containing the study data
#' @param metadata yaml file containing the metadata
#'
#' @return tibble with the taxonomic updates applied
process_taxonomic_updates  <- function(data, metadata){
  out <- data

  # copy original species name to a new column
  out[["original_name"]] <- out[["taxon_name"]]
  # create a column for `taxnomic_resolution` 
  out[["taxonomic_resolution"]] <- NA_character_
 
  # Now make any replacements specified in metadata yaml
  ## Read metadata table, quit if empty
  substitutions_table <-  util_list_to_df2(metadata[["taxonomic_updates"]])

  if(any(is.na(substitutions_table[1])) || nrow(substitutions_table) == 0) {
    return(out)
  }

  # if the substitutions table exists, but there is no taxonomic resolution specified, create a column
  if(is.null(substitutions_table[["taxonomic_resolution"]])) {
    substitutions_table[["taxonomic_resolution"]] <- NA
  }
  
  to_update <- rep(TRUE, nrow(out))

  ## Makes replacements, row by row for both taxon_name and adds taxonomic_resolution for each name
  for(i in seq_len(nrow(substitutions_table))) {

    j <- which(out[["taxon_name"]] == substitutions_table[["find"]][i])
    if( length(j) > 0 ){
      out[["taxon_name"]][j] <- substitutions_table[["replace"]][i]
      out[["taxonomic_resolution"]][j] <- substitutions_table[["taxonomic_resolution"]][i]
      to_update[j] <- FALSE
    }
  }

  # for any that haven't been updated, run script to standardize names
  out[["taxon_name"]][to_update] <- process_standardise_names(out[["taxon_name"]][to_update])

  ## Return updated table
  out
  
}

#' Combine all the AusTraits studies into the compiled AusTraits database
#'
#' `build_combine` compiles all the loaded studies into a single AusTraits
#' database object as a large list
#'
#' @param ... arguments passed to other functions
#' @param d list of all the AusTraits studies
#'
#' @return AusTraits compilation database as a large list
#' @importFrom rlang .data
#' @export
build_combine <- function(..., d=list(...)) {

  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # combine sources and remove duplicates
  sources <- d %>% lapply("[[", "sources")
  keys <- sources %>% lapply(names)  %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% purrr::reduce(c)
  sources <- sources[keys]

  definitions <- d %>% lapply("[[", "definitions") %>% purrr::reduce(c)
  definitions <- definitions[!duplicated(names(definitions))]
  definitions <- definitions[sort(names(definitions))]
  
  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "dataset_id")

  # taxonomy
  taxonomic_updates <-
    combine("taxonomic_updates", d) %>%
    dplyr::group_by(.data$original_name, .data$taxon_name, .data$taxonomic_resolution) %>%
    dplyr::mutate(dataset_id = paste(.data$dataset_id, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$original_name, .data$taxon_name, .data$taxonomic_resolution)
  
  # metadata
  contributors <- combine("contributors", d)
  metadata <- d[[1]][["metadata"]]
  
  metadata[["contributors"]] <-
    contributors %>% dplyr::select(-dplyr::any_of(c("dataset_id", "additional_role"))) %>% distinct() %>% arrange(.data$last_name, .data$given_name) %>% util_df_to_list()
  
  ret <- list(traits = combine("traits", d),
              locations = combine("locations", d),
              contexts = combine("contexts", d),
              methods = combine("methods", d),
              excluded_data = combine("excluded_data", d),
              taxonomic_updates = taxonomic_updates,
              taxa = combine("taxa", d) %>% dplyr::distinct() %>% dplyr::arrange(taxon_name),
              contributors = contributors,
              sources = sources,
              definitions = definitions,
              schema = d[[1]][["schema"]],
              metadata = metadata,
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
build_update_taxonomy <- function(austraits_raw, taxa) {

  austraits_raw$taxonomic_updates <-
    austraits_raw$taxonomic_updates %>%
    dplyr::left_join(by = "cleaned_name",
              taxa %>% dplyr::select(.data$cleaned_name, .data$cleaned_scientific_name_id, .data$cleaned_name_taxonomic_status,
                                      .data$cleaned_name_alternative_taxonomic_status, .data$taxon_id, .data$taxon_name, .data$taxon_rank)
              ) %>%
    dplyr::mutate(
      taxonomic_resolution = ifelse(!is.na(.data$taxonomic_resolution) & .data$taxonomic_resolution != .data$taxon_rank, .data$taxon_rank, .data$taxonomic_resolution),
      taxonomic_resolution = ifelse(is.na(.data$taxonomic_resolution), .data$taxon_rank, .data$taxonomic_resolution)
    ) %>%
    dplyr::distinct() %>%
    dplyr::select(-.data$taxon_rank) %>%
    dplyr::arrange(.data$cleaned_name)


  austraits_raw$traits <-
    austraits_raw$traits %>%
    dplyr::rename(cleaned_name = .data$taxon_name) %>%
    dplyr::left_join(by = "cleaned_name",
              taxa %>% dplyr::select(.data$cleaned_name, .data$taxon_name, .data$taxon_rank)
              ) %>%
    dplyr::select(.data$dataset_id, .data$taxon_name, dplyr::everything()) %>%
    dplyr::mutate(
      taxon_name = ifelse(is.na(.data$taxon_name), .data$cleaned_name, .data$taxon_name),
      taxon_name = ifelse(stringr::str_detect(.data$cleaned_name, "\\["), .data$cleaned_name, .data$taxon_name)
    ) %>%
    dplyr::select(-.data$cleaned_name)
  
# names, identifiers for all genera
  genera_tmp <- taxa %>% 
    dplyr::filter(.data$taxon_rank %in% c("Genus", "genus")) %>%
    dplyr::select(name_to_match_to = .data$taxon_name, .data$family, taxonomic_reference_genus = .data$taxonomic_reference, taxon_id_genus = .data$taxon_id,
                  scientific_name_id_genus = .data$scientific_name_id, taxonomic_status_genus = .data$taxonomic_status) %>%
    dplyr::distinct()
  
# names, identifiers for all families in APC
  families_tmp <- taxa %>% 
    dplyr::filter(.data$taxon_rank %in% c("Familia", "family")) %>%
    dplyr::select(name_to_match_to = .data$taxon_name, taxonomic_reference_family = .data$taxonomic_reference, taxon_id_family = .data$taxon_id,
    scientific_name_id_family = .data$scientific_name_id, taxonomic_status_family = .data$taxonomic_status) %>%
    dplyr::distinct()
  
### Fill in columns for trinomial, binomial, genus, and family, as appropriate 
  # and match additional taxon information to the most specific name 

  species_tmp <-
    austraits_raw$traits %>%
    dplyr::select(.data$taxon_name, .data$taxonomic_resolution) %>%
    dplyr::distinct() %>% 
    util_df_convert_character() %>%
    dplyr::left_join(by = "taxon_name",
                     taxa %>% dplyr::select(.data$taxon_name, .data$taxon_rank, .data$family) %>% dplyr::distinct() %>% util_df_convert_character()
    ) 


  species_tmp <- species_tmp %>%  
    dplyr::mutate(
      # if no taxonomic resolution is specified, then the name's taxonomic resolution is the taxon_rank for the taxon name
      taxonomic_resolution = ifelse(.data$taxon_name %in% taxa$cleaned_name, taxa$taxon_rank[match(.data$taxon_name, taxa$cleaned_name)], taxonomic_resolution),
      taxon_rank = ifelse(!is.na(.data$taxon_rank), .data$taxonomic_resolution, .data$taxon_rank),
      # field trinomial is only filled in if taxonomic resolution is an infraspecific name 
      trinomial = ifelse(.data$taxon_rank %in% c("Subspecies", "Forma", "Varietas"),             
                        stringr::str_split_fixed(.data$taxon_name, "\\[",2)[,1] %>% stringr::str_trim(), NA),
      # field binomial is filled in if taxonomic resolution is an infraspecific name or a binomial
      # all taxon names that have "extra" information (beyond the actual name) have been formatted to have that information in square brackets '[]',
      # so these can be used as a delimitor to extract the actual name
      binomial = ifelse(.data$taxon_rank %in% c("Species"), 
                        stringr::str_split_fixed(.data$taxon_name, "\\[",2)[,1] %>% stringr::str_trim(), NA),
      binomial = ifelse(.data$taxon_rank %in% c("Subspecies", "Forma", "Varietas", "Series"), 
                        stringr::word(.data$taxon_name, start = 1, end = 2), .data$binomial),
      binomial = stringr::str_trim(.data$binomial),
      # genus filled in for all names that have a taxonomic of genus or more detailed
      genus = ifelse(!.data$taxon_rank %in% c("Familia", "family"), ifelse(stringr::word(.data$taxon_name, 1) == "x", stringr::word(.data$taxon_name, start = 1, end = 2), stringr::word(.data$taxon_name, 1)), NA),
      family = ifelse(.data$taxon_rank %in% c("Familia", "family"), stringr::word(.data$taxon_name, 1), .data$family),
      # identify which name is to be matched to the various identifiers, distribution information, etc. in the taxa file
      name_to_match_to = ifelse(.data$taxon_rank %in% c("Subspecies", "Forma", "Varietas"), .data$trinomial, NA),
      name_to_match_to = ifelse(is.na(.data$name_to_match_to) & .data$taxon_rank %in% c("Species"), .data$binomial, .data$name_to_match_to),
      name_to_match_to = ifelse(is.na(.data$name_to_match_to) & .data$taxon_rank %in% c("genus", "Genus"), .data$genus, .data$name_to_match_to),
      name_to_match_to = ifelse(is.na(.data$name_to_match_to) & is.na(.data$taxon_rank), .data$genus, .data$name_to_match_to),
      name_to_match_to = ifelse(is.na(.data$name_to_match_to) & .data$taxon_rank %in% c("family", "Familia"), .data$family, .data$name_to_match_to)
      ) %>% 
      # remove family, taxon_rank; they are about to be merged back in, but matches will now be possible to more rows
      select(-.data$taxon_rank, - .data$taxonomic_resolution) %>%
      rename(family_tmp = .data$family) %>%
      util_df_convert_character() %>%
      # merge in all data from taxa 
      dplyr::left_join(by = c("name_to_match_to" = "taxon_name"),
        taxa %>% dplyr::select(-dplyr::contains("clean")) %>% dplyr::distinct() %>% util_df_convert_character()
      ) %>% 
      dplyr::arrange(.data$taxon_name) %>%
      # merge in identifiers for genera & families
      dplyr::left_join(by = c("name_to_match_to", "family"), genera_tmp) %>% 
      dplyr::left_join(by = "name_to_match_to", families_tmp) %>% 
      dplyr::mutate(
        taxonomic_status = ifelse(.data$taxon_rank %in% c("Genus", "genus"), .data$taxonomic_status_genus, .data$taxonomic_status),
        taxonomic_status = ifelse(.data$taxon_rank %in% c("Familia", "family"), .data$taxonomic_status_family, .data$taxonomic_status),
        taxonomic_reference = ifelse(.data$taxon_rank %in% c("Genus", "genus"), .data$taxonomic_reference_genus, .data$taxonomic_reference),
        taxonomic_reference = ifelse(.data$taxon_rank %in% c("Familia", "family"), .data$taxonomic_reference_family, .data$taxonomic_reference),
        taxon_id = ifelse(.data$taxon_rank %in% c("Genus", "genus"), .data$taxon_id_genus, .data$taxon_id),
        taxon_id = ifelse(.data$taxon_rank %in% c("Familia", "family"), .data$taxon_id_family, .data$taxon_id),
        scientific_name_id = ifelse(.data$taxon_rank %in% c("Genus", "genus"), .data$scientific_name_id_genus, .data$scientific_name_id),
        scientific_name_id = ifelse(.data$taxon_rank %in% c("Familia", "family"), .data$scientific_name_id_family, .data$scientific_name_id),
        taxon_distribution = ifelse(.data$taxon_rank %in% c("Familia", "family", "Genus", "genus"), NA, .data$taxon_distribution),
        establishment_means = ifelse(.data$taxon_rank %in% c("Familia", "family", "Genus", "genus"), NA, .data$establishment_means)
      ) %>% 
      dplyr::select(-.data$taxon_id_genus, -.data$taxon_id_family, -.data$scientific_name_id_genus, -.data$scientific_name_id_family, 
                    -.data$taxonomic_status_genus, -.data$taxonomic_status_family, -.data$taxonomic_reference_genus, -.data$taxonomic_reference_family,
                    -.data$name_to_match_to, -.data$family_tmp) %>%
      dplyr::select(.data$taxon_name, .data$taxonomic_reference, .data$taxon_rank, .data$trinomial, .data$binomial, 
                    .data$genus, .data$family, .data$taxon_distribution, .data$establishment_means, 
                    .data$taxonomic_status, .data$scientific_name, .data$scientific_name_authorship, .data$taxon_id, 
                    .data$scientific_name_id)

  austraits_raw$taxa <-
    species_tmp %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$taxon_name) %>%
    dplyr::distinct(.data$taxon_name, .keep_all = TRUE)

  # only now, at the very end, can `taxonomic_resolution` be removed from the traits table

  austraits_raw$traits <-
    austraits_raw$traits %>%
      dplyr::select(-.data$taxonomic_resolution, -.data$taxon_rank)

  austraits_raw$excluded_data <-
    austraits_raw$excluded_data %>%
      dplyr::select(-.data$taxonomic_resolution)

  austraits_raw  
}

#' Add version information to AusTraits
#'
#' @param austraits AusTraits database object
#' @param version version number
#' @param git_sha Git SHA
#'
#' @return AusTraits database object with version information added
#' @export
build_add_version <- function(austraits, version, git_sha) {

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
#' @return csv files of tibbles containing traits, locations, contexts, methods, excluded_data,
#' taxonomic updates, taxa, contributors
#' @export
write_plaintext <- function(austraits, path) {

  unlink(path, TRUE)
  dir.create(path, FALSE, TRUE)

  # Capture information on build into text
  build_info <- utils::capture.output(print(austraits$build_info))
  writeLines(build_info, sprintf("%s/build_info.md", path))

  # Save definitions
  for(v in c("definitions", "schema", "metadata")) {
    yaml::write_yaml(austraits[[v]], sprintf("%s/%s.yml", path, v))
  }
  # Save references
  RefManageR::WriteBib(austraits$sources, sprintf("%s/sources", path))

  # Save tables
  for (v in c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", "taxa", "contributors")) {
    readr::write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  }
}
