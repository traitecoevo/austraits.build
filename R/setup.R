#' Path to the `metadata.yml` file for specified `dataset_id`
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#'
#' @return A string
metadata_path_dataset_id <- function(dataset_id) {
  file.path("data", dataset_id, "metadata.yml")
}

#' Create a template of file `metadata.yml` for specified `dataset_id`
#'
#' Includes place-holders for major sections of the metadata
#' 
#' @inheritParams metadata_path_dataset_id
#' @param path location of file where output is saved
#' @param skip_manual allows skipping of manual selection of variables, default = FALSE
#' 
#' @importFrom readr read_csv
#' @importFrom utils menu
#' @return a yml file template for metadata
#' @export
metadata_create_template <- function(dataset_id, 
                                     path = file.path("data", dataset_id),
                                     skip_manual = FALSE
                                     ) {
  
  `%notin%` <- Negate(`%in%`)
  fields <- c("source", "contributors", "dataset")
  exclude <- c("description", "type")
  articles <- c("key", "bibtype", "year", "author", "title", "journal", "volume", "number", "pages", "doi")
  
  out <- get_schema()$metadata$elements

  out[names(out) %notin% fields] <- NA
  out$source <- out$source$values["primary"]
  out$source$primary <- out$source$primary$values[articles]
  out$source$primary[] <- "unknown"
  out$source$primary["key"] = dataset_id
  out$source$primary["bibtype"] = "Article"
  
  out$contributors <- out$contributors$elements
  out$contributors$data_collectors[c(exclude, "notes")] <- NULL
  out$contributors$data_collectors[] <- "unknown"
  out$contributors$data_collectors <- list(out$contributors$data_collectors)
  out$contributors[c("assistants", "austraits_curators")] <- "unknown"

  out$dataset <- out$dataset$values[c("data_is_long_format", "custom_R_code", "collection_date", "taxon_name", "location_name",
                                                     "description", "basis_of_record", "life_stage", "sampling_strategy", "original_file", "notes")]
  out$dataset[] <- 'unknown'
  out$dataset$custom_R_code <- NA
  
  if(skip_manual == FALSE){
    
    # Check format of data
    tmp <- menu(c("Long", "Wide"), title="Is the data long or wide format?")
    data_is_long_format <- ifelse(tmp == 1, TRUE, FALSE)
    
    out$dataset$data_is_long_format <- data_is_long_format

    data <- readr::read_csv(paste0(path, "/data.csv"), col_types = cols())
    
    # Setup config and select columns as appropriate
    if(data_is_long_format) {
      v1 <- c("taxon_name", "trait_name", "value")
    } else{
      out$dataset[c("trait_name", "value")] <- NULL
      v1 <- c("taxon_name")
    }
    
    for(v in v1) {      
      out[["dataset"]][[v]] <- metadata_user_select_column(v, names(data))
    }
    
     v2 <- c("location_name", "individual_id", "collection_date")

    for(v in v2) {
      tmp <- metadata_user_select_column(v, c(NA, names(data)))
      if(!is.na(tmp)) {
        out[["dataset"]][[v]] <- tmp
      }
      if(v == "collection_date" & is.na(tmp)){
        collection_date <- readline(prompt="Enter collection_date range in format '2007/2009': ")
        out[["dataset"]][[v]] <- collection_date
      }
    }

    if(data_is_long_format) {
    
    }

  }

  #reorder elements in dataset
  order <- c("data_is_long_format","custom_R_code","collection_date", "taxon_name","trait_name","value","location_name","individual_id",
             "description","basis_of_record", "life_stage", "sampling_strategy", "original_file", "notes")
  
  order <- order[which(order %in% names(out[["dataset"]]))]
  
  out[["dataset"]] <- out[["dataset"]][order]
  
  write_metadata(out, paste0(path, "/metadata.yml"))
}

#' Select column by user
#' 
#' `metadata_user_select_column` is used to select which columns in a dataframe/ tibble 
#' corresponds to the variable of interest. It is used compile the metadata yaml
#' file by prompting the user to choose the relevant columns. It is used in 
#' `metadata_add_locations` and `metadata_add_contexts` and `metadata_create_template`
#'
#' @param column name of the variable of interest
#' @param choices the options that can be selected from
#'
metadata_user_select_column <- function(column, choices) {
  
  tmp <- utils::menu(choices, title= sprintf("Select column for `%s`", column))

  choices[tmp]
}

#' Select variable names by user
#' 
#' `user_select names` is used to prompt the user to select the variables that 
#' are relevant for compiling the metadata yaml file. It is currently used for
#' `metadata_add_traits`, `metadata_add_locations` and `metadata_add_contexts` 
#'
#' @param title character string providing the instruction for the user
#' @param vars variable names
#'
metadata_user_select_names <- function(title, vars){

  txt <- sprintf("%s (by number separated by space; e.g. '1 2 4'):\n%s\n", title, paste(sprintf("%d: %s", seq_len(length(vars)), vars), collapse="\n"))

  success <- FALSE
  while(!success) {
    cat(txt)
    i <- readline("\nSelection: ")
    i <- strsplit(i, " ")[[1]] %>% as.integer()

    if(all(i %in% seq_len(length(vars)))) {
      success <- TRUE
    } else {
      cat("Invalid selection, please try again\n\n")
    }
  }
  vars[i]
}

#' Check the output of running `custom_R_code` specified in 
#' the metadata for specified `dataset_id`
#'
#' Function to check the output of running `custom_R_code` specified in 
#' the `metadata.yml` file for specified `dataset_id`. 
#' For the specified `dataset_id`, reads in the file `data.csv` and 
#' applies manipulations as described in the file `metadata.yml`
#'
#' @inheritParams metadata_path_dataset_id
#'
#' @export
metadata_check_custom_R_code <- function(dataset_id) {

  # read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # load and clean trait data
  readr::read_csv(file.path("data", dataset_id,  "data.csv"), col_types = cols(), guess_max = 100000) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()
}

#' For specified `dataset_id`, populate columns for traits into metadata
#'
#' This functions asks users which traits they would like to keep, and adds a template 
#' for those traits in the metadata. This template must then be finished manually.
#' 
#' Can also be used to add a trait to an existing metadata file
#'
#' @inheritParams metadata_path_dataset_id
#' 
#' @importFrom rlang .data
#' @export
metadata_add_traits <- function(dataset_id) {

  # read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # load and clean trait data
  data <- readr::read_csv(file.path("data", dataset_id,  "data.csv"), col_types = cols()) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

  # Get list of potential traits
  if(!metadata$dataset$data_is_long_format) {
    v <- names(data)
  } else {
    v <- unique(data[[metadata$dataset$trait_name]])
  }

  var_in <- metadata_user_select_names(paste("Indicate all columns you wish to keep as distinct traits in ", dataset_id), v)

  cat(sprintf("Following traits added to metadata for %s: %s.\n \tPlease complete information in %s.\n\n", dataset_id, crayon::red(paste(var_in, collapse = ", ")), dataset_id %>% metadata_path_dataset_id()))
  
  traits <- tibble::tibble(var_in = var_in,
                            unit_in = "unknown",
                            trait_name = "unknown",
                            entity_type = "unknown",
                            value_type = "unknown",
                            basis_of_value = "unknown",
                            replicates = "unknown",
                            methods = "unknown") 

  # check if existing content, if so append
  if(!all(is.null(metadata$traits)) && !is.na(metadata$traits)) {
    traits <- dplyr::bind_rows(metadata$traits %>% util_list_to_df2(), traits) %>%
      dplyr::filter(!duplicated(var_in))
  }

  metadata$traits <- traits %>% util_df_to_list()

  write_metadata_dataset(metadata, dataset_id)
}

#' For specified `dataset_id` import location data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be 
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param location_data A dataframe of site variables
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' austraits$locations %>% dplyr::filter(dataset_id == "Falster_2005_1") %>% 
#' select(-dataset_id) %>% spread(location_property, value) %>% type_convert()-> location_data
#' metadata_add_locations("Falster_2005_1", location_data)
#' }
metadata_add_locations <- function(dataset_id, location_data) {

  # read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # Choose column for location_name
  location_name <- metadata_user_select_column("location_name", names(location_data))

  # From remaining variables, choose those to keep
  location_sub <- dplyr::select(location_data, -!!location_name)
  keep <- metadata_user_select_names(paste("Indicate all columns you wish to keep as distinct location_properties in ", dataset_id), names(location_sub))

  # Save and notify
  metadata$locations <- dplyr::select(location_data, tidyr::one_of(keep)) %>%
            split(location_data[[location_name]]) %>% lapply(as.list)

  cat(sprintf("Following locations added to metadata for %s: %s\n\twith variables %s.\n\tPlease complete information in %s.\n\n", dataset_id, crayon::red(paste(names( metadata$locations), collapse = ", ")), crayon::red(paste(keep, collapse = ", ")), dataset_id %>% metadata_path_dataset_id()))
  
  write_metadata_dataset(metadata, dataset_id)
}


#' For specified `dataset_id` import context data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be 
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param overwrite Overwrite existing information
#'
#' @importFrom rlang .data
#' @export
metadata_add_contexts <- function(dataset_id, overwrite = FALSE) {
  
  # read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # load and clean trait data
  data <-
    readr::read_csv(file.path("data", dataset_id,  "data.csv"), col_types = cols()) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

  # Get list of potential columns
  v <- names(data)

  contexts <- list()
  n_existing <- 0

  # check for existing info
  if (!overwrite & !is.na(metadata$contexts[1])) {
    contexts <- metadata$contexts
    n_existing <- length(metadata$contexts)

    message(sprintf("Existing context information detected, from the following columns in the dataset: %s.", contexts %>% purrr::map_chr(~.x[["var_in"]]) %>% paste(collapse = ", ")))
  }

  var_in <- metadata_user_select_names(paste("Indicate all columns that contain additional contextual data for ", dataset_id), v)

  categories <- c("treatment", "plot", "temporal", "method", "entity_context")

  for (i in seq_along(var_in)) {
    
    ii <- n_existing + i

    category <- metadata_user_select_names(
      paste("What category does context", var_in[i], "fit in?"), categories)
    
    context_values <- data[[var_in[i]]] %>% unique()
  
    message(sprintf("\tThe following values exist for this context: %s.", context_values %>% paste(collapse = ", ")))
      
    replace_needed <- readline(prompt="Are replacement values required? (y/n) ")
      
    contexts[[ii]] <-
      list(
      context_property = "unknown",
      category = category,
      var_in = var_in[i],
      values = tibble::tibble(
        find = context_values,
        value = context_values, 
        description = "unknown"
        )
      )

    if(tolower(replace_needed) == "y") {
      contexts[[ii]][["values"]][["value"]] = "unknown"
    } else {
      contexts[[ii]][["values"]][["find"]] <- NULL
    }
  }

  metadata$contexts <- contexts

  write_metadata_dataset(metadata, dataset_id)
}

#' Adds citation details to a metadata file for given study
#'
#' @inheritParams metadata_path_dataset_id
#' @param file Name of file where reference is saved
#' @param type Type of references: `primary` or `secondary`
#' @param key The bibtex key to be used. By default set to `dataset_id`
#' @param drop Variables to ignore
#'
#' @return yml file with citation details added
#' @export
#'
metadata_add_source_bibtex <- function(dataset_id, file, type="primary", key=dataset_id, drop = c("dateobj", "month")) {

    # Read in file, convert to list, set key
    bib <- RefManageR::ReadBib(file) %>% 
      util_bib_to_list()

    bib$key <- key

    for(v in drop)
      bib[[v]] <- NULL

    if(!is.null(bib$url) & !is.null(bib$doi))
      bib[["url"]] <- NULL

    if(tolower(bib$bibtype) == "article") 
      bib[["publisher"]] <- NULL

    # Somewhat sensible ordering of elements
    order <- c("key", "bibtype", "year", "author", "journal", "title", "volume", "number","pages", "doi", "publisher", "place")
    v <-  c(order, names(bib)) %>% unique()
    v <- v[v %in% names(bib)]

    # save to metadata
    metadata <- read_metadata_dataset(dataset_id)
    metadata$source[[type]] <- bib[v]
    write_metadata_dataset(metadata, dataset_id)
}

#' Standarise doi into form https://doi.org/XXX
#' 
#' @param doi doi of reference to add
util_standardise_doi <- function(doi) {

  if (stringr::str_starts(doi, "https://doi.org"))
    return(doi)

  if (stringr::str_starts(doi, "http:"))
    return(gsub("http:", "https:", doi, fixed=TRUE))
 
  if (stringr::str_starts(doi, "doi.org"))
    return(paste0("https://", doi))

  return(paste0("https://doi.org/", doi))
}

#' Adds citation details from a doi to a metadata file for a dataset_id. 
#'
#' Uses rcrossref package to access publication details from the crossref 
#' database
#'
#' @param bib (Only use for testing purposes). Result of calling `bib rcrossref::cr_cn(doi)`
#' @inheritParams metadata_path_dataset_id 
#' @inheritParams util_standardise_doi
#' @param ... arguments passed from metadata_add_source_bibtex()
#'
#' @return metadata.yml file has citation details added
#' @export
#'
metadata_add_source_doi <- function(..., doi, bib=NULL) {
  
  doi <- util_standardise_doi(doi)

  if(is.null(bib)) 
    bib <- suppressWarnings(rcrossref::cr_cn(doi))

  if(is.null(bib)) {
    message("DOI not available in Crossref database, please fill record manually") 
    return(invisible(FALSE))
  }

  file <- tempfile()
  writeLines(bib, file)

  metadata_add_source_bibtex(file=file, ...)
}

#' Add a categorical trait value substitution into a metadata file for a dataset_id
#' 
#' `metadata_add_substitution` is used to align the categorical trait values used
#' by a contributor to the categorical values supported by AusTraits. These values
#' are defined in the `traits.yml` file
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param trait_name the AusTraits defined name for a particular trait
#' @param find trait value in the original data.csv file
#' @param replace trait value supported by AusTraits
#'
#' @return yaml file with a substitution added
#' @export
metadata_add_substitution <- function(dataset_id, trait_name, find, replace) {

  set_name <- "substitutions"

  metadata <- read_metadata_dataset(dataset_id)

  to_add <- list(trait_name = trait_name, find = find, replace = replace) 

  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  util_list_to_df2(metadata[[set_name]])  
  if(length(metadata[[set_name]]) > 0)
    if(length(which(trait_name %in% data$trait_name & find %in% data$find)) > 0) {
    message(paste(
        crayon::red(sprintf("Substitution exists for %s, %s", trait_name, find))
        , 
        sprintf("-> please review manually in %s",  metadata_path_dataset_id(dataset_id))
        ))
    return(invisible())
  }

  metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)

  message(sprintf("%s %s for trait %s : %s -> %s", crayon::red("Adding substitution in"), crayon::red(dataset_id), trait_name, find, replace))
  write_metadata_dataset(metadata, dataset_id)
}

#' Add a dataframe of trait value substitutions into a metadata file for a dataset_id
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param substitutions dataframe of trait value substitutions
#'
#'
#' @return yml file with multiple trait value substitutions added
#' @importFrom rlang .data
#' @export
metadata_add_substitutions_list <- function(dataset_id, substitutions) {
  
  #read metadata
  metadata <- read_metadata_dataset(dataset_id)
  
  #read in dataframe of substitutions, split into single-row lists, and add to metadata file
  metadata$substitutions <- 
    substitutions %>%
    dplyr::group_split(.data$trait_name, .data$find) %>% lapply(as.list)
  
  #write metadata
  write_metadata_dataset(metadata, dataset_id)
}  



#' Substitutions from csv
#' @description Function that simultaneously adds many trait value replacements, potentially across many trait_names and dataset_ids, to the respective metadata.yml files.
#' This function will be used to quickly re-align/re-assign trait values across all AusTraits studies.
#'
#' @param dataframe_of_substitutions dataframe with columns indicating dataset_id, trait_name, original trait values (find), and AusTraits aligned trait value (replace)
#' @param dataset_id study's dataset_id in AusTraits
#' @param trait_name trait name for which a trait value replacement needs to be made
#' @param find trait value submitted by the contributor for a data observation
#' @param replace AusTraits aligned trait value
#'
#' @importFrom rlang .data
#'
#' @return modified metadata files with trait value replacements
#' @export
#'
#' @examples \dontrun{
#' read_csv("export/dispersal_syndrome_substitutions.csv") %>%
#'   select(-extra) %>%
#'   filter(dataset_id == "Angevin_2011") -> dataframe_of_substitutions
#' metadata_add_substitutions_table(dataframe_of_substitutions, dataset_id, trait_name, find, replace)
#' }
metadata_add_substitutions_table <- function(dataframe_of_substitutions, dataset_id, trait_name, find, replace) {

  # split dataframe of substitutions by row
  dataframe_of_substitutions %>%
    dplyr::mutate(rows = dplyr::row_number()) %>%
    dplyr::group_split(.$rows) -> dataframe_of_substitutions

  set_name <- "substitutions"

  # add substitutions to metadata files
  for (i in 1:max(dataframe_of_substitutions)$rows) {
    metadata <- read_metadata_dataset(dataframe_of_substitutions[[i]]$dataset_id)

    to_add <- list(trait_name = dataframe_of_substitutions[[i]]$trait_name, find = dataframe_of_substitutions[[i]]$find, replace = dataframe_of_substitutions[[i]]$replace)

    if (is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
      metadata[[set_name]] <- list()
    }

    data <- util_list_to_df2(metadata[[set_name]])

    metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)

    write_metadata_dataset(metadata, dataframe_of_substitutions[[i]]$dataset_id)
  }
}


#' Add a taxonomic change into the metadata yaml file for a dataset_id
#' 
#' Add a single taxonomic change into the metadata yaml file for a specific study
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param find original name used by the contributor 
#' @param replace taxonomic name accepted by APC or APNI 
#' @param reason reason for taxonomic change
#'
#' @return yml file with taxonomic change added
#' @export
metadata_add_taxonomic_change <- function(dataset_id, find, replace, reason, taxonomic_resolution) {

  if(length(replace) > 1 ) {
    stop(sprintf("Cannot replace with two names!! (for %s -> %s)\n", crayon::red(find), crayon::red(replace)))
  }
  set_name <- "taxonomic_updates"
  metadata <- read_metadata_dataset(dataset_id)

  to_add <- list(find = find, replace = replace, reason = reason, taxonomic_resolution = taxonomic_resolution) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }
  
  # Check if find record already exists for that trait
  data <- util_list_to_df2(metadata[[set_name]])  
  if(!is.na(data) && nrow(data) > 0 && length(which(find %in% data$find)) > 0) {
    cat(sprintf("\tSubstitution already exists for %s\n", crayon::red(find)))
    return(invisible(TRUE))
  }

  metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)

  cat(sprintf("%s %s: %s -> %s (%s)\n", "\tAdding taxonomic change in", dataset_id, crayon::blue(find), crayon::green(replace), reason))
  write_metadata_dataset(metadata, dataset_id)
  
  return(invisible(TRUE))
}

#' Add a list of taxonomic updates into a metadata file for a dataset_id
#' 
#' Add multiple taxonomic changes to the metadata yaml file using a dataframe 
#' containing the taxonomic changes to be made. 
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param taxonomic_updates dataframe of taxonomic updates
#'
#' @importFrom rlang .data
#' @return yml file with multiple taxonmic updates added
#' @export
metadata_add_taxonomic_changes_list <- function(dataset_id, taxonomic_updates) {
  
  # read metadata
  metadata <- read_metadata_dataset(dataset_id)
  
  #read in dataframe of taxonomic changes, split into single-row lists, and add to metadata file
  metadata$taxonomic_updates <- 
    taxonomic_updates %>%
    dplyr::group_split(.data$find) %>% lapply(as.list)
  
  # write metadata
  write_metadata_dataset(metadata, dataset_id)
}

#' Exclude observations in a yaml file for a dataset_id
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param variable variable name
#' @param find term to find by
#' @param reason reason for exclusion
#'
#' @return yml file with excluded observations
#' @export
metadata_exclude_observations <- function(dataset_id, variable, find, reason) {

  set_name <- "exclude_observations"
  metadata <- read_metadata_dataset(dataset_id)

  to_add <- list(variable = variable, find = find, reason = reason) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }
  
  # Check if find record already exists for that trait
  data <-  util_list_to_df2(metadata[[set_name]])  
  if(!is.na(data) && nrow(data) > 0 && length(which(find == data$find & variable == data$variable & reason == data$reason)) > 0) {
    cat(sprintf("Exclusion already exists for %s\n", crayon::red(find)))
    return(invisible(TRUE))
  }

  metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)

  cat(sprintf("%s - excluding %s: %s (%s)\n", dataset_id, crayon::blue(variable), crayon::blue(find), reason))
  write_metadata_dataset(metadata, dataset_id)
  
  return(invisible(TRUE))
}

#' Update a taxonomic change into a yaml file for a dataset_id
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param find original taxonomic name 
#' @param replace updated taxonomic name to replace original taxonomic name
#' @param reason reason for change
#'
#' @return yml file with added substitution
#' @export
metadata_update_taxonomic_change <- function(dataset_id, find, replace, reason, taxonomic_resolution) {

  set_name <- "taxonomic_updates"

  metadata <- read_metadata_dataset(dataset_id)

  to_add <- list(find = find, replace = replace, reason = reason, taxonomic_resolution = taxonomic_resolution) 

  data <-  util_list_to_df2(metadata[[set_name]]) 
  i <- match(find, data$find)
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]]) || nrow(data) == 0 || length(i) == 0) {
    stop(sprintf("Substitution for %s in %s  does not exist", find, metadata))
  }

  metadata[[set_name]][[i]][["replace"]] <- replace
  metadata[[set_name]][[i]][["reason"]] <- reason
  metadata[[set_name]][[i]][["taxonomic_resolution"]] <- taxonomic_resolution
  message(sprintf("%s %s: %s -> %s (%s)", crayon::red("Updating taxonomic change in"),crayon::red(dataset_id), crayon::blue(find), crayon::green(replace), reason))

  write_metadata_dataset(metadata, dataset_id)
}

#' Remove a taxonomic change from a yaml file for a dataset_id
#'
#' @param dataset_id identifier for a particular study in the AusTraits database
#' @param find taxonomic name to find
#' @param replace taxonomic name to replace with 
#'
#' @return yml file with a taxonomic change removed
#' @export
metadata_remove_taxonomic_change <- function(dataset_id, find, replace=NULL) {

  set_name <- "taxonomic_updates"
  metadata <- read_metadata_dataset(dataset_id)

  # if it doesn't yet exist - > done
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::green("does not exist")))
    return()
  }

  # Check if find record already exists for that trait
  data <-  util_list_to_df2(metadata[[set_name]])  
  if(nrow(data) == 0) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::green("does not exist")))
    return()
  }

  if(is.null(replace))
    i <- data$find == find
  else
    i <- data$find == find & data$replace == replace


  if(any(i)) {
    metadata[[set_name]][which(i)] <- NULL
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::red("removed")))
  }

  write_metadata_dataset(metadata, dataset_id)
}


#' Find taxonomic changes within the metadata yml files
#'
#' @param find name of original species
#' @param replace name of replacement species, default = NULL
#' @param studies name of studies, default = NULL
#'
#' @importFrom stringr str_remove_all str_replace_all
#' @export
metadata_find_taxonomic_change <- function(find, replace=NULL, studies = NULL){

  if(is.null(studies))
    studies <- dirname(dir("data", pattern="metadata.yml", recursive = TRUE))

  f <- file.path("data", studies, "metadata.yml")

  contents <- lapply(f, function(x) paste0(readLines(x), collapse="\n"))

  if(!is.null(replace))
      txt <- sprintf("- find: %s\nreplace: %s", find, replace)
  else
    txt <- sprintf("- find: %s\n", find)
  i <- sapply(contents, function(s) any(grepl(txt,s, fixed=TRUE)))
  
  studies[i]
}

#' Update the remake.yml file with new studies
#' 
#' `build_setup_pipeline` rewrites the remake.yml file to include new
#' studies
#'
#' @param template template used to build 
#' @param path path to folder with data
#' @param dataset_ids dataset_ids to icnlude. By default includes all.
#'
#' @return Updated remake.yml file 
#' @export
build_setup_pipeline <- function(
  template = readLines(system.file("support", "remake.yml.whisker", package = "austraits.build")),
  path="data",
  dataset_ids = dir(path)) {

  if(!file.exists(path)) {
    stop("cannot find data directory: ", path)
  }

  # check directories have both files
  has_both_files <- sapply(dataset_ids, function(id) sprintf("%s/%s/%s", path, id, c("data.csv", "metadata.yml") ) %>% file.exists() %>% all())

  dataset_ids <- dataset_ids[has_both_files]

  vals <- list(
    dataset_ids = whisker::iteratelist(dataset_ids, value="dataset_id"),
    path=path
    )

  str <- whisker::whisker.render(template, vals)
  writeLines(str, "remake.yml")

  # Check taxon list exists
  filename <- "config/taxon_list.csv"

  if(!file.exists(filename)) {
    dplyr::tibble(
      cleaned_name = character(),
      taxonomic_reference = character(),
      cleaned_scientific_name_id = character(),
      cleaned_name_taxonomic_status = character(),
      cleaned_name_alternative_taxonomic_status = character(),
      taxon_name = character(),
      taxon_id = character(),
      scientific_name_authorship = character(),
      taxon_rank = character(),
      taxonomic_status = character(),
      family = character(),
      taxon_distribution = character(),
      establishment_means = character(),
      scientific_name = character(),
      scientific_name_id = character()
    ) %>%  readr::write_csv(filename)
  }
}

#' Find list of all unique taxa within compilation
#'
#' @param taxon_name name of column which contains the cleaned species names
#' @param austraits austraits compilation
#' @param original_name name of column which contains original species names, default = FALSE
#'
#' @importFrom rlang .data
#' @return list of all unique and distinct species names
#' @export
build_find_taxon <- function(taxon_name, austraits, original_name = FALSE) {
  
  data <- austraits$traits

  if (!original_name) {
    data <- data %>%
      dplyr::select(name = taxon_name, .data$dataset_id) %>%
      dplyr::distinct()
  } else {
    data <- data %>%
      dplyr::select(name = original_name, .data$dataset_id) %>%
      dplyr::distinct()
  }

  f <- function(sp) {
    dplyr::filter(data, .data$name == sp) %>%
      dplyr::pull(.data$dataset_id) %>%
      unique()
  }

  if (length(taxon_name) == 1) {
    f(taxon_name)
  } else {
    lapply(taxon_name, f)
  }
}
