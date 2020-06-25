#' Path to the `metadata.yml` file for specified `dataset_id`
#'
#' @param dataset_id identifier for a particular study in the AusTraits compilation
#'
#' @export 
#' @return A string
metadata_path_dataset_id <- function(dataset_id) {
  file.path("data", dataset_id, "metadata.yml")
}


#' Read the `metadata.yml` file for specified `dataset_id`
#'
#' @inheritParams metadata_path_dataset_id
#'
#' @export 
#' @return A list with contents of metadata for specified `dataset_id`
metadata_read_dataset_id <- function(dataset_id) {
  dataset_id %>% metadata_path_dataset_id() %>% read_yaml()
}
#' Write the YAML representation of metadata.yml for specified `dataset_id` to
#' file \code{data/dataset_id/metadata.yml}
#'
#' @inheritParams metadata_path_dataset_id
#' @param metadata <what param does>
#'
#' @export 
metadata_write_dataset_id <- function(metadata, dataset_id) {
  write_yaml(metadata, dataset_id %>% metadata_path_dataset_id())
}

#' Create a template of file `metadata.yml` for specified `dataset_id`
#'
#' Includes place-holders for major sections of the metadata
#' 
#' @inheritParams metadata_path_dataset_id
#' @param path location of file where output is saved
#' 
#' @export
metadata_create_template <- function(dataset_id, 
                                     path = file.path("data", dataset_id, "metadata.yml")
                                     ) {

  people <- tibble(name = "unknown", institution = "unknown", role = "unknown") 

  out <- list(
       source = list(primary=list(key=dataset_id, 
                                  bibtype = "Article",
                                  year = "unknown", 
                                  author = "unknown",
                                  title = "unknown",
                                  journal = "unknown",
                                  volume = "unknown",
                                  number = "unknown",
                                  pages = "unknown",
                                  doi = "unknown"
                                  )
                     ),
       people = people %>% df_to_list(),
       dataset = list(year_collected_start= "unknown",
                      year_collected_end= "unknown",
                      description= "unknown",
                      collection_type= "unknown",
                      sample_age_class= "unknown",
                      sampling_strategy= "unknown",
                      original_file= "unknown",
                      notes= "unknown"),
       sites = NA,
       contexts = NA,
       config = NA,
       traits = NA,
       substitutions = NA,
       taxonomic_updates = NA,
       questions = NA
       )

  # Check format of data
  tmp <- menu(c("Long", "Wide"), title="Is the data long or wide format?")
  data_is_long_format <- ifelse(tmp == 1, TRUE, FALSE)

  data <- read_csv(file.path("data", dataset_id, "data.csv"), col_types = cols())

  # Setup config and select columns as appropriate
  config <- list(data_is_long_format = data_is_long_format, 
                 variable_match = list(),
                 custom_R_code = NA)

  if(data_is_long_format) {
    v1 <- c("taxon_name", "trait_name", "value")
    v2 <- c("site_name", "context_name", "observation_id")
  } else {
    v1 <- c("taxon_name")
    v2 <- c("site_name", "context_name")
  }
  for(v in v1) {      
    config[["variable_match"]][[v]] <- user_select_column(v, names(data))
  }

  for(v in v2) {
    tmp <- user_select_column(v, c(names(data), NA))
    if(!is.na(tmp)) 
      config[["variable_match"]][[v]] <- tmp
  }

  out[["config"]] <- config

  write_yaml(out, path)
}

user_select_column <- function(column, choices) {
  tmp <- menu(choices, title= sprintf("Select column for `%s`", column))
  choices[tmp]
}

user_select_names <- function(title, vars){

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
  metadata <- metadata_read_dataset_id(dataset_id)

  # load and clean trait data
  read_csv(file.path("data", dataset_id,  "data.csv"), col_types = cols(), guess_max = 100000) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])()
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
#' @export
metadata_add_traits <- function(dataset_id) {

  # read metadata
  metadata <- metadata_read_dataset_id(dataset_id)

  # load and clean trait data
  data <- read_csv(file.path("data", dataset_id,  "data.csv"), col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])()

  # Get list of potential traits
  if(!metadata$config$data_is_long_format) {
    v <- names(data)
  } else {
    v <- unique(data[[metadata$config$variable_match$trait_name]])
  }

  var_in <- user_select_names(paste("Indicate all columns you wish to keep as distinct traits in ", dataset_id), v)

  cat(sprintf("Following traits added to metadata for %s: %s.\n \tPlease complete information in %s.\n\n", dataset_id, crayon::red(paste(var_in, collapse = ", ")), dataset_id %>% metadata_path_dataset_id()))
  
  traits <- tibble(var_in = var_in,
                            unit_in = "unknown",
                            trait_name = "unknown",
                            value_type = "unknown",
                            replicates = "unknown",
                            methods = "unknown") 

  # check if existing content, if so append
  if(!all(is.null(metadata$traits)) && !is.na(metadata$traits)) {
    traits <- bind_rows(metadata$traits %>% list_to_df(), traits) %>%
      filter(!duplicated(var_in))
  }

  metadata$traits <- traits %>% df_to_list()

  metadata_write_dataset_id(metadata, dataset_id)
}

#' For specified `dataset_id` import site data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be 
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param site_data A dataframe of site variables
#'
#' @export
#' @examples
#' austraits$sites %>% filter(dataset_id == "Falster_2005_1") %>% select(-dataset_id) %>% spread(site_property, value) %>% type_convert()-> site_data
#' metadata_add_sites("Falster_2005_1", site_data)
metadata_add_sites <- function(dataset_id, site_data) {

  # read metadata
  metadata <- metadata_read_dataset_id(dataset_id)

  # Choose column for site_name
  site_name <- user_select_column("site_name", names(site_data))

  # From remaining variables, choose those to keep
  site_sub <- select(site_data, -!!site_name)
  keep <- user_select_names(paste("Indicate all columns you wish to keep as distinct site_properties in ", dataset_id), names(site_sub))

  # Save and notify
  metadata$sites <- select(site_data, one_of(keep)) %>%
            split(site_data[[site_name]]) %>% lapply(as.list)

  cat(sprintf("Following sites added to metadata for %s: %s\n\twith variables %s.\n\tPlease complete information in %s.\n\n", dataset_id, crayon::red(paste(names( metadata$sites), collapse = ", ")), crayon::red(paste(keep, collapse = ", ")), dataset_id %>% metadata_path_dataset_id()))
  
  metadata_write_dataset_id(metadata, dataset_id)
}


#' For specified `dataset_id` import context data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be 
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param context_data A dataframe of context variables
#'
#' @export
#' @examples
#' austraits$contexts %>% filter(dataset_id == "Hall_1981") %>% select(-dataset_id) %>% spread(context_property, value) %>% type_convert()-> context_data
#' metadata_add_contexts("Hall_1981", context_data)
metadata_add_contexts <- function(dataset_id, context_data) {
  
  # read metadata
  metadata <- metadata_read_dataset_id(dataset_id)
  
  # Choose column for context_name
  context_name <- user_select_column("context_name", names(context_data))
  
  # From remaining variables, choose those to keep
  context_sub <- select(context_data, -!!context_name)
  keep <- user_select_names(paste("Indicate all columns you wish to keep as distinct context_properties in ", dataset_id), names(context_sub))
  
  # Save and notify
  metadata$contexts <- select(context_data, one_of(keep)) %>%
    split(context_data[[context_name]]) %>% lapply(as.list)
  
  cat(sprintf("Following contexts added to metadata for %s: %s\n\twith variables %s.\n\tPlease complete information in %s.\n\n", dataset_id, crayon::red(paste(names( metadata$contexts), collapse = ", ")), crayon::red(paste(keep, collapse = ", ")), dataset_id %>% metadata_path_dataset_id()))
  
  metadata_write_dataset_id(metadata, dataset_id)
}



#' Adds citation details to a metadata file for given study
#'
#' @inheritParams metadata_path_dataset_id
#' @param file Name of file where reference is saved
#' @param type Type of references: `primary` or `secondary`
#' @param key The bibtex key to be used. By default set to `dataset_id`
#' @param drop Variables to ignore
#'
#' @export
#'
metadata_add_source_bibtex <- function(dataset_id, file, type="primary", key=dataset_id, drop = c("dateobj", "month")) {

    # Read in file, convert to list, set key
    bib <- RefManageR::ReadBib(file) %>% 
      convert_bib_to_list()

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
    metadata <- metadata_read_dataset_id(dataset_id)
    metadata$source[[type]] <- bib[v]
    metadata_write_dataset_id(metadata, dataset_id)
}


#' Adds citation details from a doi to a metadata file for a dataset_id. 
#'
#' Uses rcrossref package to access publication details from the crossref 
#' database
#'
#' @inheritParams metadata_path_dataset_id 
#' @inheritParams metadata_add_source_bibtex
#' @param doi doi of reference to add
#'
#' @export
#'
metadata_add_source_doi <- function(doi, ...) {

  bib <- suppressWarnings(rcrossref::cr_cn(doi))

  if(is.null(bib)) {
    message("DOI not available in Crossref database, please fill record manually") 
    return(invisible(FALSE))
  }
  file <- tempfile()
  writeLines(bib, file)

  metadata_add_source_bibtex(file=file, ...)
}


#' function to add a substitution into a metadata file for a dataset_id
#'
#' @param dataset_id 
#' @param trait_name 
#' @param find 
#' @param replace 
#'
#' @export
#'
#' @examples
metadata_add_substitution <- function(dataset_id, trait_name, find, replace) {

  set_name <- "substitutions"

  metadata <- metadata_read_dataset_id(dataset_id)

  to_add <- list(trait_name = trait_name, find = find, replace = replace) 

  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(length(metadata[[set_name]]) > 0)
    if(length(which(trait_name %in% data$trait_name & find %in% data$find)) > 0) {
    message(paste(
        crayon::red(sprintf("Substitution exists for %s, %s", trait_name, find))
        , 
        sprintf("-> please review manually in %s",  metadata_path_dataset_id(dataset_id))
        ))
    return(invisible())
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("%s %s for trait %s : %s -> %s", crayon::red("Adding substitution in"), crayon::red(dataset_id), trait_name, find, replace))
  metadata_write_dataset_id(metadata, dataset_id)
}


#' add a taxonomic change into a yaml file for a dataset_id
#'
#' @param dataset_id 
#' @param find 
#' @param replace 
#' @param reason 
#'
#' @return
#' @export
#'
#' @examples
metadata_add_taxnomic_change <- function(dataset_id, find, replace, reason) {

  if(length(replace) > 1 ) {
    stop(sprintf("Cannot replace with two names!! (for %s -> %s)\n", crayon::red(find), crayon::red(replace)))
  }
  set_name <- "taxonomic_updates"
  metadata <- metadata_read_dataset_id(dataset_id)

  to_add <- list(find = find, replace = replace, reason = reason) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }
  
  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(!is.na(data) && nrow(data) > 0 && length(which(find %in% data$find)) > 0) {
    cat(sprintf("\tSubstitution already exists for %s\n", crayon::red(find)))
    return(invisible(TRUE))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  cat(sprintf("%s %s: %s -> %s (%s)\n", "\tAdding taxonomic change in", dataset_id, crayon::blue(find), crayon::green(replace), reason))
  metadata_write_dataset_id(metadata, dataset_id)
  
  return(invisible(TRUE))
}


#' add a taxonomic change into a yaml file for a dataset_id
#'
#' @param dataset_id 
#' @param variable 
#' @param find 
#' @param reason 
#'
#' @return
#' @export
#'
#' @examples
metadata_exclude_observations <- function(dataset_id, variable, find, reason) {

  set_name <- "exclude_observations"
  metadata <- metadata_read_dataset_id(dataset_id)

  to_add <- list(variable = variable, find = find, reason = reason) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }
  
  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(!is.na(data) && nrow(data) > 0 && length(which(find == data$find & variable == data$variable & reason == data$reason)) > 0) {
    cat(sprintf("Exclusion already exists for %s\n", crayon::red(find)))
    return(invisible(TRUE))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  cat(sprintf("%s - excluding %s: %s (%s)\n", dataset_id, crayon::blue(variable), crayon::blue(find), reason))
  metadata_write_dataset_id(metadata, dataset_id)
  
  return(invisible(TRUE))
}

#' update a substitution into a yaml file for a dataset_id
#'
#' @param dataset_id 
#' @param find 
#' @param replace 
#' @param reason 
#'
#' @return
#' @export
#'
#' @examples
metadata_update_taxnomic_change <- function(dataset_id, find, replace, reason) {

  set_name <- "taxonomic_updates"

  metadata <- metadata_read_dataset_id(dataset_id)

  to_add <- list(find = find, replace = replace, reason = reason) 

  data <-  list_to_df(metadata[[set_name]]) 
  i <- match(find, data$find)
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]]) || nrow(data) == 0 || length(i) == 0) {
    stop(sprintf("Substitution for %s in %s  does not exist", find, filename_metadata))
  }

  metadata[[set_name]][[i]][["replace"]] <- replace
  metadata[[set_name]][[i]][["reason"]] <- reason
  message(sprintf("%s %s: %s -> %s (%s)", crayon::red("Updating taxonomic change in"),crayon::red(dataset_id), crayon::blue(find), crayon::green(replace), reason))

  metadata_write_dataset_id(metadata, dataset_id)
}

#' remove a taxonomic change from a yaml file for a dataset_id
#'
#' @param dataset_id 
#' @param find 
#' @param replace 
#'
#' @return
#' @export
#'
#' @examples
metadata_remove_taxnomic_change <- function(dataset_id, find, replace=NULL) {

  set_name <- "taxonomic_updates"
  metadata <- metadata_read_dataset_id(dataset_id)

  # if it doesn't yet exist - > done
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::green("does not exist")))
    return()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(nrow(data) == 0) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::green("does not exist")))
    return()
  }

  if(is.null(replace))
    i <-data$find == find
  else
    i <- data$find == find & data$replace == replace


  if(any(i)) {
    metadata[[set_name]][which(i)] <- NULL
    message(sprintf("Taxonomic change in %s: %s -> %s %s", dataset_id, find, replace, crayon::red("removed")))
  }

  metadata_write_dataset_id(metadata, dataset_id)
}

#' Title
#'
#' @param taxon_name 
#' @param original_name 
#'
#' @return
#'
#' @examples
austraits_find_species <- function(taxon_name, original_name = FALSE){

  data <- austraits$traits

  if(!original_name)
    data <- data %>% select(name =  taxon_name, dataset_id) %>% distinct()
  else
    data <- data %>% select(name =  original_name, dataset_id) %>% distinct()

  f <- function(sp)  filter(data, name == sp) %>%  pull(dataset_id) %>% unique()

  if(length(taxon_name) == 1) 
    f(taxon_name)
  else
    lapply(taxon_name, f) 
}

#' Title
#'
#' @param find 
#' @param replace 
#' @param studies 
#'
#' @return
#' @export
#'
#' @examples
metadata_find_taxnomic_change <- function(find, replace=NULL, studies = NULL){

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

strip_names <- function(x) {
  x %>% 
    str_remove_all(" subsp.") %>% str_remove_all(" aff.")  %>% str_remove_all(" var.") %>% str_remove_all(" ser.") %>% str_remove_all(" f.") %>%
    str_remove_all(" s.l.") %>% str_remove_all(" s.s.") %>%
    str_replace_all("[-._()]", " ") %>% 
    str_squish() %>% tolower() 
}

#' Checks all taxa within against our list of known species
#' If not found, and update=TRUE, checks the unknown species against
#'
#' @param dataset_id 
#' @param update 
#' @param typos 
#' @param diffchar 
#'
#' @export
#'
#' @examples
metadata_check_taxa <- function(dataset_id, 
                                max_distance_abs = 3, max_distance_rel = 0.2,
                                try_outside_guesses = FALSE,
                                author = git2r::config()$global$user.name) {
  
  
  cat("Checking alignments in ", crayon::red(dataset_id), "\n")
  
  x <- remake::make(dataset_id)
  taxa <- remake::make("taxon_list") %>% 
    mutate(stripped_name = strip_names(cleaned_name))

  species <- 
    x$traits %>% select(original_name, taxon_name) %>% distinct() %>% 
    mutate(
      known = taxon_name %in% taxa$cleaned_name
    )

  if(all(species$known)){
    message(crayon::red("All taxa are already known\n"))
    return(invisible(NULL));   
  }
  
  # check unknown taxa
  cat(crayon::red(sum(species$known)), " names already matched; ")
  
  if(sum(!species$known) == 0 )
    break;
  
  cat(crayon::red(sum(!species$known)), " taxa are not yet matched\n")
  
  species <- species %>% filter(!known)
  
  # Check if existing substitution in metadata
  metadata <- metadata_read_dataset_id(dataset_id)
  if(!all(is.null(metadata$taxonomic_updates)) && !is.na(metadata$taxonomic_updates)) {
    metata_changes <- 
      metadata$taxonomic_updates %>% list_to_df() 
    
    species <- species %>% 
        mutate(
          known = original_name %in% metata_changes$find
        )
    
    if(any(species$known)) {
      cat(crayon::red(sum(species$known)), " of these already have substitutions in metadata:\n")
      tmp <- metata_changes %>% filter(find %in% (species %>% filter(known) %>% pull(original_name)))
      for(i in seq_along(tmp$find))
        cat(sprintf("\t%s -> %s (%s)\n", crayon::blue(tmp$find[i]), crayon::green(tmp$replace[i]), tmp$reason[i]))
      species <- species %>% filter(!known)
    }
  }
  
  species <- species$original_name[!species$known]
  
  if(length(species)==0) return(invisible());
  
  cat(crayon::red(length(species)), " species are not yet matched, checking for close matches in APC & APNI \n")
  
  taxonomic_resources <- load_taxonomic_resources()
  
  genera_accepted <-  taxonomic_resources$APC %>% filter(taxonRank %in% c('Genus'), taxonomicStatus == "accepted") 
    
  to_check <- list()
  to_review <- tibble(dataset_id = character(), taxon_name =  character())

  APC_tmp <- 
    taxonomic_resources$APC %>% 
    filter(taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
    select(canonicalName, scientificName, taxonomicStatus, ID = taxonID) %>% 
    mutate(
      stripped_canonical = strip_names(canonicalName),
      stripped_scientific = strip_names(scientificName)
      ) %>%
    distinct()
  
  to_check[["APC list (accepted)"]] <- APC_tmp %>% filter(taxonomicStatus == "accepted")
  to_check[["APC list (known names)"]] <- APC_tmp %>% filter(taxonomicStatus != "accepted")
  
  to_check[["APNI names"]] <- 
    taxonomic_resources$APNI %>% filter(nameElement != "sp.") %>% 
    select(canonicalName, scientificName, ID = scientificNameID) %>% 
    mutate( taxonomicStatus = "unplaced", 
            stripped_canonical = strip_names(canonicalName),
            stripped_scientific = strip_names(scientificName)
            ) %>%
    distinct() %>% arrange(canonicalName)

  for(s in species) {
      
      cleaned_name <- standardise_names(s)
      stripped_name <- strip_names(cleaned_name)
      genus <-stringr::str_split(s, " ")[[1]][1]
      found <- FALSE
      
      if(grepl("sp\\.$", cleaned_name)) {
        cat(sprintf("\tSkipping %s - not assessing anything ending in `sp.` Note, genus %s is %s in APC\n", 
                    crayon::blue(s), crayon::green(genus), 
                    ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
        found <- TRUE
        }
      
      for(v in names(to_check))  {
        
        if(found) break;
        
        if(s %in% to_check[[v]]$canonicalName) {
          message(sprintf("%s found in %s", crayon::green(s), v))
          found <- TRUE
          break;
        } else if(s %in% to_check[[v]]$scientificName) {
          found <- metadata_add_taxnomic_change(dataset_id, s,
                                                to_check[[v]]$canonicalName[match(s, to_check[[v]]$scientificName)], 
                                                sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break;
        } else if(stripped_name %in% to_check[[v]]$stripped_canonical) {
          found <- metadata_add_taxnomic_change(dataset_id, s,
            to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_canonical)], 
            sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
            )
          break;
        } else if(stripped_name %in% to_check[[v]]$stripped_scientific) {
          found <- metadata_add_taxnomic_change(dataset_id, s,
                                                to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_scientific)], 
                                                sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break;
        } else {
          distance_c <- adist(stripped_name, to_check[[v]]$stripped_canonical, fixed=TRUE)[1,]
          min_dist_abs_c <-  min(distance_c)
          min_dist_per_c <-  min(distance_c) / str_length(stripped_name)

          distance_s <- adist(stripped_name, to_check[[v]]$stripped_scientific, fixed=TRUE)[1,]
          min_dist_abs_s <-  min(distance_s)
          min_dist_per_s <-  min(distance_s) / str_length(stripped_name)
          
          if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs & 
            ## Within allowable number of characters (relative) 
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
            ) {
              found <- 
                metadata_add_taxnomic_change(dataset_id, s, 
                  to_check[[v]]$canonicalName[which(distance_c==min_dist_abs_c)], 
                  sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
            )
          } else if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_s <= max_distance_abs & 
            ## Within allowable number of characters (relative) 
            min_dist_per_s <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_s==min_dist_abs_s))==1
          ) {
            found <- 
              metadata_add_taxnomic_change(dataset_id, s, 
                                           to_check[[v]]$canonicalName[which(distance_s==min_dist_abs_s)], 
                                           sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
              )
          } else if(try_outside_guesses) {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            closest_names <- to_check[[v]]$canonicalName[j]

            cat(sprintf("\nFor %s - are any of these names from %s appropropriate?\n",  crayon::blue(s), v))
            tmp <- menu(c("None", sprintf("%s -- %s -- %s", crayon::green(closest_names), to_check[[v]]$taxonomicStatus[j], to_check[[v]]$ID[j])))
            if(tmp > 1){
              found <- 
                metadata_add_taxnomic_change(dataset_id, s,  closest_names[tmp-1], 
                    sprintf("Alignment with known name in %s (%s, %s)", v, author, Sys.Date()))
              }
          } else {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            
            to_review <- 
              bind_rows(to_review, 
                      tibble(dataset_id = dataset_id, source = v,
                             taxon_name = s, closest_names = to_check[[v]]$canonicalName[j], status = to_check[[v]]$taxonomicStatus[j], ID = to_check[[v]]$ID[j], 
                             genus_known = genus %in% genera_accepted$canonicalName,
                             keep = 0, reason = sprintf("Alignment with known name in %s (%s, %s)", v, author, Sys.Date()))
                      )
            
            if(v == last(names(to_check))){
              cat(sprintf("\tTaxa not found: %s. Note, genus %s is %s in APC\n", 
                        crayon::blue(s), crayon::green(genus), 
                    ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
            }
          }
        }
    }
  }
  
  if(!try_outside_guesses & nrow(to_review) > 0 ) {
    filename <- sprintf("export/taxa_review/%s.csv", dataset_id)
    dir.create(dirname(filename), FALSE, TRUE)
    write_csv(to_review, filename)
    cat(sprintf("Reveiw further suggestions for these taxa in %s\n", 
                crayon::green(filename)))
  }
    
  cat("After adding substitutions you should consider rebuilding taxon list with ", 
      crayon::blue("austraits_rebuild_taxon_list()"), "\n\n")
}

#' Title
#'
#' @param path_apc location of downloaded APC taxon file
#' @param path_apni location of downloaded APNI name file
#'
#' @export
#'
load_taxonomic_resources <- function() {
  
  file_paths <- list(
    APC = "config/NSL/APC-taxon-2020-05-14-1332.csv",
    APNI = "config/NSL/APNI-names-2020-05-14-1341.csv"
  )

  if(!all(file.exists(unlist(file_paths)))) {
    for(i in seq_along(file_paths)) {
      if(!file.exists(file_paths[[i]])) 
        cat("file missing: ", file_paths[[i]],"\n")
    }
    stop("Need to download taxonomic resources to proceed")
  }

  if(!exists("taxonomic_resources",  envir = .GlobalEnv)) {
    message(crayon::red("loading object `taxonomic_resources` into glabal environment"))
    taxonomic_resources <- list()
    taxonomic_resources$APC <- read_csv_char(file_paths$APC)
    taxonomic_resources$APNI <- read_csv_char(file_paths$APNI)
    assign("taxonomic_resources", taxonomic_resources, envir = .GlobalEnv)
  } 
  
  get0("taxonomic_resources", envir = .GlobalEnv)
}

#' Builds list of potential species from the Australian Plant Census (APC) and 
#' Australian Plant names Index (APNI)
#' 
#' Compiled list is saved at "config/taxa.csv". While this list is 
#' only an intermediate structure constructed entirely from 
#' the downloaded files, it saves us keeping copies of the entire 
#' lists (~8 vs 230Mb)
#' 
#' 
#' @export
#'
austraits_rebuild_taxon_list <- function() {

  taxonomic_resources <- load_taxonomic_resources()
  
  austraits <- remake::make("austraits_raw")

  # First align to APC where possible 
  taxa <- 
    # build list fo observed species names
    austraits$traits %>% 
    select(cleaned_name = taxon_name) %>% 
    distinct() %>%
    # match our cleaned names against names in APC list
    left_join(by = "cleaned_name",
      taxonomic_resources$APC %>% filter(!grepl("sp\\.$", canonicalName)) %>% select(cleaned_name = canonicalName, taxonIDClean = taxonID, 
                     taxonomicStatusClean = taxonomicStatus, acceptedNameUsageID)
    ) %>%
    # Also add all accepted genera species, varieties etc
    bind_rows(
      taxonomic_resources$APC %>% filter(taxonRank %in% c('Series', 'Genus', 'Species', 'Forma', 'Varietas'), taxonomicStatus == "accepted") %>% select(cleaned_name = canonicalName, taxonIDClean = taxonID, taxonomicStatusClean = taxonomicStatus, acceptedNameUsageID)
      ) %>%
    distinct() %>%
    mutate(source = ifelse(!is.na(taxonIDClean), "APC", NA)) %>% 
    # Now find accepted names for each name in the list (sometimes they are the same)
    left_join(by = "acceptedNameUsageID",
      taxonomic_resources$APC %>% 
      filter(taxonomicStatus =="accepted") %>% 
      select(acceptedNameUsageID, taxon_name = canonicalName, taxonomicStatus, scientificNameAuthorship, 
             family, taxonDistribution, taxonRank, ccAttributionIRI)
    ) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by,m with accepted at the top
    mutate(my_order = taxonomicStatusClean %>% 
             forcats::fct_relevel( c("accepted", "taxonomic synonym", "basionym", "nomenclatural synonym", "isonym", 
                                     "orthographic variant", "common name", "doubtful taxonomic synonym", "replaced synonym", 
                                     "misapplied", "doubtful pro parte taxonomic synonym", "pro parte nomenclatural synonym", 
                                     "pro parte taxonomic synonym", "pro parte misapplied", "excluded", "doubtful misapplied", 
                                     "doubtful pro parte misapplied"))) %>%
    arrange(cleaned_name, my_order) %>%
    # For each species, keep the first record (accepted if present) and 
    # record any alternative status to indicate where there was ambuguity
    group_by(cleaned_name) %>% 
    mutate(
      alternativeTaxonomicStatusClean = ifelse(taxonomicStatusClean[1] == "accepted", taxonomicStatusClean %>% unique() %>%  subset(. , . !="accepted") %>% paste0(collapse = " | ") %>% na_if(""), NA)
    ) %>%
    slice(1) %>%  
    ungroup() %>% 
    select(-my_order) %>% 
    select(cleaned_name, source, taxonIDClean, taxonomicStatusClean, alternativeTaxonomicStatusClean,
           acceptedNameUsageID, taxon_name, scientificNameAuthorship, taxonRank, taxonomicStatus, family, taxonDistribution,  
           ccAttributionIRI)

  taxa1 <- 
    taxa %>% filter(!is.na(taxonIDClean)) %>% 
    distinct() 
  
  # Now check against APNI for any species not found in APC
  # Only keep those species with a match

  taxa2 <-
    taxa %>% filter(is.na(taxon_name))  %>%
    select(cleaned_name) %>%
    left_join(by = "cleaned_name",
      taxonomic_resources$APNI %>% filter(nameElement != "sp.") %>% select(cleaned_name = canonicalName, taxonIDClean = scientificNameID, family, taxonRank)
    ) %>% group_by(cleaned_name) %>%
    mutate(
      taxonIDClean = paste(taxonIDClean, collapse = " ") %>% na_if("NA"),
      family = ifelse(n_distinct(family) > 1, NA, family[1])
    ) %>%
    ungroup() %>%
    mutate(
      source =ifelse(is.na(taxonIDClean), NA, "APNI"),
      taxon_name = ifelse(is.na(taxonIDClean), NA, cleaned_name),
      taxonomicStatusClean = ifelse(is.na(taxonIDClean), "unknown", "unplaced"),
      taxonomicStatus = taxonomicStatusClean
    )

  taxa_all <- 
    taxa1 %>% 
    bind_rows(
      taxa2 %>% filter(!is.na(taxonIDClean))
    ) %>%
    arrange(cleaned_name) 
  
  taxa_all %>%
    write_csv("config/taxon_list.csv")
}


#' Title
#'
#' @param taxon_name 
#' @param dist 
#'
#' @return
#' @export
#'
#' @examples
find_names_distance_to_neighbours <- function(taxon_name, dist=5) {

  # index of species to check
  n <- seq_len(length(taxon_name))
  
  # for each value in n, build an index of i-dist, i+dist, but not <=0, or > length(n)
  ii <- lapply(n, function(i) i + c(-dist:-1, 1:dist))
  for(i  in 1:dist)
    ii[[i]] <- ii[[i]][ii[[i]] > 0 & ii[[i]] !=i ] 
  for(i in (length(n) - 0:dist))
    ii[[i]] <- ii[[i]][ii[[i]] <= length(n) & ii[[i]] !=i ] 

  # now check every species against nearby species, get distance in chars
  unlist(lapply(n, function(i) min(adist(taxon_name[i], taxon_name[ii[[i]]]))))
}


#' Title
#'
#' @export
#'
#' @examples
austraits_run_tests <- function() {
  library(testthat)

  root.dir <- rprojroot::find_root("remake.yml")

  pwd <- setwd(root.dir)
  on.exit(setwd(pwd))

  if(!exists("dataset_ids") || is.null(dataset_ids)) {
    stop("The variable `dataset_ids` must be defined in the global namespace for the test suite to work")
  }
  
  testthat::test_dir("tests", reporter = default_reporter())
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
austraits_rebuild_remake_setup <- function( ) {

  library(whisker)

  root.dir <- rprojroot::find_root("austraits.build.Rproj")

  pwd <- setwd(root.dir)
  on.exit(setwd(pwd))
  dataset_ids <- dir("data")

  # check directories have both files

  has_both_files <- sapply(dataset_ids, function(id) sprintf("data/%s/%s", id, c("data.csv", "metadata.yml") ) %>% file.exists() %>% all())

  dataset_ids <- dataset_ids[has_both_files]

  vals <- list(dataset_ids=iteratelist(dataset_ids, value="dataset_id"))

  str <- whisker.render(readLines("scripts/remake.yml.whisker"), vals)
  writeLines(str, "remake.yml")
}



