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
    v1 <- c("species_name", "trait_name", "value")
    v2 <- c("site_name", "observation_id")
  } else {
    v1 <- c("species_name")
    v2 <- c("site_name")
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
#' 
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
#' @inheritParams metadata_path_dataset_id metadata_add_source_bibtex
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


# function to add a taxonomic change into a yaml file for any dataset_id where species occurs
metadata_add_taxnomic_change_all_studies <- function(find, replace, reason) {
  studies <- austraits_find_species(find)
  for(s in studies)
    metadata_add_taxnomic_change(s, find, replace, reason)
}

# function to add a taxonomic change into a yaml file for a dataset_id
metadata_add_taxnomic_change <- function(dataset_id, find, replace, reason) {

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
    stop(crayon::red(sprintf("Substitution exists for %s, please update manually in %s", find, filename_metadata)))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("%s %s: %s -> %s (%s)", crayon::red("Adding taxonomic change in"), crayon::red(dataset_id), crayon::blue(find), crayon::green(replace), reason))
  metadata_write_dataset_id(metadata, dataset_id)
}


# function to update a taxonomic change into a yaml file for any dataset_id where species occurs
metadata_update_taxnomic_change_all_studies <- function(find, replace, reason) {
  studies <- metadata_find_taxnomic_change(find)
  for(s in studies)
    metadata_update_taxnomic_change(s, find, replace, reason)
}


# function to update a substitution into a yaml file for a dataset_id
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

#metadata_update_taxnomic_change("Angevin_2010", "Elymus scaber", "newname", "newreason")

# function to remove a taxonomic change from a yaml file for a dataset_id
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

austraits_find_species <- function(species_name, original_name = FALSE){

  data <- austraits$traits

  if(!original_name)
    data <- data %>% select(name =  species_name, dataset_id) %>% distinct()
  else
    data <- data %>% select(name =  original_name, dataset_id) %>% distinct()

  f <- function(sp)  filter(data, name == sp) %>%  pull(dataset_id) %>% unique()

  if(length(species_name) == 1) 
    f(species_name)
  else
    lapply(species_name, f) 
}

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

# checks all taxa within against our list of known species
# If not found, and update=TRUE, checks the unknown species against

metadata_check_taxa <- function(dataset_id, update=TRUE, typos=FALSE, diffchar = 2) {
  
  x <- remake::make(dataset_id)
  accepted <- read_csv("config/species_list.csv", col_types = cols(.default = "c"))
  species <- unique(x$traits$species_name)
  i <- species %in% accepted$species_name

  if(all(i)){
    message(crayon::red("All species are known"))
    return();   
  }
  
  # check unknown species in TaxonStand
  message(crayon::red("Following species are not yet in our list: "), paste0(species[!i], collapse = ", "))
  
  if(!update) return();   

  message(crayon::red("Checking for unknown species"))

  if(typos)
    tpl <- check_taxonstand(species[!i], corr = TRUE, diffchar=diffchar)
  else
    tpl <- check_taxonstand(species[!i], corr = FALSE)


  # Species already known, name needs substituting
  tpl2 <- tpl %>% 
          format_tpl_to_accepted_df() %>% 
          mutate(accepted_i = match(TPL_ID, accepted$TPL_ID)) %>%
          filter(!is.na(accepted_i))

  existed <- nrow(tpl2)

  for(i in seq_len(existed)) {
    metadata_add_taxnomic_change(dataset_id, tpl2$species_name[i], accepted$species_name[tpl2$accepted_i[i]], 
            sprintf("Alignment with existing species identified by TaxonStand (%s)", Sys.Date()))
  }

  # Process remaining species
  tpl <- tpl %>% filter(!(ID %in% tpl2$TPL_ID))

  keep <- tpl$Taxonomic.status %in% c("Accepted", "Unresolved") & tpl$Typo == FALSE
  synonym <- tpl$Taxonomic.status %in% c("Synonym") & tpl$Typo==FALSE
  typo1 <- tpl$Taxonomic.status %in% c("Accepted", "Unresolved") & tpl$Typo==TRUE
  typo2 <- tpl$Taxonomic.status %in% c("Synonym") & tpl$Typo==TRUE
  unknown <- tpl$Taxonomic.status %in% c("")

  message(sprintf("For %s species: %d existed, %d known, %d synonyms, %d typos, %d unknown", dataset_id, existed, sum(keep), sum(synonym), sum(typo1) + sum(typo2), sum(unknown))) 

  # Add any known species
  if(any(keep)) { 

    to_add <- format_tpl_to_accepted_df(tpl[keep,])
    accepted <- austraits_add_to_accepted_species(accepted, to_add[!already_exists,])
  }

  # For known synonyms, add to a replacement and check synonym is in list of known species
  if(any(synonym)){

    data <- tpl[synonym,]
    to_add <- format_tpl_to_accepted_df(data, use.new= TRUE)
  
    for(i in seq_len(nrow(data))) {
      metadata_add_taxnomic_change(dataset_id, data$Taxon[i], to_add$species_name[i], sprintf("Synonym reported by TaxonStand (%s)", Sys.Date())) 
    }
  
    accepted <- austraits_add_to_accepted_species(accepted, to_add)
  }

  # For Typos, review if flagged 
  if(typos & any(typo1)) {
   
    data <- tpl[typo1,]
    to_add <- format_tpl_to_accepted_df(data, use.new= TRUE)

    for(i in seq_len(nrow(data))) {
      metadata_add_taxnomic_change(dataset_id, data$Taxon[i], to_add$species_name[i], sprintf("Spelling mistake identified by TaxonStand (%s)", Sys.Date())) 
    }

    accepted <- austraits_add_to_accepted_species(accepted, to_add)
  } 

  if(typos & any(typo2)) {
    # Check if we need to implement this
    message("Combination of Typo & Synonym not implemented. D Falster to review. Species: ", paste0(tpl[typo2,]$Taxon, collapse = ", "))
  }
  write_csv(accepted, "config/species_list.csv")
}


check_taxonstand <- function(species, corr = FALSE, ...){
  Taxonstand::TPL(species, corr = corr, ...) 
}

align_tpl <- function(species) {
  tpl <- check_taxonstand(species, FALSE)
  species_name <- species
  i <- tpl$New.ID != ""
  species_name[i] <- tpl %>% filter(i) %>% format_tpl_species_name()

  data.frame(original_name = species, species_name = species_name, TPL_ID = tpl$New.ID, stringsAsFactors = FALSE) %>% tbl_df()
}

format_tpl_species_name <- function(tpl) {

  species <- paste(tpl$New.Genus, tpl$New.Species, 
                       tpl$New.Infraspecific.rank,  tpl$New.Infraspecific)
  i <- tpl$New.Infraspecific.rank == ""
  species[i] <- paste(tpl$New.Genus, tpl$New.Species)[i]
  species
}

format_tpl_to_accepted_df <- function(tpl, use.new = FALSE){

  if(use.new) {
    tpl$Taxon <- paste(tpl$New.Genus, tpl$New.Species, 
                       tpl$New.Infraspecific.rank,  tpl$New.Infraspecific)
    i <- tpl$New.Infraspecific.rank == ""
    tpl$Taxon[i] <- paste(tpl$New.Genus, tpl$New.Species)[i]
    tpl$Taxonomic.status <- tpl$New.Taxonomic.status
  }

  tpl %>% 
     select(species_name = Taxon, family = Family, 
     authority = New.Authority, TPL_ID = ID, status = Taxonomic.status) %>%
     mutate(APC_name = "unknown", APC_ID = "unknown", APNI_ID = "unknown") 
}

austraits_add_to_accepted_species <- function(accepted, to_add) {

  i <- !(to_add$species_name %in% accepted$species_name)

  if(any(i)){ 
    message(crayon::red("Adding species to list of known species: "), 
          paste0(to_add$species_name[i], collapse = ", "))

    accepted <- bind_rows(accepted, to_add[i,]) %>% arrange(species_name)
  }
  accepted
}


find_names_distance_to_neighbours <- function(species_name, dist=5) {

  # index of species to check
  n <- seq_len(length(species_name))
  
  # for each value in n, build an index of i-dist, i+dist, but not <=0, or > length(n)
  ii <- lapply(n, function(i) i + c(-dist:-1, 1:dist))
  for(i  in 1:dist)
    ii[[i]] <- ii[[i]][ii[[i]] > 0 & ii[[i]] !=i ] 
  for(i in (length(n) - 0:dist))
    ii[[i]] <- ii[[i]][ii[[i]] <= length(n) & ii[[i]] !=i ] 

  # now check every species against nearby species, get distance in chars
  unlist(lapply(n, function(i) min(adist(species_name[i], species_name[ii[[i]]]))))
}


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

austraits_rebuild_remake_setup <- function( ) {

  library(whisker)

  root.dir <- rprojroot::find_root("austraits.Rproj")

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



