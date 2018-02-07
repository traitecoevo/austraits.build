

# function to add a substitution into a yaml file for a study
add_substitution <- function(study, trait_name, find, replace) {

  set_name <- "substitutions"

  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  to_add <- list(trait_name = trait_name, find = find, replace = replace) 

  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(length(which(trait_name %in% data$trait_name && find %in% data$find)) > 0) {
    stop("Substitution exists for that variable, please update manually in ", filename_metadata)
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)
  write_yaml(metadata, filename_metadata)
}



# function to add a taxonomic change into a yaml file for a study
add_taxnomic_change <- function(study, find, replace, reason) {

  set_name <- "taxonomic_updates"
  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  to_add <- list(find = find, replace = replace, reason = reason) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(nrow(data) >0 && length(which(find %in% data$find)) > 0) {
    stop("Substitution exists for that variable, please update manually in ", filename_metadata)
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)
  write_yaml(metadata, filename_metadata)
}
