

# function to add a substitution into a yaml file for a study
add_substitution <- function(study, trait_name, find, replace) {

  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  to_add <- list(trait_name = trait_name, find = find, replace = replace) 

  # add `substitutions` category if it doesn't yet exist
  if(is.null(metadata[["substitutions"]]) || is.na(metadata[["substitutions"]])) {
    metadata[["substitutions"]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[["substitutions"]])  
  if(length(which(trait_name %in% data$trait_name && find %in% data$find)) > 0) {
    stop("Substitution exists for that variable, please update manually in ", filename_metadata)
  }

  metadata[["substitutions"]] <- append_to_list(metadata[["substitutions"]], to_add)
  write_yaml(metadata, filename_metadata)
}
