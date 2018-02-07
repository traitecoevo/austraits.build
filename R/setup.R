

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
    stop(sprintf("Substitution exists for %s - %s, please update manually in %s", trait_name, find, filename_metadata))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("Adding substitution in %s for trait %s : %s -> %s", study, trait_name, find, replace))
  write_yaml(metadata, filename_metadata)
}



# function to add a taxonomic change into a yaml file for a study
add_taxnomic_change <- function(study, find, replace, reason) {

  set_name <- "taxonomic_updates"
  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  to_add <- list(find = find, replace = replace, reason = reason) 
    
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    metadata[[set_name]] <- list()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(nrow(data) >0 && length(which(find %in% data$find)) > 0) {
    stop(sprintf("Substitution exists for %s, please update manually in %s", find, filename_metadata))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("Adding taxonomic change in %s: %s -> %s (%s)", study, blue(find), green(replace), reason))
  write_yaml(metadata, filename_metadata)
}



# checks all taxa within against our list of known species
# If not found, and update=TRUE, checks the unknown species against

check_study_taxa <- function(study, update=TRUE, typos=FALSE, diffchar = 2) {
  
  x <- remake::make(study)
  accepted <- read_csv("config/species_list.csv", col_types = cols(.default = "c"))
  species <- unique(x$data$species_name)
  i <- species %in% accepted$species_name

  if(all(i)){
    message("All species are known")
    return();   
  }
  
  # check unknown species in TaxonStand
  message("Following species are not yet in our list: ", paste0(species[!i], collapse = ", "))
  
  if(!update) return();   

  message("Checking for unknown species")

  if(typos)
    tpl <- check_taxonstand(species[!i], corr = TRUE, diffchar=diffchar)
  else
    tpl <- check_taxonstand(species[!i], corr = FALSE)

  keep <- tpl$Taxonomic.status %in% c("Accepted", "Unresolved") & tpl$Typo == FALSE
  synonym <- tpl$Taxonomic.status %in% c("Synonym") & tpl$Typo==FALSE
  typo1 <- tpl$Taxonomic.status %in% c("Accepted", "Unresolved") & tpl$Typo==TRUE
  typo2 <- tpl$Taxonomic.status %in% c("Synonym") & tpl$Typo==TRUE
  unknown <- tpl$Taxonomic.status %in% c("")

  message(sprintf("For %s species: %d known, %d synonyms, %d typos, %d unknown", study, sum(keep), sum(synonym), sum(typo1) + sum(typo2), sum(unknown))) 

  # Add any known species
  if(any(keep)) { 
    to_add <- format_tpl_to_accepted_df(tpl[keep,])
    accepted <- add_to_accepted(accepted, to_add)
  }

  # For known synonyms, add to a replacement and check synonym is in list of known species
  if(any(synonym)){

    data <- tpl[synonym,]
    to_add <- format_tpl_to_accepted_df(data, use.new= TRUE)
  
    for(i in seq_len(nrow(data))) {
      add_taxnomic_change(study, data$Taxon[i], to_add$species_name[i], sprintf("Synonym reported by TaxonStand (%s)", Sys.Date())) 
    }
  
    accepted <- add_to_accepted(accepted, to_add)
  }

  # For Typos, review if flagged 
  if(typos & any(typo1)) {
   
    data <- tpl[typo1,]
    to_add <- format_tpl_to_accepted_df(data, use.new= TRUE)

    for(i in seq_len(nrow(data))) {
      add_taxnomic_change(study, data$Taxon[i], to_add$species_name[i], sprintf("Spelling mistake identified by TaxonStand (%s)", Sys.Date())) 
    }

    accepted <- add_to_accepted(accepted, to_add)
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


format_tpl_to_accepted_df <- function(tpl, use.new = FALSE){

  if(use.new) {
    tpl$Taxon <- paste(tpl$New.Genus, tpl$New.Species)
    tpl$Taxonomic.status <- tpl$New.Taxonomic.status
  }

  tpl %>% 
     select(species_name = Taxon, TPL_ID = ID, family = Family, 
     authority = New.Authority, status = Taxonomic.status)# %>%
    #   mutate(APC_name = NA_character_, APNI_ID = NA_character_) 
}


add_to_accepted <- function(accepted, to_add) {

  i <- !(to_add$species_name %in% accepted$species_name)

  if(any(i)){ 
    message("Adding species to list of known species: ", 
          paste0(to_add$species_name[i], collapse = ", "))

    accepted <- bind_rows(accepted, to_add[i,]) %>% arrange(species_name)
  }
  accepted
}

