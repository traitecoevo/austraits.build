require(tidyverse)


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
  if(length(metadata[[set_name]]) > 0 & 
     length(which(trait_name %in% data$trait_name & find %in% data$find)) > 0) {
    stop(sprintf("Substitution exists for %s - %s, please update manually in %s", trait_name, find, filename_metadata))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("%s %s for trait %s : %s -> %s", crayon::red("Adding substitution in"), crayon::red(study), trait_name, find, replace))
  write_yaml(metadata, filename_metadata)
}



# function to add a taxonomic change into a yaml file for any study where species occurs
add_taxnomic_change_all_studies <- function(find, replace, reason) {
  studies <- find_species(find)
  for(s in studies)
    add_taxnomic_change(s, find, replace, reason)
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
  if(!is.na(data) && nrow(data) > 0 && length(which(find %in% data$find)) > 0) {
    stop(crayon::red(sprintf("Substitution exists for %s, please update manually in %s", find, filename_metadata)))
  }

  metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

  message(sprintf("%s %s: %s -> %s (%s)", crayon::red("Adding taxonomic change in"), crayon::red(study), crayon::blue(find), crayon::green(replace), reason))
  write_yaml(metadata, filename_metadata)
}


# function to update a taxonomic change into a yaml file for any study where species occurs
update_taxnomic_change_all_studies <- function(find, replace, reason) {
  studies <- find_taxnomic_change(find)
  for(s in studies)
    update_taxnomic_change(s, find, replace, reason)
}


# function to update a substitution into a yaml file for a study
update_taxnomic_change <- function(study, find, replace, reason) {

  set_name <- "taxonomic_updates"

  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  to_add <- list(find = find, replace = replace, reason = reason) 

  data <-  list_to_df(metadata[[set_name]]) 
  i <- match(find, data$find)
  # add `set_name` category if it doesn't yet exist
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]]) || nrow(data) == 0 || length(i) == 0) {
    stop(sprintf("Substitution for %s in %s  does not exist", find, filename_metadata))
  }

  metadata[[set_name]][[i]][["replace"]] <- replace
  metadata[[set_name]][[i]][["reason"]] <- reason
  message(sprintf("%s %s: %s -> %s (%s)", crayon::red("Updating taxonomic change in"),crayon::red(study), crayon::blue(find), crayon::green(replace), reason))

  write_yaml(metadata, filename_metadata)
}

#update_taxnomic_change("Angevin_2010", "Elymus scaber", "newname", "newreason")

# function to remove a taxonomic change from a yaml file for a study
remove_taxnomic_change <- function(study, find, replace=NULL) {

  set_name <- "taxonomic_updates"
  filename_metadata <- file.path("data", study,  "metadata.yml")
  metadata <- read_yaml(filename_metadata)

  # if it doesn't yet exist - > done
  if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", study, find, replace, crayon::green("does not exist")))
    return()
  }

  # Check if find record already exists for that trait
  data <-  list_to_df(metadata[[set_name]])  
  if(nrow(data) == 0) {
    message(sprintf("Taxonomic change in %s: %s -> %s %s", study, find, replace, crayon::green("does not exist")))
    return()
  }

  if(is.null(replace))
    i <-data$find == find
  else
    i <- data$find == find & data$replace == replace


  if(any(i)) {
    metadata[[set_name]][which(i)] <- NULL
    message(sprintf("Taxonomic change in %s: %s -> %s %s", study, find, replace, crayon::red("removed")))
  }

  write_yaml(metadata, filename_metadata)
}

# List all studies in the data directory
list_studies <- function(){
  dirname(dir("data", pattern="metadata.yml", recursive = TRUE))
}

get_metadata_files <- function(studies = NULL){
  if(is.null(studies))
    studies <- list_studies()
  file.path("data", studies, "metadata.yml")
}

find_species <- function(species_name, original_name = FALSE){

  data <- remake::make("austraits")$data 

  if(!original_name)
    data <- data %>% select(name =  species_name, study) %>% distinct()
  else
    data <- data %>% select(name =  original_name, study) %>% distinct()

  f <- function(sp)  filter(data, name == sp) %>%  pull(study) %>% unique()

  if(length(species_name) == 1) 
    f(species_name)
  else
    lapply(species_name, f) 
}

find_taxnomic_change <- function(find, replace=NULL, studies = NULL){

  if(is.null(studies))
    studies <- list_studies()

  f <- get_metadata_files(studies)

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

check_study_taxa <- function(study, update=TRUE, typos=FALSE, diffchar = 2) {
  
  x <- remake::make(study)
  accepted <- read_csv("config/species_list.csv", col_types = cols(.default = "c"))
  species <- unique(x$data$species_name)
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

add_to_accepted <- function(accepted, to_add) {

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


run_tests <- function() {
  library(testthat)

  root.dir <- rprojroot::find_root("remake.yml")

  pwd <- setwd(root.dir)
  on.exit(setwd(pwd))

  if(!exists("dataset_ids") || is.null(dataset_ids)) {
    stop("The variable `dataset_ids` must be defined in the global namespace for the test suite to work")
  }
  
  testthat::test_dir("tests", reporter = default_reporter())
}

rebuild_remake_setup <- function( ) {

  library(whisker)

  root.dir <- rprojroot::find_root("austraits.Rproj")

  pwd <- setwd(root.dir)
  on.exit(setwd(pwd))
  study_names <- dir("data")
  vals <- list(study_names=iteratelist(study_names, value="study_name"))

  str <- whisker.render(readLines("scripts/remake.yml.whisker"), vals)
  writeLines(str, "remake.yml")
}


