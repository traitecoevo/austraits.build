
#' Check taxa against list of known species
#' 
#' Checks all taxa within against our list of known species
#' If not found, and update=TRUE, checks the unknown species against
#'
#' @param max_distance_abs numerical value for absolute max distance, default = 3
#' @param max_distance_rel numerical value for relative max distance, default = 0.2
#' @param try_outside_guesses logical value, default = FALSE
#' @param author name of author
#' @param dataset_id identifier for a particular study in the AusTraits database
#'
#' @importFrom rlang .data
#' @export
metadata_check_taxa <- function(dataset_id, 
                                max_distance_abs = 3, max_distance_rel = 0.2,
                                try_outside_guesses = FALSE,
                                author = git2r::config()$global$user.name) {
  
  
  cat("Checking alignments in ", crayon::red(dataset_id), "\n")
  
  x <- remake::make(dataset_id)
  taxa <- remake::make("taxon_list") %>% 
    dplyr::mutate(stripped_name = strip_names(cleaned_name))

  species <- 
    x$traits %>% dplyr::select(.data$original_name, .data$taxon_name) %>% dplyr::distinct() %>% 
    dplyr::mutate(
      known = .data$taxon_name %in% taxa$cleaned_name
    )

  if(all(species$known)){
    message(crayon::red("All taxa are already known\n"))
    return(invisible(NULL));   
  }
  
  # check unknown taxa
  cat(crayon::red(sum(species$known)), " names already matched; ")
  
  if(sum(!species$known) == 0 )
  
  cat(crayon::red(sum(!species$known)), " taxa are not yet matched\n")
  
  species <- species %>% dplyr::filter(!.data$known)
  
  # Check if existing substitution in metadata
  metadata <- read_metadata(file.path("data", dataset_id, "metadata.yml"))

  if(!all(is.null(metadata$taxonomic_updates)) && !is.na(metadata$taxonomic_updates)) {
    metata_changes <- 
      metadata$taxonomic_updates %>% util_list_to_df2() 
    
    species <- species %>% 
        dplyr::mutate(
          known = .data$original_name %in% metata_changes$find
        )
    
    if(any(species$known)) {
      cat(crayon::red(sum(species$known)), " of these already have substitutions in metadata:\n")
      tmp <- metata_changes %>% dplyr::filter(.data$find %in% (species %>% dplyr::filter(.data$known) %>% dplyr::pull(.data$original_name)))
      for(i in seq_along(tmp$find))
        cat(sprintf("\t%s -> %s (%s)\n", crayon::blue(tmp$find[i]), crayon::green(tmp$replace[i]), tmp$reason[i]))
      species <- species %>% dplyr::filter(!.data$known)
    }
  }

  species <- species$original_name[!species$known]
  
  if(length(species)==0) return(invisible());
  
  cat(crayon::red(length(species)), " species are not yet matched, checking for close matches in APC & APNI \n")
  
  taxonomic_resources <- load_taxonomic_resources()
  
  genera_accepted <-  taxonomic_resources$APC %>% dplyr::filter(.data$taxonRank %in% c('Genus'), .data$taxonomicStatus == "accepted") 
    
  to_check <- list()
  to_review <- tibble::tibble(dataset_id = character(), taxon_name = character())

  APC_tmp <- 
    taxonomic_resources$APC %>% 
    dplyr::filter(.data$taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
    dplyr::select(.data$canonicalName, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID) %>% 
    dplyr::mutate(
      stripped_canonical = strip_names(.data$canonicalName),
      stripped_scientific = strip_names(.data$scientificName)
      ) %>%
    dplyr::distinct()

  to_check[["APC list (accepted)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus == "accepted")
  to_check[["APC list (known names)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus != "accepted")
  
  to_check[["APNI names"]] <- 
    taxonomic_resources$APNI %>% dplyr::filter(.data$nameElement != "sp.") %>% 
    dplyr::select(.data$canonicalName, .data$scientificName, ID = .data$scientificNameID) %>% 
    dplyr::mutate(taxonomicStatus = "unplaced", 
            stripped_canonical = strip_names(.data$canonicalName),
            stripped_scientific = strip_names(.data$scientificName)
            ) %>%
    dplyr::distinct() %>% dplyr::arrange(.data$canonicalName)

  for(s in species) {
    
      cleaned_name <- process_standardise_names(s)
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
          found <- metadata_add_taxonomic_change(dataset_id, s,
                                                to_check[[v]]$canonicalName[match(s, to_check[[v]]$scientificName)], 
                                                sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break;
        } else if(stripped_name %in% to_check[[v]]$stripped_canonical) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
            to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_canonical)], 
            sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
            )
          break;
        } else if(stripped_name %in% to_check[[v]]$stripped_scientific) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
                                                to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_scientific)], 
                                                sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break;
        } else {
          distance_c <- utils::adist(stripped_name, to_check[[v]]$stripped_canonical, fixed=TRUE)[1,]
          min_dist_abs_c <-  min(distance_c)
          min_dist_per_c <-  min(distance_c) / stringr::str_length(stripped_name)

          distance_s <- utils::adist(stripped_name, to_check[[v]]$stripped_scientific, fixed=TRUE)[1,]
          min_dist_abs_s <-  min(distance_s)
          min_dist_per_s <-  min(distance_s) / stringr::str_length(stripped_name)
     
          if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs & 
            ## Within allowable number of characters (relative) 
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
            ) {
              found <- 
                metadata_add_taxonomic_change(dataset_id, s, 
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
              metadata_add_taxonomic_change(dataset_id, s, 
                                           to_check[[v]]$canonicalName[which(distance_s==min_dist_abs_s)], 
                                           sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
              )
          } else if(try_outside_guesses) {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            closest_names <- to_check[[v]]$canonicalName[j]

            cat(sprintf("\nFor %s - are any of these names from %s appropriate?\n",  crayon::blue(s), v))
            tmp <- menu(c("None", sprintf("%s -- %s -- %s", crayon::green(closest_names), to_check[[v]]$taxonomicStatus[j], to_check[[v]]$ID[j])))
            if(tmp > 1){
              found <- 
                metadata_add_taxonomic_change(dataset_id, s,  closest_names[tmp-1], 
                    sprintf("Alignment with known name in %s (%s, %s)", v, author, Sys.Date()))
              }
          } else {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            
            to_review <- 
              dplyr::bind_rows(to_review, 
                      tibble::tibble(dataset_id = dataset_id, source = v,
                             taxon_name = s, closest_names = to_check[[v]]$canonicalName[j], status = to_check[[v]]$taxonomicStatus[j], ID = to_check[[v]]$ID[j], 
                             genus_known = genus %in% genera_accepted$canonicalName,
                             keep = 0, reason = sprintf("Alignment with known name in %s (%s, %s)", v, author, Sys.Date()))
                      )
            
            if(v == dplyr::last(names(to_check))){
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
    readr::write_csv(to_review, filename)
    cat(sprintf("Review further suggestions for these taxa in %s\n", 
                crayon::green(filename)))
  }
    
  cat("After adding substitutions you should consider rebuilding taxon list with ", 
      crayon::blue("austraits_rebuild_taxon_list()"), "\n\n")
}

#' Load taxonomic resources from the APC and APNI
#' 
#' Load taxonomic resources from the Australian Plant Census and the Australian 
#' Plant Name Index. Taxonomic resources are stored as csv files in the NSL folder
#'
#' @param path_apc location of downloaded APC taxon file
#' @param path_apni location of downloaded APNI name file
#'
#' @export
load_taxonomic_resources <- function(path_apc = "config/NSL/APC-taxon-2020-05-14-1332.csv", 
                                     path_apni = "config/NSL/APNI-names-2020-05-14-1341.csv") {
  
  file_paths <- list(
    APC = path_apc,
    APNI = path_apni
  )

  if(!all(file.exists(unlist(file_paths)))) {
    for(i in seq_along(file_paths)) {
      if(!file.exists(file_paths[[i]])) 
        cat("file missing: ", file_paths[[i]],"\n")
    }
    stop("Need to download taxonomic resources to proceed")
  }

  if(!exists("taxonomic_resources",  envir = .GlobalEnv)) {
    message(crayon::red("loading object `taxonomic_resources` into global environment"))
    taxonomic_resources <- list()
    taxonomic_resources$APC <- read_csv_char(file_paths$APC)
    taxonomic_resources$APNI <- read_csv_char(file_paths$APNI) %>% distinct(.data$canonicalName, .keep_all = TRUE)
    assign("taxonomic_resources", taxonomic_resources, envir = .GlobalEnv)
  } 
  
  get0("taxonomic_resources", envir = .GlobalEnv)
}

#' Builds list of potential species from the Australian Plant Census (APC) and 
#' Australian Plant Names Index (APNI)
#' 
#' Compiled list is saved at "config/taxon_list.csv". While this list is 
#' only an intermediate structure constructed entirely from 
#' the downloaded files, it saves us keeping copies of the entire 
#' lists (~8 vs 230Mb)
#' 
#' @param austraits austraits data object
#' @importFrom rlang .data
#' @export
austraits_rebuild_taxon_list <- function(austraits) {

  taxonomic_resources <- load_taxonomic_resources()
  
  subset_accepted <- function(x) {
    x[x!= "accepted"]
  }

  # First align to APC where possible 
  taxa <- 
    # build list of observed species names
    austraits$traits %>% 
    dplyr::select(cleaned_name = .data$taxon_name) %>% 
    dplyr::distinct() %>%
    # match our cleaned names against names in APC list
    dplyr::left_join(
      by = "cleaned_name", taxonomic_resources$APC %>% 
        dplyr::filter(!grepl("sp\\.$", .data$canonicalName)) %>% 
        dplyr::select(cleaned_name = .data$canonicalName, taxonIDClean = .data$taxonID, 
                      taxonomicStatusClean = .data$taxonomicStatus, .data$acceptedNameUsageID)) %>%
    # Also add all accepted genera species, varieties etc
    dplyr::bind_rows(
      taxonomic_resources$APC %>% 
        dplyr::filter(.data$taxonRank %in% c('Series', 'Genus', 'Species', 'Forma', 'Varietas'), 
                      .data$taxonomicStatus == "accepted") %>% 
        dplyr::select(cleaned_name = .data$canonicalName, taxonIDClean = .data$taxonID, 
                      taxonomicStatusClean = .data$taxonomicStatus, .data$acceptedNameUsageID)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(source = ifelse(!is.na(.data$taxonIDClean), "APC", NA_character_)) %>% 
    # Now find accepted names for each name in the list (sometimes they are the same)
    dplyr::left_join(
      by = "acceptedNameUsageID", taxonomic_resources$APC %>% 
        dplyr::filter(.data$taxonomicStatus =="accepted") %>% 
        dplyr::select(.data$acceptedNameUsageID, taxon_name = .data$canonicalName, 
                      .data$taxonomicStatus, .data$scientificNameAuthorship, .data$family, 
                      .data$taxonDistribution, .data$taxonRank, .data$ccAttributionIRI)) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by,m with accepted at the top
    dplyr::mutate(my_order = .data$taxonomicStatusClean %>% 
             forcats::fct_relevel(c("accepted", "taxonomic synonym", "basionym", "nomenclatural synonym", "isonym", 
                                    "orthographic variant", "common name", "doubtful taxonomic synonym", "replaced synonym", 
                                    "misapplied", "doubtful pro parte taxonomic synonym", "pro parte nomenclatural synonym", 
                                    "pro parte taxonomic synonym", "pro parte misapplied", "excluded", "doubtful misapplied", 
                                    "doubtful pro parte misapplied"))) %>%
    dplyr::arrange(.data$cleaned_name, .data$my_order) %>%
    # For each species, keep the first record (accepted if present) and 
    # record any alternative status to indicate where there was ambiguity
    dplyr::group_by(.data$cleaned_name) %>% 
    dplyr::mutate(
      alternativeTaxonomicStatusClean = ifelse(.data$taxonomicStatusClean[1] == "accepted", 
                                               .data$taxonomicStatusClean %>% 
          unique() %>% 
          subset_accepted() %>% 
          paste0(collapse = " | ") %>% 
          dplyr::na_if(""), NA_character_)) %>% 
    dplyr::slice(1) %>%  
    dplyr::ungroup() %>% 
    dplyr::select(-.data$my_order) %>% 
    dplyr::select(.data$cleaned_name, .data$source, .data$taxonIDClean, .data$taxonomicStatusClean, 
                  .data$alternativeTaxonomicStatusClean, .data$acceptedNameUsageID, 
                  .data$taxon_name, .data$scientificNameAuthorship, .data$taxonRank, 
                  .data$taxonomicStatus, .data$family, .data$taxonDistribution, .data$ccAttributionIRI)

  taxa1 <- 
    taxa %>% dplyr::filter(!is.na(.data$taxonIDClean)) %>% 
    dplyr::distinct() 
  
  # Now check against APNI for any species not found in APC
  # Only keep those species with a match

  taxa2 <-
    taxa %>% 
    dplyr::filter(is.na(.data$taxon_name)) %>% 
    dplyr::select(.data$cleaned_name) %>%
    dplyr::left_join(by = "cleaned_name", taxonomic_resources$APNI %>% 
                       dplyr::filter(.data$nameElement != "sp.") %>%
                       dplyr::select(cleaned_name = .data$canonicalName, taxonIDClean = .data$scientificNameID, 
                                     .data$family, .data$taxonRank)) %>% 
    dplyr::group_by(.data$cleaned_name) %>%
    dplyr::mutate(
      taxonIDClean = paste(.data$taxonIDClean, collapse = " ") %>% 
        dplyr::na_if("NA"), family = ifelse(dplyr::n_distinct(.data$family) > 1, NA_character_, .data$family[1])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      source = as.character(ifelse(is.na(.data$taxonIDClean), NA_character_, "APNI")),
      taxon_name = as.character(ifelse(is.na(.data$taxonIDClean), NA_character_, .data$cleaned_name)),
      taxonomicStatusClean = as.character(ifelse(is.na(.data$taxonIDClean), "unknown", "unplaced")),
      taxonomicStatus = as.character(.data$taxonomicStatusClean))

  taxa_all <- taxa1 %>% 
    dplyr::bind_rows(taxa2 %>% 
        dplyr::filter(!is.na(.data$taxonIDClean))) %>% 
        dplyr::distinct(.data$cleaned_name, .keep_all = TRUE) %>%
    arrange(.data$cleaned_name) 
  
  taxa_all %>%
    readr::write_csv("config/taxon_list.csv")
}
