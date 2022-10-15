
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
    dplyr::mutate(
      stripped_name = strip_names(cleaned_name),
      trinomial = stringr::word(stripped_name, start = 1, end = 3),
      binomial = stringr::word(stripped_name, start = 1, end = 2),
      genus = stringr::word(stripped_name, start = 1, end = 1)
    )

  species <- 
    x$traits %>% dplyr::select(.data$original_name, .data$taxon_name) %>% dplyr::distinct() %>% 
    dplyr::mutate(
      known = .data$taxon_name %in% taxa$cleaned_name,
      resolution = "taxonRank"
    )

  if(all(species$known)){
    message(crayon::red("All taxa are already known\n"))
    return(invisible(NULL));   
  }
  
  # check unknown taxa
  cat(crayon::red(sum(species$known)), " names already matched; ")
  
  if(sum(!species$known) == 0 )
    #break; in old version XXXX 

  cat(crayon::red(sum(!species$known)), " taxa are not yet matched\n")
  
  # XXXX instead of filter from the full species list to no species, is it possible
  # XXXX to keep flipping known to TRUE, but keep the whole list, so that we can assemble a list of "resolution rank" values
  # XXXX through processing? That will tell us how many of the columns `family`, genus, species, infraspecies
  # need to be filled in austraits$taxa for a given taxon_name
  
  species_to_check <- species
  species <- species %>% dplyr::filter(!.data$known)
  
  # Check if existing substitution in metadata
  metadata <- read_metadata(file.path("data", dataset_id, "metadata.yml"))

  if(!all(is.null(metadata$taxonomic_updates)) && !is.na(metadata$taxonomic_updates)) {
    metadata_changes <- 
      metadata$taxonomic_updates %>% util_list_to_df2() 

  # if a there is already a substitution for an unknown species, its status is flipped to `known`
  # XXXX we should add a parameter, `recheck` that allows these names to be rechecked
  # XXXX then, if `recheck = TRUE` & a match now exists, the only substitution is replaced with a new one.
  # XXXX this would avoid the necessity of revisiting and searching for taxon that have a temporary match in place
    
    species <- species %>% 
        dplyr::mutate(
          known = .data$original_name %in% metadata_changes$find
        )
    
    if(any(species$known)) {
      cat(crayon::red(sum(species$known)), " of these already have substitutions in metadata:\n")
      tmp <- metadata_changes %>% dplyr::filter(.data$find %in% (species %>% dplyr::filter(.data$known) %>% dplyr::pull(.data$original_name)))
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
  family_accepted <-  taxonomic_resources$APC %>% dplyr::filter(.data$taxonRank %in% c('Familia'), .data$taxonomicStatus == "accepted") 

  to_check <- list()
  to_review <- tibble::tibble(dataset_id = character(), taxon_name = character())
  

  APC_tmp <- 
    taxonomic_resources$APC %>% 
    dplyr::filter(.data$taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
    dplyr::select(.data$canonicalName, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>% 
    dplyr::mutate(
            stripped_canonical = strip_names(.data$canonicalName),
            stripped_scientific = strip_names(.data$scientificName),
            #trinomial = ifelse(taxonRank %in% c("Series", "Subspecies", "Species", "Forma", "Varietas") & nameType == "scientific" , 
            #                   stringr::word(stripped_canonical, start = 1, end = 3), NA),
            binomial = ifelse(nameType == "scientific", stringr::word(stripped_canonical, start = 1, end = 2), NA),
            genus = stringr::word(stripped_canonical, 1)
          ) %>%
    dplyr::distinct()

  to_check[["APC list (accepted)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus == "accepted")
  to_check[["APC list (known names)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus != "accepted")
  
  to_check[["APNI names"]] <- 
    taxonomic_resources$APNI %>% dplyr::filter(.data$nameElement != "sp.") %>% 
    dplyr::select(.data$canonicalName, .data$scientificName, ID = .data$scientificNameID, .data$nameType, .data$taxonRank) %>% 
    dplyr::mutate(taxonomicStatus = "unplaced for APC", 
            stripped_canonical = strip_names(.data$canonicalName),
            stripped_scientific = strip_names(.data$scientificName),
            #trinomial = ifelse(taxonRank %in% c("Series", "Subspecies", "Species", "Forma", "Varietas") & nameType == "scientific", 
            #                   stringr::word(stripped_canonical, start = 1, end = 3), NA),
            binomial = ifelse(nameType == "scientific", stringr::word(stripped_canonical, start = 1, end = 2), NA),
            genus = stringr::word(stripped_canonical, 1)
          ) %>%
    dplyr::distinct() %>% dplyr::arrange(.data$canonicalName)

  for(s in species) {
    
    cleaned_name <- process_standardise_names(s)
    stripped_name <- strip_names(cleaned_name)
    genus <-stringr::str_split(s, " ")[[1]][1]
    
    if (stringr::str_count(stripped_name, '\\w+') > 2) {
      trinomial <- stringr::word(stripped_name, start = 1, end = 3)
    } else {
      trinomial <- NA_character_
    }
    
    if (stringr::str_count(stripped_name, '\\w+') > 1) {
      binomial <- stringr::word(stripped_name, start = 1, end = 2)
    } else {
      binomial <- NA_character_
    }
    
    found <- FALSE

    # 1. Automatically reformat certain string patterns
          
      # If a name already ends in "sp.", check if it aligns to a genus or family
      # assign the appropriate taxon resolution.
      # XXXX TO DO: Add the dataset_id in square brackets to indicate whose "genus sp." this is
      # XXXX TO DO: Add not just dataset_id, but dataset_id_1, _2, etc. if there are multiple genus sp. in a dataset.
    
      # Indicate genera not being matched and omit these names from further searches

    if(stringr::str_detect(cleaned_name,"sp.$")) {
      cat(sprintf("\tSkipping %s - not assessing names ending in `sp.` Note, genus %s is %s in APC\n", 
                  crayon::blue(s), crayon::green(genus), 
                  ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
      
      binomial <- NA # set binomial to NA, since now only aligning to genus.
      trinomial <- NA # set trinomial to NA, since now only aligning to genus.z
      resolution <- ifelse(genus %in% genera_accepted$canonicalName, "genus", resolution)
      # searching for genus because that is the title we've given to the first word in the taxon name string
      resolution <- ifelse(genus %in% family_accepted$canonicalName & (!genus %in% genera_accepted$canonicalName), "family", resolution)
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", dataset_id, "]"),
          sprintf("Rewording taxon with ending with `sp.` to indicate a genus-level alignment (%s)", Sys.Date()))  
    }
      
    # Reword record where pattern is `genus aff. species` to `genus sp. [original name]` 
    # to clarify this taxon is only aligned to genus
    # stringr detects "aff." ; "aff " ; "affinis " - but not any aff without space, to avoid picking up starts of actual names
    
    if(stringr::str_detect(cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(cleaned_name, " affinis ")) {
      cat(sprintf("\tTaxon %s with `affinis` preceding species name and will be aligned to genus %s in APC\n", 
                crayon::blue(s), crayon::green(genus)))
      resolution <- ifelse(genus %in% genera_accepted$canonicalName, "genus", resolution)
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
        sprintf("Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment (%s)", v, Sys.Date()))  
    }

    # Reword record where pattern is `genus species/species` to `genus sp. [original name]` 
    # to clarify this taxon is only aligned to genus
    
    if(stringr::str_detect(cleaned_name, "[:alpha:]\\/")|stringr::str_detect(cleaned_name, "\\s\\/")) {
      cat(sprintf("\tSpecies identification uncertain and taxon %s will be aligned to genus %s in APC\n", 
                  crayon::blue(s), crayon::green(genus)))
      
      resolution <- ifelse(genus %in% genera_accepted$canonicalName, "genus", resolution)
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
        sprintf("Rewording taxon where `/` indicates uncertain species identification to align with genus (%s)", v, Sys.Date()))  
    }

    for(v in names(to_check))  {
        
        if(found) break;

    # 2. Indicate full names that match perfectly as submitted, 
        # cycling through APC accepted names,
        # APC known names (isonyms, synonyms), APNI names
      
      # check for successful match to canonical name (no author) 
        if(s %in% to_check[[v]]$canonicalName) {
          message(sprintf("%s found in %s", crayon::green(s), v))
          found <- TRUE
          break;
  
      # check for successful match to scientific name 
        } else if(s %in% to_check[[v]]$scientificName) {
          found <- metadata_add_taxonomic_change(
            dataset_id, s, 
            to_check[[v]]$canonicalName[match(s, to_check[[v]]$scientificName)], 
            sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break; 
          
      # match synonymous terms (taxonomic synonyms, isonyms, etc) among canonical names (no authorities)
        } else if(stripped_name %in% to_check[[v]]$stripped_canonical) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
            to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_canonical)], 
            sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break; 
          
      # match synonymous terms (taxonomic synonyms, isonyms, etc) among scientific names (with authorities)
        } else if(stripped_name %in% to_check[[v]]$stripped_scientific) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
                                                to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_scientific)], 
                                                sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())
          )
          break; 
      
      #Fifth, fuzzy matching based on selected absolute/relative distances
      # XXXX If the binomial or trinomial matches, can the appropriate`resolution` be recorded?
        } else {
          distance_c <- utils::adist(stripped_name, to_check[[v]]$stripped_canonical, fixed=TRUE)[1,]
          min_dist_abs_c <-  min(distance_c)
          min_dist_per_c <-  min(distance_c) / stringr::str_length(stripped_name)

          distance_s <- utils::adist(stripped_name, to_check[[v]]$stripped_scientific, fixed=TRUE)[1,]
          min_dist_abs_s <-  min(distance_s)
          min_dist_per_s <-  min(distance_s) / stringr::str_length(stripped_name)

          # only allow fuzzy matches if the first 4 letters of the genus are identical
          # This code works when run on its own, but not as part of the function
          # I 've tried lots of variants; if(allowed & ... ) seemed best, but still didn't run
          # XXXX TO DO - instead of specifying 4, this could be a parameter in the function? 
          allowed <- genus %>% stringr::str_extract("[:alpha:]{4}") ==  
             to_check[[v]]$canonicalName[which(distance_c==min_dist_abs_c)] %>% 
             stringr::str_extract("[:alpha:]{4}") %>%
             stringr::str_to_sentence()
            
     
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
                  sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())) 
              break;
          
              
          # Now checking trinomials and binomials
          # XXXX But it seems like this code is overwriting the main fuzzy matching which confuses me.
              
          } else if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs &
            ## Within allowable number of characters (relative)
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
          ) {
              found <-
                metadata_add_taxonomic_change(dataset_id, s, paste0(
                  to_check[[v]]$trinomial[which(distance_c==min_dist_abs_c)], "[", cleaned_name, "]"),
                  sprintf("Automatic alignment with trinomial when notes ignored, %s (%s)", v, Sys.Date()))
                  break;

          } else if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs &
            ## Within allowable number of characters (relative)
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
          ) {
              found <-
                metadata_add_taxonomic_change(dataset_id, s,
                  paste0(to_check[[v]]$binomial[which(distance_c==min_dist_abs_c), "[", stripped_name, "]"]),
                  sprintf("Automatic alignment when infraspecific information ignored, %s (%s)", v, Sys.Date()))
                  break;
              
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
                                           sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date()))
            break;
            # found <- 
            #   metadata_add_taxonomic_change(dataset_id, trinomial, 
            #                                 to_check[[v]]$trinomial[which(distance_s==min_dist_abs_s)], 
            #                                  sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())) 
            # break;
            # found <- 
            #   metadata_add_taxonomic_change(dataset_id, binomial, 
            #                                 to_check[[v]]$binomial[which(distance_s==min_dist_abs_s)], 
            #                                 sprintf("Automatic alignment with name in %s (%s)", v, Sys.Date())) 
            # break;
            
            # Manipulations for strings that only match to genus
            
            # Beginning a second set of string manipulations for taxon names that are not matched at the end of round 1 of fuzzy matching.
            # Taxa where the entire taxon name string, the first three words, and the first two words all fail to match
            # should now be matched to genus (or rarely family).
            # The `taxon name` itself should be reformatted to standardise it.
            
            
            
            # Align hybrid species to genus and reformat name
            } else if(stringr::str_detect(cleaned_name," [xX] ")) {
            cat(sprintf("\t%s is a hybrid species not in APC/APNI and has been aligned with to its genus %s \n", 
                        crayon::blue(s), crayon::green(genus))) 
            resolution <- ifelse(genus %in% genera_accepted$canonicalName, "genus", resolution)
            found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [",cleaned_name,"]"),
                                                   sprintf("Rewording hybrid species name to align with genus, %s (%s)", v, Sys.Date())
            )
            break; 
        
            # match first three words only - because sometimes a valid name + notes
            } else if(trinomial %in% to_check[[v]]$stripped_canonical) {
              resolution <- "trinomial"
              found <- metadata_add_taxonomic_change(dataset_id, s,
                                                     to_check[[v]]$canonicalName[match(trinomial, to_check[[v]]$stripped_canonical)] %>% stringr::str_to_sentence(), 
                                                     sprintf("Automatic alignment when notes ignored, %s (%s)", v, Sys.Date())
              )
              break;
              
              # match first two words only - because sometimes a valid name + notes  
            } else if(binomial %in% to_check[[v]]$binomial) {
              resolution <- "binomial"
              found <- metadata_add_taxonomic_change(dataset_id, s,
                                                     to_check[[v]]$binomial[match(binomial, to_check[[v]]$binomial)] %>% stringr::str_to_sentence(), 
                                                     sprintf("Automatic alignment when infraspecific information ignored, %s (%s)", v, Sys.Date())
              )
              break; 
            
            } else if(stringr::str_detect(cleaned_name,"sp.$") | stringr::str_detect(cleaned_name, "sp\\. ")) {
              cat(sprintf("\tSkipping %s - not assessing anything ending in `sp.` Note, genus %s is %s in APC\n", 
                          crayon::blue(s), crayon::green(genus), 
                          ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
              
              binomial <- NA # set binomial to NA, since now only aligning to genus.
              trinomial <- NA # set trinomial to NA, since now only aligning to genus.
              resolution <- ifelse(genus %in% genera_accepted$canonicalName, "genus", resolution)
              resolution <- ifelse(genus %in% family_accepted$canonicalName, "family", resolution)
              found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                                     sprintf("Rewording name to be recognised as genus rank by APC, %s (%s)", v, Sys.Date()))
              
              break;
              
            # Capture identifications to the level of family
              
              } else if(stringr::str_detect(word(cleaned_name, 1), "aceae$")) {
                  cat(sprintf("\t%s is a family rank name and will only be aligned to family \n",
                              crayon::blue(s)))
                  resolution <- ifelse(genus %in% family_accepted$canonicalName, "family", resolution)
                  found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                                         sprintf("Rewording name to be recognised as family rank by APC, %s (%s)", v, Sys.Date()))

              break;
              
            # Capture remaining identifications to the level of genus
                  
              } else if(genus %in% genera_accepted$canonicalName) {
                cat(sprintf("\t%s is a genus rank name and will only be aligned to genus \n",
                            crayon::blue(s)))
                resolution <- "genus"
                found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                                       sprintf("Rewording name to be recognised as genus rank by APC, %s (%s)", v, Sys.Date()))
                
                break;
                  
          # continue with fuzzy matching  
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
            
          # XXXX Confused. Lots of species still ending up here, when I feel like they should be captured earlier
            if(v == dplyr::last(names(to_check))){
              cat(sprintf("\tTaxa not found: %s. Note, genus %s is %s in APC\n", 
                        crayon::blue(s), crayon::green(genus), 
                    ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
            }
          }
        } #ends fuzzy matching loop
      
      

        # XXXX TO DO Add another shorter fuzzy matching sequence that deals with genus names for taxon names where resolution is now set
        # to `genus`; if this becomes possible, then resolution would be set genus regardless of matching.
        # Matching to genus rank names has the advantage that if genera change, it will automatically align with synonymous name
        # As part of this, should also be automatically rewording naming patterns that automatically indicate a genus-level alignment
      
    } #ends for(v in names(to_check)) - cycling through two APC, APNI lists.
  } #ends for s in species - cycling through species
  
  
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
    #APC = path_apc,
    #APNI = path_apni
    APC = "config/NSL/APC-taxon-2022-02-24-0732.csv",
    APNI = "config/NSL/APNI-names-2022-02-24-0712.csv"
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
    taxonomic_resources$APNI <- read_csv_char(file_paths$APNI) %>% dplyr::distinct(.data$canonicalName, .keep_all = TRUE)
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
    arrange(.data$cleaned_name) 
  
  taxa_all %>%
    readr::write_csv("config/taxon_list.csv")
}
