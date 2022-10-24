
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
      #I'm not sure this is needed - in the future this should be part of taxa table XXXX
      trinomial = stringr::word(stripped_name, start = 1, end = 3),
      binomial = stringr::word(stripped_name, start = 1, end = 2),
      genus = stringr::word(stripped_name, start = 1, end = 1)
    )

  species <- 
    x$traits %>% dplyr::select(.data$original_name, .data$taxon_name) %>% dplyr::distinct() %>% 
    dplyr::mutate(
      known = .data$taxon_name %in% taxa$cleaned_name,
    )

  if(all(species$known)){
    message(crayon::red("All taxa are already known\n"))
    return(invisible(NULL));   
  }
  
  # check unknown taxa
  cat(crayon::red(sum(species$known)), " names already matched; ")
  
  if(sum(!species$known) == 0) 
    #break; in old version XXXX 
  cat(crayon::red(sum(!species$known)), " taxa are not yet matched\n")
  
  
  species_to_check <- species
  species <- species %>% dplyr::filter(!.data$known)
  
  # Check if existing substitution in metadata
  metadata <- read_metadata(file.path("data", dataset_id, "metadata.yml"))

  # XXXX this line causes a warning - I haven't changed it
  if(!all(is.null(metadata$taxonomic_updates)) && !is.na(metadata$taxonomic_updates)) {
    metadata_changes <- 
      metadata$taxonomic_updates %>% util_list_to_df2() 

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
            binomial = ifelse(.data$nameType == "scientific", stringr::word(.data$stripped_canonical, start = 1, end = 2), "zzzzzzzzzzzzz zzzzzzzzzzzz"),
            genus = stringr::word(.data$stripped_canonical, 1),
            genus_first_3_letters = stringr::str_extract(.data$canonicalName, "[:alpha:]{3}") %>% stringr::str_to_sentence()
          ) %>%
    dplyr::distinct()

  to_check[["APC list (accepted)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus == "accepted")
  to_check[["APC list (known names)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus != "accepted")
  
  to_check[["APNI names"]] <- 
    taxonomic_resources$APNI %>% dplyr::filter(.data$nameElement != "sp.") %>% 
    dplyr::select(.data$canonicalName, .data$scientificName, ID = .data$scientificNameID, .data$nameType, .data$taxonRank) %>% 
    dplyr::mutate(
            taxonomicStatus = "unplaced for APC", 
            stripped_canonical = strip_names(.data$canonicalName),
            stripped_scientific = strip_names(.data$scientificName),
            binomial = ifelse(.data$nameType == "scientific", stringr::word(.data$stripped_canonical, start = 1, end = 2), NA),
            genus = stringr::word(.data$stripped_canonical, 1),
            genus_first_3_letters = stringr::str_extract(.data$canonicalName, "[:alpha:]{3}") %>% stringr::str_to_sentence()
          ) %>%
    dplyr::distinct() %>% dplyr::arrange(.data$canonicalName)

  for(s in species) {
    
    cleaned_name <- process_standardise_names(s)
    stripped_name <- strip_names(cleaned_name)
    
    # "genus" designated as first word in original name string (occasionally a family) 
    genus <- stringr::str_split(s, " ")[[1]][1]
    
    # create trinomial string if original name has 3+ words
    if (stringr::str_count(stripped_name, '\\w+') > 2) {
      trinomial <- stringr::word(stripped_name, start = 1, end = 3)
    } else {
      trinomial <- NA_character_
    }
    
    # create binomial string if original name has 2+ words
    if (stringr::str_count(stripped_name, '\\w+') > 1) {
      binomial <- stringr::word(stripped_name, start = 1, end = 2)
    } else {
      binomial <- NA_character_
    }
    
    found <- FALSE

    # 1. Automatically reformat certain string patterns that you don't want misaligned to a particular species through fuzzy matching
          
      # 1a. If a name already ends in "sp.", check if it aligns to a genus
    
      # Indicate genera not being matched and omit these names from further searches

     #XXXX next 3 lines removed  - scheme different since it is now that we are acknowleding certain names are genus-aligned
     if(stringr::str_detect(cleaned_name,"sp.$") & (genus %in% genera_accepted$canonicalName)) {
      #   cat(sprintf("\tSkipping %s - not assessing names ending in `sp.` Note, genus %s is %s in APC\n", 
      #               crayon::blue(s), crayon::green(genus), 
      #               ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
         cat(sprintf("\tName %s displays pattern `genus sp.` and it is not being further assessed because genus %s in APC\n", 
                     crayon::blue(s), crayon::green(genus)))      
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", dataset_id, "]"),
          sprintf("match_1a. Rewording taxon with ending with `sp.` to indicate a genus-level alignment with APC accepted name (%s)", Sys.Date()), "genus")  
     }
       
      # 1a.2. If a name already ends in "sp.", check if it aligns to a family
          # searching on variable `genus` because that is the title we've given to the first word in the taxon name string
      if(stringr::str_detect(cleaned_name,"sp.$") & (genus %in% family_accepted$canonicalName)) {
          cat(sprintf("\tName %s displays pattern `family sp.` and it is not being further assessed because family %s in APC\n", 
                       crayon::blue(s), crayon::green(genus)))
        found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", dataset_id, "]"),
                                               sprintf("match_1a.2. Rewording taxon with ending with `sp.` to indicate a family-level alignment with APC accepted name (%s)", Sys.Date()), "genus")
        
    # XXXX Daniel - this "found" is not inside a loop where found leads to break; should I therefore be adding a "break" at the end of the if statement?
    }
      
    # 1b. Reword name where pattern is `genus aff. species` to `genus sp. [original name] to ensure name is only matched to genus.` 
    # to clarify this taxon is only aligned to genus
    # stringr detects "aff." ; "aff " ; "affinis " - but not any aff without space, to avoid picking up starts of actual names
    
    if(stringr::str_detect(cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(cleaned_name, " affinis ")) {
      cat(sprintf("\tTaxon %s with `affinis` preceding species name and will be aligned to genus %s in APC\n", 
                crayon::blue(s), crayon::green(genus)))
      
      if(genus %in% genera_accepted$canonicalName | genus %in% family_accepted$canonicalName) {
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
        sprintf("match_1b. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment %s (%s)", v, Sys.Date()), "genus")
      } else {
       # XXXX genus fuzzy matching function
        metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
          sprintf("match_1bx. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment, but genus doesn't match (%s)", Sys.Date()), "genus")
      }
    }

    # 1c. Reword record where taxon is `graded` to ensure name is only matched to genus. 
    # stringr detects " -- " 
    
    if(stringr::str_detect(cleaned_name, "\\ -- ")) {
      cat(sprintf("\tTaxon %s that are intergrades of two species will be aligned to genus %s in APC\n", 
                  crayon::blue(s), crayon::green(genus)))
      if(genus %in% genera_accepted$canonicalName | genus %in% family_accepted$canonicalName) {
        found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
                                             sprintf("match_1c. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment %s (%s)", v, Sys.Date()), "genus")  
      } else {
        #XXXX if "genus fuzzy matching" can become a function, it would be run here
          metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
                                        sprintf("match_1cx. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment, but genus doesn't match. (%s)", Sys.Date()), "genus")
      }
    }
    
    # 1d. Reword record where pattern is `genus species/species` to `genus sp. [original name]` 
    # to ensure name is only matched to genus. 
    # stringr detects "/"
    
    if(stringr::str_detect(cleaned_name, "[:alpha:]\\/")|stringr::str_detect(cleaned_name, "\\s\\/")) {
      cat(sprintf("\tSpecies identification uncertain and taxon %s will be aligned to genus %s in APC\n", 
                  crayon::blue(s), crayon::green(genus)))
      if(genus %in% genera_accepted$canonicalName | genus %in% family_accepted$canonicalName) {
      found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
        sprintf("match_1d. Rewording taxon where `/` indicates uncertain species identification to align with genus in %s (%s)", v, Sys.Date()), "genus")
      } else {
          metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [", cleaned_name, "]"),
                                      sprintf("match_1dx. Rewording taxon where `/` indicates uncertain species identification, but genus doesn't match. (%s)", Sys.Date()), "genus")
      }
    }

    for(v in names(to_check))  {
        
        if(found) break;

    # 2. Indicate full names that match perfectly as submitted,
        # cycling through APC accepted names,
        # APC known names (isonyms, synonyms), APNI names.

      # check for successful match to canonical name (no author)
        if(s %in% to_check[[v]]$canonicalName) {
          message(sprintf("%s found in %s", crayon::green(s), v))
          found <- TRUE
          break;
          # XXXX Daniel - why have both found and break here?

      # check for successful match to scientific name
        } else if(s %in% to_check[[v]]$scientificName) {
          found <- metadata_add_taxonomic_change(
            dataset_id, s, 
            to_check[[v]]$canonicalName[match(s, to_check[[v]]$scientificName)], 
            sprintf("match_2a. Automatic alignment with scientific name in %s (%s)", v, Sys.Date()), 
            to_check[[v]]$taxonRank[match(s, to_check[[v]]$scientificName)])

          break;

      # match synonymous terms (taxonomic synonyms, isonyms, etc) among canonical names (no authorities)
        } else if(stripped_name %in% to_check[[v]]$stripped_canonical) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
            to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_canonical)], 
            sprintf("match_2b. Automatic alignment with synonymous term among canonical names in %s (%s)", v, Sys.Date()), to_check[[v]]$taxonRank[match(stripped_name, to_check[[v]]$stripped_canonical)])

          break;

      # match synonymous terms (taxonomic synonyms, isonyms, etc) among scientific names (with authorities)
        } else if(stripped_name %in% to_check[[v]]$stripped_scientific) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
              to_check[[v]]$canonicalName[match(stripped_name, to_check[[v]]$stripped_scientific)],
              sprintf("match_2c. Automatic alignment with synonymous term among scientific names in %s (%s)", v, Sys.Date()), 
              to_check[[v]]$taxonRank[match(stripped_name, to_check[[v]]$stripped_scientific)])

          break;

        # XXXX all 3 lists should be looped through before continuing to #3 - right now fuzzy matching on APC accepted before hunting for name on APC synonym; but can't have 2 loops through lists without nested breaks

        } else {

      #3. Fuzzy matching based on selected absolute/relative distances.
          distance_c <- utils::adist(stripped_name, to_check[[v]]$stripped_canonical, fixed=TRUE)[1,]
          min_dist_abs_c <-  min(distance_c)
          min_dist_per_c <-  min(distance_c) / stringr::str_length(stripped_name)

          distance_s <- utils::adist(stripped_name, to_check[[v]]$stripped_scientific, fixed=TRUE)[1,]
          min_dist_abs_s <-  min(distance_s)
          min_dist_per_s <-  min(distance_s) / stringr::str_length(stripped_name)

          utils::adist(stripped_name, to_check[[v]]$stripped_scientific, fixed=TRUE)[1,]

          # determine 1st four letter of genus name to restrict fuzzy matching
          genus_input <-
            genus %>%
            stringr::str_extract("[:alpha:]{3}")

          if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs & 
            ## Within allowable number of characters (relative)
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
            ) {
            # only allow fuzzy matches if the first 3 letters of the genus are identical
              if(genus_input == to_check[[v]]$genus_first_3_letters[which(distance_c==min_dist_abs_c)]) {
              found <- 
                metadata_add_taxonomic_change(dataset_id, s, 
                  to_check[[v]]$canonicalName[which(distance_c==min_dist_abs_c)], 
                  sprintf("match_3a. Automatic alignment with name in %s (%s)", v, Sys.Date()), 
                  to_check[[v]]$taxonRank[which(distance_c==min_dist_abs_c)]) 

              }
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
                                           sprintf("match_3b. Automatic alignment with name in %s (%s)", v, Sys.Date()), to_check[[v]]$taxonRank[which(distance_s==min_dist_abs_s)])
          } else if(try_outside_guesses) {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            closest_names <- to_check[[v]]$canonicalName[j]
            
            cat(sprintf("\nFor %s - are any of these names from %s appropriate?\n",  crayon::blue(s), v))
            tmp <- menu(c("None", sprintf("%s -- %s -- %s", crayon::green(closest_names), to_check[[v]]$taxonomicStatus[j], to_check[[v]]$ID[j])))
            if(tmp > 1){
              found <- 
                metadata_add_taxonomic_change(dataset_id, s,  closest_names[tmp-1], 
                                              sprintf("match_3c. Alignment with known name in %s (%s, %s)", v, author, Sys.Date()), NA)
            }
          } else {
            j <- which(distance_c %in% (sort(distance_c)[1:5]))
            
            to_review <- 
              dplyr::bind_rows(to_review, 
                              tibble::tibble(dataset_id = dataset_id, source = v,
                                              taxon_name = s, closest_names = to_check[[v]]$canonicalName[j], status = to_check[[v]]$taxonomicStatus[j], ID = to_check[[v]]$ID[j], 
                                              genus_known = genus %in% genera_accepted$canonicalName,
                                              keep = 0, reason = sprintf("match_3d. Alignment with known name in %s (%s, %s)", v, author, Sys.Date()), NA)
              )

          }
        }
    } #ends for(v in names(to_check)) - cycling through two APC, APNI lists.

  # Manipulations for strings that weren't matched with fuzzy matching

    for(v in c("APC list (accepted)"))  {

      if(found) break;
      
    # 4. Beginning a second set of string manipulations for taxon names 
        #that are not matched at the end of round 1 of fuzzy matching.
        #for now it will just match to the APC accepted list.

    # 4a. Reformat name for hybrid species that aren't a recorded name, so they align to genus.
        # stringr detects " x "
      if(stringr::str_detect(cleaned_name," [xX] ")) {
          cat(sprintf("\t%s is a hybrid species not in APC/APNI and has been aligned with to its genus %s \n", 
                      crayon::blue(s), crayon::green(genus)))
          found <- metadata_add_taxonomic_change(dataset_id, s, paste0(genus, " sp. [",cleaned_name,"]"),
                                                sprintf("match_4a. Rewording hybrid species name to align with genus, %s (%s)", v, Sys.Date()), "genus")
        
        
    # 4b. Match first three words only - because sometimes the submitted name is a valid trinomial + notes.
      } else if(trinomial %in% to_check[[v]]$stripped_canonical) {
          cat(sprintf("\t%s can be aligned to trinomial %s \n", 
                    crayon::blue(s), crayon::green(cleaned_name %>% stringr::word(start = 1, end = 4))))
          found <- metadata_add_taxonomic_change(dataset_id, s,
                                                paste0(cleaned_name %>% stringr::word(start = 1, end = 4), " [", cleaned_name, "]"), 
                                                sprintf("match_4b. Automatic alignment when notes ignored, %s (%s)", v, Sys.Date()), "trinomial")

      
    # 4c. Match first two words only - because sometimes the submitted name is a valid binomial + notes 
        # or a valid binomial + invalid infraspecific epithet.
      } else if(binomial %in% to_check[[v]]$binomial) {
          found <- metadata_add_taxonomic_change(dataset_id, s,
                                               paste0(binomial %>% stringr::str_to_sentence(), " [", cleaned_name, "]"),
                                               sprintf("match_4c. Automatic alignment when infraspecific information & notes ignored, %s (%s)", v, Sys.Date()), "binomial")
    
    
    # 4d. Capture names that are identified as being at the genus (level by the presence of `sp.` - but now sp. anywhere in the name.
        # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
      } else if((stringr::str_detect(cleaned_name,"sp.$") | stringr::str_detect(cleaned_name, "sp\\. ")) &
                (genus %in% genera_accepted$canonicalName)) {
          found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                              sprintf("match_4d. Rewording name to be recognised as genus rank by %s (%s)", v, Sys.Date()), "genus")
        
      # 4e. Capture names that are identified as being at the family level by the presence of `sp.` - but now sp. anywhere in the name.
        # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
      } else if(
          (stringr::str_detect(cleaned_name,"sp.$") | stringr::str_detect(cleaned_name, "sp\\. ")) &
          (genus %in% family_accepted$canonicalName)
        ) {
            found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                               sprintf("match_4e. Rewording name to be recognised as family rank by %s (%s)", v, Sys.Date()), "genus")
        
    # 5. A second round of fuzzy matching on original name, considering just 1st 3 words (trinomials), 1st 2 words (binomials) or 1st word (genus; family)
        
      # 5a. Fuzzy matching on first three words (trinomials).

      } else if(
          !is.na(trinomial)
        ) {
            distance_c <- utils::adist(trinomial, to_check[[v]]$stripped_canonical, fixed=TRUE)[1,]
            min_dist_abs_c <-  min(distance_c)
            min_dist_per_c <-  min(distance_c) / stringr::str_length(stripped_name)
    
            # determine 1st three letters of genus name to restrict fuzzy matching
            genus_input <- 
              genus %>% 
              stringr::str_extract("[:alpha:]{3}")
  
              if(
                ## Within allowable number of characters (absolute)
                min_dist_abs_c <= max_distance_abs &
                ## Within allowable number of characters (relative)
                min_dist_per_c <= max_distance_rel &
                ## Is a unique solution
                length(which(distance_c==min_dist_abs_c))==1
              ) {
                # only allow fuzzy matches if the first 3 letters of the genus are identical
                  if(
                    genus_input == to_check[[v]]$genus_first_3_letters[which(distance_c==min_dist_abs_c)]
                  ) {
                      words_in_name <- 1 + stringr::str_count(s, " ")
                    found <-
                        metadata_add_taxonomic_change(dataset_id, s, paste0(to_check[[v]]$cleaned_name, " [", stringr::word(s, start = 5, end = words_in_name), "; ", dataset_id, "]"),
                          sprintf("match_5a. Automatic fuzzy matching alignment with trinomial when notes ignored, %s (%s)", v, Sys.Date()), "trinomial")
                  }
              }

    # 5b. Fuzzy matching on first two words (binomials).
      # XXXXX ERROR - actual binomials are not being caught be this properly - "browser" triggered if called just before this "if" loop, 
            # but not on the first line within it, even if conditions met. But then that binomial is also not being moved forward to 5c, 5d - so lost 
            # this is causing an error if there are 2-word names (i.e. binomials) that need to be aligned to genus - they will get caught here.
      } else if(
        !is.na(binomial) &
        !(stringr::word(binomial, 2) == "sp")
          ) { 
            distance_c <- utils::adist(binomial, to_check[[v]]$binomial, fixed=TRUE)[1,]
            min_dist_abs_c <-  min(distance_c)
            min_dist_per_c <-  min(distance_c) / stringr::str_length(binomial)
  
            genus_input <- 
              genus %>% 
              stringr::str_extract("[:alpha:]{3}")
  
            if(
              ## Within allowable number of characters (absolute)
              min_dist_abs_c <= max_distance_abs &
              ## Within allowable number of characters (relative)
              min_dist_per_c <= max_distance_rel &
              ## Is a unique solution
              length(which(distance_c==min_dist_abs_c))==1
            ) {
              # only allow fuzzy matches if the first 3 letters of the genus are identical
              if(
                genus_input == to_check[[v]]$genus_first_3_letters[which(distance_c==min_dist_abs_c)]
              ) {
                words_in_name <- 1 + stringr::str_count(s, " ")
                found <-
                   metadata_add_taxonomic_change(dataset_id, s, 
                   paste0(to_check[[v]]$cleaned_name, " [", stringr::word(s, start = 3, end = words_in_name), "; ", dataset_id, "]"),
                     sprintf("match_5b. Automatic fuzzy matching alignment with binomial when notes ignored, %s (%s)", v, Sys.Date()),"binomial")
                }
              }
          
      # 5c. Capture identifications to the level of genus.
          # Taxa where the entire taxon name string, the first three words, and the first two words all fail to match
          # and which don't have patterns that indicate they are hybrids, graded, or affinis
          # should now be attempted to be matched to genus (or rarely family).
          
      # XXXX TO DO - this should also cycle through synonyms, but not sure how that would work - for instance to align Dryandra sp. to Banksia sp.
      # XXXX I don't fully understand at what point synonyms are really matched.

      } else if(
          genus %in% genera_accepted$canonicalName
        ) {
          cat(sprintf("\t%s Can only be identified to a genus rank name and will only be aligned to genus \n",
                    crayon::blue(s)))
          found <- metadata_add_taxonomic_change(dataset_id, s, 
              paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
              sprintf("match_5c. Name only recognised to rank of genus rank by %s (%s)", v, Sys.Date()), "genus")
          
      # 5d. Capture identifications to the level of family.
          # `genus` is the first word of the original name taxon string, so it might also represent a family
      } else if(
          stringr::str_detect(word(cleaned_name, 1), "aceae$") & 
          genus %in% family_accepted$canonicalName
        ) {
          cat(sprintf("\t%s Can only be identified to a family rank name and will only be aligned to family \n",
                      crayon::blue(s)))
          found <- metadata_add_taxonomic_change(dataset_id, s, paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
                                                 sprintf("match_5d. Rewording name to be recognised as family rank by %s (%s)", v, Sys.Date()), "family")
        
      # 5e. Fuzzy matching on first word (genus or family).
        # Taxa where the entire taxon name string, the first three words, the first two words, and the first word all fail to match
        # should now be attempted to be matched to genus (or rarely family).
        # (Only considering genus for now.)

      } else {
          distance_c <- utils::adist(genus, stringr::word(genera_accepted$canonicalName,1), fixed=TRUE)[1,]
          min_dist_abs_c <-  min(distance_c)
          min_dist_per_c <-  min(distance_c) / stringr::str_length(genus)

        if(
            ## Within allowable number of characters (absolute)
            min_dist_abs_c <= max_distance_abs &
            ## Within allowable number of characters (relative)
            min_dist_per_c <= max_distance_rel &
            ## Is a unique solution
            length(which(distance_c==min_dist_abs_c))==1
          ) {
            found <-
              metadata_add_taxonomic_change(dataset_id, s, paste0(
                genera_accepted$canonicalName[which(distance_c==min_dist_abs_c)], " sp. [", dataset_id, "]"),
                sprintf("5e. Genus matched by fuzzy matching to a name in %s (%s)", v, Sys.Date()), "genus")
            } else {
              cat(sprintf("\tTaxa %s ABSOLUTELY not found\n",
                      crayon::blue(s)))
            }
      #if(v == dplyr::last(names(to_check))){
      #  cat(sprintf("\tTaxa ABSOLUTELY not found: %s. Note, genus %s is %s in APC\n", 
      #              crayon::blue(s), crayon::green(genus), 
      #              ifelse(genus %in% genera_accepted$canonicalName, crayon::green("IS"), crayon::red("IS NOT"))))
      #}
        
      } # end extra matches just on APC accepted
    
    } # end for v in "APC list (accepted)", replacement sets 4 & 5
     
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

} #ends function


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
    APC = "config/NSL/APC-taxon-2022-10-21-4554.csv",
    APNI = "config/NSL/APNI-names-2022-10-21-4546.csv"
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
    # build list of observed taxon names
    austraits$traits %>% 
    dplyr::select(cleaned_name = .data$taxon_name) %>% 
    dplyr::distinct() %>%
    # match our cleaned names against names in APC list
    dplyr::left_join(
      by = "cleaned_name", taxonomic_resources$APC %>% 
        dplyr::filter(!grepl("sp\\.$", .data$canonicalName)) %>% 
        dplyr::select(cleaned_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                      cleaned_name_taxonomic_status = .data$taxonomicStatus, accepted_name_usage_id = .data$acceptedNameUsageID)) %>% 
    # Also add all accepted genera species, varieties etc from APC
    dplyr::bind_rows(
      taxonomic_resources$APC %>% 
        # XXXX subspecies was missing from this list
        dplyr::filter(.data$taxonRank %in% c('Familia', 'Series', 'Genus', 'Species', 'Forma', 'Varietas', 'Subspecies'), 
                      .data$taxonomicStatus == "accepted") %>% 
        dplyr::select(cleaned_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                      cleaned_name_taxonomic_status = .data$taxonomicStatus, accepted_name_usage_id = .data$acceptedNameUsageID)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(taxonomic_reference = ifelse(!is.na(.data$cleaned_scientific_name_id), "APC", NA_character_)) %>% 
    #dplyr::mutate(scientific_name_id = .data$cleaned_scientific_name_id) %>%
    # Now find accepted names for each name in the list (sometimes they are the same)
    dplyr::left_join(
      by = "accepted_name_usage_id", taxonomic_resources$APC %>% 
        # XXXX next line confuses me. How do you draw in all the other variants if you're only merging in accepted names?
        dplyr::filter(.data$taxonomicStatus =="accepted") %>% 
        dplyr::select(accepted_name_usage_id = .data$acceptedNameUsageID,
                      taxon_id = .data$taxonID, taxon_name = .data$canonicalName, 
                      taxonomic_status = .data$taxonomicStatus,  
                      scientific_name = .data$scientificName, scientific_name_id = .data$scientificNameID, 
                      scientific_name_authorship = .data$scientificNameAuthorship, .data$family,
                      taxon_distribution = .data$taxonDistribution, taxon_rank = .data$taxonRank)) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort in the order listed below with accepted at the top
    # have currently removed some that don't exist - and confused how they will ever exist if you've filtered to only merge in `accepted names`
    # removed: "replaced synonym", "doubtful pro parte taxonomic synonym", "pro parte taxonomic synonym", "doubtful misapplied", "doubtful pro parte misapplied"
    dplyr::mutate(my_order = .data$cleaned_name_taxonomic_status %>% 
             forcats::fct_relevel(c("accepted", "taxonomic synonym", "basionym", "nomenclatural synonym", 
                                    "orthographic variant", "doubtful taxonomic synonym", 
                                    "misapplied", "pro parte misapplied", "excluded"))) %>%
    dplyr::arrange(.data$cleaned_name, .data$my_order) %>%
    # For each species, keep the first record (accepted if present) and 
    # record any alternative status to indicate where there was ambiguity
    dplyr::group_by(.data$cleaned_name) %>% 
    dplyr::mutate(
      cleaned_name_alternative_taxonomic_status = ifelse(.data$cleaned_name_taxonomic_status[1] == "accepted", 
                                               .data$cleaned_name_taxonomic_status %>% 
          unique() %>% 
          subset_accepted() %>% 
          paste0(collapse = " | ") %>% 
          dplyr::na_if(""), NA_character_)) %>% 
    dplyr::slice(1) %>%  
    dplyr::ungroup() %>% 
    dplyr::select(-.data$my_order) %>% 
    dplyr::select(.data$cleaned_name, .data$taxonomic_reference, .data$cleaned_scientific_name_id, .data$cleaned_name_taxonomic_status, 
                  .data$cleaned_name_alternative_taxonomic_status, 
                  .data$taxon_name, .data$taxon_id, .data$scientific_name_authorship, .data$taxon_rank, 
                  .data$taxonomic_status, .data$family, .data$taxon_distribution, 
                  .data$scientific_name, .data$scientific_name_id)

  taxa1 <- 
    taxa %>% dplyr::filter(!is.na(.data$cleaned_scientific_name_id)) %>% 
    dplyr::distinct() 
  
  # Now check against APNI for any species not found in APC
  # Only keep those species with a match

  taxa2 <-
    taxa %>% 
    dplyr::filter(is.na(.data$taxon_name)) %>%
    dplyr::select(.data$cleaned_name) %>%
    dplyr::left_join(by = "cleaned_name", taxonomic_resources$APNI %>% 
                       dplyr::filter(.data$nameElement != "sp.") %>%
                       dplyr::select(cleaned_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                                     .data$family, taxon_rank = .data$taxonRank, scientific_name = .data$scientificName)) %>% 
    dplyr::group_by(.data$cleaned_name) %>%
    dplyr::mutate(
      cleaned_scientific_name_id = paste(.data$cleaned_scientific_name_id, collapse = " ") %>% 
        dplyr::na_if("NA"), family = ifelse(dplyr::n_distinct(.data$family) > 1, NA_character_, .data$family[1])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      taxonomic_reference = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), NA_character_, "APNI")),
      taxon_name = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), NA_character_, .data$cleaned_name)),
      cleaned_name_taxonomic_status = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), "unknown", "unplaced by APNI")),
      taxonomic_status = as.character(.data$cleaned_name_taxonomic_status),
      scientific_name_id = cleaned_scientific_name_id
      )

  taxa_all <- taxa1 %>% 
    dplyr::bind_rows(taxa2 %>% 
        dplyr::filter(!is.na(.data$cleaned_scientific_name_id))) %>% 
    arrange(.data$cleaned_name) 
  
  taxa_all %>%
    readr::write_csv("config/taxon_list.csv")
}
