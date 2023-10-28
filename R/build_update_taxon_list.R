build_update_taxon_list <- function(austraits, taxon_list) {
  
  resources <- APCalign::load_taxonomic_resources()
  ranks_in_database <- unique(austraits$taxa$taxon_rank)
  
  higher_ranks <- c("phylum", "class", "order", "family", "genus")
  highest_ranks <- c("phylum", "class", "order")
  higher_ranks_taxon_list <- higher_ranks[higher_ranks %in% unique(austraits$taxa$taxon_rank)]
  highest_rank_column <- higher_ranks_taxon_list[1]
  highest_ranks_taxon_list <- highest_ranks[highest_ranks %in% unique(austraits$taxa$taxon_rank)]
  
  # Create reduced APC list, just including a few columns and taxon ranks relevant to AusTraits
  APC_tmp <- resources$APC %>% 
    select(canonical_name, taxon_rank) %>% 
    mutate(taxonomic_reference_APC = "APC") %>% 
    filter(taxon_rank %in% c("family", "genus", "species", "form", "subspecies", "variety", "series")) %>%
    rename(aligned_name = canonical_name, taxon_rank_APC = taxon_rank)
  
  # Create reduced APNI list, just including a few columns and taxon ranks relevant to AusTraits
  APNI_tmp <- resources$APNI %>% 
    select(canonical_name, taxon_rank) %>% 
    mutate(taxonomic_reference_APNI = "APNI") %>% 
    filter(taxon_rank %in% c("family", "genus", "species", "form", "subspecies", "variety", "series")) %>% 
    rename(aligned_name = canonical_name, taxon_rank_APNI = taxon_rank)
  
  # List of taxa that are explicitly excluded in metadata - don't want these in the taxon_list
  excluded_in_metadata <- austraits$excluded_data %>% filter(error == "Observation excluded in metadata") %>% distinct(original_name)
  
  # Start with taxonomic_updates table, which is all original names, aligned names, by dataset
  all_taxa <- 
    austraits$taxonomic_updates %>%
    dplyr::rename(aligned_name = cleaned_name) %>%
    dplyr::select(dplyr::all_of(c("original_name", "aligned_name", "taxonomic_resolution"))) %>%
    dplyr::arrange(aligned_name, taxonomic_resolution) %>%
    dplyr::distinct(original_name, aligned_name, .keep_all = TRUE) %>%
    # Need to merge in taxon_ranks for taxa that have not had a taxonomic alignment added in the metadata file
    dplyr::left_join(by = "aligned_name",
                     austraits$taxa %>%
                       dplyr::select(dplyr::all_of(c("taxon_name", "taxon_rank"))) %>%
                       dplyr::rename(aligned_name = taxon_name, taxon_rank_taxa = taxon_rank) %>%
                       arrange(aligned_name, taxon_rank_taxa) %>%
                       distinct(aligned_name, .keep_all = TRUE)
    ) %>%
    # For taxa with: 1) taxon_rank = genus, family, or 2) notes in []
    # Need to mutate a separate column with `name_to_match_to` for joining in other columns
    dplyr::mutate(
      name_to_match_to = aligned_name,
      taxonomic_resolution = ifelse(is.na(taxonomic_resolution), taxon_rank_taxa, taxonomic_resolution),
      name_to_match_to = 
        ifelse(taxonomic_resolution %in% c("family", "genus"), 
               if_else(stringr::word(name_to_match_to, 1) == "x", stringr::word(name_to_match_to, start = 1, end = 2), stringr::word(name_to_match_to, 1)), 
                       name_to_match_to),
      name_to_match_to = ifelse(stringr::str_detect(name_to_match_to, "\\[") & taxonomic_resolution %in% c("Species", "species"), stringr::word(name_to_match_to, start = 1, end = 2), name_to_match_to)
      ) %>%
    # Merge in taxon_ranks & taxonomic_reference for all aligned name; this information not yet present for most names where original_name = aligned_name
    dplyr::left_join(APC_tmp %>% rename(name_to_match_to = aligned_name) %>% distinct(name_to_match_to, .keep_all = TRUE)) %>%
    dplyr::left_join(APNI_tmp %>% rename(name_to_match_to = aligned_name) %>% distinct(name_to_match_to, .keep_all = TRUE)) %>%
    dplyr::mutate(
      taxon_rank = taxon_rank_APC,
      taxon_rank = ifelse(is.na(taxon_rank), taxon_rank_APNI, taxon_rank),
      taxon_rank = ifelse(is.na(taxon_rank), taxonomic_resolution, taxon_rank),
      taxonomic_dataset = taxonomic_reference_APC,
      taxonomic_dataset = ifelse(is.na(taxonomic_dataset), taxonomic_reference_APNI, taxonomic_dataset)
      ) %>%
    # Remove taxa that are excluded in metadata
    dplyr::filter(!(original_name %in% excluded_in_metadata$original_name & is.na(taxon_rank)))
  
  # XX For now filter out taxa that need to be run through `APCalign::align_taxa()`
  taxa_for_taxon_list <- all_taxa %>% 
    filter(!is.na(taxon_rank) | !is.na(taxonomic_dataset)) %>%
    select(original_name, aligned_name, taxon_rank, taxonomic_dataset) %>%
    mutate(aligned_reason = NA)
  
  # Use function `APCalign::update_taxonomy` to update names and add identifier columns
  updated <- APCalign::update_taxonomy(taxa_for_taxon_list, resources = resources)
  
  taxon_list_new <- updated %>% 
    # Remove columns from APCalign's output that aren't needed
    dplyr::select(-dplyr::any_of(c("accepted_name", "row_number", "number_of_collapsed_taxa", "taxonomic_status_aligned", "update_reason", "aligned_reason", "scientific_name_authorship"))) %>%
    # Rename columns to match AusTraits conventions
    dplyr::rename(dplyr::all_of(c(
        "taxon_name" = "suggested_name",
        "cleaned_name" = "aligned_name",
        "taxonomic_reference" = "taxonomic_dataset",
        "taxon_id" = "taxon_ID",
        "scientific_name_id" = "scientific_name_ID"
      ))) %>%
    # In AusTraits we also want to document identifiers for `aligned_names`, not just for the final `taxon_name`
    # We do this by rejoining columns from APC, but now to the aligned_names, not the taxon_names
    # XX These are currently all prefixed with "cleaned"
    dplyr::left_join(by = c("cleaned_name", "taxon_name"),
      resources$APC %>% 
        dplyr::mutate(
          accepted_name = resources$`APC list (accepted)`$canonical_name[match(accepted_name_usage_ID, resources$`APC list (accepted)`$taxon_ID)],
          taxon_name = accepted_name
          ) %>%
        dplyr::select(dplyr::all_of(c("canonical_name", "scientific_name_ID", "taxonomic_status", "taxon_ID", "accepted_name", "taxon_name"))) %>%
        dplyr::rename(dplyr::all_of(c(
          "cleaned_scientific_name_id" = "scientific_name_ID",
          "cleaned_name_taxonomic_status" = "taxonomic_status",
          "cleaned_name_taxon_id" = "taxon_ID",
          "cleaned_name" = "canonical_name"
          ))) %>%
        dplyr::distinct(cleaned_name, taxon_name, .keep_all = TRUE)
    ) %>%
    # For taxon names that are aligned at the genus- or family-level, we need to replace the taxon & scientific name identifiers
    # with those for the relevant genus or family.
    dplyr::mutate(
      taxon_rank = ifelse(
        taxon_rank %in% c("genus", "family"),
        taxon_rank,
        resources$`APC list (accepted)`$taxon_rank[match(taxon_id, resources$`APC list (accepted)`$taxon_ID)]
        ),
      taxon_id_family = resources$family_accepted$taxon_ID[match(updated$family, resources$family_accepted$canonical_name)],
      taxon_id = ifelse(taxon_rank %in% c("genus", "family"), NA, taxon_id),
      scientific_name_id = ifelse(taxon_rank %in% c("genus", "family"), NA, scientific_name_id)
      #scientific_name_id = ifelse(taxon_rank %in% c("genus"), resources$genera_accepted$scientific_name_ID[match(updated$genus, resources$genera_accepted$canonical_name)], scientific_name_id),
      #scientific_name_id = ifelse(taxon_rank %in% c("family"), resources$family_accepted$scientific_name_ID[match(updated$family, resources$family_accepted$canonical_name)], scientific_name_id),
    ) %>%
    # The function `APCalign::update_taxonomy` includes alternative possible names as part of the taxon name.
    # We want this information in a separate column.
    dplyr::mutate(
        taxon_name = stringr::str_split(taxon_name, "\\[alternative possible names\\:")) %>%
    tidyr::unnest_wider(taxon_name, names_sep = "_") %>%
    dplyr::rename(taxon_name = taxon_name_1, taxon_name_alternatives = taxon_name_2) %>%
    dplyr::mutate(
      taxon_name_alternatives = stringr::str_replace(taxon_name_alternatives, "\\]$", "")
      ) %>%
    # Add in data for genus, binomial and trinomial, as appropriate.
    dplyr::mutate(
      trinomial = ifelse(.data$taxon_rank %in% c("subspecies", "form", "variety", "series"),
                         stringr::str_split_fixed(.data$taxon_name, "\\[", 2)[, 1] %>% stringr::str_trim(), NA),
      # Field binomial is filled in if taxonomic resolution is an infraspecific name or a binomial
      # All taxon names that have "extra" information (beyond the actual name) have been formatted
      # to have that information in square brackets '[]', so these can be used as a delimitor to
      # extract the actual name
      binomial = ifelse(.data$taxon_rank %in% c("species"),
                        stringr::str_split_fixed(.data$taxon_name, "\\[", 2)[, 1] %>% stringr::str_trim(), NA),
      binomial = ifelse(.data$taxon_rank %in% c("subspecies", "form", "variety", "series"),
                        stringr::word(.data$taxon_name, start = 1, end = 2), .data$binomial),
      binomial = stringr::str_trim(.data$binomial),
      # Genus filled in for all names that have a taxonomic of genus or more detailed
      genus = ifelse(
        !.data$taxon_rank %in% c("family", "order", "class", "phylum", "kingdom"),
        ifelse(stringr::word(.data$taxon_name, 1) == "x",
               stringr::word(.data$taxon_name, start = 1, end = 2),
               stringr::word(.data$taxon_name, 1)),
        NA)
    ) %>%
    # Add in `establishment_means`, indicating if a taxon is native, naturalised or both
    # This code is based on the exact syntax for taxon_distribution in APC; 
    # the word `native` is used only if a taxon is both native and naturalised in a state
    dplyr::mutate(
      count_naturalised = stringr::str_count(.data$taxon_distribution, "naturalised"),
      count_n_and_n = stringr::str_count(.data$taxon_distribution, "native and naturalised"),
      count_states = stringr::str_count(.data$taxon_distribution, ",") + 1,
      establishment_means = ifelse(.data$count_naturalised > 0 & .data$count_n_and_n == 0, "naturalised", NA),
      establishment_means = ifelse(.data$count_n_and_n > 0 | (.data$count_naturalised > 0 & .data$count_states > .data$count_naturalised), "native and naturalised", .data$establishment_means),
      establishment_means = ifelse(.data$count_naturalised == 0 & .data$count_n_and_n == 0, "native", .data$establishment_means),
      establishment_means = ifelse(.data$taxon_rank %in% higher_ranks, NA, .data$establishment_means),
      taxon_distribution = ifelse(.data$taxon_rank %in% higher_ranks, NA, .data$taxon_distribution)
      ) %>%
    dplyr::select(-dplyr::all_of(c("count_naturalised", "count_n_and_n", "count_states")))

  # Confirm all information previously filled in is accurate - which it currently isn't
  # XX 840 taxon_id's have changed
  # XX Could redo with a for loop - but wanted this for now
    
  if(update_fields == TRUE) {
    taxon_list_with_replacements <- taxon_list %>%
      mutate(
        #scientific_name = NA,
        #cleaned_name_taxon_id = NA,
        taxon_id = ifelse(taxon_id != taxon_list_new$taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)] & 
                                 !is.na(taxon_list_new$taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)]),
                          taxon_list_new$taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)],
                          taxon_id),
        #scientific_name = ifelse(scientific_name != taxon_list_new$scientific_name[match(cleaned_name, taxon_list_new$cleaned_name)] & 
        #                              !is.na(taxon_list_new$scientific_name[match(cleaned_name, taxon_list_new$cleaned_name)]),
        #                            taxon_list_new$scientific_name[match(cleaned_name, taxon_list_new$cleaned_name)],
        #                         scientific_name),
        scientific_name_id = ifelse(scientific_name_id != taxon_list_new$scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)] & 
                            !is.na(taxon_list_new$scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)]),
                          taxon_list_new$scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)],
                          scientific_name_id),
        #cleaned_name_taxon_id = ifelse(cleaned_name_taxon_id != taxon_list_new$cleaned_name_taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)] & 
        #                    !is.na(taxon_list_new$cleaned_name_taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)]),
        #                  taxon_list_new$cleaned_name_taxon_id[match(cleaned_name, taxon_list_new$cleaned_name)],
        #                  cleaned_name_taxon_id),
        cleaned_scientific_name_id = ifelse(cleaned_scientific_name_id != taxon_list_new$cleaned_scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)] & 
                                      !is.na(taxon_list_new$cleaned_scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)]),
                                    taxon_list_new$cleaned_scientific_name_id[match(cleaned_name, taxon_list_new$cleaned_name)],
                                    cleaned_scientific_name_id),
        cleaned_name_taxonomic_status = ifelse(cleaned_name_taxonomic_status != taxon_list_new$cleaned_name_taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)] & 
                                              !is.na(taxon_list_new$cleaned_name_taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)]),
                                            taxon_list_new$cleaned_name_taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)],
                                          cleaned_name_taxonomic_status),
        taxonomic_status = ifelse(taxonomic_status != taxon_list_new$taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)] & 
                                            !is.na(taxon_list_new$taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)]),
                                          taxon_list_new$taxonomic_status[match(cleaned_name, taxon_list_new$cleaned_name)],
                                  taxonomic_status)
        )
  } else {
    taxon_list_with_replacements <- taxon_list
  }
  
    # New taxon list
    taxon_list_replace <- taxon_list %>%
      # First bind rows for cleaned names not yet in AusTraits taxon_list.csv file
      bind_rows(taxon_list_new %>% filter(!cleaned_name %in% taxon_list$cleaned_name)) %>%
      # Arrange by names - hopefully this will be best solution for keeping changes clean
      arrange(taxon_name, cleaned_name) %>%
      distinct(taxon_name, cleaned_name, .keep_all = TRUE)
  
    taxon_list_replace %>% 
      write_csv("config/taxon_list.csv")
}
