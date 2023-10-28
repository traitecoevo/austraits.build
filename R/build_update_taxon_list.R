build_update_taxon_list <- function(austraits, taxon_list, replace = FALSE) {
  
  resources <- APCalign::load_taxonomic_resources()
  
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
        ifelse(taxonomic_resolution %in% c("family", "Familia", "Family", "genus", "Genus"), 
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
  
  # Filter out taxa that need to be run through `APCalign::align_taxa()` first
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
        "cleaned_scientific_name_id" = "scientific_name_ID"
      ))) %>%
    # In AusTraits we also want to document identifiers for `aligned_names`, not just for the final `taxon_name`
    # We do this by rejoining columns from APC, but now to the aligned_names, not the taxon_names
    # XX These are currently all prefixed with "cleaned"
    dplyr::left_join(by = c("cleaned_name", "taxon_name"),
      resources$APC %>% 
        dplyr::mutate(
          accepted_name = resources$`APC list (accepted)`$canonical_name[match(accepted_name_usage_ID, resources$`APC list (accepted)`$taxon_ID)],
          scientific_name_id = resources$`APC list (accepted)`$scientific_name_ID[match(accepted_name_usage_ID, resources$`APC list (accepted)`$taxon_ID)],
          taxon_name = accepted_name
          ) %>%
        dplyr::select(dplyr::all_of(c("scientific_name_id", "taxonomic_status", "taxon_ID", "accepted_name", "taxon_name", "canonical_name"))) %>%
        dplyr::rename(dplyr::all_of(c(
          "cleaned_name_taxonomic_status" = "taxonomic_status",
          "cleaned_name_taxon_id" = "taxon_ID",
          "cleaned_name" = "canonical_name",
          "taxon_name" = "accepted_name"))) %>%
        dplyr::distinct(cleaned_name, taxon_name, .keep_all = TRUE)
    ) %>%
    # For taxon names that are aligned at the genus- or family-level, we need to replace the taxon & scientific name identifiers
    # with those for the relevent genus or family.
    dplyr::mutate(
      taxon_rank = ifelse(
        taxon_rank %in% c("genus", "family"),
        taxon_rank,
        ifelse(
          taxonomic_reference == "APC",
          resources$`APC list (accepted)`$taxon_rank[match(taxon_id, resources$`APC list (accepted)`$taxon_ID)],
          taxon_rank
        )),
      taxon_id_family = resources$family_accepted$taxon_ID[match(updated$family, resources$family_accepted$canonical_name)],
      taxon_id = ifelse(taxon_rank %in% c("genus", "family"), NA, taxon_id),
      scientific_name_id = ifelse(taxon_rank %in% c("genus", "family"), NA, scientific_name_id)
    ) %>%
    # The function `APCalign::update_taxonomy` includes alternative possible names as part of the taxon name.
    # We want this information in a separate column.
    dplyr::mutate(
        taxon_name = stringr::str_split(taxon_name, "\\[alternative possible names\\:")) %>%
    tidyr::unnest_wider(taxon_name, names_sep = "_") %>%
    dplyr::rename(taxon_name = taxon_name_1, taxon_name_alternatives = taxon_name_2) %>%
    dplyr::mutate(taxon_name_alternatives = stringr::str_replace(taxon_name_alternatives, "\\]$", "")) %>%
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
    ) %>%
    dplyr::select(-dplyr::all_of(c("count_naturalised", "count_n_and_n", "count_states")))
    
  # New taxon list  
   if (replace = TRUE) {
      taxon_list_replace <- taxon_list_new %>% 
        arrange(taxon_name, cleaned_name) %>%
        distinct(taxon_name, cleaned_name, .keep_all = TRUE)
     
   } else {
     
      taxon_list_replace <- taxon_list %>%
        # First bind rows for cleaned names not yet in AusTraits taxon_list.csv file
        bind_rows(taxon_list_new %>% filter(!cleaned_name %in% taxon_list$cleaned_name)) %>%
        # Arrange by names - hopefully this will be best solution for keeping GitHub commits more transparent
        arrange(taxon_name, cleaned_name) %>%
        distinct(taxon_name, cleaned_name, .keep_all = TRUE)
   }
  
  taxon_list_replace %>% 
      write_csv("config/taxon_list.csv")
}
