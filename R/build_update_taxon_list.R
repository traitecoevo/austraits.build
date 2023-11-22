build_update_taxon_list <- function(austraits, taxon_list, replace = FALSE) {

  resources <- APCalign::load_taxonomic_resources()
  ranks_in_database <- unique(austraits$taxa$taxon_rank)

  higher_ranks <- c("phylum", "class", "order", "family", "genus")
  highest_ranks <- c("phylum", "class", "order")
  higher_ranks_taxon_list <- higher_ranks[higher_ranks %in% unique(austraits$taxa$taxon_rank)]
  highest_rank_column <- higher_ranks_taxon_list[1]
  highest_ranks_taxon_list <- highest_ranks[highest_ranks %in% unique(austraits$taxa$taxon_rank)]

  # Create reduced APC list, just including a few columns and taxon ranks relevant to AusTraits
  APC_tmp <- resources$APC %>%
    dplyr::select(dplyr::all_of(c("canonical_name", "taxon_rank"))) %>%
    dplyr::mutate(taxonomic_dataset_APC = "APC") %>%
    dplyr::filter(taxon_rank %in% c("family", "genus", "species", "form", "subspecies", "variety", "series")) %>%
    dplyr::rename(dplyr::all_of(c("aligned_name" = "canonical_name", "taxon_rank_APC" = "taxon_rank")))

  # Create reduced APNI list, just including a few columns and taxon ranks relevant to AusTraits
  APNI_tmp <- resources$APNI %>%
    dplyr::select(dplyr::all_of(c("canonical_name", "taxon_rank"))) %>%
    dplyr::mutate(taxonomic_dataset_APNI = "APNI") %>%
    dplyr::filter(taxon_rank %in% c("family", "genus", "species", "form", "subspecies", "variety", "series")) %>%
    dplyr::rename(dplyr::all_of(c("aligned_name" = "canonical_name", "taxon_rank_APNI" = "taxon_rank")))

  # List of taxa that are explicitly excluded in metadata - don't want these in the taxon_list
  # These should be excluded from `taxonomic_updates` table during processing, but good to check
  excluded_in_metadata <- austraits$excluded_data %>%
    dplyr::filter(error == "Observation excluded in metadata") %>%
    dplyr::distinct(original_name)

  # Start with taxonomic_updates table, which is all original names, aligned names, by dataset
  all_taxa <-
    austraits$taxonomic_updates %>%
    dplyr::select(dplyr::all_of(c("original_name", "aligned_name", "taxonomic_resolution"))) %>%
    # In case the same `original_name`, `aligned_name` combination occurs twice, but only once with
    # `taxonomic_resolution` attached, arrange names, taxon_ranks
    dplyr::arrange(aligned_name, taxonomic_resolution) %>%
    # Keep unique names
    dplyr::distinct(original_name, aligned_name, .keep_all = TRUE) %>%
    # Need to merge in taxon_ranks for taxa that have not had a taxonomic alignment added in the metadata file
    dplyr::left_join(
      by = "aligned_name",
      austraits$taxa %>%
        dplyr::select(dplyr::all_of(c("taxon_name", "taxon_rank"))) %>%
        dplyr::rename(dplyr::all_of(c("aligned_name" = "taxon_name", "taxon_rank_taxa" = "taxon_rank"))) %>%
        dplyr::arrange(aligned_name, taxon_rank_taxa) %>%
        dplyr::distinct(aligned_name, .keep_all = TRUE)
    ) %>%
    # For taxa with: 1) taxon_rank = genus, family, or 2) notes in []
    # Need to mutate a separate column with `name_to_match_to` for joining in other columns
    dplyr::mutate(
      name_to_match_to = aligned_name,
      taxonomic_resolution = ifelse(is.na(taxonomic_resolution), taxon_rank_taxa, taxonomic_resolution),
      name_to_match_to =
        ifelse(
          taxonomic_resolution %in% c("family", "genus"),
          ifelse(
            stringr::word(name_to_match_to, 1) == "x",
            stringr::word(name_to_match_to, start = 1, end = 2),
            stringr::word(name_to_match_to, 1)),
          name_to_match_to),
      name_to_match_to = ifelse(
        stringr::str_detect(name_to_match_to, "\\[") & taxonomic_resolution %in% c("species"),
        stringr::word(name_to_match_to, start = 1, end = 2),
        name_to_match_to),
      name_to_match_to = ifelse(
        stringr::str_detect(name_to_match_to, "\\[") & taxonomic_resolution %in% c("subspecies", "variety", "form"),
        stringr::word(name_to_match_to, start = 1, end = 3),
        name_to_match_to),
      ) %>%
    # Merge in taxon_ranks & taxonomic_dataset for all aligned name; this information not yet present for
    # most names where original_name = aligned_name
    dplyr::left_join(APC_tmp %>% dplyr::rename("name_to_match_to" = "aligned_name") %>%
    dplyr::distinct(name_to_match_to, .keep_all = TRUE)) %>%
    dplyr::left_join(APNI_tmp %>% dplyr::rename("name_to_match_to" = "aligned_name") %>%
    dplyr::distinct(name_to_match_to, .keep_all = TRUE)) %>%
    dplyr::mutate(
      taxon_rank = taxon_rank_APC,
      taxon_rank = ifelse(is.na(taxon_rank), taxon_rank_APNI, taxon_rank),
      taxon_rank = ifelse(is.na(taxon_rank), taxonomic_resolution, taxon_rank),
      taxonomic_dataset = taxonomic_dataset_APC,
      taxonomic_dataset = ifelse(is.na(taxonomic_dataset), taxonomic_dataset_APNI, taxonomic_dataset)
      ) %>%
    # Remove taxa that are excluded in metadata
    dplyr::filter(!(original_name %in% excluded_in_metadata$original_name & is.na(taxon_rank)))

  # Filter out taxa that need to be run through `APCalign::align_taxa()` first
  taxa_for_taxon_list <- all_taxa %>%
    dplyr::filter(!(is.na(taxon_rank) & is.na(taxonomic_dataset))) %>%
    dplyr::select(dplyr::all_of(c("original_name", "aligned_name", "taxon_rank", "taxonomic_dataset"))) %>%
    dplyr::mutate(aligned_reason = NA)

  # Use function `APCalign::update_taxonomy` to update names and add identifier columns
  updated <- APCalign::update_taxonomy(taxa_for_taxon_list, resources = resources)

  taxon_list_new <- updated %>%
    # Remove columns from APCalign's output that aren't needed
    dplyr::select(-dplyr::any_of(
      c("accepted_name", "row_number", "number_of_collapsed_taxa", "taxonomic_status_aligned",
      "update_reason", "aligned_reason", "scientific_name_authorship"))) %>%
    # Rename columns to match AusTraits conventions
    dplyr::rename(dplyr::all_of(c(
      "taxon_name" = "suggested_name",
      "aligned_name" = "aligned_name",
      "taxonomic_dataset" = "taxonomic_dataset",
      "taxon_id" = "taxon_ID",
      "scientific_name_id" = "scientific_name_ID",
      "taxon_id_genus" = "taxon_ID_genus"
      ))) %>%
    # In AusTraits we also want to document identifiers for `aligned_names`, not just for the final `taxon_name`
    # We do this by rejoining columns from APC, but now to the aligned_names, not the taxon_names
    dplyr::left_join(by = c("aligned_name", "taxon_name"),
      resources$APC %>%
        dplyr::mutate(
          accepted_name = resources$`APC list (accepted)`$canonical_name[match(accepted_name_usage_ID, resources$`APC list (accepted)`$taxon_ID)],
          taxon_name = accepted_name
        ) %>%
        dplyr::select(dplyr::all_of(c(
          "scientific_name_ID", "taxonomic_status", "taxon_ID",
          "accepted_name", "taxon_name", "canonical_name"))) %>%
        dplyr::rename(dplyr::all_of(c(
          "aligned_name_taxonomic_status" = "taxonomic_status",
          "aligned_name_taxon_id" = "taxon_ID",
          "aligned_name" = "canonical_name",
          "cleaned_scientific_name_id" = "scientific_name_ID"
          ))) %>%
        dplyr::distinct(aligned_name, taxon_name, .keep_all = TRUE)
    ) %>%
    # For taxon names that are aligned at the genus- or family-level, we need to replace the taxon & scientific
    # name identifiers with those for the relevant genus or family
    dplyr::mutate(
      # Genus filled in for all names that have a taxonomic of genus or more detailed
      genus = ifelse(
        !.data$taxon_rank %in% c("family", "order", "class", "phylum", "kingdom"),
        ifelse(
          stringr::word(.data$taxon_name, 1) == "x",
          stringr::word(.data$taxon_name, start = 1, end = 2),
          stringr::word(.data$taxon_name, 1)),
        NA),
      taxon_rank = ifelse(
        taxonomic_status == "unknown" & aligned_name %in% austraits$taxonomic_updates$aligned_name,
        austraits$taxonomic_updates$taxonomic_resolution[match(aligned_name, austraits$taxonomic_updates$aligned_name)],
        taxon_rank
        ),
      taxon_rank = ifelse(
        taxon_rank %in% c("genus", "family"),
        taxon_rank,
        ifelse(
          (taxonomic_dataset == "APC" & taxonomic_status == "accepted"),
          resources$`APC list (accepted)`$taxon_rank[match(taxon_id, resources$`APC list (accepted)`$taxon_ID)],
          taxon_rank
        )),
      # For taxon names that are valid names (per herbarium standards) or repeatedly reported invasives, but
      # not in APC/APNI, map on families, genus_ids - APCalign doesn't do this
      taxon_id_genus = ifelse(
        taxonomic_status == "unknown" & aligned_name %in% austraits$taxonomic_updates$aligned_name,
        resources$genera_all$taxon_ID[match(genus, resources$genera_all$genus)], taxon_id_genus
      ),
      family = ifelse(
        taxonomic_status == "unknown" & aligned_name %in% austraits$taxonomic_updates$aligned_name,
        resources$APC$family[match(genus, resources$APC$genus)], family
      ),
      taxon_id_family = resources$family_accepted$taxon_ID[match(family, resources$family_accepted$canonical_name)],
      taxon_id = ifelse(taxon_rank %in% c("genus", "family"), NA, taxon_id),
      scientific_name_id = ifelse(
        taxonomic_dataset == "APC",
        resources$`APC list (accepted)`$scientific_name_ID[match(scientific_name, resources$`APC list (accepted)`$scientific_name)],
        scientific_name_id),
      scientific_name_id = ifelse(taxon_rank %in% c("genus", "family"), NA, scientific_name_id)
    ) %>%
    # The function `APCalign::update_taxonomy` includes alternative possible names as part of the taxon name.
    # We want this information in a separate column.
    dplyr::mutate(taxon_name = stringr::str_split(taxon_name, "\\[alternative possible names\\:")) %>%
    tidyr::unnest_wider(taxon_name, names_sep = "_") %>%
    dplyr::rename(dplyr::all_of(c("taxon_name" = "taxon_name_1", "taxon_name_alternatives" = "taxon_name_2"))) %>%
    dplyr::mutate(taxon_name_alternatives = stringr::str_replace(taxon_name_alternatives, "\\]$", "")) %>%
    # Add in data for genus, binomial and trinomial, as appropriate.
    dplyr::mutate(
      trinomial = ifelse(
        .data$taxon_rank %in% c("subspecies", "form", "variety", "series"),
        stringr::str_split_fixed(.data$taxon_name, "\\[", 2)[, 1] %>% stringr::str_trim(), NA),
      # Field binomial is filled in if taxonomic resolution is an infraspecific name or a binomial
      # All taxon names that have "extra" information (beyond the actual name) have been formatted
      # to have that information in square brackets '[]', so these can be used as a delimitor to
      # extract the actual name
      binomial = ifelse(
        .data$taxon_rank %in% c("species"),
        stringr::str_split_fixed(.data$taxon_name, "\\[", 2)[, 1] %>% stringr::str_trim(), NA),
      binomial = ifelse(
        .data$taxon_rank %in% c("subspecies", "form", "variety", "series"),
        stringr::word(.data$taxon_name, start = 1, end = 2), .data$binomial),
      binomial = stringr::str_trim(.data$binomial)
    ) %>%
    # Add in `establishment_means`, indicating if a taxon is native, naturalised or both
    # This code is based on the exact syntax for taxon_distribution in APC;
    # the word `native` is used only if a taxon is both native and naturalised in a state
    dplyr::mutate(
      count_naturalised = stringr::str_count(.data$taxon_distribution, "naturalised"),
      count_n_and_n = stringr::str_count(.data$taxon_distribution, "native and naturalised"),
      count_states = stringr::str_count(.data$taxon_distribution, ",") + 1,
      establishment_means = ifelse(.data$count_naturalised > 0 & .data$count_n_and_n == 0, "naturalised", NA),
      establishment_means = ifelse(
        .data$count_n_and_n > 0 | (.data$count_naturalised > 0 & .data$count_states > .data$count_naturalised),
        "native and naturalised", .data$establishment_means),
      establishment_means = ifelse(
        .data$count_naturalised == 0 & .data$count_n_and_n == 0,
        "native", .data$establishment_means),
      establishment_means = ifelse(.data$taxon_rank %in% higher_ranks, NA, .data$establishment_means),
      taxon_distribution = ifelse(.data$taxon_rank %in% higher_ranks, NA, .data$taxon_distribution)
      ) %>%
    dplyr::select(dplyr::all_of(
      c("aligned_name", "taxon_name", "taxon_rank", "taxonomic_status", "taxonomic_dataset",
      "taxon_name_alternatives", "genus", "family", "binomial", "trinomial", "taxon_distribution",
      "establishment_means", "scientific_name", "taxon_id", "taxon_id_genus", "taxon_id_family",
      "scientific_name_id", "aligned_name_taxon_id", "aligned_name_taxonomic_status")))

    # New taxon list

   if (replace == TRUE) {

      taxon_list_replace <- taxon_list_new %>%
        dplyr::arrange(taxon_name, aligned_name) %>%
        dplyr::distinct(taxon_name, aligned_name, .keep_all = TRUE)

   } else {

      taxon_list_replace <- taxon_list %>%
        # First bind rows for cleaned names not yet in AusTraits taxon_list.csv file
        dplyr::bind_rows(taxon_list_new %>% dplyr::filter(!aligned_name %in% taxon_list$aligned_name)) %>%
        # Arrange by names - hopefully this will be best solution for keeping GitHub commits more transparent
        dplyr::arrange(taxon_name, aligned_name) %>%
        dplyr::distinct(taxon_name, aligned_name, .keep_all = TRUE)

   }

  taxon_list_replace %>%
      readr::write_csv("config/taxon_list.csv", na = "")

}
