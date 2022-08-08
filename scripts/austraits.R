#' Check possible duplicates across studies
#' 
#' `label_suspected_duplicates` checks across studies to identify possible
#' duplicates
#'
#' @param austraits_traits AusTraits traits data frame
#' @param priority_sources default is NULL, a vector of studies to check against 
#' for duplicates
#'
#' @importFrom rlang .data
#' @return data frame containing potential duplicated data
label_suspected_duplicates <- function(austraits_traits, priority_sources = NULL) {
  
  # copy traits and create a new variable with year of dataset_id
  # we will preference studies with a lower value
  if(is.null(priority_sources))
    priority_sources <- 
      c(
        "Kew_2019_1", "Kew_2019_2", "Kew_2019_3", "Kew_2019_4", "Kew_2019_5", "Kew_2019_6",
        "ANBG_2019", "GrassBase_2014", "CPBR_2002", "NTH_2014","RBGK_2014", 
        "NHNSW_2016", "RBGSYD__2014_2", "RBGSYD_2014", "TMAG_2009", "WAH_1998", "WAH_2016",
        "Brock_1993", "Barlow_1981", "Hyland_2003"    
      )
  
  tmp <- austraits_traits %>% 
    # Extract year from dataset_id, so that we can keep the older record
    mutate(
      priority_source = (.data$dataset_id %in% priority_sources),
      year =  stringr::str_split(.data$dataset_id, "_") %>% 
        lapply(function(i) i[2]) %>% unlist() %>% 
        gsub("0000", "9999", .)
    ) %>%
    # sort to align suspected duplicates
    dplyr::arrange(.data$trait_name, .data$taxon_name, .data$value, 
                   dplyr::desc(.data$priority_source), .data$year) %>%
    # detect duplicates based on combination of variables
    dplyr::mutate(
      to_check = paste(.data$trait_name, .data$taxon_name, .data$value), 
      duplicate = .data$to_check %>% duplicated()
    ) %>% 
    # remove temporary variables
    dplyr::select(-.data$year, -.data$priority_source) %>%
    # original sorting
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type) %>% 
    split(., .$duplicate)
  
  tmp[[2]] <- tmp[[2]] %>%
    dplyr::mutate(
      i = match(.data$to_check, tmp[[1]]$to_check),
      duplicate_dataset_id = tmp[[1]]$dataset_id[i],
      duplicate_obs_id = tmp[[1]]$observation_id[i]
    )
  
  tmp %>%  
    dplyr::bind_rows() %>% 
    dplyr::select(-.data$to_check, -.data$i) %>%
    # original sorting
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)
}

#' Move suspected duplicates to excluded data
#'
#' `remove_suspected_duplicates` moves suspected duplicates from the 'traits' table 
#' to excluded_data.
#'
#' @param austraits AusTraits data object
#' @param priority_sources default = NULL. A vector of studies to check against
#' for possible duplicates
#' 
#' @importFrom rlang .data
#' @return AusTraits data object
#' @export
remove_suspected_duplicates <- function(austraits, 
                                        priority_sources = NULL
) {
  tmp <- label_suspected_duplicates(austraits$traits, priority_sources)
  
  # update `traits` with unique values only
  austraits$traits <- tmp %>% 
    dplyr::filter(!.data$duplicate) %>% 
    # remove temporary variables
    dplyr::select(-tidyr::starts_with("duplicate")) %>%
    # original sorting
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)
  
  # add suspected duplicates to bottom of excluded_data, noting 
  # observation_id of the matching data in the error column
  austraits$excluded_data <- austraits$excluded_data %>% 
    dplyr::bind_rows(
      tmp %>% 
        dplyr::filter(.data$duplicate) %>% 
        dplyr::mutate(error = sprintf("Duplicate of %s", .data$duplicate_obs_id)) %>%
        # remove temporary variables
        dplyr::select(-tidyr::starts_with("duplicate"))
    ) %>%
    # original sorting
    dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)
  
  austraits
}


### Doesnt appear to be migrated to austraits package nor is it used in austraits.build ###
compare_versions <- function (v1, v2, path = "export/tmp", dataset_id=NULL, trait_name = NULL) {
  unlink(path, TRUE, TRUE)
  dir.create(path, FALSE, TRUE)

  v1 <- readRDS(v1)
  v2 <- readRDS(v2)

  if(!is.null(dataset_id)){
    v1 <- v1 %>% austraits::extract_dataset(dataset_id)
    v2 <- v2 %>% austraits::extract_dataset(dataset_id)
  }

  if(!is.null(trait_name)){
    v1 <- v1 %>% austraits::extract_trait(trait_name)
    v2 <- v2 %>% austraits::extract_trait(trait_name)
  }


  v1 %>% export_version_plaintext(path)
  repo <- git2r::init(path)
  git2r::add(repo, "*")
  v2 %>% export_version_plaintext(path)

  message(paste0("Comparison saved in ", path, ". Run ` git -C ", path, " diff --word-diff-regex='[^[:space:],]+' ` in terminal to view differences"))
}
