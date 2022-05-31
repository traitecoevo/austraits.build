
build_comparison_set <- function(root.dir = rprojroot::find_root("remake.yml")) {

# some datasets to compare against
#  Baker_2019 - 1 excluded taxon, 1 taxonomic update, substitutions
#  Bloomfield_2018 -  quite a few excluded numeric values; includes sites, date
#  Catford_2014 -  complex custom R code
#  Duan_2015 -  complex contexts
#  Westoby_2014 -  big collection of numeric traits; but no "issues"; includes sites
#  Tomlinson_2019 - complete taxonomic changes
  
  definitions <- load_schema()
  unit_conversions <- austraits.build:::make_unit_conversion_functions(file.path(root.dir, "config/unit_conversions.csv"))

  f_build <- function(x, definitions, unit_conversions) {
    config <-  austraits.build:::subset_config(file.path(root.dir, "data", x, "metadata.yml"), definitions, unit_conversions)
    data <-  austraits.build:::load_dataset(file.path(root.dir, "data", x, "data.csv"), config)
    data
  }
  
  Baker_2019 <- f_build("Baker_2019", definitions, unit_conversions)
  Bloomfield_2018 <- f_build("Bloomfield_2018", definitions, unit_conversions)
  Catford_2014 <- f_build("Catford_2014", definitions, unit_conversions)
  Duan_2015 <- f_build("Duan_2015", definitions, unit_conversions)
  Maslin_2012 <- f_build("Maslin_2012", definitions, unit_conversions)
  Tomlinson_2019 <- f_build("Tomlinson_2019", definitions, unit_conversions)
  Westoby_2014 <- f_build("Westoby_2014", definitions, unit_conversions)
  
  austraits_raw <-  austraits.build:::combine_datasets(Baker_2019, Bloomfield_2018, Catford_2014, Duan_2015, Maslin_2012, Tomlinson_2019, Westoby_2014, definitions=definitions)
  
  # take a subset to reduce size of saved output
  austraits_raw$traits <- austraits_raw$traits %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup
  austraits_raw$excluded_data <- austraits_raw$excluded_data %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup

  # take a subset of components that make for meaningful comparison
  austraits_raw
}


test_that("constancy of with version 3.0.2", {
  
  # some datasets to compare against
  expect_no_error(austraits_raw <- build_comparison_set(root.dir), info = "Building comparison set")
  
  file_comparison <- "tests/build/comparison_set_3.0.2.rds"
  
  # Uncomment to update building of comparison set
  # saveRDS(austraits_raw, file.path(root.dir, file_comparison))
  austraits_raw_comparison <- readRDS(file.path(root.dir, file_comparison))
  
  # change some names so comparison to new version still runs
  austraits_raw_comparison$traits$trait_name <- austraits_raw_comparison$traits$trait_name %>%
    gsub("seed_mass", "seed_dry_mass", . ) %>%
    gsub("seed_breadth", "seed_height", .)
  
  austraits_raw_comparison$methods$trait_name <- austraits_raw_comparison$methods$trait_name %>%
    gsub("seed_mass", "seed_dry_mass", . ) %>%
    gsub("seed_breadth", "seed_height", .)
  
  austraits_raw_comparison$traits$replicates <- austraits_raw_comparison$traits$replicates %>%
    gsub("3 replicates on 1 individual per species or 1 replicate on each individual", "3",. )
  

  # Compare some select columns of select elements 
  v <- "traits"
  vv <- c("dataset_id", "taxon_name", "site_name", "context_name", "observation_id", "trait_name", "value", "unit", "value_type", "replicates", "original_name")
  # these traits have known changes in names or values
  not_to_check <-  c("seed_dry_mass", "seed_mass", "dispersal_syndrome", "dispersal_appendage")
  v1 <- austraits_raw_comparison[[v]][,vv] %>% 
    dplyr::arrange(observation_id, trait_name) %>% 
    filter(!trait_name %in% not_to_check)
  v2 <- austraits_raw[[v]][,vv] %>% 
    dplyr::arrange(observation_id, trait_name) %>% 
    filter(!trait_name %in% not_to_check)
  expect_equal(v2, v1,
    info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)

  v <- "sites"
  vv <- c("dataset_id", "site_name", "site_property", "value")
  to_check <-  c("desciption", "latitude (deg)", "logitude (deg)")
  v1 <- austraits_raw_comparison[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, site_name) %>% 
    filter(site_property %in% to_check)
  v2 <- austraits_raw[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, site_name) %>% 
    filter(site_property %in% to_check)
  expect_equal(v2, v1,
               info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
  
  v <- "methods"
  vv <- c("dataset_id", "trait_name", "source_primary_key", "source_secondary_key")
  v1 <- austraits_raw_comparison[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, trait_name)
  v2 <- austraits_raw[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, trait_name)
  expect_equal(v2, v1,
               info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
    
})
