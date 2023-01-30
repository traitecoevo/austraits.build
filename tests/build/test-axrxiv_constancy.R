
build_comparison_set <- function(root.dir, definitions, unit_conversions, schema, resource_metadata, taxon_list) {

# some datasets to compare against
#  Baker_2019 - 1 excluded taxon, 1 taxonomic update, substitutions
#  Bloomfield_2018 -  quite a few excluded numeric values; includes locations, date
#  Catford_2014 -  complex custom R code
#  Duan_2015 -  complex contexts
#  Westoby_2014 -  big collection of numeric traits; but no "issues"; includes locations
#  Tomlinson_2019 - complete taxonomic changes
  
  f_build <- function(x, definitions, unit_conversions, schema, resource_metadata, taxon_list) {
    config <-  dataset_configure(file.path(root.dir, "data", x, "metadata.yml"), definitions, unit_conversions)
    data <-  dataset_process(file.path(root.dir, "data", x, "data.csv"), config, schema, resource_metadata)
    data <- build_update_taxonomy(data, taxon_list)
    
    data
  }
  
  Baker_2019 <- f_build("Baker_2019", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Bloomfield_2018 <- f_build("Bloomfield_2018", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Catford_2014 <- f_build("Catford_2014", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Duan_2015 <- f_build("Duan_2015", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Maslin_2012 <- f_build("Maslin_2012", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Tomlinson_2019 <- f_build("Tomlinson_2019", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  Westoby_2014 <- f_build("Westoby_2014", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  austraits_raw <-  austraits.build:::build_combine(Baker_2019, Bloomfield_2018, Catford_2014, Duan_2015, Maslin_2012, Tomlinson_2019, Westoby_2014)
  
  # take a subset to reduce size of saved output
  austraits_raw$traits <- austraits_raw$traits %>% group_by(dataset_id) %>% ungroup
  austraits_raw$excluded_data <- austraits_raw$excluded_data %>% group_by(dataset_id) %>% ungroup

  # take a subset of components that make for meaningful comparison
  austraits_raw
}


test_that("constancy of with version 3.0.2", {
  
  # some datasets to compare against
  expect_no_error(
    austraits_raw <- build_comparison_set(root.dir, definitions, unit_conversions, schema, resource_metadata, taxon_list)
    , info = "Building comparison set")
  
  file_comparison <- "comparison_set_3.0.2.rds"
  
  # Uncomment to update building of comparison set
  # saveRDS(austraits_raw, file.path(root.dir, file_comparison))
  austraits_raw_comparison <- readRDS(file_comparison)
  
  austraits_raw_comparison$sites -> austraits_raw_comparison$locations 
  austraits_raw_comparison$sites <- NULL
  austraits_raw_comparison$locations %>% rename(location_name = site_name, location_property = site_property) -> austraits_raw_comparison$locations
  
  austraits_raw$locations %>% select(dataset_id, location_id, location_name) %>% distinct() -> location_names
  austraits_raw$traits %>% left_join(by = c("dataset_id", "location_id"), location_names) -> austraits_raw$traits
  
  # change some names so comparison to new version still runs
  austraits_raw_comparison$traits$trait_name <- austraits_raw_comparison$traits$trait_name %>%
    gsub("seed_mass", "seed_dry_mass", . ) %>%
    gsub("seed_breadth", "seed_height", .)  %>%
    gsub("growth_habit", "stem_growth_habit", .) %>%
    gsub("vessel_density_leaves", "leaf_vessel_density", .) %>%
    gsub("vessel_diameter_leaves", "leaf_vessel_diameter", .) %>%
    gsub("flood_regime_classification", "plant_flood_regime_classification", .) %>%
    gsub("leaf_hydraulic_conductivity", "leaf_specific_conductance", .)
  
  austraits_raw_comparison$methods$trait_name <- austraits_raw_comparison$methods$trait_name %>%
    gsub("seed_mass", "seed_dry_mass", . ) %>%
    gsub("seed_breadth", "seed_height", .) %>%
    gsub("growth_habit", "stem_growth_habit", .) %>%
    gsub("vessel_density_leaves", "leaf_vessel_density", .) %>%
    gsub("vessel_diameter_leaves", "leaf_vessel_diameter", .) %>%
    gsub("flood_regime_classification", "plant_flood_regime_classification", .) %>%
    gsub("leaf_hydraulic_conductivity", "leaf_specific_conductance", .)

    austraits_raw_comparison$traits$unit <- austraits_raw_comparison$traits$unit %>%
    gsub("dimensionless", "{dimensionless}", . ) %>%
    gsub("mm2_leaf/mm2_sapwood", "mm2{leaf}/mm2{sapwood}", . ) 
  
  austraits_raw_comparison$traits$replicates <- austraits_raw_comparison$traits$replicates %>%
    gsub("3 replicates on 1 individual per species or 1 replicate on each individual", "3",. )

  austraits_raw_comparison$traits$value <- austraits_raw_comparison$traits$value %>%
    gsub("climber_liana", "climber_woody", . )
  
  # Compare some select columns of select elements 
  v <- "traits"
  vv <- c("dataset_id", "taxon_name", "trait_name", "value", "unit", "original_name")
  # these traits have known changes in names or values
  trait_to_check <- c("plant_flood_regime_classification", "life_history", "plant_growth_form", "plant_height", "growth_habit", "leaf_area", 
                      "leaf_dry_mass", "root_shoot_ratio", "leaf_compoundness", "leaf_length", "leaf_width", "seed_shape", "seed_width", 
                      "leaf_phenology", "huber_value", "leaf_B_per_dry_mass", "water_potential_predawn", "vessel_density_leaves", 
                      "vessel_diameter_leaves", "wood_density", "leaf_specific_conductance", "water_potential_50percent_lost_conductivity",
                      "water_potential_88percent_lost_conductivity")


  v_curr <- austraits_raw[[v]][,vv] %>% 
    filter(trait_name %in% trait_to_check) %>%
    select(-taxon_name) %>%
    mutate(in_current = "in_current")

  v_old <- austraits_raw_comparison[[v]][, vv] %>%
    filter(trait_name %in% trait_to_check) %>%
    mutate(in_old = "old_version") %>%
    left_join(
      by = c("dataset_id", "trait_name", "value", "unit", "original_name"),
      v_curr)

  # Check data from previous compilation is contained within new compilation
  # The datasets won't be the same, as the comparison set only includes a subset of each dataset and ordering will have changed between versions
  
  expect_equal(sum(is.na(v_old$in_current)), 0, info = paste("comparing", v, "to ", file_comparison))

  v <- "locations"
  vv <- c("dataset_id", "location_name", "location_property", "value")
  to_check <-  c("desciption", "latitude (deg)", "longitude (deg)")
  v1 <- austraits_raw_comparison[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, location_name, location_property) %>%
    filter(location_property %in% to_check, dataset_id != "Bloomfield_2018") 
  v2 <- austraits_raw[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, location_name, location_property) %>%
    filter(location_property %in% to_check, dataset_id != "Bloomfield_2018")

 expect_equal(v2, v1,
               info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
  
  v <- "methods"
  vv <- c("dataset_id", "trait_name", "source_primary_key", "source_secondary_key")
  v1 <- austraits_raw_comparison[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, trait_name) %>%
    filter(trait_name %in% trait_to_check)

  v2 <- austraits_raw[[v]][,vv] %>% 
    dplyr::arrange(dataset_id, trait_name) %>%
      filter(trait_name %in% trait_to_check)
  expect_equal(v2, v1,
               info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
    
})
