
build_comparison_set <- function(root.dir = rprojroot::find_root("remake.yml")) {

# some datasets to compare against
#  Baker_2019 - 1 excluded taxon, 1 taxonomic update, substitutions
#  Bloomfield_2018 -  quite a few excluded numeric values; includes sites, date
#  Catford_2014 -  complex custom R code
#  Duan_2015 -  complex contexts
#  Westoby_2014 -  big collection of numeric traits; but no "issues"; includes sites
#  Tomlinson_2019 - complete taxonomic changes
  
  definitions <- yaml::read_yaml(file.path(root.dir, "config/definitions.yml"))
  unit_conversions <- austraits.build:::make_unit_conversion_functions(file.path(root.dir, "config/unit_conversions.csv"))

  f_build <- function(x, definitions, unit_conversions) {
    config <-  austraits.build:::subset_config(file.path(root.dir, "data", x, "metadata.yml"), definitions, unit_conversions)
    data <-  austraits.build:::load_study(file.path(root.dir, "data", x, "data.csv"), config)
    data
  }
  
  Baker_2019 <- f_build("Baker_2019", definitions, unit_conversions)
  Bloomfield_2018 <- f_build("Bloomfield_2018", definitions, unit_conversions)
  Catford_2014 <- f_build("Catford_2014", definitions, unit_conversions)
  Duan_2015 <- f_build("Duan_2015", definitions, unit_conversions)
  Maslin_2012 <- f_build("Maslin_2012", definitions, unit_conversions)
  Tomlinson_2019 <- f_build("Tomlinson_2019", definitions, unit_conversions)
  Westoby_2014 <- f_build("Westoby_2014", definitions, unit_conversions)
  
  austraits_raw <-  austraits.build:::combine_austraits(Baker_2019, Bloomfield_2018, Catford_2014, Duan_2015, Maslin_2012, Tomlinson_2019, Westoby_2014, definitions=definitions)
  
  # take a subset to reduce size of saved output
  austraits_raw$traits <- austraits_raw$traits %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup
  austraits_raw$excluded_data <- austraits_raw$excluded_data %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup

  # take a subset of components that make for meaningful comparison
  austraits_raw[c("traits", "sites", "contexts", "methods", "excluded_data", "sources")]
}


test_that("constancy of product", {
  
  # some datasets to compare against
  expect_no_error(austraits_raw <- build_comparison_set(root.dir), info = "Building comparison set")
  
  file_comparison <- "tests/build/comparison_set_3.0.2.rds"
  
  # Uncomment to update building of comparison set
  # saveRDS(austraits_raw, file.path(root.dir, file_comparison))
  
  austraits_raw_comparison <- readRDS(file.path(root.dir, file_comparison))
  
  for(v in names(austraits_raw)) {
    expect_equal(austraits_raw[[v]], austraits_raw_comparison[[v]], info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
  }
  
})
