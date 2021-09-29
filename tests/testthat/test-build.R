
build_comparison_set <- function(root.dir = rprojroot::find_root("remake.yml")) {

# some datasets to compare against
#   ANBG_2019 - long format; example where some rows of data are not "traits" and therefore yields "unsupported trait" error; lots of substitutions
#  Baker_2019 - 1 excluded taxon, 1 taxonomic update, substitutions
#  Bloomfield_2018 -  quite a few excluded numeric values; includes sites, date
#  Catford_2014 -  complex custom R code
#  Duan_2015 -  complex contexts
#  Westoby_2014 -  big collection of numeric traits; but no "issues"; includes sites
#  Tomlinson_2019 - complete taxonomic changes
  
  definitions <- read_yaml(file.path(root.dir, "config/definitions.yml"))
  unit_conversions <- make_unit_conversion_functions(file.path(root.dir, "config/unit_conversions.csv"))

  Baker_2019_config <- subset_config(file.path(root.dir, "data/Baker_2019/metadata.yml"), definitions, unit_conversions)
  Baker_2019 <- load_study(file.path(root.dir, "data/Baker_2019/data.csv"), Baker_2019_config)
  
  Bloomfield_2018_config <- subset_config(file.path(root.dir, "data/Bloomfield_2018/metadata.yml"), definitions, unit_conversions)
  Bloomfield_2018 <- load_study(file.path(root.dir, "data/Bloomfield_2018/data.csv"), Bloomfield_2018_config)

  Catford_2014_config <- subset_config(file.path(root.dir, "data/Catford_2014/metadata.yml"), definitions, unit_conversions)
  Catford_2014 <- load_study(file.path(root.dir, "data/Catford_2014/data.csv"), Catford_2014_config)
  
  Duan_2015_config <- subset_config(file.path(root.dir, "data/Duan_2015/metadata.yml"), definitions, unit_conversions)
  Duan_2015 <- load_study(file.path(root.dir, "data/Duan_2015/data.csv"), Duan_2015_config)

  Maslin_2012_config <- subset_config(file.path(root.dir, "data/Maslin_2012/metadata.yml"), definitions, unit_conversions)
  Maslin_2012 <- load_study(file.path(root.dir, "data/Maslin_2012/data.csv"), Maslin_2012_config)
  
  Tomlinson_2019_config <- subset_config(file.path(root.dir, "data/Tomlinson_2019/metadata.yml"), definitions, unit_conversions)
  Tomlinson_2019 <- load_study(file.path(root.dir, "data/Tomlinson_2019/data.csv"), Tomlinson_2019_config)

  Westoby_2014_config <- subset_config(file.path(root.dir, "data/Westoby_2014/metadata.yml"), definitions, unit_conversions)
  Westoby_2014 <- load_study(file.path(root.dir, "data/Westoby_2014/data.csv"), Westoby_2014_config)
  
  austraits_raw <- combine_austraits(Baker_2019, Bloomfield_2018, Catford_2014, Duan_2015, Maslin_2012, Tomlinson_2019, Westoby_2014, definitions=definitions)
  
  # take a subset to reduce size of saved output
  austraits_raw$traits <- austraits_raw$traits %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup
  austraits_raw$excluded_data <- austraits_raw$excluded_data %>% group_by(dataset_id) %>% slice(1:2000) %>% ungroup

  # take a subset of components that make for meaningful comparison
  austraits_raw[c("traits", "sites", "contexts", "methods", "excluded_data", "sources")]
}

test_that("constancy of product", {

  # some datasets to compare against
  expect_no_error(austraits_raw <- build_comparison_set(root.dir), info = "Building comaprison set")
  
  file_comparison <- "tests/testthat/comparison_set_3.0.2.rds"
  
  # Unocmment to update building of caomaprison set
  #  saveRDS(austraits_raw, file.path(root.dir, file_comparison))
  
  austraits_raw_comaprsion <- readRDS(file.path(root.dir, file_comparison))
  
  for(v in names(austraits_raw)) {
    expect_equal(austraits_raw[[v]], austraits_raw_comaprsion[[v]], info = paste("comparing", v, "to ", file_comparison), ignore_attr = TRUE)
  }
 
})


test_that("structure of asset", {

  root.dir = rprojroot::find_root("remake.yml")
  
  definitions <- read_yaml(file.path(root.dir, "config/definitions.yml"))
  unit_conversions <- make_unit_conversion_functions(file.path(root.dir, "config/unit_conversions.csv"))
  taxon_list <- read_csv_char(file.path(root.dir, "config/taxon_list.csv"))

  
  expect_no_error({
    Catford_2014_config <- subset_config(file.path(root.dir, "data/Catford_2014/metadata.yml"), definitions, unit_conversions)
    Catford_2014 <- load_study(file.path(root.dir, "data/Catford_2014/data.csv"), Catford_2014_config)
  }, info = "Building Catford_2014")
  
  expect_no_error({
    Falster_2005_1_config <- subset_config(file.path(root.dir, "data/Falster_2005_1/metadata.yml"), definitions, unit_conversions)
    Falster_2005_1 <- load_study(file.path(root.dir, "data/Falster_2005_1/data.csv"), Falster_2005_1_config)
  }, info = "Building Falster_2005_1")
  
  expect_no_error({austraits1 <- combine_austraits(Catford_2014, Falster_2005_1, definitions=definitions)}, 
    info = "Combining sources")
  
  expect_no_error({austraits <- update_taxonomy(austraits1, taxon_list)}, 
    info = "Updating taxonomy")
  
  vars <- definitions$austraits$elements %>% names()
  vars_tables <- vars[!(vars %in% c("definitions", "sources", "build_info"))]
  
  test_list_named(austraits, vars, info = "Main elements")

  for(v in vars_tables) {
    test_dataframe_named(austraits[[v]], definitions$austraits$elements[[v]]$elements %>% names(), info = paste("structure of ", v))
  }

  # contains allowed traits
  expect_isin(austraits$traits$trait_name %>% unique(), definitions$traits$elements %>% names(), info = paste("traits ", v))

})

