

# load packages needed for generating reports
suppressWarnings({
  library(austraits)
  library(knitr)
  library(kableExtra)
})

test_that("test taxonomic resources are working",{
  
  unlink("config", TRUE)
  unlink("data", TRUE)
  expect_true(file.copy("../testthat/config", ".", recursive = TRUE, overwrite = TRUE))
  expect_true(file.copy("../testthat/data", ".", recursive = TRUE, overwrite = TRUE))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  unlink(".git", recursive = TRUE)
  expect_no_error(zip::unzip("config/testgit.zip"))
  unlink("config/taxon_list.csv")
  expect_false(file.exists("config/taxon_list.csv"))
  
  suppressWarnings(rm(taxonomic_resources))
  expect_no_error(x <- load_taxonomic_resources())
  expect_equal(taxonomic_resources, x)
  expect_named(x, c("APC", "APNI"))
  expect_equal(length(x), 2)
  expect_type(x$APC, "list")
  expect_type(x$APNI, "list")

  unlink(".remake",  recursive=TRUE)
  expect_silent(build_setup_pipeline())
  expect_true(file.exists("config/taxon_list.csv"))
  expect_no_error(austraits_raw <- remake::make("austraits_raw"))
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))
  vars <- c("cleaned_name", "source", "taxonIDClean", "taxonomicStatusClean", "alternativeTaxonomicStatusClean", "acceptedNameUsageID", "taxon_name", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "family", "taxonDistribution", "ccAttributionIRI")
  expect_named(taxa1, vars)
  expect_length(taxa1, 13)
  expect_true(nrow(taxa1) == 0)
  expect_no_error(austraits_rebuild_taxon_list(austraits_raw, taxonomic_resources))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_silent(taxa2 <- read_csv_char("config/taxon_list.csv"))
  expect_named(taxa2, vars)
  expect_length(taxa2, 13)
  expect_equal(nrow(taxa2), 5)
  expect_no_error(austraits <- remake::make("austraits"))
  
  expect_length(austraits_raw$taxa, 1)
  expect_length(austraits$taxa, 10)
  expect_equal(nrow(austraits$taxa), nrow(austraits_raw$taxa))
  
  unlink("data")
  unlink("config")
})
