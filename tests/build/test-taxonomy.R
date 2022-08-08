

# load packages needed for generating reports
suppressWarnings({
  library(austraits)
  library(knitr)
  library(kableExtra)
})

test_that("test load_taxonomic_resources is working",{
  suppressWarnings(rm(taxonomic_resources))
  expect_no_error(x <- load_taxonomic_resources())
  expect_equal(taxonomic_resources, x)
  expect_named(x, c("APC", "APNI"))
  expect_equal(length(x), 2)
  expect_type(x$APC, "list")
  expect_type(x$APNI, "list")
})

test_that("test setup_build_process is working",{

  unlink(".git", recursive = TRUE)
  expect_false(file.exists("remake.yml"))
  expect_false(file.exists("config/taxon_list.csv"))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  
  expect_no_error(zip::unzip("testgit.zip"))
  expect_no_error(sha <- git2r::sha(git2r::last_commit()))
  expect_error(setup_build_process(path = "Datas"))
  
  expect_silent(setup_build_process())
  expect_true(file.exists("remake.yml"))
  expect_silent(yaml::read_yaml("remake.yml"))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_no_error(austraits_raw <- remake::make("austraits_raw"))
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))
  vars <- c("cleaned_name", "source", "taxonIDClean", "taxonomicStatusClean", "alternativeTaxonomicStatusClean", "acceptedNameUsageID", "taxon_name", "scientificNameAuthorship", "taxonRank", "taxonomicStatus", "family", "taxonDistribution", "ccAttributionIRI")
  expect_named(taxa1, vars)
  expect_length(taxa1, 13)
  expect_true(nrow(taxa1) == 0)
  expect_no_error(suppressWarnings(austraits_rebuild_taxon_list(austraits_raw) ))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_silent(taxa2 <- read_csv_char("config/taxon_list.csv"))
  expect_named(taxa2, vars)
  expect_length(taxa2, 13)
  expect_true(nrow(taxa2) == 5)
  
  expect_no_error(austraits_versioned <- remake::make("austraits_versioned"))
  
  expect_null(austraits_raw$build_info$version)
  expect_null(austraits_raw$build_info$git_SHA)
  expect_equal(austraits_versioned$build_info$version, "3.0.2.9000")
  expect_true(is.character(austraits_versioned$build_info$git_SHA))
  expect_equal(austraits_versioned$build_info$git_SHA, sha)
  expect_equal(austraits_versioned$build_info$git_SHA, "6c73238d8d048781d9a4f5239a03813be313f0dd")
  
  expect_length(austraits_raw$taxa, 1)
  expect_length(austraits_versioned$taxa, 10)
  expect_equal(nrow(austraits_versioned$taxa), nrow(austraits_raw$taxa))
  
  expect_no_error(
    dataset_generate_report(dataset_id = "Test_2022", austraits = austraits_versioned, overwrite = TRUE)
  )
})
