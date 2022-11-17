

test_that("test taxonomic resources are working",{
  
  unlink("config", TRUE)
  unlink("data", TRUE)
  expect_true(file.copy("../testthat/config", ".", recursive = TRUE, overwrite = TRUE))
  expect_true(file.copy("../testthat/data", ".", recursive = TRUE, overwrite = TRUE))
  expect_true(file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv", overwrite = TRUE))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  unlink(".git", recursive = TRUE)
  expect_no_error(zip::unzip("config/testgit.zip"))
  
  unlink(".remake",  recursive=TRUE)
  expect_silent(build_setup_pipeline())
  expect_true(file.exists("config/taxon_list.csv"))
  expect_no_error(austraits_raw <- remake::make("austraits_raw"))
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))
  vars <- c("cleaned_name", "taxonomic_reference", "cleaned_scientific_name_id", "cleaned_name_taxonomic_status",
  "cleaned_name_alternative_taxonomic_status", "taxon_name", "taxon_id", "scientific_name_authorship", "taxon_rank",
  "taxonomic_status", "family", "taxon_distribution", "establishment_means", "scientific_name", 
  "scientific_name_id")
  expect_named(taxa1, vars)
  expect_length(taxa1, 15)
  expect_true(nrow(taxa1) == 5)
  expect_no_error(austraits <- remake::make("austraits"))
})
