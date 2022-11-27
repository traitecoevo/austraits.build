

# load packages needed for generating reports
suppressWarnings({
  library(austraits)
  library(knitr)
  library(kableExtra)
})

test_that("metadata_create_template is working",{
  unlink("data/Test_2022/metadata.yml")

  expect_silent(schema <- get_schema())

  expect_invisible(metadata_create_template(dataset_id = "Test_2022",
                                            path = file.path("data", "Test_2022"),
                                            skip_manual = TRUE))
  test_metadata <- read_metadata("data/Test_2022/metadata.yml")

  metadata_names <- c("source", "contributors", "dataset", "locations", "contexts", "traits",
                      "substitutions", "taxonomic_updates", "exclude_observations",
                      "questions")
  ## Test metadata exists with correct names
  expect_named(test_metadata)
  expect_equal(names(test_metadata), metadata_names)
  expect_equal(length(test_metadata$source$primary), 10)
  expect_isin(names(test_metadata$dataset), schema$metadata$elements$dataset$values %>% names())
 })

test_that("metadata_path_dataset_id is working",{
  expect_silent(metadata_path_dataset_id("Test_2022"))
  expect_equal(metadata_path_dataset_id("Test_2022"), "data/Test_2022/metadata.yml")
  expect_equal(class(metadata_path_dataset_id("Test_2022")), "character")
})

test_that("read_metadata_dataset is working",{
  expect_silent(read_metadata_dataset("Test_2022"))
  expect_equal(class(read_metadata_dataset("Test_2022")), "list")
})

test_that("write_metadata_dataset is working",{
  metadata <- read_metadata_dataset("Test_2022")

  unlink("data/Test_2022/metadata.yml")
  expect_false(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(write_metadata_dataset(metadata, "Test_2022"))
  expect_true(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(read_metadata_dataset("Test_2022"))
  expect_equal(class(read_metadata_dataset("Test_2022")), "list")
})


test_that("metadata_add_source_doi is working",{

  doi <- "https://doi.org/10.3389/fmars.2021.671145"
  doi2 <- "https://doi.org/10.1111/j.0022-0477.2005.00992.x"

  expect_equal(doi, util_standardise_doi(doi))
  expect_equal(doi, util_standardise_doi("https://doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, util_standardise_doi("http://doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, util_standardise_doi("doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, util_standardise_doi("10.3389/fmars.2021.671145"))

  # We won't actually test querying of rcrossref, to avoid unnecessary fails
  # passing in bib avoids calling corssref

  # Create and load test data
  # bib <- rcrossref::cr_cn(doi)
  # writeLines(bib, "tests/testthat/data/test.bib")
  # bib2 <- rcrossref::cr_cn(doi2)
  # writeLines(bib2, "tests/testthat/data/test2.bib")
  bib <- readLines("data/test.bib") %>% paste(collapse = "\n")
  bib2 <- readLines("data/test2.bib") %>% paste(collapse="\n")

  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi=doi, bib=bib))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi2, bib = bib2, type = "secondary"))

  ret <- read_metadata("data/Test_2022/metadata.yml")
  expect_equal(ret$source$primary$journal, "Frontiers in Marine Science")
  expect_equal(ret$source$primary$year, "2021")
  expect_equal(paste0("https://doi.org/", ret$source$primary$doi), doi)

  expect_equal(ret$source$secondary$journal, "Journal of Ecology")
  expect_equal(ret$source$secondary$year, "2005")
  expect_equal(paste0("https://doi.org/", ret$source$secondary$doi), doi2)
})


test_that("metadata_check_custom_R_code is working",{
  expect_equal(class(metadata_check_custom_R_code("Test_2022")), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(metadata_check_custom_R_code("Test_2022")), 13)
  expect_equal(nrow(metadata_check_custom_R_code("Test_2022")), 45)

  expect_visible(metadata_check_custom_R_code("Test_2022"))
})

test_that("metadata_add_source_bibtex is working",{
  expect_silent(metadata_add_source_bibtex(dataset_id = "Test_2022", file = "data/test2.bib"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Journal of Ecology")
})

test_that("metadata_add_substitution is working",{
  expect_silent(suppressMessages(metadata_add_substitution("Test_2022", "leaf_mass_per_area",
                                                           "leaf_area", "leaf_mass_per_area")))
  expect_equal(length(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]), 3)
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$trait_name, "leaf_mass_per_area")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$find, "leaf_area")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$replace, "leaf_mass_per_area")
})

test_that("metadata_add_taxonomic_change is working",{
  expect_output(metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves", "Tissue"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$find, "flower")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$replace, "tree")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$reason, "leaves")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$taxonomic_resolution, "Tissue")
})

test_that("metadata_exclude_observations is working",{
  expect_output(metadata_exclude_observations("Test_2022", "stem", "branch", "test"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$variable, "stem")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$find, "branch")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$reason, "test")
})

test_that("metadata_update_taxonomic_change is working",{
  expect_error(metadata_update_taxonomic_change("Test_2022", "grass", "bark", "soil", "Substrate"))
  expect_invisible(suppressMessages(metadata_update_taxonomic_change("Test_2022", "flower", "bark", "soil", "Substrate")))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$find, "flower")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$replace, "bark")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$reason, "soil")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$taxonomic_resolution, "Substrate")
})

test_that("metadata_remove_taxonomic_change is working",{
  expect_invisible(metadata_remove_taxonomic_change("Test_2022", "flower"))
})

test_that("test dataset_test is working",{
  expect_error(dataset_test())
})  

test_that("test build_setup_pipeline is working",{

  
  unlink("remake.yml")
  unlink("config/taxon_list.csv")

  unlink(".git", recursive = TRUE)
  expect_false(file.exists("remake.yml"))
  expect_false(file.exists("config/taxon_list.csv"))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  
  expect_no_error(zip::unzip("config/testgit.zip"))
  expect_no_error(sha <- git2r::sha(git2r::last_commit()))
  expect_error(build_setup_pipeline(path = "Datas"))
  
  expect_silent(build_setup_pipeline())
  expect_true(file.exists("remake.yml"))
  expect_silent(yaml::read_yaml("remake.yml"))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))

  vars <-  c('cleaned_name', 'taxonomic_reference', 'cleaned_scientific_name_id', 'cleaned_name_taxonomic_status', 'cleaned_name_alternative_taxonomic_status', 'taxon_name', 'taxon_id', 'scientific_name_authorship', 'taxon_rank', 'taxonomic_status', 'family', 'taxon_distribution', 'establishment_means', 'scientific_name', 'scientific_name_id') 

  expect_named(taxa1, vars)
  expect_length(taxa1, 15)
  expect_true(nrow(taxa1) == 0)
  expect_true(file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv", TRUE))
  expect_silent(taxa2 <- read_csv_char("config/taxon_list.csv"))
  expect_named(taxa2, vars)
  expect_length(taxa2, 15)
  expect_true(nrow(taxa2) == 7)
  
  unlink(".remake", recursive = TRUE)
  expect_no_error(austraits_raw <- remake::make("austraits_raw"))
  expect_no_error(austraits <- remake::make("austraits"))
  
  expect_null(austraits_raw$build_info$version)
  expect_null(austraits_raw$build_info$git_SHA)
  expect_equal(austraits$build_info$version, "4.0.0")
  expect_true(is.character(austraits$build_info$git_SHA))
  expect_equal(austraits$build_info$git_SHA, sha)
  expect_equal(austraits$build_info$git_SHA, "6c73238d8d048781d9a4f5239a03813be313f0dd")
  
  expect_length(austraits_raw$taxa, 14)
  expect_length(austraits$taxa, 14)
  expect_equal(nrow(austraits$taxa), nrow(austraits_raw$taxa))
})

test_that("reports and plots produced",{
  
  expect_no_error(austraits <- remake::make("austraits"))
  
  expect_no_error(
   p <- 1 #austraits::plot_trait_distribution_beeswarm(austraits, "huber_value", "dataset_id", highlight = "Test_2022", hide_ids = TRUE)
  )

  expect_no_error(
    dataset_report(dataset_id = "Test_2022", austraits = austraits, overwrite = TRUE)
  )
})

testthat::test_that("Is test_data working", {
  
  expect_silent(
    out <- dataset_test("Test_2022", reporter = testthat::SilentReporter)
  )

  expect_true(
    all(c("SilentReporter", "Reporter", "R6") %in% class(out))
  )

})

testthat::test_that("test metadata_add_substitutions_table", {
  substitutions_df <- tibble::tibble(
    dataset_id = "Test_2022",
    trait_name = "Tree",
    find = "Root",
    replace = "Branch"
  )

  path_metadata <- "data/Test_2022/metadata.yml"

  metadata_create_template(
    dataset_id = "Test_2022",
    path = "data",
    skip_manual = TRUE
  )

  metadata <- read_metadata(path_metadata)
  metadata$substitutions <- NA
  write_metadata(metadata, path_metadata)
  expect_invisible(metadata_add_substitutions_table(substitutions_df, "Test_2022", "trait_name", "find", "replace"))
  expect_equal(read_metadata(path_metadata)$substitutions %>% sapply(`%in%`, x = "Tree") %>% any(), TRUE)
})
