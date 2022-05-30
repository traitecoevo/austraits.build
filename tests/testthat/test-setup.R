library(testthat)

test_that("metadata_create_template is working",{
  expect_invisible(metadata_create_template(dataset_id = "Test_2022",
                                            path = file.path("data", "Test_2022"),
                                            skip_manual = TRUE))
  test_metadata <- read_metadata("data/Test_2022/metadata.yml")
  
  metadata_names <- c("source", "contributors", "dataset", "sites", "contexts", "traits", 
                      "substitutions", "taxonomic_updates", "exclude_observations", 
                      "questions")
  data_collector_names <- c("last_name", "given_name", "affiliation", "ORCID", "additional_role")
  
  ## Test metadata exists with correct names 
  expect_named(test_metadata)
  expect_equal(names(test_metadata), metadata_names)
  expect_equal(length(test_metadata$source$primary), 10)
  
  ## Test contributors exist with the correct names
  expect_equal(length(test_metadata$contributors), 3)
  expect_equal(length(test_metadata$contributors$data_collectors), 5)
  expect_equal(names(test_metadata$contributors$data_collectors), data_collector_names)
  expect_equal(length(test_metadata$contributors$assistants), 1)
  expect_equal(length(test_metadata$contributors$austraits_curators), 1)
  expect_equal(length(test_metadata$contributors$austraits_curators), 1)
  
  expect_equal(length(test_metadata$dataset), 15)
})

test_that("metadata_path_dataset_id is working",{
  expect_silent(metadata_path_dataset_id("Test_2022"))
  expect_equal(metadata_path_dataset_id("Test_2022"), "data/Test_2022/metadata.yml")
  expect_equal(class(metadata_path_dataset_id("Test_2022")), "character")
})

test_that("metadata_read_dataset_id is working",{
  expect_silent(metadata_read_dataset_id("Test_2022"))
  expect_equal(class(metadata_read_dataset_id("Test_2022")), "list")
})  

test_that("metadata_write_dataset_id is working",{
  metadata <- metadata_read_dataset_id("Test_2022")
  
  unlink("data/Test_2022/metadata.yml")
  expect_false(file.exists("data/Test_2022/metadata.yml"))
  
  expect_silent(metadata_write_dataset_id(metadata, "Test_2022"))
  expect_true(file.exists("data/Test_2022/metadata.yml"))
  
  expect_silent(metadata_read_dataset_id("Test_2022"))
  expect_equal(class(metadata_read_dataset_id("Test_2022")), "list")
})  

test_that("metadata_add_source_doi is working",{
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = "https://doi.org/10.3389/fmars.2021.671145"))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = "http://doi.org/10.3389/fmars.2021.671145"))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = "doi.org/10.3389/fmars.2021.671145"))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.3389/fmars.2021.671145"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Frontiers in Marine Science")
  
  expect_silent(metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.1111/j.0022-0477.2005.00992.x"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Journal of Ecology")
})


test_that("metadata_check_custom_R_code is working",{
  expect_equal(class(metadata_check_custom_R_code("Test_2022")), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(metadata_check_custom_R_code("Test_2022")), 13)
  expect_equal(nrow(metadata_check_custom_R_code("Test_2022")), 45)
  
  expect_visible(metadata_check_custom_R_code("Test_2022"))
})

test_that("metadata_add_source_bibtex is working",{
  doi = "https://doi.org/10.3389/fmars.2021.671145"
  bib <- suppressWarnings(rcrossref::cr_cn(doi))
  file <- tempfile()
  writeLines(bib, file)
  expect_silent(metadata_add_source_bibtex(dataset_id = "Test_2022", file = file))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Frontiers in Marine Science")
})

test_that("metadata_add_substitution is working",{
  expect_silent(suppressMessages(metadata_add_substitution("Test_2022", "specific_leaf_area", 
                                                           "leaf_area", "specific_leaf_area")))
  expect_equal(length(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]), 3)
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$trait_name, "specific_leaf_area")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$find, "leaf_area")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]$replace, "specific_leaf_area")
})

test_that("metadata_add_taxonomic_change is working",{
  expect_output(metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$find, "flower")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$replace, "tree")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$reason, "leaves")
})

test_that("metadata_exclude_observations is working",{
  expect_output(metadata_exclude_observations("Test_2022", "stem", "branch", "test"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$variable, "stem")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$find, "branch")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]$reason, "test")
})  

test_that("metadata_update_taxonomic_change is working",{
  expect_error(metadata_update_taxonomic_change("Test_2022", "grass", "bark", "soil"))
  expect_invisible(suppressMessages(metadata_update_taxonomic_change("Test_2022", "flower", "bark", "soil")))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$find, "flower")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$replace, "bark")
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]$reason, "soil")
})  

test_that("metadata_remove_taxonomic_change is working",{
  expect_invisible(metadata_remove_taxonomic_change("Test_2022", "flower"))
})
