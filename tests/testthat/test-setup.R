library(testthat)

setwd("tests/testthat")
 
test_that("metadata_path_dataset_id is working",{
  expect_silent(metadata_path_dataset_id("Test_2022"))
  expect_equal(metadata_path_dataset_id("Test_2022"), "data/Test_2022/metadata.yml")
  expect_equal(class(metadata_path_dataset_id("Test_2022")), "character")
})

test_that("metadata_read_dataset_id is working",{
  expect_silent(metadata_read_dataset_id("Test_2022"))
  expect_equal(class(metadata_read_dataset_id("Test_2022")), "list")
})  

test_that("metadata_add_source_doi is working",{
  metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.1111/j.0022-0477.2005.00992.x")
  
  expect_silent(metadata_add_source_doi(dataset_id = "Test_2022", doi = "https://doi.org/10.3389/fmars.2021.671145"))
  expect_silent(metadata_add_source_doi(dataset_id = "Test_2022", doi = "http://doi.org/10.3389/fmars.2021.671145"))
  expect_silent(metadata_add_source_doi(dataset_id = "Test_2022", doi = "doi.org/10.3389/fmars.2021.671145"))
  expect_silent(metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.3389/fmars.2021.671145"))
})


