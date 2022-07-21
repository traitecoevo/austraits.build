
unlink("data/Test_2022/metadata.yml")

test_that("test austraits_rebuild_taxon_list is working",{

#  expect_silent(setup_build_process())
#  if(file.exists("config/taxon_list.csv")) unlink("config/taxon_list.csv")
  
#  expect_false(file.exists("config/taxon_list.csv"))
#  expect_length(suppressWarnings(austraits_rebuild_taxon_list()), 13)
#  expect_named(suppressWarnings(austraits_rebuild_taxon_list()))
#  expect_true(file.exists("config/taxon_list.csv"))
})

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
  expect_equal(length(test_metadata$contributors$data_collectors), 1)
  expect_equal(length(test_metadata$contributors$data_collectors[[1]]), 5)
  expect_equal(names(test_metadata$contributors$data_collectors[[1]]), data_collector_names)
  expect_equal(length(test_metadata$contributors$assistants), 1)
  expect_equal(length(test_metadata$contributors$austraits_curators), 1)
  expect_equal(length(test_metadata$contributors$austraits_curators), 1)

  expect_equal(length(test_metadata$dataset), 18)
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

  doi <- "https://doi.org/10.3389/fmars.2021.671145"
  doi2 <- "https://doi.org/10.1111/j.0022-0477.2005.00992.x"

  expect_equal(doi, standardise_doi(doi))
  expect_equal(doi, standardise_doi("https://doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, standardise_doi("http://doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, standardise_doi("doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, standardise_doi("10.3389/fmars.2021.671145"))

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

test_that("test load_taxonomic_resources is working",{
  expect_silent(x <- load_taxonomic_resources())
  expect_named(x, c("APC", "APNI"))
  expect_equal(length(x), 2)
  expect_type(x$APC, "list")
  expect_type(x$APNI, "list")
})

test_that("test test_data_setup is working",{
  expect_error(test_data_setup())
})  

test_that("test setup_build_process is working",{
#  expect_error(setup_build_process(path = "Datas"))
#  expect_silent(setup_build_process())
#  expect_silent(yaml::read_yaml("remake.yml"))
})
