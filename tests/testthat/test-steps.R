library(testthat)
  
test_that("test subset_config is working",{
  expect_equal(class(subset_config("data/Test_2022/test-metadata.yml",
                yaml::read_yaml("config/traits.yml"),
                make_unit_conversion_functions("config/unit_conversions.csv"))), "list")
  expect_length(subset_config("data/Test_2022/test-metadata.yml",
                yaml::read_yaml("config/traits.yml"),
                make_unit_conversion_functions("config/unit_conversions.csv")), 4)
  expect_named(subset_config("data/Test_2022/test-metadata.yml",
                             yaml::read_yaml("config/traits.yml"),
                             make_unit_conversion_functions("config/unit_conversions.csv")),
               c("dataset_id", "metadata", "trait_definitions", "unit_conversion_functions"))
})

test_that("test load_dataset is working",{
  Test_data <- "data/Test_2022/data.csv"
  schema <- load_schema()
  traits_definitions <- load_schema("config/traits.yml", I("traits"))
  unit_conversions <- make_unit_conversion_functions("config/unit_conversions.csv")
  Test_config <- subset_config("data/Test_2022/test-metadata.yml",
                               traits_definitions,
                               unit_conversions)
  austraits_names <- c("dataset_id", "traits", "sites", "contexts", "methods", "excluded_data", 
                       "taxonomic_updates", "taxa", "contributors", "sources", "trait_definitions", "schema")
  
  expect_equal(class(load_dataset(Test_data, Test_config, schema)), "list")
  expect_length(load_dataset(Test_data, Test_config, schema), 12)
  expect_named(load_dataset(Test_data, Test_config, schema), austraits_names)
  
  expect_equal(nrow(load_dataset(Test_data, Test_config, schema)$excluded_data), 0)
  expect_equal(nrow(load_dataset(Test_data, Test_config, schema, filter_missing_values = TRUE)$excluded_data), 0)
  expect_equal(nrow(load_dataset(Test_data, Test_config, schema, filter_missing_values = FALSE)$excluded_data), 44)
})

test_that("test custom_manipulation is working",{
  metadata <- Test_config$metadata
  data <- readr::read_csv(Test_data, col_types = cols(), guess_max = 100000, progress=FALSE)
  
  expect_equal(ncol(data), 13)
  expect_equal(ncol(custom_manipulation(metadata[["dataset"]][["custom_R_code"]])(data)), 14)
  expect_silent(custom_manipulation(NULL))
})

# test_that("test flag_unsupported_traits is working",{
#   flag_unsupported_traits(data, trait_definitions)
# })
# 
# test_that("test flag_excluded_observations is working",{
#   flag_excluded_observations
# })



