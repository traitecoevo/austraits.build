
Test_data <- "data/Test_2022/data.csv"

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml",  "metadata")
traits_definitions <- get_schema("config/traits.yml", I("traits"))
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
Test_config <- dataset_configure("data/Test_2022/test-metadata.yml",
                             traits_definitions,
                             unit_conversions)

test_that("test dataset_configure is working",{
  expect_equal(class(dataset_configure("data/Test_2022/test-metadata.yml",
                yaml::read_yaml("config/traits.yml"),
                get_unit_conversions("config/unit_conversions.csv"))), "list")
  expect_length(dataset_configure("data/Test_2022/test-metadata.yml",
                yaml::read_yaml("config/traits.yml"),
                get_unit_conversions("config/unit_conversions.csv")), 4)
  expect_named(dataset_configure("data/Test_2022/test-metadata.yml",
                             yaml::read_yaml("config/traits.yml"),
                             get_unit_conversions("config/unit_conversions.csv")),
               c("dataset_id", "metadata", "definitions", "unit_conversion_functions"))
})

test_that("test dataset_process is working",{
  austraits_names <- schema$austraits$elements %>% names()
  
  expect_no_error(  x <- dataset_process(Test_data, Test_config, schema,resource_metadata))
  expect_equal(class(x), "list")
  expect_length(x, 13)
  expect_named(x, austraits_names)
  expect_equal(nrow(x$excluded_data), 0)
  expect_equal(nrow(dataset_process(Test_data, Test_config, schema, resource_metadata, filter_missing_values = TRUE)$excluded_data), 0)
  expect_equal(nrow(dataset_process(Test_data, Test_config, schema, resource_metadata, filter_missing_values = FALSE)$excluded_data), 44)
})

test_that("test process_custom_code is working",{
  metadata <- Test_config$metadata
  data <- readr::read_csv(Test_data, col_types = cols(), guess_max = 100000, progress=FALSE)
  
  expect_equal(ncol(data), 13)
  expect_equal(ncol(process_custom_code(metadata[["dataset"]][["custom_R_code"]])(data)), 14)
  expect_silent(process_custom_code(NULL))
})

# test_that("test process_flag_unsupported_traits is working",{
#   process_flag_unsupported_traits(data, definitions)
# })
# 
# test_that("test process_flag_excluded_observations is working",{
#   process_flag_excluded_observations
# })
