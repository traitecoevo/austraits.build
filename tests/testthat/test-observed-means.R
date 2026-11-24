## The observed means written at release (`R/observed_means.R`, run by
## `post_export()` in `R/post_export.R`). The calculation itself is
## `traits.use::summarise_trait_means()`, tested in traits.use; these test what
## is AusTraits' own: the value-type rule, and the files a release carries.
##
## Run from the repository root with
## `testthat::test_dir("tests/testthat", stop_on_failure = TRUE)`.

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "observed_means.R"), local = TRUE)

test_that("AVH_2026's label bounds are used as observations", {
  # One specimen bound per record, as the AVH height dataset has them
  records <- tibble::tibble(
    dataset_id = "AVH_2026", taxon_name = "Acacia a", observation_id = c("01", "02", "03"),
    trait_name = "plant_height", value = c("1", "4", "100"), unit = "m",
    value_type = c("minimum", "maximum", "raw"), basis_of_record = "preserved_specimen",
    location_id = NA_character_, original_name = "Acacia a"
  )

  means <- observed_means(records)
  # the two bounds are two observations in one group; `raw` is not a value
  # type this dataset is given
  expect_equal(means$value, 2)
  expect_equal(means$n_records, 2)
  expect_equal(means$n_flora, 0)

  # another dataset's bounds are a flora range each, and its raw record is used
  other <- observed_means(dplyr::mutate(records, dataset_id = "Other_2020"))
  expect_equal(other$n_records, 3)
  expect_equal(other$n_flora, 2)
})

test_that("the dictionary must match the columns written", {
  means <- observed_means(traits.use::austraits_lite)
  path <- withr::local_tempdir()
  expect_error(
    write_observed_means(dplyr::rename(means, mean = "value"), path, "0.0.0"),
    "do not match"
  )
  expect_error(
    write_observed_means(dplyr::relocate(means, "unit"), path, "0.0.0"),
    "the order differs"
  )
  expect_length(dir(path), 0)
})

test_that("`post_export()` writes the observed means of a database", {
  skip_if_not_installed("arrow")
  withr::local_dir(root)

  database <- traits.use::austraits_lite
  path <- withr::local_tempdir()

  suppressMessages(traits.build::build_post_export(database, path))

  version <- database$build_info$version
  stem <- file.path(path, sprintf("austraits-%s-observed-means", version))
  expect_true(all(file.exists(paste0(stem, c(".parquet", ".csv.gz", "-dictionary.csv")))))
  expect_true(file.exists(file.path(path, sprintf("austraits-%s-flattened.parquet", version))))

  means <- arrow::read_parquet(paste0(stem, ".parquet"))
  dictionary <- readr::read_csv(paste0(stem, "-dictionary.csv"), show_col_types = FALSE)
  expect_equal(names(means), dictionary$column)
  expect_equal(names(dictionary), c("column", "type", "definition"))
  expect_false(anyNA(dictionary$definition))

  # every trait of the database, one row per taxon x trait x unit
  expect_setequal(means$trait_name[means$type == "categorical"],
                  unique(database$traits$trait_name[is.na(database$traits$unit)]))
  expect_equal(unique(means$estimate_type), "observed_mean")
  # geometric, or arithmetic for a trait on a signed scale
  expect_true(all(means$method %in% c("geometric", "arithmetic")))
  expect_true(all(means$method[means$type == "categorical"] == "geometric"))
  expect_equal(anyDuplicated(means[c("taxon_name", "trait_name", "unit")]), 0)

  # the csv holds the same table
  csv <- readr::read_csv(paste0(stem, ".csv.gz"), show_col_types = FALSE, guess_max = Inf)
  expect_equal(dim(csv), dim(means))
  expect_equal(csv$value, means$value)

  # the hook leaves nothing behind in the global environment
  expect_false(exists("observed_means", envir = globalenv(), inherits = FALSE))
})
