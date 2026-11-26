## The observed means written at release (`R/observed_means.R`, run by
## `post_export()` in `R/post_export.R`). The calculation itself is
## `traits.use::summarise_trait_means()`, tested in traits.use; these test what
## is AusTraits' own: the value-type rule, and the files a release carries.
##
## Run from the repository root with
## `testthat::test_dir("tests/testthat", stop_on_failure = TRUE)`.

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "observed_means.R"), local = TRUE)

test_that("AVH_2026's label bounds are used as records", {
  # As the AVH height dataset has them: one bound per specimen, basis field,
  # each specimen at its own location; one location with two specimens
  records <- tibble::tibble(
    dataset_id = "AVH_2026", taxon_name = "Acacia a", observation_id = c("01", "02", "03", "04"),
    trait_name = "plant_height", value = c("1", "4", "2", "8"), unit = "m",
    value_type = c("minimum", "maximum", "maximum", "maximum"), basis_of_record = "field",
    location_id = c("01", "02", "03", "03"), original_name = "Acacia a",
    taxonomic_dataset = "APC", taxonomic_status = "accepted", taxon_rank = "species"
  )

  means <- observed_means(records)
  # three location groups: 1, 4 and the geometric mean of 2 and 8, which is 4
  expect_equal(means$n_records, 4)
  expect_equal(means$n_locations, 3)
  expect_equal(means$value, exp(mean(log(c(1, 4, 4)))))
  expect_equal(means$n_flora, 0)

  # without the rule, field bounds are not used at all
  other <- traits.use::summarise_trait_means(records)
  expect_equal(nrow(other), 0)
})

test_that("only APC-accepted species are published (#922)", {
  taxa <- tibble::tribble(
    ~taxon_name,                                ~taxonomic_dataset, ~taxonomic_status,  ~taxon_rank,
    "Acacia a",                                 "APC",              "accepted",         "species",
    "Acacia b subsp. c",                        "APC",              "accepted",         "subspecies",
    "Acacia x grayana",                         "APC",              "accepted",         "species",
    "Acacia d x Acacia e",                      "APC",              "accepted",         "species",
    "(Acacia d x Acacia e) x Acacia f",         "APC",              "accepted",         "species",
    "Acacia g subsp. h - Acacia g subsp. i",    "APC",              "accepted",         "subspecies",
    "Acacia",                                   "APC",              "genus accepted",   "genus",
    "Acacia sp. [Beerburrum; Dorey_2023]",      NA,                 NA,                 NA,
    "Grevillea sp. Trayning (W. Johnston 071)", "APNI",             "unplaced",         "species",
    "Cirsium palustre",                         "APC",              "excluded",         "species",
    "Platanus x acerifolia",                    "APC",              "nomenclatural synonym", "species"
  )
  expect_equal(observed_means_accepted(taxa),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_error(observed_means_accepted(taxa["taxon_name"]), "needs the taxonomy")

  # the database and its flattened table give the same means, all of
  # accepted species; the subspecies' records count towards their species
  database <- traits.use::austraits_lite
  means <- observed_means(database)
  accepted <- database$taxa$taxon_name[observed_means_accepted(database$taxa)]
  species <- unique(sub(" (subsp|var|f)\\. .*$", "", accepted))
  expect_true(all(means$taxon_name %in% species))
  expect_true(all(means$taxon_rank %in% "species"))
  dropped <- setdiff(unique(database$traits$taxon_name), accepted)
  expect_gt(length(dropped), 0)
  expect_false(any(dropped %in% means$taxon_name))
  # (a flat table has no definitions, so its signed traits can differ in
  # method; the taxa and traits published are the same)
  keys <- c("taxon_name", "trait_name", "unit")
  flat <- observed_means(traits.use::flatten_database(database))
  expect_equal(flat[keys], means[keys])
})

test_that("the dictionary must match the columns written", {
  means <- observed_means(traits.use::austraits_lite)
  path <- withr::local_tempdir()

  # a database without some taxon columns writes the columns it has
  fewer <- dplyr::select(means, -"establishment_means")
  expect_length(write_observed_means(fewer, path, "0.0.0"), 4)
  unlink(dir(path, full.names = TRUE))

  # an unversioned database cannot name its files
  expect_error(write_observed_means(means, path, NULL), "no version")
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
  expect_true(all(file.exists(paste0(stem, c(".parquet", ".rds", ".csv.gz", "-dictionary.csv")))))
  expect_true(file.exists(file.path(path, sprintf("austraits-%s-flattened.parquet", version))))

  means <- arrow::read_parquet(paste0(stem, ".parquet"))
  dictionary <- readr::read_csv(paste0(stem, "-dictionary.csv"), show_col_types = FALSE)
  expect_equal(names(means), dictionary$column)
  expect_equal(names(dictionary), c("column", "type", "definition"))
  expect_false(anyNA(dictionary$definition))

  # every categorical trait of the database, and every numeric trait with a
  # usable record, of APC-accepted species; one row per taxon x trait x unit
  records <- database$traits[database$traits$taxon_name %in%
                               database$taxa$taxon_name[observed_means_accepted(database$taxa)], ]
  expect_setequal(means$trait_name[means$type == "categorical"],
                  unique(records$trait_name[is.na(records$unit)]))
  numeric_traits <- unique(records$trait_name[!is.na(records$unit) &
                                                records$value_type %in% c("raw", "mean", "median")])
  expect_true(all(numeric_traits %in% means$trait_name))
  expect_equal(unique(means$estimate_type), "observed_mean")
  # arithmetic for traits on a signed scale, which austraits_lite has
  signed <- names(Filter(function(d) isTRUE(as.numeric(d$allowed_values_min) < 0), database$definitions))
  expect_equal(sort(unique(means$trait_name[means$method == "arithmetic"])),
               sort(intersect(signed, means$trait_name)))
  expect_gt(sum(means$method == "arithmetic"), 0)
  # categorical rows: the most frequent value, or for month traits the months
  # at least half the records give
  expect_true(all(means$method[means$type == "categorical"] %in% c("mode", "majority")))
  expect_equal(anyDuplicated(means[c("taxon_name", "trait_name", "unit")]), 0)

  # the rds and the csv.gz hold the same table
  expect_equal(as.data.frame(readRDS(paste0(stem, ".rds"))), as.data.frame(means))
  csv <- readr::read_csv(paste0(stem, ".csv.gz"), show_col_types = FALSE, guess_max = Inf)
  expect_equal(dim(csv), dim(means))
  expect_equal(csv$value, means$value)

  # the hook leaves nothing behind in the global environment
  expect_false(exists("observed_means", envir = globalenv(), inherits = FALSE))
})
