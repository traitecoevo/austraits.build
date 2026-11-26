## The observed means written at release (`R/observed_means.R`, run by
## `post_export()` in `R/post_export.R`). The calculation itself is
## `traits.use::summarise_trait_means()`, tested in traits.use; these test what
## is AusTraits' own: the taxa published (APC-accepted species only), the
## value-type rule, and the files a release carries.
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

test_that("hybrid formulae are told from nothospecies and phrase names", {
  formulae <- c(
    "Acacia ampliceps x Acacia bivenosa",
    "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis",
    "Apium prostratum var. filiforme - Apium prostratum var. prostratum",
    "Salix x fragilis x Salix nigra",
    "Acacia monticola x (Acacia d x Acacia e)"
  )
  not_formulae <- c(
    "Acacia x grayana",
    "Eucalyptus pauciflora var. x cylindrocarpa",
    "Populus x Loch Valley (I.C.Clarke 2861)",
    "Acacia sp. Beyondie - Newman (B.R.Maslin 1234)",
    "Acacia aneura"
  )
  expect_equal(observed_means_hybrid_formula(formulae), rep(TRUE, 5))
  expect_equal(observed_means_hybrid_formula(not_formulae), rep(FALSE, 5))
  expect_equal(observed_means_hybrid_formula(character()), logical())
})

test_that("only APC-accepted species are published (#922)", {
  # each row fails one condition at most, so each condition is tested alone
  taxa <- tibble::tribble(
    ~taxon_name,                                ~taxonomic_dataset, ~taxonomic_status,  ~taxon_rank,   ~ok,
    "Acacia a",                                 "APC",              "accepted",         "species",     TRUE,
    "Acacia b subsp. c",                        "APC",              "accepted",         "subspecies",  TRUE,
    "Acacia h var. i",                          "APC",              "accepted",         "variety",     TRUE,
    "Acacia j f. k",                            "APC",              "accepted",         "form",        TRUE,
    "Acacia x grayana",                         "APC",              "accepted",         "species",     TRUE,
    "Populus x Loch Valley (I.C.Clarke 2861)",  "APC",              "accepted",         "species",     TRUE,
    "Acacia d x Acacia e",                      "APC",              "accepted",         "species",     FALSE,
    "Acacia",                                   "APC",              "accepted",         "genus",       FALSE,
    "Acacia l",                                 "APNI",             "accepted",         "species",     FALSE,
    "Acacia m",                                 "APC",              "excluded",         "species",     FALSE,
    "Acacia n",                                 NA,                 NA,                 NA,            FALSE,
    NA,                                         "APC",              "accepted",         "species",     FALSE
  )
  expect_equal(observed_means_accepted(taxa), taxa$ok)
  expect_error(observed_means_accepted(taxa["taxon_name"]), "needs the taxonomy")
  expect_setequal(observed_means_species(taxa),
                  c("Acacia a", "Acacia b", "Acacia h", "Acacia j", "Acacia x grayana",
                    "Populus x Loch Valley (I.C.Clarke 2861)"))

  # records of an infraspecific name the APC does not place count towards
  # the accepted species; a species published only through its accepted
  # subspecies; nothing of genera, unplaced or excluded names
  record <- function(taxon_name, value, dataset, status, rank) {
    tibble::tibble(
      dataset_id = "D1", taxon_name = taxon_name, observation_id = "01",
      trait_name = "plant_height", value = as.character(value), unit = "m",
      value_type = "raw", basis_of_record = "field", location_id = dataset,
      original_name = taxon_name, taxonomic_dataset = dataset,
      taxonomic_status = status, taxon_rank = rank
    )
  }
  records <- dplyr::bind_rows(
    record("Aloe vera", 1, "APC", "accepted", "species"),
    record("Aloe vera var. vera", 4, "APNI", "unplaced", "variety"),
    record("Acacia b subsp. c", 2, "APC", "accepted", "subspecies"),
    record("Acacia", 3, "APC", "genus accepted", "genus"),
    record("Grevillea sp. Trayning (W. Johnston 071)", 3, "APNI", "unplaced", "species"),
    record("Cirsium palustre", 3, "APC", "excluded", "species")
  )
  means <- observed_means(records)
  expect_equal(means$taxon_name, c("Acacia b", "Aloe vera"))
  expect_equal(means$n_records, c(1, 2))
  expect_equal(means$value, c(2, 2))

  # a database and its flattened table publish the same taxa and traits
  # (a flat table has no definitions, so its signed traits can differ in
  # method), all species, of APC-accepted names
  database <- traits.use::austraits_lite
  means <- observed_means(database)
  expect_true(all(means$taxon_name %in% observed_means_species(database$taxa)))
  expect_true(all(means$taxon_rank %in% "species"))
  dropped <- database$taxa$taxon_name[!observed_means_accepted(database$taxa) &
                                        !database$taxa$taxon_rank %in% c("subspecies", "variety", "form")]
  expect_gt(length(intersect(dropped, database$traits$taxon_name)), 0)
  expect_false(any(dropped %in% means$taxon_name))
  keys <- c("taxon_name", "trait_name", "unit")
  flat <- observed_means(traits.use::flatten_database(database))
  expect_equal(flat[keys], means[keys])
  unsigned <- !means$method %in% "arithmetic" & !flat$method %in% "arithmetic"
  expect_equal(flat[unsigned, c(keys, "value", "value_mode", "n_records")],
               means[unsigned, c(keys, "value", "value_mode", "n_records")])
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
  # the dictionary says what AusTraits publishes, not traits.use's default
  expect_match(dictionary$definition[dictionary$column == "taxon_name"], "Australian Plant Census")
  expect_true(all(means$taxon_name %in% observed_means_species(database$taxa)))

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
