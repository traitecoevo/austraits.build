## The observed means written at release (`R/observed_means.R`, run by
## `post_export()` in `R/post_export.R`).
##
## Run from the repository root with
## `testthat::test_dir("tests/testthat", stop_on_failure = TRUE)`.

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "observed_means.R"), local = TRUE)

# One trait record, with the columns `observed_means_records()` returns
record <- function(taxon_name, trait_name, value, value_type = "raw",
                   dataset_id = "D1", location_id = "01", observation_id = "01",
                   basis_of_record = "field", unit = "m") {
  tibble::tibble(
    dataset_id = dataset_id, taxon_name = taxon_name, observation_id = observation_id,
    trait_name = trait_name, value = as.character(value), unit = unit,
    value_type = value_type, basis_of_record = basis_of_record,
    location_id = location_id, original_name = taxon_name,
    taxon_rank = "species", genus = sub(" .*", "", taxon_name), family = "Fabaceae",
    establishment_means = "native", taxon_id = paste0("id-", taxon_name)
  )
}

test_that("every listed trait is defined in config/traits.yml", {
  expect_no_error(check_observed_means_traits(file = file.path(root, "config", "traits.yml")))
  expect_equal(anyDuplicated(observed_means_traits()), 0)
})

test_that("a listed trait missing from the definitions is an error naming it", {
  file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(traits = list(elements = list(plant_height = list(type = "numeric")))), file)
  expect_no_error(check_observed_means_traits("plant_height", file))
  expect_error(
    check_observed_means_traits(c("plant_height", "plant_hite", "leaf_lenth"), file),
    "not in `.*`: plant_hite, leaf_lenth\\. Update `observed_means_traits\\(\\)`"
  )
})

test_that("a numeric value is the geometric mean of location means and flora midpoints", {
  records <- dplyr::bind_rows(
    # one location with two records: geometric mean 2, arithmetic mean 2.5
    record("Acacia a", "plant_height", c(1, 4)),
    # a second location: 8
    record("Acacia a", "plant_height", 8, location_id = "02"),
    # a flora observation: geometric midpoint 4, arithmetic midpoint 5
    record("Acacia a", "plant_height", c(2, 8), value_type = c("minimum", "maximum"),
           dataset_id = "D2", location_id = NA, basis_of_record = "literature"),
    # none of these is used: a zero, a non-number, and a bound measured in the field
    record("Acacia a", "plant_height", 0),
    record("Acacia a", "plant_height", "tall"),
    record("Acacia a", "plant_height", 100, value_type = "maximum")
  )

  means <- estimate_observed_means(records)

  expect_equal(nrow(means), 1)
  expect_equal(means$value, 4)
  expect_equal(means$log_value, log(4))
  expect_equal(means$arithmetic_mean, mean(c(2.5, 8, 5)))
  expect_equal(c(means$value_min, means$value_max), c(1, 8))
  expect_equal(means$n_records, 5)
  expect_equal(means$n_locations, 2)
  expect_equal(means$n_datasets, 2)
  expect_equal(means$n_flora, 2)
  expect_equal(means$n_field, 3)
  expect_equal(means$sd_log, sd(log(c(1, 4, 8, 2, 8))))
  expect_equal(means$dataset_id, "D1; D2")
  expect_equal(means$type, "numerical")
  expect_equal(means$reliability, "medium")
  expect_equal(means$taxon_id, "id-Acacia a")
})

test_that("a lone flora bound stands in for the midpoint", {
  records <- record("Acacia b", "plant_height", 3, value_type = "minimum",
                    basis_of_record = "preserved_specimen")
  means <- estimate_observed_means(records)
  expect_equal(means$value, 3)
  expect_equal(means$n_flora, 1)
  expect_true(is.na(means$sd_log))
  expect_equal(means$reliability, "low")
})

test_that("a dataset with a value-type rule enters by the value types listed", {
  records <- dplyr::bind_rows(
    record("Acacia c", "plant_height", c(1, 4), value_type = c("minimum", "maximum"),
           dataset_id = "AVH_2026", location_id = NA, observation_id = c("01", "02"),
           basis_of_record = "preserved_specimen"),
    # not a listed value type for this dataset
    record("Acacia c", "plant_height", 100, dataset_id = "AVH_2026", location_id = NA)
  )
  means <- estimate_observed_means(records)
  # the bounds are observations in one dataset x location group, not a flora midpoint
  expect_equal(means$value, 2)
  expect_equal(means$n_records, 2)
  expect_equal(means$n_locations, 1)
  expect_equal(means$n_flora, 0)

  # without the rule the bounds are a flora observation each, and the raw record is used
  no_rules <- tibble::tibble(dataset_id = character(), value_type = character())
  expect_equal(estimate_observed_means(records, no_rules)$n_records, 3)
})

test_that("units of one trait are kept apart", {
  records <- dplyr::bind_rows(
    record("Acacia a", "plant_height", 2),
    record("Acacia a", "plant_height", 200, unit = "cm")
  )
  means <- estimate_observed_means(records)
  expect_equal(nrow(means), 2)
  expect_equal(means$value[order(means$unit)], c(200, 2))
  expect_equal(sort(means$unit), c("cm", "m"))
})

test_that("a categorical trait gets its modal value, with ties joined in sorted order", {
  records <- dplyr::bind_rows(
    record("Acacia a", "plant_growth_form", c("tree shrub", "tree", "shrub", "herb"), unit = NA),
    record("Acacia b", "plant_growth_form", c("tree", "tree"), unit = NA, dataset_id = c("D1", "D2"))
  )
  means <- estimate_observed_means(records)

  a <- means[means$taxon_name == "Acacia a", ]
  expect_equal(a$value_mode, "shrub tree")
  expect_equal(a$value_count, "herb (1); shrub (2); tree (2)")
  expect_equal(a$n_records, 4)
  expect_equal(a$type, "categorical")
  expect_true(is.na(a$value))

  b <- means[means$taxon_name == "Acacia b", ]
  expect_equal(b$value_mode, "tree")
  expect_equal(b$n_datasets, 2)
})

test_that("subspecies, varieties and forms are rolled up to their species", {
  taxa <- tibble::tibble(
    taxon_name = c("Acacia a", "Acacia a subsp. x", "Acacia d var. y", "Acacia"),
    taxon_rank = c("species", "subspecies", "variety", "genus"),
    binomial = c("Acacia a", "Acacia a", "Acacia d", NA),
    establishment_means = c("native", "naturalised", "native", NA),
    taxon_id = c("id-a", "id-a-x", "id-d-y", "id-genus")
  )
  flattened <- dplyr::bind_rows(
    record("Acacia a", "plant_height", 2),
    record("Acacia a subsp. x", "plant_height", 8, location_id = "02"),
    record("Acacia d var. y", "plant_height", 5),
    record("Acacia", "plant_height", 7),
    record("Acacia a", "leaf_length", 30, unit = "mm")
  ) |>
    dplyr::select(-"establishment_means", -"taxon_id") |>
    dplyr::mutate(taxon_rank = taxa$taxon_rank[match(taxon_name, taxa$taxon_name)])

  records <- observed_means_records(flattened, taxa, traits = "plant_height")
  expect_equal(records$taxon_name, c("Acacia a", "Acacia a", "Acacia d", "Acacia"))
  expect_equal(records$taxon_rank, c("species", "species", "species", "genus"))
  # details come from the species' own row; a species with no row has none
  expect_equal(records$taxon_id, c("id-a", "id-a", NA, "id-genus"))
  expect_equal(records$establishment_means, c("native", "native", NA, NA))

  means <- estimate_observed_means(records)
  expect_equal(means$taxon_name, c("Acacia", "Acacia a", "Acacia d"))
  expect_equal(means$value[means$taxon_name == "Acacia a"], 4)
})

test_that("hybrids roll up to the nothospecies, and hybrid formulae not at all", {
  # Names and binomials as the taxa table of the development build has them:
  # for a hybrid, `binomial` is the first two words, which is not the species
  taxa <- tibble::tibble(
    taxon_name = c(
      "Mentha x piperita var. citrata",
      "Acacia trachycarpa x Acacia tumida var. pilbarensis",
      "Myoporum acuminatum - Myoporum boninense subsp. australe",
      "Prostanthera latifolia"
    ),
    taxon_rank = c("variety", "variety", "subspecies", "variety"),
    binomial = c("Mentha x", "Acacia trachycarpa", "Myoporum acuminatum", "Prostanthera latifolia"),
    establishment_means = NA_character_,
    taxon_id = NA_character_
  )
  flattened <- dplyr::bind_rows(lapply(taxa$taxon_name, record, trait_name = "plant_height", value = 1)) |>
    dplyr::select(-"establishment_means", -"taxon_id") |>
    dplyr::mutate(taxon_rank = taxa$taxon_rank)

  records <- observed_means_records(flattened, taxa, traits = "plant_height")
  expect_equal(records$taxon_name, c("Mentha x piperita", taxa$taxon_name[2:4]))
  expect_equal(records$taxon_rank, c("species", "variety", "subspecies", "variety"))
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

  expect_gt(nrow(means), 0)
  expect_true(all(means$trait_name %in% observed_means_traits()))
  expect_equal(unique(means$estimate_type), "observed_mean")
  expect_equal(unique(means$method), "geometric")
  # one row per taxon x trait x unit
  expect_equal(anyDuplicated(means[c("taxon_name", "trait_name", "unit")]), 0)
  # no infraspecific taxa, other than hybrid formulae
  infraspecific <- means$taxon_rank %in% c("subspecies", "variety", "form")
  expect_false(any(infraspecific & !grepl("\\s(x|-)\\s[A-Z]", means$taxon_name)))

  numeric <- means[means$type == "numerical", ]
  expect_true(all(numeric$value > 0))
  expect_true(all(numeric$value >= numeric$value_min * (1 - 1e-9)))
  expect_true(all(numeric$value <= numeric$value_max * (1 + 1e-9)))
  expect_true(all(numeric$arithmetic_mean >= numeric$value * (1 - 1e-9)))

  # the csv holds the same table
  csv <- readr::read_csv(paste0(stem, ".csv.gz"), show_col_types = FALSE, guess_max = Inf)
  expect_equal(dim(csv), dim(means))
  expect_equal(csv$value, means$value)

  # the hook leaves nothing behind in the global environment
  expect_false(exists("observed_means", envir = globalenv(), inherits = FALSE))
})
