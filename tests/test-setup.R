
config_files <- c("data.csv", "context.csv", "metadata.yml")

test_dataframe <- function(data, expected_colnames, info) {
  expect_not_NA(colnames(data), info = info)
  expect_allowed_text(colnames(data), info = info)
  expect_unique(colnames(data), info = info)
  expect_allowed_text(unlist(data), info = info)
  expect_is(data, "data.frame", info = info)
  expect_named(data, expected_colnames, info= info)
}

test_list <- function(data, info) {
  expect_allowed_text(unlist(data), info = info)
  expect_is(data, "list", info = info)
}

test_list_named <- function(data, expected_names, info) {
  test_list(data, info)
  expect_not_NA(names(data), info = info)
  expect_allowed_text(names(data), info = info)
  expect_unique(names(data), info = info)
  expect_named(data, expected_names, info= info)
}

expect_list_elements_contain <- function(object, expected, ...) {
  tmp <- lapply(object, function(x) expect_contains(names(x), expected, ...))
  invisible(object)
}

for (s in study_names) {

  context(sprintf("%s", basename(s)))
  test_that("Setup", {

  # Exists
    files <- file.path(s, config_files)
    for(f in files) {
      expect_that(file.exists(f), is_true(), info = f)
    }

  # Context
  f <- files[2]
  context <- read_csv(f)
  test_dataframe(context, c("site_name","trait_name","unit","value","notes"), info=f)

  # Metadata
  f <- files[3]
  expect_allowed_text(readLines(f), info = f)
  metadata <- read_yaml(f)
  vals <- c("source","people","dataset","config","traits","substitutions")
  test_list_named(metadata, vals, info=f)

  vals <- c("is_vertical", "variable_match","custom_R_code")
  test_list_named(metadata[["config"]], vals, info=f)

  # source
  test_list(metadata[["source"]], info=f)
  vals <- c("primary", "type", "handle", "secondary")
  test_list_named(metadata[["source"]], vals, info=f)

  # people
  test_list(metadata[["people"]], info=f)
  vals <- c("name", "institution", "role")
  expect_list_elements_contain(metadata[["people"]], vals, info=f)

  # dataset
  vals <- c("year_collected_start", "year_collected_end", "description", "collection_type", "sample_age_class", "sampling_strategy", "original_file", "notes")
  test_list_named(metadata[["dataset"]], vals, info=f)

  # config
  vals <- c("is_vertical", "variable_match", "custom_R_code")
  test_list_named(metadata[["config"]], vals, info=f)
  expect_is(metadata[["config"]][["is_vertical"]], "logical")

  # config - Variable_match
  expect_is(metadata[["config"]][["variable_match"]], "list")
  var_in <- unlist(metadata[["config"]][["variable_match"]])
  var_out <- names(metadata[["config"]][["variable_match"]])
  vals <- c("species_name", "site_name", "trait_name", "unit", "value")
  expect_isin(var_out, vals, info=f)

  # Traits
  vals <- c("var_in", "unit_in", "trait_name", "value_type", "replicates", "precision", "methodology_ids")
  expect_list_elements_contain(metadata[["traits"]], vals, info=f)
  trait_names <- sapply(metadata[["traits"]], "[[", "trait_name")
  expect_isin(trait_names, variable_definitions[["trait_name"]], info=f)
  value_types <- sapply(metadata[["traits"]], "[[", "value_type")
  allowed <- c("unknown", "raw", "mean", "max", "min", "lower_quantile", "upper_quantile", "expert_opinion")
  expect_isin(value_types, allowed, info=f)
  cfgChar <- list_to_df(metadata[["traits"]])
  expect_is(cfgChar, "data.frame")

  # Substitutions
  if(!is.na(metadata[["substitutions"]][1])) {
    vals <- c("trait_name", "find", "replace")
    expect_list_elements_contain(metadata[["substitutions"]], vals)
    trait_names <- sapply(metadata[["substitutions"]], "[[", "trait_name")
    expect_isin(unique(trait_names), variable_definitions[["trait_name"]], info=f)
    expect_isin(unique(trait_names), unique(sapply(metadata[["traits"]], "[[", "trait_name")), info=paste0(f, " - substitutions"))
  }

  # data.csv
  f <- files[1]
  data <- read_csv(f)
  test_dataframe(data, names(data), info=f)

  # custom R code
  txt <- metadata[["config"]][["custom_R_code"]]
  expect_false(grepl("#", txt), label=paste0(files[3], "-custom_R_code cannot contain comments"))
  expect_no_error(custom_manipulation(txt)(data), label=paste0(files[3], "-custom_R_code"))

  # Apply custom manipulations
  data <- custom_manipulation(txt)(data)  

  ## Check config files contain all relevant columns
  if(metadata[["config"]][["is_vertical"]]) {

    # For vertical datasets, expect all values of "trait column" found in traits
    i <- match("trait_name", var_out)
    values <- unique(data[[var_in[i]]])
    expect_contains(cfgChar[["var_in"]], values, info=files[3])
  } else {
    # For wide datasets, expect variables in cfgChar are header in the data
    values <- names(data)
    expect_isin(cfgChar[["var_in"]],values, info=files[3])
  }

  })
}
