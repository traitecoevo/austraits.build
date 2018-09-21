
config_files <- c("data.csv", "metadata.yml")

test_dataframe <- function(data, expected_colnames, info) {
  expect_not_NA(colnames(data), info = info)
  expect_allowed_text(colnames(data), info = info)
  expect_unique(colnames(data), info = info)
  expect_is(data, "data.frame", info = info)
  expect_named(data, expected_colnames, info= info)
}

test_list <- function(data, info) {
  expect_is(data, "list", info = info)
}

test_list_named <- function(data, expected_names, info) {
  test_list(data, info)
  expect_not_NA(names(data), info = info)
  expect_allowed_text(names(data), info = info)
  expect_unique(names(data), info = info)
  expect_named(data, expected_names, info= info)
}

test_list_named_contains <- function(data, expected_names, info) {
  test_list(data, info)
  expect_not_NA(names(data), info = info)
  expect_allowed_text(names(data), info = info)
  expect_unique(names(data), info = info)
  expect_named(data, info= info)
  expect_isin(names(data), expected_names)
}


for (dataset_id in dataset_ids) {


  s <- file.path(root.dir, "data", dataset_id)

  context(sprintf("%s", dataset_id))
  test_that(dataset_id, {

  # Exists
    files <- file.path(s, config_files)
    for(f in files) {
      expect_that(file.exists(f), is_true(), info = f)
    }

  # Metadata
  f <- files[2]
  expect_allowed_text(readLines(f), info = f)
  expect_silent(metadata <- read_yaml(f))
  test_list_named(metadata, definitions$metadata$elements %>% names(), info=f)

  # source
  test_list(metadata[["source"]], info=f)
  expect_isin(names(metadata[["source"]]), definitions$metadata$elements$source$values %>% names(), info=f)
  vals <- c("key", "bibtype", "author", "title", "year")
  expect_isin(vals, names(metadata[["source"]][["primary"]]), info=f)
  if(!is.null(metadata[["source"]][["secondary"]])){
    expect_isin(vals, names(metadata[["source"]][["secondary"]]), info=f)
  }

  # people
  test_list(metadata[["people"]], info=f)
  expect_list_elements_contain(metadata[["people"]], definitions$metadata$elements$people$elements %>%names(), info=f)

  # dataset
  test_list_named(metadata[["dataset"]], definitions$metadata$elements$dataset$values %>% names(), info=f)

  # config
  test_list_named(metadata[["config"]], definitions$metadata$elements$config$elements %>% names(), info=f)
  expect_is(metadata[["config"]][["is_vertical"]], "logical")
  
  expect_is(metadata[["config"]][["variable_match"]], "list")
  expect_isin(names(metadata[["config"]][["variable_match"]]), 
        c("species_name", "value","trait_name","site_name", "observation_id"), 
        info=f)

  # Traits 
  expect_list_elements_contain(metadata[["traits"]], definitions$metadata$elements$traits$elements %>% names(), info=f)
  trait_names <- sapply(metadata[["traits"]], "[[", "trait_name")
  expect_isin(trait_names, definitions$traits$elements %>% names(), info=f)
  value_types <- sapply(metadata[["traits"]], "[[", "value_type")
  allowed <- definitions$value_type$values %>% names
  expect_isin(value_types, allowed, info=paste0(f, " - value types"))
  cfgChar <- list_to_df(metadata[["traits"]])
  expect_is(cfgChar, "data.frame")

  # Substitutions
  if(!is.na(metadata[["substitutions"]][1])) {
    expect_list_elements_contain(metadata[["substitutions"]], definitions$metadata$elements$substitutions$values %>% names())
    trait_names <- sapply(metadata[["substitutions"]], "[[", "trait_name")
    expect_isin(unique(trait_names), definitions$traits$elements %>% names(), info=f)
    expect_isin(unique(trait_names), unique(sapply(metadata[["traits"]], "[[", "trait_name")), info=paste0(f, " - substitutions"))
  }

  # data.csv
  f <- files[1]
  expect_silent(data <- read_csv(f, col_types = cols()))
  test_dataframe(data, names(data), info=f)

  # custom R code
  txt <- metadata[["config"]][["custom_R_code"]]
  expect_false(grepl("#", txt), label=paste0(files[3], "-custom_R_code cannot contain comments"))
  expect_no_error(custom_manipulation(txt)(data), label=paste0(files[3], "-custom_R_code"))

  # Apply custom manipulations
  data <- custom_manipulation(txt)(data)  

  ## Check config files contain all relevant columns
  if(metadata[["config"]][["is_vertical"]]) {

    # Variable match
    expect_isin(names(metadata[["config"]][["variable_match"]]), c("species_name",  "trait_name", "value","site_name"), info=paste0(f, " - variable_match"))  

    # For vertical datasets, expect all values of "trait column" found in traits
    var_out <- names(metadata[["config"]][["variable_match"]])
    var_in <- unlist(metadata[["config"]][["variable_match"]])
    i <- match("trait_name", var_out)
    values <- unique(data[[var_in[i]]])
    expect_contains(cfgChar[["var_in"]], values, info=files[3])
  } else {

    # Variable match
    expect_isin(names(metadata[["config"]][["variable_match"]]), c("species_name", "site_name"), info=paste0(f, " - variable_match"))

    # For wide datasets, expect variables in cfgChar are header in the data
    values <- names(data)
    expect_isin(cfgChar[["var_in"]],values, info=files[3])
  }

  ## For numeric trait data, check it looks reasonable



  })
}
