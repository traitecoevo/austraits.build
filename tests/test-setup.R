
config_files <- c("data.csv", "metadata.yml")

test_dataframe_valid <- function(data, info) {
  expect_not_NA(colnames(data), info = info)
  expect_allowed_text(colnames(data), info = info)
  expect_unique(colnames(data), info = info)
  expect_is(data, "data.frame", info = info)
}

test_dataframe_named <- function(data, expected_colnames, info) {
  test_dataframe_valid(data, info)
  expect_named(data, expected_colnames, info= info)
}

test_dataframe_names_contain <- function(data, expected_colnames, info) {
  test_dataframe_valid(data, info)
  expect_contains(names(data), expected_colnames, info= info)
}

test_list <- function(data, info) {
  expect_is(data, "list", info = info)
}

test_list_names_valid <- function(data, info) {
  test_list(data, info)
  expect_not_NA(names(data), info = info)
  expect_allowed_text(names(data), info = info)
  expect_unique(names(data), info = info)
}


test_list_named <- function(data, expected_names, info) {
  test_list_names_valid(data, info)
  expect_named(data, expected_names, info= info)
}

test_list_named_contains <- function(data, expected_names, info) {
  test_list_names_valid(data, info)
  expect_isin(names(data), expected_names)
}


for (dataset_id in dataset_ids) {


  s <- file.path(root.dir, "data", dataset_id)

  context(sprintf("%s", dataset_id))
  test_that(dataset_id, {

  # Exists
  files <- file.path(s, config_files)
  for(f in files) {
    expect_true(file.exists(f), info = f)
  }
  
  # check for other files
  vals <- c("data.csv", "metadata.yml", "raw")
  expect_isin(dir(s), vals, info=paste(f, " disallowed files"))

  # Metadata
  f <- files[2]
  expect_allowed_text(readLines(f), info = f)
  expect_silent(metadata <- read_yaml(f))
  test_list_named(metadata, definitions$metadata$elements %>% names(), info=f)

  # source
  test_list(metadata[["source"]], info=f)
  test_list_names_valid(metadata[["source"]], info=f)
  
  v <- names(metadata[["source"]])
  i <- grepl("primary", v) | grepl("secondary", v)

  expect_contains(v, "primary", info=f)

  expect_true(sum(grepl("primary", v)) <=1,  info=paste(f, "sources can have max 1 type labelled 'primary': ", paste(v, collapse = ", ") ))

  expect_true(all(i),  info=paste(f, "sources must be either primary or secondary:", paste(v[!i], collapse = ", ") ))

  vals <- c("key", "bibtype", "author", "title", "year")

  for(bib in names(metadata[["source"]])) {
    expect_contains(names(metadata[["source"]][[bib]]), vals, info=f)
  }

  keys <- unlist(lapply(metadata[["source"]], "[[", "key"))

  expect_unique(keys, info = paste(f, "sources must have unique keys:", paste(keys, collapse = ", ") ))

  # people
  test_list(metadata[["people"]], info=f)
  expect_list_elements_contain(metadata[["people"]], definitions$metadata$elements$people$elements %>%names(), info=f)

  # dataset
  test_list_named(metadata[["dataset"]], definitions$metadata$elements$dataset$values %>% names(), info=f)

  # config
  test_list_named(metadata[["config"]], definitions$metadata$elements$config$elements %>% names(), info=f)
  expect_is(metadata[["config"]][["data_is_long_format"]], "logical")
  
  expect_is(metadata[["config"]][["variable_match"]], "list")

  # minimal requirements
  expect_isin(names(metadata[["config"]][["variable_match"]]), 
        c("species_name", "value","trait_name","site_name", "observation_id", "context_name"), 
        info=f, " - minimal requirements for variable_match")


  # contexts
  if(length(unlist(metadata[["contexts"]])) > 1){

    test_list(metadata[["contexts"]], info=f)

    expect_silent(
      contexts <-
      metadata$contexts %>%
      format_sites(dataset_id, context = TRUE) %>%
      add_all_columns(definitions, "contexts")
    )

    test_dataframe_names_contain(contexts, c("dataset_id", "context_name", "context_property", "value"), info=paste0(f, " - contexts"))

    for(v in names(metadata$contexts)) {
      test_list(metadata[["contexts"]][[v]], info=f)
      expect_contains(names(metadata[["contexts"]][[v]]), c("type", "description"), info=paste0(f, " - context: ", v))
    }
  }

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
 
    # check for allowable values of categorical variables
    expect_no_error(x <- metadata[["substitutions"]] %>% list_to_df() %>% split(.$trait_name))
   
    for(trait in names(x)) {
      if(!is.null(definitions$traits$elements[[ trait ]]) && definitions$traits$elements[[ trait ]]$type == "categorical") {
        to_check <- x[[trait]]$replace %>% unique() 
        allowable <- c(definitions$traits$elements[[ trait ]]$values %>% names(), NA)
        failing <- to_check[!(is.na(to_check) | 
                              to_check %in% allowable | 
                              to_check %>% sapply(check_all_values_in, allowable)
                              )]
        expect_length_zero(failing,
                    info = sprintf("%s - substitutions for `%s` have invalid replacement values", f, trait),
                    label=failing %>% paste(collapse = ", "))
      }
    }
  }

  # data.csv
  f <- files[1]
  expect_silent(data <- read_csv(f, col_types = cols(), guess_max = 100000))
  test_dataframe_valid(data, info=f)

  # custom R code
  txt <- metadata[["config"]][["custom_R_code"]]
  #expect_false(grepl("#", txt), label=paste0(files[3], "-custom_R_code cannot contain comments, except on last line"))
  expect_no_error(custom_manipulation(txt)(data), label=paste0(files[3], "-custom_R_code"))

  # Apply custom manipulations
  data <- custom_manipulation(txt)(data)  

  ## Check config files contain all relevant columns
  if(metadata[["config"]][["data_is_long_format"]]) {

    # Variable match
    expect_isin(names(metadata[["config"]][["variable_match"]]), c("species_name",  "trait_name", "value","site_name", "observation_id", "context_name"), info=paste0(f, " - variable_match"))  

    # For vertical datasets, expect all values of "trait column" found in traits
    var_out <- names(metadata[["config"]][["variable_match"]])
    var_in <- unlist(metadata[["config"]][["variable_match"]])
    i <- match("trait_name", var_out)
    values <- unique(data[[var_in[i]]])
    expect_contains(cfgChar[["var_in"]], values, info=files[2])
  } else {

    # Variable match
    expect_isin(names(metadata[["config"]][["variable_match"]]), c("species_name", "site_name", "observation_id", "context_name"), info=paste0(f, " - variable_match"))

    # For wide datasets, expect variables in cfgChar are header in the data
    values <- names(data)
    expect_isin(cfgChar[["var_in"]], values, info=files[2])
  }


  ## TODO

  ## Make sure specified columns exist


  ## TODO

  ## For numeric trait data, check it looks reasonable & converts properly

  ## check site_names are in sites dataset

  ## check context_names are in context dataset
  
  })
}
