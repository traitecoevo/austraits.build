
config_files <- c("configDataset.csv","configLookups.csv","configPlantCharacters.csv","configPlantVarNames.csv","data.csv","metadata.csv", "context.csv")

for (s in study_names) {

  context(sprintf("%s", basename(s)))
  test_that("Setup", {

  # Exists
    files <- file.path(s, config_files)
    for(f in files) {
      expect_that(file.exists(f), is_true(), info = f)
    }

  # configDataset.csv
  f <- files[1]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  expect_named(data, c("key","value"), info=f)
  vals <- c("plant_data_filename","header","skip","plant_char_vertical","process_plant_char")
  expect_contains(data[["key"]], vals, info=f)
  configDataset <- data

  # configLookups.csv
  f <- files[2]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  expect_named(data, c("trait_name","lookup","value"), info=f)

  # configPlantCharacters.csv
  f <- files[3]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  expect_named(data, c("var_name","trait_name","unit"), info=f)
  expect_isin(data[["trait_name"]], variable_definitions[["trait_name"]], info=f)
  configPlantCharacters <- data

  # configPlantVarNames.csv
  f <- files[4]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  expect_named(data, c("var_in","var_out"), info=f)
  vals <- c("species_name", "site_name", "metadata_id","primary_source_id", "trait_name", "unit", "value", "lookup")
  expect_isin(data[["var_out"]], vals, info=f)
  configPlantVarNames <- data

  # data.csv
  f <- files[5]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)

  ## Check config files contain all relevant columns
  if(get_config(configDataset, "plant_char_vertical")) {
    # For vertical datasets, expect all columns present in configPlantVarNames
    expect_equal(configPlantVarNames[["var_in"]], names(data), info=files[4])

    # Expect all values of "trait column" found in configPlantCharacters
    i <- match("trait_name", configPlantVarNames[["var_out"]])
    values <- unique(data[[configPlantVarNames[["var_in"]][i]]])
    expect_contains(configPlantCharacters[["var_name"]], values, info=files[3])
  } else {
    # For wide datasets, expect all columns are either in configPlantVarNames or configPlantCharacters
    # First check those in configPlantVarNames
    values <- names(data)
    expect_isin(configPlantCharacters[["var_name"]],values, info=files[3])
    # Now remove values in configPlantVarNames and check remainder are in configPlantCharacters
    values <- values[!is.na(values) & !values%in% configPlantVarNames[["var_in"]]]
    expect_contains(configPlantCharacters[["var_name"]], values, info=files[3])
    expect_equal(configPlantCharacters[["var_name"]], values, info=files[3])
  }

  # metadata.csv
  f <- files[6]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  vals <- c("dataset_id","dataset_num","metadata_id","primary_source_id","source_access","compiled_for","data_contributor","contributor_institution_2016","year_collected","primary_data_collector","data_collector_institution_2016","primary_lab_leader_when_collected","primary_lab_leader_institution","data_description","collection_type","sample_age_class","original_file","need_further_checking","notes")
  expect_contains(names(data), vals, info=f)

  # context.csv
  f <- files[7]
  data <- read_csv(f)
  expect_allowed_text(colnames(data), info = f)
  expect_allowed_text(unlist(data), info = f)
  expect_is(data, "data.frame", info = f)
  vals <- c("site_name","trait_name","unit","value","notes")
  expect_contains(names(data), vals, info=f)

  })
}
