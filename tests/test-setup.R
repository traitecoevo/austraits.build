
config_files <- c("configDataset.csv","configLookups.csv","configPlantCharacters.csv","configPlantVarNames.csv","data.csv","metadata.csv")

      #"configSiteCharacters.csv","configSiteDataVarNames.csv","configSiteVarNames.csv", "site_data.csv

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
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)
  expect_that(names(data), equals(c("key","value")), info=f)
  vals <- c("plant_data_filename","site_data_filename","header","skip","plant_char_vertical","site_char_vertical","process_plant_char","process_site_char","process_site_data")
  expect_that(data[["key"]], contains(vals), info=f)
  configDataset <- data

  # configLookups.csv
  f <- files[2]
  data <- read_csv(f)
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)
  expect_that(names(data), equals(c("character","lookup","value")), info=f)

  # configPlantCharacters.csv
  f <- files[3]
  data <- read_csv(f)
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)
  expect_that(names(data), equals(c("var_name","character","unit")), info=f)
#  expect_that(data[["character"]], isin(variable_definitions[["variable"]]), info=f)
  configPlantCharacters <- data

  # configPlantVarNames.csv
  f <- files[4]
  data <- read_csv(f)
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)
  expect_that(names(data), equals(c("var_in","var_out")), info=f)
  vals <- c("species_name", "site_name", "metadata_id","primary_source_id", "character", "unit", "value", "lookup")
  expect_that(data[["var_out"]], isin(vals), info=f)
  configPlantVarNames <- data

  # data.csv
  f <- files[5]
  data <- read_csv(f)
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)

  f <- function(x) {
    x[!x%in% c("-", "metadata_id","primary_source_id")]
  }
  ## Check headers in data
  if(getConfig(configDataset, "plant_char_vertical")) {
    expect_that(f(configPlantVarNames[["var_in"]]), isin(names(data)), info=s)
  } else {
    expect_that(f(configPlantCharacters[["var_name"]]), isin(names(data)), info=s)
  }

  # metadata.csv
  f <- files[6]
  data <- read_csv(f)
  expect_that(colnames(data), is_allowed_text(), info = f)
  expect_that(unlist(data), is_allowed_text(), info = f)
  expect_that(data, is_a("data.frame"), info = f)
  vals <- c("dataset_id","dataset_num","metadata_id","primary_source_id","source_access","compiled_for","data_contributor","contributor_institution_2016","year_collected","primary_data_collector","data_collector_institution_2016","primary_lab_leader_when_collected","primary_lab_leader_institution","data_description","collection_type","sample_age_class","original_file","need_further_checking","notes")
  expect_that(names(data), contains(vals), info=f)
  })
}
