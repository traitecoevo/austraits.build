
# Better than expect_silent as contains `info` and allows for complete failures
expect_no_error <- function (object, regexp = NULL, ..., info = NULL, label = NULL)
{
  error <- tryCatch({
    object
    NULL
  }, error = function(e) {
    e
  })
  expect(is.null(error), sprintf("%s threw an error: %s", label, paste(error$message, collapse=",")), info = info)
  invisible(NULL)
}


expect_unique <- function (object, info = NULL, label = NULL) {
  x <- table(unlist(object))
  i <- x==1
  comp <- compare(all(i), TRUE)
  expect(comp$equal,
         sprintf("%s - not unique: %s", info, paste(names(x)[!i], collapse= ", ")))
  invisible(object)
}

expect_isin <- function(object, expected, ..., info = NULL, label = NULL,
                        expected.label = NULL, na.rm=TRUE) {
  
  if(na.rm)
    object <- object[!is.na(object)]
  i <- object %in% expected
  
  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - should not contain: %s", info, paste(object[!i], collapse= ", "))
  )
  
  invisible(object)
}

expect_not_NA <- function (object, info = NULL, label = NULL) {
  i <- !is.na(object)
  comp <- compare(all(i), TRUE)
  expect(comp$equal,
         sprintf("%s - contains NAs: %s", info, label))
  invisible(object)
}

test_list <- function(data, info) {
  expect_true(class(data)=="list", info = info)
}

test_list_names_valid <- function(data, info) {
  test_list(data, info)
  expect_not_NA(names(data), info = info)
#  expect_allowed_text(names(data), info = info)
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


test_dataframe_valid <- function(data, info) {
  expect_not_NA(colnames(data), info = info)
#  expect_allowed_text(colnames(data), info = info)
  expect_unique(colnames(data), info = info)
  expect_true(is.data.frame(data), info = info)
}

test_dataframe_named <- function(data, expected_colnames, info) {
  test_dataframe_valid(data, info)
  expect_named(data, expected_colnames, info= info)
}


test_build_dataset <- function(path_metadata, path_data, info, definitions, unit_conversions, schema, resource_metadata, taxon_list) {
  
  # test it builds with no errors
  expect_no_error({
    build_config <- dataset_configure(path_metadata, definitions, unit_conversions)
  }, info = paste(info, " config"))
  
  expect_no_error({
    build_dataset_raw <- dataset_process(path_data, build_config, schema, resource_metadata)
  }, info = paste(info, " dataset_process"))
  
  expect_no_error({
    build_dataset <- build_update_taxonomy(build_dataset_raw, taxon_list)
  }, info = paste(info, " update taxonomy"))
  
  test_structure(build_dataset, info, schema, definitions, single_dataset = TRUE)
  
  build_dataset
}

test_structure <- function(data, info, schema, definitions, single_dataset = TRUE) {
  
  vars_austraits <-
    schema$austraits$elements %>% names()

  vars_tables <- vars_austraits %>% subset(., !(. %in% c("dataset_id", "definitions", "schema", "sources", "metadata", "build_info")))

  # test lists have the right objects
  comparison <- vars_austraits
  
  test_list_named(data, comparison, info = c(info, " - main elements"))
  
  # test structure of tables
  for(v in vars_tables) {
    
    comparison <- schema$austraits$elements[[v]]$elements %>% names()
       
    test_dataframe_named(data[[v]], comparison, info = paste(info, " - structure of ", v))
  }
  
  # contains allowed traits
  expect_isin(data$traits$trait_name %>% unique(), definitions$elements %>% names(), info = paste("traits ", v))
}
