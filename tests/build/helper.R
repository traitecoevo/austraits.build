requireNamespace("testthat", quietly = TRUE)
library(austraits.build)

root.dir <- rprojroot::find_root("remake.yml")
schema <- load_schema()
trait_definitions <- load_schema(file.path(root.dir, "config/traits.yml"), I("traits"))

unit_conversions <- austraits.build:::make_unit_conversion_functions(file.path(root.dir, "config/unit_conversions.csv"))
taxon_list <- read_csv_char(file.path(root.dir, "config/taxon_list.csv"))

vars_austraits <- 
  schema$austraits$elements %>% names() 

vars_dataset <- 
  vars_austraits %>% c("dataset_id", .) %>% 
  subset(., !grepl("build",.))

vars_tables <- vars_austraits %>% subset(., !(. %in% c("dataset_id", "trait_definitions", "schema", "sources", "build_info")))

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


test_build_dataset <- function(path_metadata, path_data, info, trait_definitions, unit_conversions, schema) {
  
  # test it builds with no errors
  expect_no_error({
    build_config <- subset_config(path_metadata, trait_definitions, unit_conversions)
  }, info = paste(info, " config"))
  
  expect_no_error({
    build_dataset <- load_dataset(path_data, build_config, schema)
  }, info = paste(info, " load_dataset"))
  
  test_structure(build_dataset, info, single_dataset = TRUE)
  
  build_dataset
}

test_structure <- function(data, info, single_dataset = TRUE) {
  
  # test lists have the right objects
  comparison <- vars_austraits
  if(single_dataset) comparison <- vars_dataset
  
  test_list_named(data, comparison, info = c(info, " - main elements"))
  
  # test structure of tables
  for(v in vars_tables) {
    
    comparison <- schema$austraits$elements[[v]]$elements %>% names()
    
    # individual studies only have some the variables
    if(single_dataset) {
      if(v == "taxa")
        comparison <- comparison[1]
      else if(v == "taxonomic_updates")
        comparison <- comparison[1:3]
    }
    
    test_dataframe_named(data[[v]], comparison, info = paste(info, " - structure of ", v))
  }
  
  # contains allowed traits
  expect_isin(data$traits$trait_name %>% unique(), trait_definitions$elements %>% names(), info = paste("traits ", v))
}
