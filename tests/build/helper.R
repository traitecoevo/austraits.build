requireNamespace("testthat", quietly = TRUE)
library(austraits.build)

root.dir <- rprojroot::find_root("remake.yml")
definitions <- yaml::read_yaml(file.path(root.dir, "config/definitions.yml"))

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
