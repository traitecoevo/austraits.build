
library(readr)
library(dplyr, warn.conflicts=FALSE)

## assumes variable `keys` is defined, being the names of datasets to test
study_names <- file.path("..", "data", keys)

tmp <- lapply(list.files("../R", full.names=TRUE), source)

variable_definitions <- read_csv("../config/definitions_traits.csv")

make_label <- testthat:::make_label

## New expect_that helper functions; test that a number is in a range,
## or that a range contains a number.

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

expect_contains <- function(object, expected, ..., info = NULL, label = NULL,
                         expected.label = NULL) {

 i <- expected %in% object

  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - does not contain: %s", info, paste(expected[!i], collapse= ", "))
  )

  invisible(object)
}

expect_not_NA <- function (object, info = NULL, label = NULL) {
    lab <- make_label(object, label)
    i <- !is.na(object)
    comp <- compare(all(i), TRUE)
    expect(comp$equal,
            sprintf("%s - contains NAs: %s", info, lab))
    invisible(object)
}

expect_unique <- function (object, info = NULL, label = NULL) {
    x <- table(unlist(object))
    i <- x==1
    comp <- compare(all(i), TRUE)
    expect(comp$equal,
            sprintf("%s - not unique: %s", info, paste(names(x)[!i], collapse= ", ")))
    invisible(object)
}

expect_allowed_text <- function(object, info = NULL, label = NULL) {

  allowed <- TRUE
  if(length(object) > 0 ) {
    allowed <- !simplify2array(lapply(object,disallowed_chars))
  }

  expect(
    identical(as.vector(all(allowed)), TRUE),
    sprintf("%s -- disallowed characters detected: %s", info, paste0(object[which(!allowed)], collapse = " ")),
    info = info
  )
  invisible(object)
}

disallowed_chars <- function(x) {
  i <- charToRaw(x)
  # allow all ascii text
  is_ascii <- i < 0x7F
  # allow some utf8 characters, those with accents over letters for foreign names
  # list of codes is here: http://www.utf8-chartable.de/
  # note c3 is needed because this is prefix for allowed UTF8 chars
  is_allowed <- (0x80 < i & i < 0xbf | i == 0xc3)
  any(!(is_ascii | is_allowed))
}
