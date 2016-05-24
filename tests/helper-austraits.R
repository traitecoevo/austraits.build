
library(readr)

## assumes variable `keys` is defined, being the names of datasets to test
study_names <- file.path("..", "data", keys)

tmp <- lapply(list.files("../R", full.names=TRUE), source)

variable_definitions <- read_csv("../config/variableDefinitions.csv")


## New expect_that helper functions; test that a number is in a range,
## or that a range contains a number.

isin <- function(my_list) {
  function(values) {
    i <- values %in% my_list
   testthat::expectation(all(i),
                          paste("should not contain:", paste(values[!i], collapse= ", ")))
  }
}

contains <- function(my_list) {
  function(values) {
    i <- my_list %in% values
   # if(!all(i)) browser()
   testthat::expectation(all(i),
                          paste("does not contain: ", paste(my_list[!i], collapse= ",")))
  }
}

is_within_interval <- function(lower, upper) {
  if ( missing(upper) && length(lower) == 2 ) {
    upper <- lower[[2]]
    lower <- lower[[1]]
  }
  if (lower >= upper) {
    stop("lower must be smaller than upper")
  }
  err <- paste0("is not within range [", lower, ", ", upper, "]")
  function(actual) {
    testthat::expectation(actual > lower && actual < upper, err)
  }
}

inrange <- function(value) {
  function(range) {
    if (length(range) != 2L) {
      stop("Expected a vector of length 2")
    }
    if (range[[1]] >= range[[2]]) {
      stop("Expected that range[1] is smaller than range[2]")
    }
    testthat::expectation(value > range[[1]] && value < range[[2]],
                          paste("does not contain", value))
  }
}

is_greater_than <- function(value) {
  function(actual) {
    testthat::expectation(actual > value, paste("is not greater than", value))
  }
}

is_less_than <- function(value) {
  function(actual) {
    testthat::expectation(actual < value, paste("is not less than", value))
  }
}

is_at_most <- function(value) {
  function(actual) {
    testthat::expectation(actual <= value, paste("is greater than", value))
  }
}

is_at_least <- function(value) {
  function(actual) {
    testthat::expectation(actual >= value, paste("is less than", value))
  }
}


#' Test for UTF-8 characters
#' This test will check every column in a data.frame for possible unicode characters.
#' Inspired by https://github.com/ropensci/testdat/blob/master/R/test_utf8.R
is_ascii <- function() {
  function(dat) {
    if(length(dat) > 0 ) {
      utf8 <- simplify2array(lapply(dat,non_ascii))
    } else {
      utf8 <- FALSE
    }
#    if(any(utf8)) browser()
    testthat::expectation(any(utf8) == FALSE,  sprintf("UTF-8 characters detected: %s", paste0(dat[which(utf8)], collapse = " ")))
  }
}

non_ascii <- function(x) {
  any(charToRaw(x) > 0x7F)
}

