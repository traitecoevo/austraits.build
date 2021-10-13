
library(stringr)
library(readr)
library(dplyr, warn.conflicts=FALSE)

## assumes variable `dataset_ids` is defined, being the names of datasets to test

root.dir <- rprojroot::find_root("remake.yml")

tmp <- lapply(list.files(file.path(root.dir, "R"), full.names=TRUE), source)

definitions <- read_yaml(file.path(root.dir, "config/definitions.yml"))

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


#' Expectation: one set contains the other
expect_contains <- function(object, expected, ..., info = NULL) {

  i <- expected %in% object

  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - does not contain: %s", info, paste(expected[!i], collapse= ", "))
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

expect_length_zero <- function (object, info = NULL, label = NULL) {
    comp <- compare(length(object), 0)
    expect(comp$equal,
            sprintf("%s: %s", info, label))
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

  if(length(object) > 0 ) {
    disallowed <- object %>% lapply(check_disallowed_chars) %>% simplify2array()
 
    check <- disallowed %>% lapply(any) %>% unlist()
    
    txt <- "\n"
    for( i in which(check) ) {
      txt <- sprintf("%s\t- ln %s: %s\n", txt, i,
               colour_characters(object[[i]], which(disallowed[[i]]))
                      )
    }

  # if(any(check)) browser()

  expect(
    identical(as.vector(all(!check)), TRUE),
    sprintf("%s -- disallowed characters detected: %s", info, txt)
  )
  }
  invisible(object)
}

colour_characters <- function(x, i = NULL) {
  
  chars <- x %>% charToRaw() %>% lapply(rawToChar) %>% unlist()
  
  # Wrapper around characters to print as colour
  # obtained from crayon::red(x)
  if(!is.null(i))
    chars[i] <- sprintf("\033[31m%s\033[39m", chars[i])
  
  paste0(chars, collapse="")
}

check_disallowed_chars <- function(x) {
  i <- charToRaw(x)
  # allow all ascii text
  is_ascii <- i < 0x7F

  # allow some utf8 characters, those with accents over letters for foreign names
  # list of codes is here: http://www.utf8-chartable.de/
  # note c3 is needed because this is prefix for allowed UTF8 chars
  exceptions <- c("âíåæäãàáíčóöøéłńl°êÜüùúû±µµ“”‘’-–—≈˜×")

  is_allowed <- i %in% charToRaw(exceptions)
  !(is_ascii | is_allowed)
}

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

expect_list_elements_contain <- function(object, expected, info) {

  for(i in seq_along(object)) expect_contains(names(object[[i]]), expected, info = paste(info, i))

  invisible(NULL)
}

