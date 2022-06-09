library(testthat)

test_that("test load_schema",{
  expect_silent(load_schema())
  expect_equal(class(load_schema()), "list")
})
