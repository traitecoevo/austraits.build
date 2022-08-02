
test_that("test load_schema",{
  expect_silent(load_schema())
  expect_equal(class(load_schema()), "list")
  expect_equal(length(load_schema()), 5)
  expect_equal(class(load_schema(subsection = "austraits")), "list")
  expect_equal(length(load_schema(subsection = "austraits")), 3)
  expect_equal(length(load_schema(subsection = "metadata")), 3)
  expect_equal(length(load_schema(subsection = "entity_type")), 3)
})
