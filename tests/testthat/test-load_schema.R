
test_that("test get_schema",{
  expect_silent(get_schema())
  expect_equal(class(get_schema()), "list")
  expect_equal(length(get_schema()), 5)
  expect_equal(class(get_schema(subsection = "austraits")), "list")
  expect_equal(length(get_schema(subsection = "austraits")), 3)
  expect_equal(length(get_schema(subsection = "metadata")), 3)
  expect_equal(length(get_schema(subsection = "entity_type")), 3)
})
