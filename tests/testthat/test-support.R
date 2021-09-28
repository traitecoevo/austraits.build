
test_that("extract_list_element returns an element in character",{
  expect_type(extract_list_element(1, definitions$traits$elements, "units"), "character")
  expect_type(extract_list_element("seed_mass", definitions$traits$elements, "type"), "character")
  expect_error(extract_list_element((length(definitions$traits$elements)+1), definitions$traits$elements, "units"), "subscript out of bounds")
})

test_that("split_then_sort returns alphabetically sorted characters",{
  expect_type(split_then_sort("z y x"), "character")
  expect_match(split_then_sort("z y x"), "x y z")
  expect_match(split_then_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})

test_that("df_to_list, expect_match, write_yaml",{
  expect_match(class(df_to_list(iris)), "list")
  
  expect_match(class(list_to_df(df_to_list(iris)))[1], "tbl_df")
  
  expect_match(class(append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), "list")
  expect_equal(length(append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), 3)
  
  
  tmp <- "ignore/iris.yaml"
  dir.create(dirname(tmp), FALSE, TRUE)
  write_yaml(iris, tmp)
  expect_true(file.exists(tmp),  info = f)
  file.remove(tmp)
})
