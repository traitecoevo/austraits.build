test_that("null_as returns NA",{
  expect_equal(null_as(NULL), NA)
  expect_equal(null_as(1), 1)
})

test_that("extract_list_element returns an element in character",{
  test_list <- df_to_list(iris)
  names(test_list) <- paste("row", seq_len(nrow(iris)))
  
  expect_type(extract_list_element(1, test_list, "Sepal.Length"), "double")
  expect_equal(extract_list_element(1, test_list,  "Sepal.Length"), 5.1)
  expect_equal(extract_list_element("row 2", test_list,  "Sepal.Length"), 4.9)
  expect_error(extract_list_element((length(test_list)+1), test_list, "units"), "subscript out of bounds")
})

test_that("rename_columns renames a column",{
  test_column <- iris[1:5,1:2]
  expect_equal(rename_columns(test_column, "", ""), test_column)
  expect_equal(names(rename_columns(test_column, "Sepal.Length", "SepalLength")[1]), "SepalLength")
  expect_warning(rename_columns(test_column, "Sepal.Length", c("SepalLength", "SepalWidth")))
  expect_error(rename_columns(test_column, Sepal.Length, "SepalLength"))
})

test_that("split_then_sort returns alphabetically sorted characters",{
  expect_type(austraits.build:::split_then_sort("z y x"), "character")
  expect_match(austraits.build:::split_then_sort("z y x"), "x y z")
  expect_match(austraits.build:::split_then_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})

test_that("df_to_list",{
  expect_match(class(df_to_list(iris)), "list")
  expect_type(df_to_list(iris), "list")
  
  expect_match(class(austraits.build:::append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), "list")
  expect_equal(length( austraits.build:::append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), 3)
})

test_that("list_to_df",{
  expect_equal(list_to_df(NULL), NA)
  expect_equal(list_to_df(NA), NA)
  
  my_list <- df_to_list(iris)
  expect_match(class(list_to_df(my_list))[1], "tbl_df")

  my_list <- list(NA)
  expect_equal(list_to_df(my_list), NA)
})
  
test_that("append_to_list",{
  my_list <- as.list(iris)
  expect_equal(append_to_list(my_list, NULL), my_list)
  expect_length((append_to_list(my_list, NA)), 6)
  expect_gt(length(append_to_list(my_list, NA)), length(my_list))
  expect_error(append_to_list(my_list), 'argument "to_append" is missing, with no default')
})

