


test_that("extract_list_element returns an element in character",{
  
  test_list <- df_to_list(iris)
  names(test_list) <- paste("row", seq_len(nrow(iris)))
  
  expect_type(extract_list_element(1, test_list, "Sepal.Length"), "double")
  expect_equal(extract_list_element(1, test_list,  "Sepal.Length"), 5.1)
  expect_equal(extract_list_element("row 2", test_list,  "Sepal.Length"), 4.9)
  expect_error(extract_list_element((length(test_list)+1), test_list, "units"), "subscript out of bounds")
})

test_that("split_then_sort returns alphabetically sorted characters",{
  expect_type(austraits.build:::split_then_sort("z y x"), "character")
  expect_match(austraits.build:::split_then_sort("z y x"), "x y z")
  expect_match(austraits.build:::split_then_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})

test_that("df_to_list",{
  expect_match(class(df_to_list(iris)), "list")
  
  expect_match(class(list_to_df(df_to_list(iris)))[1], "tbl_df")
  
  expect_match(class(austraits.build:::append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), "list")
  expect_equal(length( austraits.build:::append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), 3)


})

test_that("list_to_df",{
  expect_equal(list_to_df(NULL), NA)
  expect_equal(list_to_df(NA), NA)
  
  my_list <- list(NA)
  expect_error(list_to_df())
  
})