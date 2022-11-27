test_that("util_replace_null returns NA",{
  expect_equal(util_replace_null(NULL), NA)
  expect_equal(util_replace_null(1), 1)
})

test_that("util_extract_list_element returns an element in character",{
  test_list <- util_df_to_list(iris)
  names(test_list) <- paste("row", seq_len(nrow(iris)))
  
  expect_type(util_extract_list_element(1, test_list, "Sepal.Length"), "double")
  expect_equal(util_extract_list_element(1, test_list,  "Sepal.Length"), 5.1)
  expect_equal(util_extract_list_element("row 2", test_list,  "Sepal.Length"), 4.9)
  expect_error(util_extract_list_element((length(test_list)+1), test_list, "units"), "subscript out of bounds")
})

test_that("util_separate_and_sort returns alphabetically sorted characters",{
  expect_type(util_separate_and_sort("z y x"), "character")
  expect_match(util_separate_and_sort("z y x"), "x y z")
  expect_match(util_separate_and_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})

test_that("util_df_to_list",{
  expect_match(class(util_df_to_list(iris)), "list")
  expect_type(util_df_to_list(iris), "list")
  
  expect_match(class(util_append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), "list")
  expect_equal(length(util_append_to_list(as.list(iris)[c(1,2)], as.list(iris)[c(3)])), 3)
})

test_that("util_list_to_df2",{
  expect_equal(util_list_to_df2(NULL), NA)
  expect_equal(util_list_to_df2(NA), NA)
  
  my_list <- util_df_to_list(iris)
  expect_match(class(util_list_to_df2(my_list))[1], "tbl_df")

  my_list <- list(NA)
  expect_equal(util_list_to_df2(my_list), NA)
})
  
test_that("util_append_to_list",{
  my_list <- as.list(iris)
  expect_equal(util_append_to_list(my_list, NULL), my_list)
  expect_length((util_append_to_list(my_list, NA)), 6)
  expect_gt(length(util_append_to_list(my_list, NA)), length(my_list))
  expect_error(util_append_to_list(my_list), 'argument "to_append" is missing, with no default')
})

test_that("util_strip_taxon_names",{
  v1 <-  c("banksia serrata", "Banksia_serrata", "banksia  serrata", "Banksia Serrata") 
  v2 <- util_strip_taxon_names(v1)

  expect_true(all(v2 == v1[1]))
  expect_equal(length(v1), length(v2))

  v1 <- c("banksia serrata spinulosa", "Banksia_serrata var. SpinUlosa", "banksia  serrata s.l. spinulosa", "Banksia Serrata aff. spinulosa")
  v2 <- util_strip_taxon_names(v1)

  expect_true(all(v2 == v1[1]))
  expect_equal(length(v1), length(v2))
})

