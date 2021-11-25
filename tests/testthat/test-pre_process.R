
testthat::test_that("get_month returns correct month",{
  expect_match(class(get_month(1)), "character")

  expect_match(get_month(1), "Jan")
  expect_match(get_month(1), "Jan")
  expect_match(get_month(1.5), "Jan")
  expect_equal(get_month(13), "NA")
  expect_equal(get_month("Jan"), "NA")
  expect_equal(get_month(NA), "NA")
})

testthat::test_that("format_flowering_months returns a single 12 character string",{
  expect_match(class(format_flowering_months(1, 1)), "character")
  expect_equal(stringr::str_length(format_flowering_months(1, 1)), 12)
  
  expect_match(format_flowering_months(1, 6), "yyyyyynnnnnn")
  expect_match(format_flowering_months(1, 12), "yyyyyyyyyyyy")
  expect_match(format_flowering_months(12, 12), "nnnnnnnnnnny")
  expect_equal(format_flowering_months(12, 6), "yyyyyynnnnny")
  expect_equal(format_flowering_months(0, 6), "NA")
  expect_equal(format_flowering_months(1, 13), "NA")
  expect_equal(format_flowering_months(12, 13), "NA")
  expect_equal(format_flowering_months(1, NA), "NA")
})

testthat::test_that("convert_month_range_vec_to_binary returns a single 12 character string",{
  expect_match(class(convert_month_range_vec_to_binary("Mar")), "character")
  expect_equal(stringr::str_length(convert_month_range_vec_to_binary("Mar")), 12)
  
  expect_match(convert_month_range_vec_to_binary("Mar"), "nnynnnnnnnnn")
  expect_match(convert_month_range_vec_to_binary("Mar-Apr"), "nnyynnnnnnnn")
  expect_match(convert_month_range_vec_to_binary("Nov-Apr"), "yyyynnnnnnyy")
  expect_match(convert_month_range_vec_to_binary("nov-apr"), "yyyynnnnnnyy")
  expect_match(convert_month_range_vec_to_binary("Feb-Mar;Oct-Nov"), "nyynnnnnnyyn")
  expect_equal(convert_month_range_vec_to_binary("Mar,Apr"), "nnyynnnnnnnn")
  expect_equal(convert_month_range_vec_to_binary(c("Mar", "Apr")), c("nnynnnnnnnnn","nnnynnnnnnnn"))
  expect_equal(convert_month_range_vec_to_binary("March"), "NA")
  expect_equal(convert_month_range_vec_to_binary(3), "NA")
  expect_equal(convert_month_range_vec_to_binary(NA), "NA")
})

testthat::test_that("convert_01_ny returns correct character",{
  expect_match(class(convert_01_ny("1")), "character")
  
  expect_match(convert_01_ny("0"), "n")
  expect_match(convert_01_ny("1"), "y")
  expect_match(convert_01_ny("00"), "nn")
  expect_match(convert_01_ny("10"), "yn")
  expect_match(convert_01_ny("2"), "2")
  expect_equal(convert_01_ny(NA), "NA")
  expect_equal(convert_01_ny("NA"), "NA")
  expect_equal(convert_01_ny("a"), "a")
  expect_equal(convert_01_ny("abc"), "abc")
  expect_equal(convert_01_ny("a100"), "aynn")
})

testthat::test_that("convert_month_range_string_to_binary",{
  expect_match(class(convert_month_range_string_to_binary("Mar/Apr")), "character")
  expect_equal(stringr::str_length(convert_month_range_string_to_binary("all year")), 12)

  expect_match(convert_month_range_string_to_binary("Mar"), "001000000000")
})

testthat::test_that("convert_month_range_string_to_binary_worker",{
  expect_match(class(convert_month_range_string_to_binary_worker("all year")), "numeric")
  expect_equal(sum(convert_month_range_string_to_binary_worker("all year")), 12)
  
  expect_equal(convert_month_range_string_to_binary_worker("all year"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("All Year"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("AllYear"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("ALLYEAR"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer"), c(1,1,0,0,0,0,0,0,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Autumn"), c(0,0,1,1,1,0,0,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Winter"), c(0,0,0,0,0,1,1,1,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Spring"), c(0,0,0,0,0,0,0,0,1,1,1,0))
  expect_equal(convert_month_range_string_to_binary_worker("Summer-Spring"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer/Spring"), c(1,1,0,0,0,0,0,0,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer/winter"), c(1,1,0,0,0,1,1,1,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer,winter"), c(1,1,0,0,0,1,1,1,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Jan-Mar"), c(1,1,1,0,0,0,0,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan/Mar"), c(1,0,1,0,0,0,0,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan/Mar/Jul"), c(1,0,1,0,0,0,1,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan,Mar"), c(1,0,1,0,0,0,0,0,0,0,0,0)) 
  expect_equal(convert_month_range_string_to_binary_worker("Jan,Mar,Jul"), c(1,0,1,0,0,0,1,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan,Mar/Jul"), c(1,0,1,0,0,0,1,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan-Mar/Jul-Aug"), c(1,1,1,0,0,0,1,1,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan-Mar,Jul-Aug"), c(1,1,1,0,0,0,1,1,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan-Mar;Jul-Aug"), c(1,1,1,0,0,0,1,1,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Jan-Mar:Jul-Aug"), c(1,1,1,0,0,0,1,1,0,0,0,0))
  
  expect_equal(convert_month_range_string_to_binary_worker(NA), NA)
  expect_equal(convert_month_range_string_to_binary_worker("January"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("1-5"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Ephemeral"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Irregular"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Periodic"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("After fire"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Doesnt flower"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Summer;winter"), NA)
})

