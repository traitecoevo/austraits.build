
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
  expect_equal(convert_month_range_string_to_binary_worker("All Year-All Year"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer"), c(1,1,0,0,0,0,0,0,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Autumn"), c(0,0,1,1,1,0,0,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Winter"), c(0,0,0,0,0,1,1,1,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("Spring"), c(0,0,0,0,0,0,0,0,1,1,1,0))
  expect_equal(convert_month_range_string_to_binary_worker("Summer-Spring"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer/Spring"), c(1,1,0,0,0,0,0,0,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer/winter"), c(1,1,0,0,0,1,1,1,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer,winter"), c(1,1,0,0,0,1,1,1,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer;winter"), c(1,1,0,0,0,1,1,1,0,0,0,1))
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
  expect_equal(convert_month_range_string_to_binary_worker("Mar-Jan"), c(1,0,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("Mar-Mar"), c(0,0,1,0,0,0,0,0,0,0,0,0))
  expect_equal(convert_month_range_string_to_binary_worker("summer-summer"), c(1,1,0,0,0,0,0,0,0,0,0,1))
  expect_equal(convert_month_range_string_to_binary_worker("Summer-spring"), c(1,1,1,1,1,1,1,1,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("spring-summer"), c(1,1,0,0,0,0,0,0,1,1,1,1))
  expect_equal(convert_month_range_string_to_binary_worker("SPRING-SPRING"), c(0,0,0,0,0,0,0,0,1,1,1,0))
  
  expect_equal(convert_month_range_string_to_binary_worker(NA), NA)
  expect_equal(convert_month_range_string_to_binary_worker("January"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("1-5"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Ephemeral"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Irregular"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Periodic"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("After fire"), NA)
  expect_equal(convert_month_range_string_to_binary_worker("Doesnt flower"), NA)
})

testthat::test_that("test separate_range",{
  data <- tibble::tibble(range = rep("1-10",10))
  
  expect_equal(class(austraits.build:::separate_range(data,"range","min","max")), c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(data), 1)
  expect_equal(ncol(austraits.build:::separate_range(data,"range","min","max")), 2)
  expect_equal(unique(austraits.build:::separate_range(data,"range","min","max")$min), "1")
  expect_equal(unique(austraits.build:::separate_range(data,"range","min","max")$max), "10")
})

testthat::test_that("test replace_duplicates_with_NA",{
  expect_equal(austraits.build:::replace_duplicates_with_NA(1:10), 1:10)
  expect_equal(austraits.build:::replace_duplicates_with_NA(c(1,1,1)), c(1, NA, NA))
  expect_equal(austraits.build:::replace_duplicates_with_NA(c("1","1","1")), c("1", NA, NA))
  expect_equal(austraits.build:::replace_duplicates_with_NA(c("1",1,"1")), c("1", NA, NA))
  expect_equal(austraits.build:::replace_duplicates_with_NA(c("A",1,"A")), c("A", 1, NA))
})

testthat::test_that("test format_min_max_as_range",{
  data <- tibble::tibble(min = rep(1,10),
                         max = rep(c(1,2),5))
  
  expect_equal(class(format_min_max_as_range(data, "min", "max", "range", "type")), c("tbl_df","tbl","data.frame"))
  expect_equal(format_min_max_as_range(data, "min", "max", "range", "type") %>% pull(type) %>% unique(), c("mean","range"))
  expect_equal(names(format_min_max_as_range(data, "min", "max", "range", "type")), c("min", "max", "range", "type"))
  expect_equal(
    format_min_max_as_range(data, "min", "max", "range", "type") %>% filter(range == 1) %>% pull(type) %>% unique(), "mean"
  )
  expect_equal(
    format_min_max_as_range(data, "min", "max", "range", "type") %>% filter(grepl("--", range)) %>% pull(type) %>% unique(), "range"
  )
}) 

testthat::test_that("test move_values_to_new_trait",{
  data <- tibble::tibble(Root = rep("Soil",10))
  
  expect_equal(ncol(data), 1)
  
  expect_equal(ncol(move_values_to_new_trait(data, "Root", "Branch", "Soil", "Leaves", "Soil")), 2)
  expect_equal(nrow(move_values_to_new_trait(data, "Root", "Branch", "Soil", "Leaves", "Soil")), 10)
})  
  
testthat::test_that("test add_values_to_additional_trait_long",{
  data <- tibble::tibble(trait = rep("Texture",10),
                         value = rep(c("plumose", "tomentose", "smooth", "rough", "spiky"),2))
  
  expect_equal(unique(data$trait), "Texture")
  expect_equal(nrow(data), 10)
  
  expect_equal(unique(add_values_to_additional_trait_long(
    data, "Root", "trait", "value", c("plumose", "tomentose"), c("Soil"))$trait),  c("Texture", "Root"))
  expect_equal(nrow(add_values_to_additional_trait_long(
    data, "Root", "trait", "value", c("plumose", "tomentose"), c("Soil"))), 14)
})  

testthat::test_that("test move_values_to_new_trait_long",{
  data <- tibble::tibble(trait = rep("Texture",10),
                         value = rep(c("plumose", "tomentose", "smooth", "rough", "spiky"),2))
  
  expect_equal(unique(data$trait), "Texture")
  expect_equal(nrow(data), 10)
  
  expect_equal(unique(move_values_to_new_trait_long(data, "Texture", "seed_surface_hairs", 
                                                    "trait", "value", c("plumose", "tomentose"))$trait), 
               c("seed_surface_hairs", "Texture"))
  expect_equal(ncol(move_values_to_new_trait_long(data, "Texture", "seed_surface_hairs", 
                                                    "trait", "value", c("plumose", "tomentose"))), 2)
  expect_equal(nrow(move_values_to_new_trait_long(data, "Texture", "seed_surface_hairs", 
                                                    "trait", "value", c("plumose", "tomentose"))), 10)
})  

testthat::test_that("test substitutions_from_csv",{
  substitutions_df <- tibble::tibble(dataset_id = "Test_2022",
                                     trait_name = "Tree",
                                     find = "Root",
                                     replace = "Branch")
  
  metadata <- read_metadata("data/Test_2022/metadata.yml")
  metadata$substitutions <- NA
  write_metadata(metadata, "data/Test_2022/metadata.yml")
  expect_invisible(substitutions_from_csv(substitutions_df, "Test_2022", "trait_name", "find", "replace"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$substitutions %>% sapply(`%in%`, x = "Tree") %>% any(), TRUE)
})
  