
test_that("test datasets", {
  
  examples.dir <- file.path(root.dir, "tests/build/examples/")
  
  Catford_2014 <- test_build_study(file.path(root.dir, "data/Catford_2014/metadata.yml"), file.path(root.dir, "data/Catford_2014/data.csv"), "Catford 2014")
  Falster_2005_1 <- test_build_study(file.path(root.dir, "data/Falster_2005_1/metadata.yml"), file.path(root.dir, "data/Falster_2005_1/data.csv"), "Falster_2005_1")
  
  expect_no_error({austraits1 <- austraits.build:::combine_austraits(Catford_2014, Falster_2005_1, definitions=definitions)}, 
                  info = "Combining sources")
  
  expect_no_error({austraits <- austraits.build:::update_taxonomy(austraits1, taxon_list)}, 
                  info = "Updating taxonomy")
  
  
  test_structure(austraits, "Check structure", single_study = FALSE)
  
  # Build example -- this runs a bunch of tests already
  Ex1 <- test_build_study(file.path(examples.dir, "test1-metadata.yml"), file.path(examples.dir, "test1-data.csv"), "Example 1")
  
  # now test for specific content in examples
  
  
})


