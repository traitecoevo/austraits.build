
testthat::test_that("test datasets", {
  
  examples.dir <- file.path(root.dir, "tests/build/examples/")
  
  Catford_2014 <- test_build_study(file.path(root.dir, "data/Catford_2014/metadata.yml"), file.path(root.dir, "data/Catford_2014/data.csv"), "Catford_2014")
  Falster_2005_1 <- test_build_study(file.path(root.dir, "data/Falster_2005_1/metadata.yml"), file.path(root.dir, "data/Falster_2005_1/data.csv"), "Falster_2005_1")
  
  expect_no_error({austraits1 <- austraits.build:::combine_austraits(Catford_2014, Falster_2005_1, definitions=definitions)}, 
                  info = "Combining sources")
  
  expect_no_error({austraits <- austraits.build:::update_taxonomy(austraits1, taxon_list)}, 
                  info = "Updating taxonomy")
  
  
  test_structure(austraits, "Check structure", single_study = FALSE)
  
  # Build example -- this runs a bunch of tests already
  
  # Example 1 - Test for collection_type and sample_age_class at the dataset level
  # test1-metadata and test1-data are copies of falster_2005_1
  Ex1 <- test_build_study(file.path(examples.dir, "test1-metadata.yml"), file.path(examples.dir, "test1-data.csv"), "Example 1")
  
  expect_equal(Ex1$traits$collection_type %>% unique, "field")
  expect_equal(Ex1$traits$sample_age_class %>% unique, "adult")
  expect_equal(Ex1$traits %>% filter(collection_type == "field") %>% nrow(), 406)
  expect_equal(Ex1$traits %>% filter(sample_age_class == "adult") %>% nrow(), 406)
  expect_equal(nrow(Ex1$excluded_data), 44)
  
  # Example 2 - Test variables are read in at the trait level
  # test2-metadata collection_type for Leaf N trait changed  to lab, collection_type for every other trait
  # has not been specified so should take the dataset level value
  ## Ex2 collection_type for Leaf N changed  to lab
  Ex2 <- test_build_study(file.path(examples.dir, "test2-metadata.yml"), file.path(examples.dir, "test1-data.csv"), "Example 2")
  
  expect_equal(Ex2$traits$collection_type %>% unique, c("field", "lab"))
  expect_equal(Ex2$traits %>% filter(collection_type == "field") %>% nrow(), 361)
  expect_equal(Ex2$traits %>% filter(collection_type == "lab") %>% nrow(), 45)
  expect_equal(Ex2$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% unique, "lab")
  expect_equal(Ex2$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% length, 45)
  
  # Example 3 - Test variables stored as a column in data.csv is read in correctly 
  # test2-data collection_type column has been added in data.csv
  # test2.1 metadata collection_type and sample_age_class changed to match column name
  # collection_type in the column contains "wild" while the dataset level collection_type is "field"
  # the values in the column should take precedence over the dataset value
  Ex3 <- test_build_study(file.path(examples.dir, "test2.1-metadata.yml"), file.path(examples.dir, "test2-data.csv"), "Example 3")
  
  expect_equal(Ex3$traits$collection_type %>% unique, c("wild", "lab"))
  expect_equal(Ex3$traits %>% filter(collection_type == "wild") %>% nrow(), 361)
  expect_equal(Ex3$traits$sample_age_class %>% unique, "seedling")
  expect_equal(Ex3$traits %>% filter(sample_age_class == "seedling") %>% nrow(), 406)
  expect_equal(read_yaml(file.path(examples.dir, "test2.1-metadata.yml"))$dataset$collection_type, "collection_type")
  expect_equal(read_yaml(file.path(examples.dir, "test2.1-metadata.yml"))$dataset$sample_age_class, "sample_age_class")
  
  
  # Example 4 - Test variables stored as a column in data.csv are replaced with trait level value,
  # also introduced a value at the trait level Leaf_N ~ lab
  # test3-data collection_type column has been added in data.csv with missing values 
  # Similar to EX 3 but this time values should be filled in for traits that have a value specified
  Ex4 <- test_build_study(file.path(examples.dir, "test2.1-metadata.yml"), file.path(examples.dir, "test3-data.csv"), "Example 4")
  
  expect_equal(Ex4$traits$collection_type %>% unique, c("NA", "lab", "wild"))
  expect_equal(Ex4$traits %>% filter(is.na(collection_type)) %>% nrow(), 352)
  expect_equal(Ex4$traits %>% filter(collection_type == "lab") %>% nrow(), 45)
  expect_equal(Ex4$traits %>% filter(collection_type == "wild") %>% nrow(), 9)
  
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% unique, c("lab"))
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% grep(pattern = "lab") %>% length , 45)
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% grep(pattern = "wild") %>% length , 0)
  
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(collection_type) %>% unique, c("NA", "wild"))
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(collection_type) %>% is.na %>% sum, 28)
  expect_equal(Ex4$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(collection_type) %>% grep(pattern = "wild") %>% length , 1)
  
  # Example 5 - Test a combination of trait, site and dataset level values
  # Collection type has been specified for dataset level ~ field, site level ~ Cape_Tribulation
  # trait level ~ leaf_N and column ~ wild
  # column values should take precedence followed by traits values, followed by sites and then dataset values
  Ex5 <- test_build_study(file.path(examples.dir, "test3-metadata.yml"), file.path(examples.dir, "test3-data.csv"), "Example 5")
  
  expect_equal(Ex5$traits$collection_type %>% unique, c("NA", "lab", "Cape_Tribulation", "wild"))
  expect_equal(Ex5$traits %>% filter(is.na(collection_type)) %>% nrow(), 81)
  expect_equal(Ex5$traits %>% filter(collection_type == "Cape_Tribulation") %>% nrow(), 271)
  expect_equal(Ex5$traits %>% filter(collection_type == "lab") %>% nrow(), 45)
  expect_equal(Ex5$traits %>% filter(collection_type == "wild") %>% nrow(), 9)
  
  expect_equal(Ex5$traits %>% select(taxon_name, collection_type) %>% 
                 filter(taxon_name == "Trema aspera") %>% pull(collection_type) %>% unique, c("wild", "lab"))
  expect_equal(Ex5$traits %>% select(taxon_name, collection_type) %>% 
                 filter(taxon_name == "Trema aspera") %>% pull(collection_type) %>% length, 10)
  
  expect_equal(Ex5$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% grep(pattern ="lab") %>% length, 45)
  expect_equal(Ex5$traits %>% select(trait_name, collection_type) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(collection_type) %>% grep(pattern ="wild") %>% length, 0)
  
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Atherton") %>%
                 pull(collection_type) %>% unique, c("NA", "lab"))
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Atherton") %>%
                 pull(collection_type) %>% length, 91)
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Atherton") %>%
                 pull(collection_type) %>% is.na %>% sum, 81)
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Atherton") %>%
                 pull(collection_type) %>% grep(pattern = "lab") %>% length, 10)
  
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Cape Tribulation") %>%
                 pull(collection_type) %>% unique, c("Cape_Tribulation", "lab", "wild"))
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Cape Tribulation") %>%
                 pull(collection_type) %>% length, 315)
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Cape Tribulation") %>%
                 pull(collection_type) %>% grep(pattern = "Cape_Tribulation") %>% length, 271)
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Cape Tribulation") %>%
                 pull(collection_type) %>% grep(pattern = "lab") %>% length, 35)
  expect_equal(Ex5$traits %>% select(site_name, collection_type) %>% filter(site_name == "Cape Tribulation") %>%
                 pull(collection_type) %>% grep(pattern = "wild") %>% length, 9)
})

