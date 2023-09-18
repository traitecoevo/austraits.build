
testthat::test_that("test datasets", {
  


  schema <- get_schema()
  resource_metadata <- get_schema("config/metadata.yml",  "metadata")
  definitions <- get_schema("config/traits.yml", "traits")
  unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
  taxon_list <- read_csv_char("config/taxon_list.csv")


  examples.dir <- "examples"
  
  # Build example -- this runs a bunch of tests already
  
  # Example 1 - Test for basis_of_record and life_stage at the dataset level
  # test1-metadata and test1-data are copies of Falster_2005_1
  Ex1 <- test_build_dataset(file.path(examples.dir, "test1-metadata.yml"), file.path(examples.dir, "test1-data.csv"), "Example 1", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  expect_equal(Ex1$traits$basis_of_record %>% unique, "field")
  expect_equal(Ex1$traits$life_stage %>% unique, "adult")
  expect_equal(Ex1$traits %>% filter(basis_of_record == "field") %>% nrow(), 406)
  expect_equal(Ex1$traits %>% filter(life_stage == "adult") %>% nrow(), 406)
  expect_equal(nrow(Ex1$excluded_data), 0)
  
  # Example 2 - Test variables are read in at the trait level
  # test2-metadata basis_of_record for Leaf N trait changed  to lab, basis_of_record for every other trait
  # has not been specified so should take the dataset level value
  ## Ex2 basis_of_record for Leaf N changed  to lab
  Ex2 <- test_build_dataset(file.path(examples.dir, "test2-metadata.yml"), file.path(examples.dir, "test1-data.csv"), "Example 2", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  expect_equal(Ex2$traits$basis_of_record %>% unique, c("field", "lab"))
  expect_equal(Ex2$traits %>% filter(basis_of_record == "field") %>% nrow(), 361)
  expect_equal(Ex2$traits %>% filter(basis_of_record == "lab") %>% nrow(), 45)
  expect_equal(Ex2$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% unique, "lab")
  expect_equal(Ex2$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% length, 45)
  
  # Example 3 - Test variables stored as a column in data.csv is read in correctly 
  # test2-data basis_of_record column has been added in data.csv
  # test2.1 metadata basis_of_record and life_stage changed to match column name
  # basis_of_record in the column contains "wild" while the dataset level basis_of_record is "field"
  # the values in the column should take precedence over the dataset value
  Ex3 <- test_build_dataset(file.path(examples.dir, "test2.1-metadata.yml"), file.path(examples.dir, "test2-data.csv"), "Example 3", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  expect_equal(Ex3$traits$basis_of_record %>% unique, c("wild", "lab"))
  expect_equal(Ex3$traits %>% filter(basis_of_record == "wild") %>% nrow(), 361)
  expect_equal(Ex3$traits$life_stage %>% unique, "seedling")
  expect_equal(Ex3$traits %>% filter(life_stage == "seedling") %>% nrow(), 406)
  expect_equal(yaml::read_yaml(file.path(examples.dir, "test2.1-metadata.yml"))$dataset$basis_of_record, "basis_of_record")
  expect_equal(yaml::read_yaml(file.path(examples.dir, "test2.1-metadata.yml"))$dataset$life_stage, "life_stage")
  
  
  # Example 4 - Test variables stored as a column in data.csv are replaced with trait level value,
  # also introduced a value at the trait level Leaf_N ~ lab
  # test3-data basis_of_record column has been added in data.csv with missing values 
  # Similar to EX 3 but this time values should be filled in for traits that have a value specified
  Ex4 <- test_build_dataset(file.path(examples.dir, "test2.1-metadata.yml"), file.path(examples.dir, "test3-data.csv"), "Example 4", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  expect_equal(Ex4$traits$basis_of_record %>% unique, c(NA, "lab", "wild"))
  expect_equal(Ex4$traits %>% filter(is.na(basis_of_record)) %>% nrow(), 352)
  expect_equal(Ex4$traits %>% filter(basis_of_record == "lab") %>% nrow(), 45)
  expect_equal(Ex4$traits %>% filter(basis_of_record == "wild") %>% nrow(), 9)
  
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% unique, c("lab"))
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% grep(pattern = "lab") %>% length , 45)
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% grep(pattern = "wild") %>% length , 0)
  
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(basis_of_record) %>% unique, c(NA, "wild"))
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(basis_of_record) %>% is.na %>% sum, 28)
  expect_equal(Ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>% 
                 filter(trait_name == "seed_dry_mass") %>%
                 pull(basis_of_record) %>% grep(pattern = "wild") %>% length , 1)
  
  # Example 5 - Test a combination of trait, site and dataset level values
  # Basis of record has been specified for dataset level ~ field, site level ~ Cape_Tribulation
  # trait level ~ leaf_N and column ~ wild
  # column values should take precedence followed by traits values, followed by locations and then dataset values
  Ex5 <- test_build_dataset(file.path(examples.dir, "test3-metadata.yml"), file.path(examples.dir, "test3-data.csv"), "Example 5", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  expect_equal(Ex5$traits$basis_of_record %>% unique, c(NA, "lab", "Cape_Tribulation"))
  expect_equal(Ex5$traits %>% filter(is.na(basis_of_record)) %>% nrow(), 81)
  expect_equal(Ex5$traits %>% filter(basis_of_record == "Cape_Tribulation") %>% nrow(), 315)
  expect_equal(Ex5$traits %>% filter(basis_of_record == "lab") %>% nrow(), 10)
  expect_equal(Ex5$traits %>% filter(basis_of_record == "wild") %>% nrow(), 0)
  
  expect_equal(Ex5$traits %>% select(c("taxon_name", "basis_of_record")) %>% 
                 filter(taxon_name == "Trema aspera") %>% pull(basis_of_record) %>% unique, c("Cape_Tribulation"))
  expect_equal(Ex5$traits %>% select(c("taxon_name", "basis_of_record")) %>% 
                 filter(taxon_name == "Trema aspera") %>% pull(basis_of_record) %>% length, 10)
  
  expect_equal(Ex5$traits %>% select(c("trait_name", "basis_of_record")) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% grep(pattern ="lab") %>% length, 10)
  expect_equal(Ex5$traits %>% select(c("trait_name", "basis_of_record")) %>% 
                 filter(trait_name == "leaf_N_per_dry_mass") %>%
                 pull(basis_of_record) %>% grep(pattern ="wild") %>% length, 0)
  
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% unique, c(NA, "lab"))
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% length, 91)
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% is.na %>% sum, 81)
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% grep(pattern = "lab") %>% length, 10)
  
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% unique, c("Cape_Tribulation"))
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% length, 315)
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "Cape_Tribulation") %>% length, 315)
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "lab") %>% length, 0)
  expect_equal(Ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "wild") %>% length, 0)
  
  expect_equal(Ex5$traits %>% pull(location_id) %>% unique, Ex5$locations %>% pull(location_id) %>% unique)
  expect_equal(Ex5$traits %>% select(c("location_id")) %>% unique() %>% nrow(), Ex5$locations %>% select(c("location_name")) %>% unique() %>% nrow())

  # Example 6 - Tests focus on context and the various context identifiers
  # Based on Crous_2013
  # Have also added data for sex to test this field (commented out for now)

  Ex6 <- test_build_dataset(file.path(examples.dir, "test4-metadata.yml"), file.path(examples.dir, "test4-data.csv"), "Example 6", definitions, unit_conversions, schema, resource_metadata, taxon_list)
  
  #expect_equal(Ex6$traits$sex %>% unique, c("male", "female"))
  expect_equal(Ex6$traits$location_id %>% unique, c("01"))
  #expect_equal(Ex6$traits %>% filter(sex == "male") %>% nrow(), 85)
  expect_equal(Ex6$traits %>% distinct(method_id, temporal_id, treatment_id) %>% nrow(), 36)

  expect_equal(Ex6$contexts$category %>% unique, c("temporal", "treatment", "method"))
  expect_equal(Ex6$contexts %>% nrow(), 9)
  expect_equal(Ex6$contexts %>% nrow(), Ex6$contexts %>% group_by(link_id, link_vals) %>% distinct() %>% nrow())
  expect_equal(Ex6$contexts %>% pull(context_property) %>% unique() %>% length, 4)

  expect_equal(Ex6$traits %>% filter(trait_name == "fruit_colour") %>% pull(value) %>% unique, c("pink", "black", "red"))

  expect_equal(Ex6$traits %>% pull(observation_id) %>% unique() %>% length(), 35)

})
