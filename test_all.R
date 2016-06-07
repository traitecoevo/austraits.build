#!/usr/bin/env Rscript
library(testthat)

# To test all data
keys <- dir("data")

# To test single file
#keys <- "dataset_004"

testthat::test_dir("tests/", reporter = RstudioReporter)
