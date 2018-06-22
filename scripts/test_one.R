#!/usr/bin/env Rscript
library(testthat)

keys <- commandArgs(TRUE)[1]

testthat::test_dir("tests/", reporter = RstudioReporter)
