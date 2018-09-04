#!/usr/bin/env Rscript

# Script to build reports.
# Example usage:
# ./scripts/compile_reports.R TRUE
# ./scripts/compile_reports.R TRUE Angevin_2010
# ./scripts/compile_reports.R TRUE Angevin_2010 Bragg_2002

args <- commandArgs(TRUE)

dataset_ids <- args[-c(1)]  # Can be NA -- will be set later

if(is.na(args[1])) {
  overwrite <- FALSE
} else {
  overwrite <- as.logical(args[1])
}

library(tidyverse)
source("R/report_utils.R")

# define if does not already exist
austraits <- readRDS("export/data/austraits.rds")
build_study_reports(dataset_ids, overwrite)
