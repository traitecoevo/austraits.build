rm(list = ls())

source("~/Work/traits/code/R/data_process/process_datasets.R")

setwd("~/Work/traits/austraits_git/data/processed_datasets/")

# process a single dataset in given directory
processDataset("data/dataset_032", "output", verbose = TRUE)

# alternatively, process all datasets
for (dir in list.dirs("data", recursive = FALSE)) {
  processDataset(dir, "output", verbose = TRUE)
}
