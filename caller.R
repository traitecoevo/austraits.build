rm(list = ls())

source("tmp.R")

# alternatively, process all datasets
for (dir in list.dirs("data", recursive = FALSE)) {
  processDataset(dir, verbose = TRUE)
}

