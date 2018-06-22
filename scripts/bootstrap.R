#!/usr/bin/env Rscript
library(whisker)

study_names <- dir("data")
vals <- list(study_names=iteratelist(study_names, value="study_name"))

str <- whisker.render(readLines("scripts/remake.yml.whisker"), vals)
writeLines(str, "remake.yml")

