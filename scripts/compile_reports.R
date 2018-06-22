#!/usr/bin/env Rscript
dataset_ids <- commandArgs(TRUE)[1]

library(tidyverse)
e <- new.env()

# is not defined, do for all studies
if(is.na(dataset_ids))
  dataset_ids <- dir("data")

for(dataset_id in dataset_ids) {
  cat(sprintf("Building report for %s: ", dataset_id))

  # assigns variable tinto an environment that can be accessed by knitr
  assign("dataset_id", dataset_id, e)
  output_md <- sprintf("export/reports/%s.md", dataset_id)
  knitr::knit("vignettes/report_study.Rmd", output_md, quiet=TRUE, envir=e)
  cat(" -> knitted")
  rmarkdown::render(output_md, "html_document", quiet=TRUE)
  cat(" -> done\n")
}
