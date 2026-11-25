suppressPackageStartupMessages({library(dplyr);library(stringr)}); d <- readr::read_csv("~/GitHub/austraits.build-data.scraping.scripts/data_extra/foa_profiles.csv", col_types=readr::cols(.default="c"))
g <- strsplit(Sys.getenv("G"), ",")[[1]]; pat <- Sys.getenv("P")
x <- d %>% filter(scientific_name %in% g, rank %in% c("family","genus")); for(i in 1:nrow(x)) cat("\n##",x$scientific_name[i], "\n", paste(str_extract_all(x$Description[i], paste0("[^.]*(?:", pat, ")[^.]*\\."))[[1]], collapse=" ... "), "\n")
