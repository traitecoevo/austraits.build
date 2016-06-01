rm(list = ls())


source("tmp.R")

# alternatively, process all datasets
for (dir in list.dirs("data", recursive = FALSE)) {
  processDataset(dir, verbose = TRUE)
}

# Now combine site.csv and
library(readr)
library(tidyr)
library(dplyr)

for (d in list.dirs("data", recursive = FALSE, full.names=TRUE)) {
  f <- file.path(d, "sites.csv")
  if(file.exists(f)){
    df <- readr::read_csv(f) %>%
     tidyr::gather(trait_name, value, 2:3) %>%
     mutate(unit="deg") %>%
     select(site_name,trait_name,unit,value)
  } else {
    df <- NULL
  }
  f2 <- file.path(d, "siteMeasurements.csv")
  if(file.exists(f2)){
    df2 <- readr::read_csv(f2)
  } else {
    df2 <- NULL
  }
  if(!is.null(df))
    df <-  mutate_each_(df, funs(as.character), names(df))
  if(!is.null(df2))
    df2 <-  mutate_each_(df2, funs(as.character), names(df2))

  data <- dplyr::bind_rows(df, df2) %>% mutate(notes="") %>% arrange(site_name)

  if(nrow(data) == 0 )
    data <- data.frame(site_name=character(),trait_name=character(),unit=character(),value=character(), notes=character())

  write_csv(data, file.path(d, "context.csv"))
}
