# match and replace instances of numeric range data having been converted to dates
# e.g. "Jan-5" should be "1-5", "5-Oct" should be "5-10"

library(readr)
library(stringr)

correctRangeData <- function(data, cols) {
  # a named vector of month abbreviations, to use as regex pattern
  reg <- setNames(seq_len(12), month.abb)
  
  data[fixCols] <-
    sapply(fixCols, function(col) {
      stringr::str_replace_all(data[[col]], reg)  
    })
  
  data
}

fileName <- "~/Work/traits/traitecoevo_austraits_git/austraits/data/dataset_029/data.csv"
fixCols <- c("leaf length (mm)", "leaf width (mm)", "seed length (mm)", "seed diameter (mm)", "seed depth (mm)")

data <- read_csv(fileName)
data <- correctRangeData(data, fixCols)

write_csv(data, fileName, na = "")
