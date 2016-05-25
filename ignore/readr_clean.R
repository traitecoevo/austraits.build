setwd('../')


library(readr)
for(f in list.files("data", pattern="configPlantCharacters.csv", full.names=TRUE, recursive=TRUE)) {
  write_csv(read_csv(f), f)
}