library(readr)

#issue #27

# which datasets have 'plant_char_vertical = TRUE'
# output df verticals

setwd("../")
setwd(paste(getwd(), "/data", sep = ""))

conv_filenames <- list.files(pattern="configPlantCharacters.csv", recursive=TRUE)

metadata_filenames <- list.files(pattern="metadata.csv", recursive=TRUE)

config_filenames <- list.files(pattern="configDataset.csv", recursive=TRUE)

verticals <- data.frame(matrix(ncol = 2, nrow = length(conv_filenames)))
colnames(verticals) <- c("dataset_id", "plant_char_vertical")

for(i in 1:length(conv_filenames)) {

file_conv <- read_csv(conv_filenames[i])
file_metadata <- read_csv(metadata_filenames[i])   
file_config <- read_csv(config_filenames[i])  

verticals$dataset_id[i] <- file_metadata$dataset_id
verticals$plant_char_vertical[i] <- file_config$value[5]

}

# extract trait names and units from raw data and populate configPlantCharacters.csv files

populate_long_configPlantCharacters <- function() {

data_filenames <- list.files(pattern="\\bdata.csv\\b", recursive=TRUE) # \\b is a 'boundary' and allows for exact matches
data_filenames <- data_filenames[verticals$plant_char_vertical == TRUE]
conv_filenames <- conv_filenames[verticals$plant_char_vertical == TRUE]

  for(i in 1:length(data_filenames)) {
   
    df <- read.csv(data_filenames[i], header=T, stringsAsFactors=F)
    df <- unique(df[,c('trait', 'units')])
    out <- data.frame('var_name' = df$trait, character = NA, unit = df$units)
    out[] <- lapply(out, as.character)
   
    if(length(out[is.na(out$unit),]$unit) > 0) {
      out[is.na(out$unit),]$unit <- ""
    }
    
    out[is.na(out$unit),]$unit
    
    write.csv(out, conv_filenames[i], row.names=FALSE)               
     
  }
  
}

populate_long_configPlantCharacters()
  
