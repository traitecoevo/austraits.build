# for each dataset: 
# go into corresponding directory, load configPlantCharacters.csv, metadata.csv and configDataset.csv
# get the dataset_id from metadata.csv, and subset populate_config.csv by dataset_id
# then in each subset of populate_config.csv, 
# lookup 'character' and 'unit' names in configPlantCharacters.csv files and replace them with 'traitname_short' and 'unit_checked', respectively

populate_config <- read.csv("../code/populate_config.csv", header=TRUE, stringsAsFactors = FALSE)

populate_configPlantCharacters <- function() { 
  
  setwd("../")
  setwd(paste(getwd(), "/data", sep = ""))
  
  conv_filenames <- list.files(pattern="configPlantCharacters.csv", recursive=TRUE)
  conv_filenames <- conv_filenames[!grepl("114", conv_filenames)] # 114 has no metadata
  
  metadata_filenames <- list.files(pattern="metadata.csv", recursive=TRUE)
  metadata_filenames <- metadata_filenames[!grepl("114", metadata_filenames)] # 114 has no metadata
  
  config_filenames <- list.files(pattern="configDataset.csv", recursive=TRUE)
  config_filenames <- config_filenames[!grepl("114", config_filenames)] # 114 has no metadata
  
  
  for(i in 1:length(conv_filenames)) {
    
    file_conv <- read.csv(conv_filenames[i], stringsAsFactors=FALSE)
    file_metadata <- read.csv(metadata_filenames[i], stringsAsFactors=FALSE)   
    file_config <- read.csv(config_filenames[i], stringsAsFactors=FALSE)  
    
    dataset_num <- file_metadata$dataset_num
    populate_config_sub <- subset(populate_config, dataset_id == dataset_num)
    
    file_conv <- na.omit(file_conv) # some traits have NA values for character or unit
    
    for(j in unique(file_conv$character)) {
      
      x <- which(file_conv$character == j)
      
      y <- match(j, populate_config_sub$character)
      
      file_conv[x,]$character <- populate_config_sub[y,]$traitname_short
      file_conv[x,]$unit <- populate_config_sub[y,]$unit_checked
      
    }
    
    write.csv(file_conv, conv_filenames[i], row.names=FALSE)
    
  }
  
}

populate_configPlantCharacters()


