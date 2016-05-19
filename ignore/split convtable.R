require(plyr)

## split conversion table among data folders ##

conversions <- read.csv('../code/conversion_latest8.csv', header=TRUE, stringsAsFactors=FALSE)

# updates old dataset_id to new dataset_id with three digits
update_dataset_id <- function(df) {
  df$dataset_id <- as.numeric(df$dataset_id)
  df[which(df$dataset_id %in% c(1:9)),]$dataset_id <- paste("00", df[which(df$dataset_id %in% c(1:9)),]$dataset_id, sep = "")
  df[which(df$dataset_id %in% c(10:99)),]$dataset_id <- paste("0", df[which(df$dataset_id %in% c(10:99)),]$dataset_id, sep = "")
  df$dataset_id <- paste("dataset_", df$dataset_id, sep = "")
  return(df)
}

conversions <- update_dataset_id(conversions)

conversions1 <- subset(conversions, dataset_id == "dataset_002")


# subsets conversion tables according to dataset_id and writes to data directories
split_table <- function(df, outname) {
  
  outname <- substitute(deparse(outname)) # outname is the desired file name
  folder <- unique(df$dataset_id)
  
  df_sub <- df[df$dataset_id == folder,]
  
  write.csv(df_sub, sprintf("../data/%s/%s.csv", folder, outname))
  
}

ddply(conversions, .(dataset_id), split_table)
