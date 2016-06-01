##
# functions to process raw datasets for austraits project
##

# trims whitespace from begining and end of string
# str: string to trim
trim <- function (str) {
  gsub("^\\s+|\\s+$", "", str)
}

# gets config value for given key value, use this for configDataset only (use for configDataset.csv only)
# cfg: dataframe containing config data
# key: the key for which we want the value to be returned
getConfig <- function(cfg, key) {
  cfg$value[cfg$key == key]
}

# a wrapper for processData()
# processes a single dataset by checking the configuration for the dataset and calling processData() one or more
# times to optionally process plant traits, site traits and site data
# dirData: dir of the dataset to be processed
# dirOutput: dir of the output folder
# verbose: display status/progress messages or not?
processDataset <- function(dirData, verbose = FALSE) {
  
  # get datasetId (used for the dir name) for this dataset
  datasetId <- basename(dirData)
  
  dirOutput <- "data"
 # read configuration file for dataset
  cfgDataset <- read.csv(paste0(dirData, "/configDataset.csv"), stringsAsFactors = FALSE)
  
  # the configuration file specifies how to process the dataset:
  # process_plant_char == TRUE: process plant traits (in practice this will always be done, all datasets contain plant trait data)
  # process_site_char == TRUE: process site traits (not all datasets contain site traits)
  # process_site_data == TRUE: process site data (i.e. just a list of sites, maybe including lonlat data)
  
  # plant trait data are written to plantMeasurements.csv
  # site trait data are written to siteMeasurements.csv
  # site data are written to sites.csv

  if (getConfig(cfgDataset, "process_site_char")) {
    write.csv(processData(type = "site", dirData, dirOutput, verbose), paste0(dirOutput, "/", datasetId, "/siteMeasurements.csv"), row.names = FALSE)
  }
  
  if (getConfig(cfgDataset, "process_site_data")) {  
    siteData <- processData(type = "sitedata", dirData, dirOutput, verbose)
    #write.csv(unique(siteData[siteData$site_name != "", ]), paste0(datasetPath, "/output/sites.csv"), row.names = FALSE)  
    write.csv(siteData, paste0(dirOutput, "/", datasetId, "/sites.csv"), row.names = FALSE, na = "")  
  }
  
}

# processes a single dataset 
# type: which type of data are we processing, one of "plant", "site" or "sitedata"
# dirData: dir of the dataset to be processed
# dirOutput: dir of the output folder
# verbose: display status/progress messages or not?
processData <- function(type = "plant", dirData, dirOutput, verbose = FALSE) {
  
  if (verbose) {
    message(paste0(basename(dirData), ": ", type))
  }    
  
  # read configuration file for dataset
  cfgDataset <- read.csv(paste0(dirData, "/configDataset.csv"), stringsAsFactors = FALSE)
  
  # get config data for dataset
  DATASET_ID <- basename(dirData) #getConfig(cfgDataset, "dataset_id")
  DATASET_NAME <- getConfig(cfgDataset, paste0(switch(type, plant = "plant", site = "site", sitedata = "site"), "_data_filename"))
  DATASET_HEADER <- getConfig(cfgDataset, "header")
  DATASET_SKIP <- as.numeric(getConfig(cfgDataset, "skip"))
  DATASET_VERT <- switch(type, plant = cfgDataset$value[cfgDataset$key == "plant_char_vertical"],
                               site = cfgDataset$value[cfgDataset$key == "site_char_vertical"],
                               sitedata = FALSE)
  
  # get config data for variable names -
  # there is a different config file depending on the type of dataset (plant/site/site_data)
  cfgVarNames <- read.csv(paste0(dirData, "/config", switch(type, plant = "Plant", site = "Site", sitedata = "SiteData"), "VarNames.csv"), stringsAsFactors = FALSE)
  
  # get config data for plant or site characters -
  # there is a different config file depending on the type of dataset (plant/site)
  cfgChar <- switch(type, plant = read.csv(paste0(dirData, "/configPlantCharacters.csv"), stringsAsFactors = FALSE),
                           site = read.csv(paste0(dirData, "/configSiteCharacters.csv"), stringsAsFactors = FALSE))
  
  # get config data for lookups
  cfgLookup <- read.csv(paste0(dirData, "/configLookups.csv"), stringsAsFactors = FALSE)
  
  # read dataset
  data <- read.csv(paste0(dirData, "/", DATASET_NAME), check.names = FALSE, stringsAsFactors = FALSE)
  
  # read metadata
  meta <- read.csv(paste0(dirData, "/metadata.csv"), stringsAsFactors = FALSE)
  
  # get old dataset number - need this to trim the number from the end of site names in some cases (was added previously by RVG)
  datasetNum <- meta$dataset_num[meta$dataset_id == DATASET_ID]
  
  # trim whitespace from variable names which commonly had trailing whitespace in the original data files
  colnames(data) <- trim(colnames(data))
    
  
  # skip (remove) rows from top of dataset as specified in dataset config
  if (DATASET_SKIP > 0)
    data <- data[-DATASET_SKIP, ]
    
  # get list of input and output varnames from config
  # but only where the input varname exists in dataset (i.e. in config var_in is not "-")
  varNamesIn <- trim(c(cfgVarNames[cfgVarNames$var_in != "-", ]$var_in))
  varNamesOut <- c(cfgVarNames[cfgVarNames$var_in != "-", ]$var_out)
  
  # get list of missing varnames, i.e. those that we need in the output, but are not in the input
  varNamesMissing <- c(cfgVarNames[cfgVarNames$var_in == "-", ]$var_out)
  
  # get list of all output varnames
  varNamesOutAll <- c(cfgVarNames$var_out)
  
  # all varNamesIn must exist in dataset, if not then we need to stop and fix the problem
  if (length(setdiff(varNamesIn, colnames(data)) != 0)) {
    stop(setdiff(varNamesIn, colnames(data)))
  }
  
  # create temporary dataframe with data for vars that we want to keep (and that exist in the dataset) and set to correct varnames
  df <- setNames(data.frame(data[, varNamesIn]), varNamesOut)
  
  # append blank columns for the vars that are missing from the input dataset
  df <- cbind(df, setNames(data.frame(matrix(ncol = length(varNamesMissing), nrow = nrow(data)), stringsAsFactors = FALSE), varNamesMissing))
  
  # rearrange colnames to desired order for new dataset
  # NOTE: we need to coerce to data.frame and use setNames because if there is only a single column it reverts to a list
  df <- setNames(data.frame(df[, varNamesOutAll]), varNamesOutAll)
  
  # correct site names if necessary, remove dataset_id appended to end (e.g. "Agnes Banks_2")
  # in some cases these numbers were added manually by RVG to site names in the raw data 
  if ("site_name" %in% colnames(df)) {
    
    regex <- paste0("_", datasetNum, "$")
    
    df$site_name <- sapply(df$site_name, function(x) {
      # get position of, e.g., "_2" in site name
      # NOTE: this throws a warning for some reason, saying the pattern has length > 1. Not sure why, doesn't seem to matter.
      pos <- unlist(regexpr(regex, x))
      # if "_2" (or whatever) doesn't appear in site name then just return the site name
      # otherwise return the corrected site name with "_2" (or whatever) removed
      if (is.na(pos) | pos == -1) {
        return(x)      
      } else {
        return(substr(x, 1, pos - 1))
      }
    })
    
  }
  
  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  if (length(setdiff(cfgChar[, "var_name"], colnames(data)) != 0)) {
    stop(paste(DATASET_ID, ": missing traits: ", setdiff(cfgChar[, "var_name"], colnames(data))))
  }  
  
  if (DATASET_VERT == TRUE | type == "sitedata") {
    # if the dataset is already "vertical" (i.e. long rather than wide) then no further processing is needed
    out <- df
    
  } else {
    # if the dataset is "wide" then process each variable in turn, to create the "long" dataset -
    # say the original dataset has 20 rows of data and 5 traits, then we will end up with 100 rows
    for (i in 1:nrow(cfgChar)) {
      
      # create a temporary dataframe which is a copy of df
      # df is our data frame containing all the columns we want EXCEPT for the trait data itself
      x <- df
        
      # to x we append columns of data for character name, unit and value (the latter is retrieved from the data)
      x$trait_name <- cfgChar[i, "trait_name"]
      x$unit <- cfgChar[i, "unit"]
      x$value <- as.character(data[, cfgChar[i, "var_name"]])
      
      # build up the output dataset by, with each iteration, rbinding x and its newly added columns of trait data
      # (if first time through the loop then we don't need to append)
      if (!exists("out")) {
        out <- x
      } else {
        out <- rbind(out, x)
      }
    }
    
  }  
  
  # # get character lookup values where necessary
  # if (type != "sitedata") {
  #   out$lookup <- mapply(function(x, y) {
  #     # for each value of each character: get lookup corresponding to the value
  #     as.character(cfgLookup$value[cfgLookup$character == x & cfgLookup$lookup == y][1])
  #   }, out$character, out$value, SIMPLIFY = TRUE)
  # }
  
  return(out)
}

