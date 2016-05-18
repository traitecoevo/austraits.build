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

# processes a single dataset 
processData <- function(DATASET_ID, data, cfgDataset, cfgVarNames, cfgChar, cfgLookup, meta) {

  # type: which type of data are we processing, one of "plant", "site" or "sitedata"
  type <- "plant"
  # get config data for dataset
  DATASET_HEADER <- getConfig(cfgDataset, "header")
  DATASET_SKIP <- as.numeric(getConfig(cfgDataset, "skip"))
  DATASET_VERT <- cfgDataset$value[cfgDataset$key == "plant_char_vertical"]

  # get old dataset number - need this to trim the number from the end of site names in some cases (was added previously by RVG)
  datasetNum <- meta$dataset_num[meta$dataset_id == DATASET_ID]
  
  # trim whitespace from variable names which commonly had trailing whitespace in the original data files
  colnames(data) <- trim(colnames(data))
    
  # add metadata_id and project_source_id to dataset if not there already -
  # for some datasets RVG added these data manually, especially in cases where there were multiple metadata for a single dataset
  # but if that's not the case we need to get the id values from the metadata table and add them to the dataset
  if (!"metadata_id" %in% colnames(data)) {
    # get metadata_id and primary_source_id from metadata table
    metadataId <- meta$metadata_id[meta$dataset_id == DATASET_ID]
    primarySourceId <- meta$primary_source_id[meta$dataset_id == DATASET_ID]
    
    # for those datasets where we are adding metadata_id and primary_source_id we expect a single value for
    # each of these, if not then there is something wrong
    if (length(metadataId) != 1 | length(primarySourceId) != 1) {
      stop(paste0(DATASET_ID, ": Check metadata and primary source IDs"), call. = FALSE)
    }
    
    # create columns for metadata_id and primary_source_id
    data$metadata_id <- metadataId
    data$primary_source_id <- primarySourceId
  }
  
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
      x$character <- cfgChar[i, "character"]
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
  
  # get character lookup values where necessary
  if (type != "sitedata") {
    out$lookup <- mapply(function(x, y) {
      # for each value of each character: get lookup corresponding to the value
      as.character(cfgLookup$value[cfgLookup$character == x & cfgLookup$lookup == y][1])
    }, out$character, out$value, SIMPLIFY = TRUE)
  }
  
  out
}
