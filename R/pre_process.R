#' Return month for given indices
#' 
#' Returns abbreviated month for given indices/ integers,
#' if needed, i is coerced to integer
#' warnings suppressed because as.integer gives a warning for NAs
#' 
#' @param i vector with indices/integers representing months 
#'
#' @return vector containing abbreviated months
#'
#' @export
#' @examples get_month(c(1,1,2,3,3,3,4,5,6))
get_month <- function(i) {
  month.abb[suppressWarnings(as.integer(i))]
}

#' Create flowering times from start to end pair
#' 
#' A common processing pattern when creating flowering times
#' We have an integer for start and end month
#' Converts these into binary vector
#'
#' @param start numeric value between 1 and 12 representing earliest flowering month
#' @param end numeric value between 1 and 12 representing latest flowering month
#'
#' @export
#' @return a 12 element character string consisting of y & n, e.g. "yyyynnnnnnnn"  
format_flowering_months <- function(start, end){
  x <- rep(NA_character_, length(start))
  i <- !is.na(start) & !is.na(end)
  x[i] <- paste(get_month(start[i]), get_month(end[i]), sep = "-")
  x[i] <- convert_month_range_vec_to_binary(x[i])
  x
}

#' Convert vectors of month range to 12 element character strings of binary data
#'
#' Converts vectors of month range type values (i.e. 'Jan-Apr') to character strings 
#' of length 12 consisting of y & n, e.g. "yyyynnnnnnnn"
#'
#' @param vec vector of month range using abbreviated month names (e.g. "Jan-Apr")
#'
#' @export
#' @return character string of length 12 consisting of y & n, e.g. "yyyynnnnnnnn"
convert_month_range_vec_to_binary <- function(vec) {
  out <- 
    unlist(lapply(vec, function(x) convert_month_range_string_to_binary(x))) %>%
    convert_01_ny()
  out[out=="NA"] <- NA_character_
  out
}

#' Convert 0/1 to n/y
#' 
#' Convert binary 0/1 to n/y representing yes & no
#'
#' @param txt vector of character strings
#'
#' @return vector of character strings
#' @export
#'
#' @examples convert_01_ny(c(0,1,1,1,0))
convert_01_ny <- function(txt) {
  txt %>%   
  gsub("1", "y", ., fixed=TRUE) %>%
  gsub("0", "n", ., fixed=TRUE)
}

#' Convert month range to 12 element binary string
#' 
#' Converts flowering and fruiting month ranges to a string of 12 characters of binary data
#' consisting of 1 & 0 e.g. "11100000111"
#'
#' @param str character string of month range using abbreviated month names. e.g. "Oct- Mar"
#'
#' @export
#' @return a character string  length-12, e.g. "111000000111"
convert_month_range_string_to_binary <- function(str) {
  convert_month_range_string_to_binary_worker(str) %>% 
    paste(collapse="") 
}

#' Convert month range to binary
#' 
#' Converts flowering and fruiting month ranges to 12 element character strings of 0 and 1 representing Jan - Dec
#' e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
#'
#' @param str character string with abbreviated months (i.e. "Jan", "Feb"). Also accepts other
#' terms such as (all year) and seasons (e.g. summer). The text can also include a range 
#' separated with "-" (e.g. "Jan-Jul")   
#' 
#' @return a numeric vector with 12 elements, e.g. c(1,1,1,0,0,0,0,0,0,0,0,1)   
convert_month_range_string_to_binary_worker <- function(str) {
  str <- str %>% stringr::str_trim() %>%
    tolower()
  
  regexMonths <- paste0("(", paste(tolower(month.abb), collapse="|"), ")")
  seasons <- c("summer", "summer", "autumn", "autumn", "autumn", "winter", "winter", "winter", "spring", "spring", "spring", "summer")
  regexSeasons <- paste0("(", paste(tolower(unique(seasons)), collapse="|"), ")")
  
  # all year only
  if (grepl("^(all *year|ay)", str)) {
    return(rep(1, 12))
  }
  
  # ephemeral only
  if (grepl("ephemeral", str)) {
    return(NA)
  }
  
  # after fire 
  if (grepl("after fire", str)) {
    return(NA)
  }
  
  # periodic
  if (grepl("periodic", str)) {
    return(NA)
  }
  
   # irregular
  if (grepl("irregular", str)) {
    return(NA)
  }
  
  ### months
  
  # single month only
  if (grepl(paste0("^", regexMonths, "$"), str)) {
    return(`[<-`(rep(0, 12), match(str, tolower(month.abb)), 1))
  }
  
  # months separated with / or ,
  if (grepl(paste0("^", regexMonths, " *([/,] *", regexMonths, ")+$"), str)) {
    m <- regmatches(str, gregexpr(regexMonths, str))[[1]]
    return(`[<-`(rep(0, 12), which(tolower(month.abb) %in% m), 1))
  }  
  
  # one or more month ranges separated with / or , or ; or :
  if (grepl(paste0("^", regexMonths, " *- *", regexMonths, "([/,;:] *", regexMonths, " *- *", regexMonths, ")*$"), str)) {  
    m <- regmatches(str, gregexpr(paste0(regexMonths, " *- *", regexMonths), str))[[1]]
    bin <- rep(0, 12)
    
    for (i in 1:length(m)) {
      monthFrom <- match(regmatches(m[i], regexpr(paste0("^", regexMonths), m[i])), tolower(month.abb))
      monthTo <- match(regmatches(m[i], regexpr(paste0(regexMonths, "$"), m[i])), tolower(month.abb))
      
      if (monthFrom <= monthTo) {
        bin <- `[<-`(bin, seq(monthFrom, monthTo), 1)
      } else {
        bin <- `[<-`(bin, c(seq(1, monthTo), seq(monthFrom, 12)), 1)
      }       
    }
    return(bin)
  }
  
  ### seasons
  
  # single season only
  if (grepl(paste0("^", regexSeasons, "$"), str)) {
    return(`[<-`(rep(0, 12), which(seasons == str), 1))
  }
  
  # seasons separated with / or ,
  if (grepl(paste0("^", regexSeasons, " *([/,;:] *", regexSeasons, ")+$"), str)) {
    m <- regmatches(str, gregexpr(regexSeasons, str))[[1]]
    return(`[<-`(rep(0, 12), which(tolower(seasons) %in% m), 1))
  }  
  
  # season range - currently handles only one season range, e.g. "autumn-spring" (this seems reasonable)
  if (grepl(paste0("^", regexSeasons, " *- *", regexSeasons, "$"), str)) {
    monthFrom <- min(which(seasons == regmatches(str, regexpr(paste0("^", regexSeasons), str))))
    if (monthFrom == 1) { monthFrom <- 12 }  # correction for summer!
    monthTo <- max(which(seasons == regmatches(str, regexpr(paste0(regexSeasons, "$"), str))))
    if (monthTo == 12) { monthTo <- 2 }  # correction for summer!
    
    if (monthFrom <= monthTo) {
      return(`[<-`(rep(0, 12), seq(monthFrom, monthTo), 1))
    } else {
      return(`[<-`(rep(0, 12), c(seq(1, monthTo), seq(monthFrom, 12)), 1))
    }
  }
  
  return(NA)
}

#' Separate cells with a range to min and max
#' 
#' Separate values cells with a range into columns with minimum and maximum
#'
#' @param data data frame
#' @param x name of variable containing range
#' @param y1 name of variable to hold minimum
#' @param y2 name of variable to hold maximum
#' @param sep separator, by default "-"
#' @param remove logical, default = TRUE
#'
#' @return modified data frame
#'
separate_range <- function(data, x, y1, y2, sep="-", remove=TRUE) {
  data <- tidyr::separate(data, !!x, sep = "-", into = c(y1, y2), remove=remove, fill="right")
  data[[y2]] <- ifelse(is.na(data[[y2]]), data[[y1]], data[[y2]])
  data
}

#' Replace duplicate values with NA
#' 
#' replace_duplicates_with_NA replaces duplicate values in x with NA. 
#'
#' @param x vector containing values
#'
#' @return vector with duplicate values as NA
#' @importFrom rlang .data
replace_duplicates_with_NA <- function(x) {
  base::replace(x, duplicated(x), NA)
}


#' Move select trait values from a pre-existing column (trait_name) to a new column (new trait_name)
#'
#' @param data data frame, representing a specific dataset_id
#' @param original_trait name of the variable in the original data file, representing a trait in a wide dataset
#' @param new_trait name of the new variable being created, representing an additional trait in a wide dataset
#' @param original_values values of the original trait that need to be remapped to a different (new) trait
#' @param value_for_new_trait the appropriate value of the new trait; this may be identical to the original values, or may be a slightly different word/syntax 
#' @param value_to_keep the appropriate value to retain for the old trait; this may be identical to the original values or may be NA 
#'
#' @return
#' @export
#'
#' @examples
#' data <- read_csv(data/"Hughes_1992/data.csv")
#' data %>% move_values_to_new_trait(data, "growth form", "root_structure", "Saprophyte", "saprophyte") -> data
move_values_to_new_trait <- function(data, original_trait, new_trait, original_values, values_for_new_trait, values_to_keep) {
  
       for (j in 1:length(original_values)) {
            
            i <- data[[original_trait]] == original_values[[j]]
            
            data[[new_trait]] = ifelse(i, values_for_new_trait[[j]], data[[new_trait]])
            data[[original_trait]] = ifelse(i, values_to_keep[[j]], data[[original_trait]])
            data
       }
       
  return(data)
}


add_values_to_additional_trait_long <- 
  function(data, new_trait, traits_column, values_column, original_values, new_values) {  
    i <- filter(data,data[[values_column]] %in% original_values)
    i[[traits_column]] <- new_trait
    i[[values_column]] <- new_values
    data <- bind_rows(data,i)
  }


move_values_to_new_trait_long <- 
  function(data, original_trait, new_trait, traits_column, values_column, original_values) {
    
    i <- data[[values_column]] %in% original_values
    
    data[[traits_column]] = ifelse(i, new_trait, data[[traits_column]])

    data
  } 



#' Substitutions from csv
#' @description Function that simultaneously adds many trait value replacements, potentially across many trait_names and dataset_ids, to the respective metadata.yml files.
#' This function will be used to quickly re-align/re-assign trait values across all AusTraits studies.
#'
#' @param dataframe_of_substitutions dataframe with columns indicating dataset_id, trait_name, original trait values (find), and AusTraits aligned trait value (replace)
#' @param dataset_id study's dataset_id in AusTraits
#' @param trait_name trait name for which a trait value replacement needs to be made
#' @param find trait value submitted by the contributor for a data observation
#' @param replace AusTraits aligned trait value
#'
#' @return 
#' @export
#'
#' @examples read_csv("export/dispersal_syndrome_substitutions.csv") %>% select(-extra) %>% filter(dataset_id == "Angevin_2011") -> dataframe_of_substitutions
#' @examples substitutions_from_csv(dataframe_of_substitutions,dataset_id,trait_name,find,replace)

substitutions_from_csv <- function(dataframe_of_substitutions,dataset_id,trait_name,find,replace) {

  #split dataframe of substitutions by row  
  dataframe_of_substitutions %>%
    dplyr::mutate(rows = row_number()) %>% 
    dplyr::group_split(rows) -> dataframe_of_substitutions

  set_name <- "substitutions"

  #add substitutions to metadata files
  for (i in 1:max(dataframe_of_substitutions)$rows) {
    metadata <- metadata_read_dataset_id(dataframe_of_substitutions[[i]]$dataset_id)

    to_add <- list(trait_name = dataframe_of_substitutions[[i]]$trait_name, find = dataframe_of_substitutions[[i]]$find, replace = dataframe_of_substitutions[[i]]$replace)

    if(is.null(metadata[[set_name]]) || is.na(metadata[[set_name]])) {
      metadata[[set_name]] <- list()
    }

    data <-  list_to_df(metadata[[set_name]])  

    metadata[[set_name]] <- append_to_list(metadata[[set_name]], to_add)

    metadata_write_dataset_id(metadata, dataframe_of_substitutions[[i]]$dataset_id)
  }  
}
