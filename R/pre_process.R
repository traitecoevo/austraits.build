trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Returns month for given indices
# if needed, i is coerced to integer
# warnings suppressed because as.integer gives a warning for NAs
get_month <- function(i) {
  month.abb[suppressWarnings(as.integer(i))]
}

#' Create flowertimes from start to end pair
#' 
#' A common processing pattern when creating flowing times
#' We have an integer for start and end month
#' Converts these into binary vector
#'
#' @param start <what param does>
#' @param end <what param does>
#'
#' @export
#' @return a 12 element character string, e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
format_flowering_months <- function(start, end){
  x <- rep(NA_character_, length(start))
  i <- !is.na(start) & !is.na(end)
  x[i] <- paste(get_month(start[i]), get_month(end[i]), sep = "-")
  x[i] <- convert_month_range_vec_to_binary(x[i])
  x
}

#' convert vectors of month range to 12 element character strings of binary data
#'
#' converts vectors of month range type values (i.e. 'Jan-Apr') to character strings 
#' of length 12 consisting of y & n, e.g. "yyynnnnnnyyy"
#'
#' @param vec <what param does>
#'
#' @export
#' @return character string of length 12 consisting of y & n, e.g. "yyynnnnnnyyy"
convert_month_range_vec_to_binary <- function(vec) {
  out <- 
    unlist(lapply(vec, function(x) convert_month_range_string_to_binary(x))) %>%
    convert_01_ny()
  out[out=="NA"] <- NA_character_
  out
}

convert_01_ny <- function(txt) {
  txt %>%   
  gsub("1", "y", ., fixed=TRUE) %>%
  gsub("0", "n", ., fixed=TRUE)
}

#' Converts flowering and fruiting month ranges to 12 element character strings of binary data
#' consisting of 1 & 0 e.g. "111000000111"
#'
#' @param str text string
#'
#' @export
#' @return a character string  length-12, e.g. "111000000111"
convert_month_range_string_to_binary <- function(str) {
  convert_month_range_string_to_binary_worker(str) %>% 
    paste(collapse="") 
}

#' Converts flowering and fruiting month ranges to 12 element character strings of binary data
#' 
#' e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
#'
#' @param str text string
#'
#' @return a 12 element character string, e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
convert_month_range_string_to_binary_worker <- function(str) {
  str <- trim(str) %>%
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
  
   # periodic
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
  
  # one or more month ranges
  if (grepl(paste0("^", regexMonths, " *- *", regexMonths, "([/,] *", regexMonths, " *- *", regexMonths, ")*$"), str)) {  
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
  if (grepl(paste0("^", regexSeasons, " *([/,] *", regexSeasons, ")+$"), str)) {
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


#' Separate values cells with a range into columns with minimum and maximum
#'
#' @param data data frame
#' @param x name of variable containing range
#' @param y1 name of variable to hold minimum
#' @param y2 name of variable to hold maximum
#' @param sep seperator, by default "-"
#'
#' @return modified data frame
#'
separate_range <- function(data, x, y1, y2, sep="-", remove=TRUE) {
  data <- separate(data, !!x, sep = "-", into = c(y1, y2), remove=remove, fill="right")
  data[[y2]] <- ifelse(is.na(data[[y2]]), data[[y1]], data[[y2]])
  data
}


#' replace_duplicates_with_NA replaces duplicate values in x with NA. 
#'
#' @param x vector
#'
#' @return vector
#'
replace_duplicates_with_NA <- function(x) {
  x %>% replace(., duplicated(.), NA)
}

#' Move select trait values from a pre-existing column (trait_name) to a new column (new trait_name)
#'
#' @param data data frame, representing a specific dataset_id
#' @param original_trait name of the variable in the original data file, representing a trait in a wide dataset
#' @param new_trait name of the new variable being created, representing an additional trait in a wide dataset
#' @param original_values values of the original trait that need to be remapped to a different (new) trait
#' @param value_to_use the appropriate value of the new trait; this may be identical to the original values, or may be a slightly different word/syntax 
#'
#' @return
#' @export
#'
#' @examples
#' data <- read_csv(data/"Hughes_1992/data.csv")
#' data %>% move_values_to_new_trait(data, "growth form", "root_structure", "Saprophyte", "saprophyte") -> data
move_values_to_new_trait <- 
  function(data, original_trait, new_trait, original_values, value_to_use) {
    
    i <- data[[original_trait]] %in% original_values
    
    data[[new_trait]]= ifelse(i, value_to_use, NA)
    
    data[[original_trait]] = ifelse(i, NA, original_trait)

    data
}