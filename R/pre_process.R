
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
#' @examples
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
#' @examples
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
#' @param str text string
#'
#' @return a 12 element character string, e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
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


#' Separate cells with a range to min and max
#' 
#' Separate values cells with a range into columns with minimum and maximum
#'
#' @param data data frame
#' @param x name of variable containing range
#' @param y1 name of variable to hold minimum
#' @param y2 name of variable to hold maximum
#' @param sep separator, by default "-"
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
#'
replace_duplicates_with_NA <- function(x) {
  x %>% replace(., duplicated(.), NA)
}
