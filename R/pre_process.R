trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Returns month for given indices
# if needed, i is coerced to integer
# warnings suppressed because as.integer gives a warning for NAs
get_month <- function(i) {
  month.abb[suppressWarnings(as.integer(i))]
}

# A common processing pattern when creating flowing times
# We have an integer for start and end month
# Converts these into binary vector
format_flowering_months <- function(start, end){
  x <- rep(NA_character_, length(start))
  i <- !is.na(start) & !is.na(end)
  x[i] <- paste(get_month(start[i]), get_month(end[i]), sep = "-")
  x[i] <- convert_month_range_vec_to_binary(x[i])
  x
}

# converts vectors of month range type values (i.e. 'Jan-Apr') to vectors of 12 element character strings of binary data
# e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)
# wrapper function for convert_month_range_string_to_binary
convert_month_range_vec_to_binary <- function(vec) {
  out <- unlist(lapply(vec, function(x)
        paste0(convert_month_range_string_to_binary(x), collapse="")))
  out[out=="NA"] <- NA_character_
  out
}

# converts flowering and fruiting month ranges to 12 element character strings of binary data
# e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
convert_month_range_string_to_binary <- function(str) {
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

collapse_multirow_phenology_data_to_binary_vec <- function(data, trait="flowering month", renamed_trait="flowering time") {

# this function takes multirow phenology data and collapses it to a 12 digit binary string 
# e.g.  Acacia dealbata, 3
#       Acacia dealbata, 4
#       Acacia dealbata, 5
#       Acacia dealbata, 6
#   >>  Acacia dealbata, 001111000000
#
# args allow for any input trait to be used and for the desired output trait name to be specified. See defaults for example  
      
  # create comma separated character string containing month numbers for each species

  data_summary <- data[data$trait == trait,] %>%
                    group_by(species) %>%
                    summarise(
                      months_string = paste(lookup_id, collapse = ", "))

  # collapse this info into a 12 digit binary string
  my.list <- vector("list", nrow(data_summary))
  
  for(i in 1:nrow(data_summary)) {
    
    data_months <- trim(strsplit(data_summary$months_string[i], ",")[[1]])
    outvec <- c(0,0,0,0,0,0,0,0,0,0,0,0)
    
    for(j in 1:12) {
      if(any(data_months %in% j)) {
        outvec[j] <- 1
      }
    }
    
    sp <- data_summary$species[i]
    out <- data.frame(cbind(sp, paste(outvec, collapse= "")), stringsAsFactors=FALSE)
    my.list[[i]] <- out
    
  }
  
  # append reworked values to dataset and remove multirow data
  x <- bind_rows(my.list)
  y <- x
  y$sp <- NULL
  y$V2 <- NULL
  
  y$species <- x$sp
  y$col_id <- NA
  y$lookup_id <- NA
  y$lookup_data <- x$V2
  y$trait <- renamed_trait
  y$units <- NA
  
  data <- bind_rows(data, y)
  data <- data[!data$trait == trait,]
  return(data)
}

