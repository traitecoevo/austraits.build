removeNA <- function(df, column) {
  
  column <- deparse(substitute(column))
  df <- df[!is.na(df[column]),]
  df <- df[df[column] != "",]
  
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# converts vectors of month range type values (i.e. 'Jan-Apr') to vectors of 12 element character strings of binary data
# e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
# wrapper function for getBinaryMonths
convert_month_range_to_binary <- function(x, trait) {
  flowering_bin <- lapply(x[[trait]], getBinaryMonths)
  x[[trait]] <- sapply(flowering_bin, paste0, collapse="")
  return(x)
}


# converts flowering and fruiting month ranges to 12 element character strings of binary data
# e.g. c(1,1,1,1,0,0,0,0,0,0,0,1)  
getBinaryMonths <- function(str) {
  str <- trim(str) %>%
    tolower()
  
  regexMonths <- paste0("(", paste(tolower(month.abb), collapse="|"), ")")
  seasons <- c("summer", "summer", "autumn", "autumn", "autumn", "winter", "winter", "winter", "spring", "spring", "spring", "summer")
  regexSeasons <- paste0("(", paste(tolower(unique(seasons)), collapse="|"), ")")
  
  # all year only
  if (grepl("^(all *year|ay)$", str)) {
    return(rep(1, 12))
  }
  
  # ephemeral only
  
  # TODO - maybe ignore ephemeral?? what does it mean? what to return?
  if (grepl("ephemeral", str)) {
    return(paste("ephemeral"))
  }
  
  # after fire 
  
  if (grepl("after fire", str)) {
    return(paste("after fire"))
  }
  
  # periodic
  
  if (grepl("periodic", str)) {
    return(paste("periodic"))
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