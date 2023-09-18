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
#' @examples get_month(c(1, 1, 2, 3, 3, 3, 4, 5, 6))
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
#' @return a 12 element character string consisting of y & n, e.g. "yyyynnnnnnnn"
format_flowering_months <- function(start, end) {
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
#' @return character string of length 12 consisting of y & n, e.g. "yyyynnnnnnnn"
convert_month_range_vec_to_binary <- function(vec) {
  out <-
    unlist(lapply(vec, function(x) convert_month_range_string_to_binary(x))) %>%
    convert_01_ny()
  out[out == "NA"] <- NA_character_
  out
}

#' Convert 0/1 to n/y
#'
#' Convert binary 0/1 to n/y representing yes & no
#'
#' @param txt vector of character strings
#'
#' @return vector of character strings
#'
#' @examples convert_01_ny(c(0, 1, 1, 1, 0))
convert_01_ny <- function(txt) {
  txt <- gsub("1", "y", txt, fixed = TRUE)
  gsub("0", "n", txt, fixed = TRUE)
}

#' Convert month range to 12 element binary string
#'
#' Converts flowering and fruiting month ranges to a string of 12 characters of binary data
#' consisting of 1 & 0 e.g. "11100000111"
#'
#' @param str character string of month range using abbreviated month names. e.g. "Oct- Mar"
#'
#' @return a character string  length-12, e.g. "111000000111"
convert_month_range_string_to_binary <- function(str) {
  convert_month_range_string_to_binary_worker(str) %>%
    paste(collapse = "")
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
  str <- str %>%
    stringr::str_trim() %>%
    tolower()

  regexMonths <- paste0("(", paste(tolower(month.abb), collapse = "|"), ")")
  seasons <- c("summer", "summer", "autumn", "autumn", "autumn", "winter", "winter", "winter", "spring", "spring", "spring", "summer")
  regexSeasons <- paste0("(", paste(tolower(unique(seasons)), collapse = "|"), ")")

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
    if (monthFrom == 1) {
      monthFrom <- 12
    } # correction for summer!
    monthTo <- max(which(seasons == regmatches(str, regexpr(paste0(regexSeasons, "$"), str))))
    if (monthTo == 12) {
      monthTo <- 2
    } # correction for summer!

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
separate_range <- function(data, x, y1, y2, sep = "-", remove = TRUE) {
  data <- tidyr::separate(data, !!x, sep = "-", into = c(y1, y2), remove = remove, fill = "right")
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

#' Merge minimum and maximum trait values into a single range column.
#'
#' This is an appropriate function to use for recombining range values extracted from floras and taxonomic treatments that were previously separated in AusTraits
#'
#' @param data name of the dataset being mutated
#' @param min_column name of the variable with the minimum trait values in the wide datas file
#' @param max_column name of the variable with the maximum trait values in the wide datas file
#' @param range_column name of a variable being created that combines the minimum and maximum values
#' @param column_value_type name of a variable being created that indicates the value type of the associated column
#'
#' @return data frame with a two new columns containing manipulated trait data
#'
#' @examples
#' \dontrun{
#' data <- read_csv("data/Chinnock_2007/data.csv")
#' data %>%
#'   format_min_max_as_range("min leaf length (mm)", "max leaf length (mm)", "leaf_length_range", "leaf_length_value_type") %>%
#'   format_min_max_as_range("min leaf width (mm)", "max leaf width (mm)", "leaf_width_range", "leaf_width_value_type") %>%
#'   format_min_max_as_range("seed length min (mm)", "seed length max (mm)", "seed_length_range", "seed_length_value_type")
#' }
format_min_max_as_range <- function(data, min_column, max_column, range_column, column_value_type) {
  data[[range_column]] <- ifelse((!is.na(data[[min_column]])),
    paste(data[[min_column]], data[[max_column]], sep = "--"),
    data[[max_column]]
  )
  data[[range_column]] <- ifelse((is.na(data[[max_column]])),
    data[[min_column]],
    data[[range_column]]
  )
  data[[range_column]] <- ifelse(data[[min_column]] == data[[max_column]],
    data[[max_column]],
    data[[range_column]]
  )
  data[[column_value_type]] <- ifelse(stringr::str_detect(data[[range_column]], "--"), "range", "mean")

  return(data)
}

#' Move select trait values from a pre-existing column (trait_name) to a new column (new trait_name)
#'
#' @param data data frame, representing a specific dataset_id
#' @param original_trait name of the variable in the original data file, representing a trait in a wide dataset
#' @param new_trait name of the new variable being created, representing an additional trait in a wide dataset
#' @param original_values values of the original trait that need to be remapped to a different (new) trait
#' @param values_for_new_trait the appropriate value of the new trait; this may be identical to the original values, or may be a slightly different word/syntax
#' @param values_to_keep the appropriate value to retain for the old trait; this may be identical to the original values or may be NA
#'
#' @return data frame with a new column containing additional trait data
#'
#' @examples
#' \dontrun{
#' data <- read_csv(data / "Hughes_1992/data.csv")
#' data %>% move_values_to_new_trait(
#'   data, "growth form", "root_structure",
#'   "Saprophyte", "saprophyte"
#' ) -> data
#' }
move_values_to_new_trait <- function(data, original_trait, new_trait, original_values, values_for_new_trait, values_to_keep) {
  # if (!new_trait %in% colnames(data)){
  #  data[[new_trait]] = NA_character_
  # }

  # data[[original_trait]] = str_to_lower(data[[original_trait]])

  for (j in 1:length(original_values)) {
    i <- data[[original_trait]] == original_values[[j]]
    i <- ifelse(is.na(i), "FALSE", i)

    data[[new_trait]] <- ifelse(i, values_for_new_trait[[j]], data[[new_trait]])
    data[[original_trait]] <- ifelse(i, values_to_keep[[j]], data[[original_trait]])
    data
  }

  # data = data %>% mutate(across(everything(), ~na_if(.x, "")))

  return(data)
}

#' Add values to an additional trait for datasets in long format
#'
#' @param data data frame, representing a specific dataset_id
#' @param new_trait name of the new variable being created, representing an additional trait in a long dataset
#' @param traits_column column containing the trait names
#' @param values_column column containing the trait values
#' @param original_values values of the original trait that need to be remapped to a different (new) trait
#' @param new_values values to be added to the new trait
#'
#' @return data frame containing additional rows of data for a new trait
#'
add_values_to_additional_trait_long <-
  function(data, new_trait, traits_column, values_column, original_values, new_values) {
    i <- filter(data, data[[values_column]] %in% original_values)
    i[[traits_column]] <- new_trait
    i[[values_column]] <- new_values
    data <- dplyr::bind_rows(data, i)
  }

#' Move values in new trait in long format
#'
#' @param data data frame, representing a specific dataset_id
#' @param original_trait name of the variable in the original data file, representing a trait in a long dataset
#' @param new_trait name of the new variable being created, representing an additional trait in a long dataset
#' @param traits_column column containing the trait names
#' @param values_column column containing the trait values
#' @param original_values values of the original trait that need to be remapped to a different (new) trait
#'
#' @return data frame containing additional trait names in the traits column
move_values_to_new_trait_long <-
  function(data, original_trait, new_trait, traits_column, values_column, original_values) {
    i <- data[[values_column]] %in% original_values

    data[[traits_column]] <- ifelse(i, new_trait, data[[traits_column]])

    data
  }
