# Test code with testthat
# Source: https://www.youtube.com/watch?v=bx92oCMxUhg

### Errorneous code
library(stringr)

# example input: first_of_last_month("2018-05-31")
first_of_last_month <- function(mystring){
  # take first 4 characters (year)
  yr <- substr(mystring, 1,4)
  # take characters 6 and 7 (month)
  mon <- as.numeric(substr(mystring, 6, 7))
  
  # function of str_pad is not clear to me
  lastmon <- stringr::str_pad(mon - 1, width = 2, pad = 0)
  result <- paste(yr, lastmon, "01", sep = "-")
  return(result)
}

### Correct code: v2 (only with R)

# example input: first_of_last_month("2018-05-31")
first_of_last_month_v2 <- function(mystring){
  mydate <- as.Date(mystring)
  first_this_month <- as.Date(as.character(cut.Date(mydate, breaks = "month")))
  result <- as.character(seq(first_this_month, length = 2, by = "-1 months")[2])
  return(result)
}

### Correct code: v3
library(lubridate)

# example input: first_of_last_month("2018-05-31")
first_of_last_month_v3 <- function(mystring){
  mydate <- as.Date(mystring)
  mydate_first_of_month <- floor_date(mydate, "month")
  result <- as.character(mydate_first_of_month - months(1))
  return(result)
}


