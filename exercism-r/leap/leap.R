# Report if it is a leap year
# Leap year: 
# * evenly divisible by 4
#   * except years evenly divisible by 100 unless the year is also evenly divisible by 400


leap <- function(year) {
  #year %% 4 == 0 & year %% 100 != 0 | year %% 4 == 0 & (year %% 100 == 0 & year %% 400 == 0)
  #year %% 4 == 0 & (year %% 100 != 0 | (year %% 100 == 0 & year %% 400 == 0))
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

# solution from rganelli
leap_rganelli <- function(year) {
  # If a year is evenly divisible by 16, it is either evenly divisible by 400 or not evenly divisible by 100.
  # 400 is evenly divisible by 16 whereas 100,200 and 300 are not. 
  # Ergo, the set of integers evenly divisible by 16 and 100 are all evenly divisible by 400.
  year %% 16 == 0 & year %% 4 == 0
}

# solution from y0wel
leap_y0wel <- function(year) {
  if (year %% 4 == 0) {
    if (year %% 100 != 0) {
      TRUE
    } else {
      if (year %% 400 == 0) {
        TRUE
      } else {
        FALSE
      }
    }
  } else {
    FALSE
  }
}




#                          %%4
#                         /  \
#                        y    n
#                       /      \
#                    !%%100   NotLeapYear
#                     /  \
#                    y    n
#                   /      \
#              LeapYear    %%400
#                          /   \
#                         y     n
#                        /       \
#                   LeapYear   NotLeapYear
