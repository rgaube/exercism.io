# Return string "One for X, one for me."
# If no input for X is provided return "you".

two_fer <- function(input = "you") {
  
  # check if input is of type character and if input vector is of length 1
  if (is.character(input)) {
    paste0("One for ", input, ", one for me.")
  }
}

# others solution: y0wel (Tobias)
two_fer_y0wel <- function(input = "") {
  paste0("One for ", ifelse(input != "", input, "you"), ", one for me.")
}
