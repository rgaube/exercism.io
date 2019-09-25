# The square of the sum of the first ten natural numbers is (1 + 2 + ... + 10)² = 55² = 3025.
# The sum of the squares of the first ten natural numbers is 1² + 2² + ... + 10² = 385.

difference_of_squares <- function(natural_number) {
	
	squareofsum <- function(natural_number) {
		sum <- 0
    for(i in 1:natural_number) sum <- sum(sum,i)
		sum^2
	}
  
  sumofsquares <- function(natural_number) {
		# pre-allocate vector
	  square <- c(length(natural_number))
    for(i in 1:natural_number) square[i] <- i^2
		sum(square)
	}
	squareofsum(natural_number)-sumofsquares(natural_number)
}

# other solution: minsym
difference_of_squares_minsym <- function(natural_number) {
  squsum  <- (sum(1:natural_number))^2
  sumsqu   <- sum((1:natural_number)^2)
  squsum - sumsqu
}

# other solution: lennuli
difference_of_squares_lenulli <- function(natural_number) {
  (sum(seq(1,natural_number)))^2-sum((seq(1,natural_number)^2))
}

# other solution: chuchoter
difference_of_squares_chuchoter <- function(natural_number) {
  s <- seq(1:natural_number)
  sum(s)^2 - sum(s^2)
}
