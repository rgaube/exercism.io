# This is a stub function to take two strings
# and calculate the hamming distance

# suggested solution by mentor
hamming <- function(strand1, strand2) {
  
  # Make single characters in string easily accessible for comparison
  s1 <- unlist(strsplit(strand1,""))
  s2 <- unlist(strsplit(strand2,""))
  
  stopifnot(length(s1)==length(s2))
  
  # Check for positions of differing nucleotides and count them
  # Better performance than my solution: length(which(s1 != s2)) 
  sum(s1 != s2)
}

# my solution
hamming_v1 <- function(strand1, strand2) {
  
  # Make single characters in string easily accessible for comparison
  s1 <- unlist(strsplit(strand1,""))
  s2 <- unlist(strsplit(strand2,""))
  
  if(length(s1)!=length(s2)) stop ("Strands must be of equal length")
  
  # Check for positions of differing nucleotides and count them
  length(which(s1 != s2))
}

# others solutions 1: assertthat
hamming_rganelli <- function(strand1, strand2) {
  
  # check strands are same length
  assertthat::assert_that(assertthat::are_equal(nchar(strand1), nchar(strand2)),
                          msg = "Strands not equal length")
  
  # compare each character position-wise and sum up number of differences
  dna_c <- strsplit(c(strand1, strand2), "")
  result <- purrr::map2_lgl(dna_c[[1]], dna_c[[2]], ~ ..1 == ..2)
  length(result) - sum(result)
  
}

# others solutions 1: nchar
hamming_clechasseur <- function(strand1, strand2) {
  stopifnot(nchar(strand1) == nchar(strand2))
  sum(unlist(strsplit(strand1, "")) != unlist(strsplit(strand2, "")))
}

# others solution: y0wel
hamming_y0wel <- function(strand1, strand2) {
  if (nchar(strand1) != nchar(strand2)) {
    stop("Strands differ in length!")
  } else {
    strand1 <- strsplit(strand1, split = "")[[1]]
    strand2 <- strsplit(strand2, split = "")[[1]]
    result <- strand1 == strand2
    length(result[which(result == FALSE)])
  }
  
}

# others solutions 1: mapply
hamming_corylamontagne <- function(strand1, strand2) {
  if (nchar(strand1) != nchar(strand2))
    throw("Strand size mismatch", 0)
  mapply(function(x,y) sum(x!=y),strsplit(strand1,""),strsplit(strand2,""))
}

# -------------- Test data ---------------
# identical strands
s1 <- "AC"
s2 <- "AC"
# reverse strands
s3 <- "CA"
# differing strands (3 mismatches)
s4 <- "GAGCCT"
s5 <- "CATCGT"
# strands of differing length: error
s6 <- "GAGCCT"
s7 <- "CATCG"
# empty strands
s8 <- ""
s9 <- ""

hamming(s4,s5)
