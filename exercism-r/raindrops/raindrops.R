# solution with corrections from mentor:
# Note that for line 4, you could instead do: isfactor <- number %% c(3, 5, 7) == 0
# since most (base) R functions are usually vectorised.
# And then for line 5, as a style suggestion, since isfactor is a boolean vector, 
# I'd recommend something like all(!isfactor) since it's a little more direct than the "all equal to zero" comparison?
raindrops <- function(number) {
  sounds <- c('Pling', 'Plang', 'Plong')
  isfactor <- number %% c(3,5,7) == 0
  ifelse(all(!isfactor), paste(number), paste0(sounds[which(isfactor)], collapse = ""))
}

# vectorized: initial submission
raindrops_my <- function(number) {
  sounds <- c('Pling', 'Plang', 'Plong')
  isfactor <- c(number %% 3 == 0, number %% 5 == 0, number %% 7 == 0)
  ifelse(all(isfactor == 0), paste(number), paste0(sounds[which(isfactor == 1)], collapse = ""))
}

# Testing
raindrops(3)
raindrops(5)
raindrops(7)
raindrops(105)
raindrops(35)
raindrops(15)
raindrops(8)

# --- Solutions from others

raindrops_scottwiles <- function(number) {
  sounds <- c("Pling" = 3, "Plang" = 5, "Plong" = 7)
  outputs <- number %% sounds == 0
  
  if (!any(outputs)) {
    as.character(number)
    
  } else {
    paste0(names(sounds)[which(outputs)], collapse = "")
  }
}

raindrops_lennuli <- function(number) {
  outputs=c("Pling","Plang","Plong")
  output=c("","","")
  factors=c(3,5,7)
  for (i in 1:3) {
    if (number %% factors[i] ==0) {
      output[i]=outputs[i]
    }
  }
  output=paste0(output, collapse="")
  return(max(output,toString(number)))
}

raindrops_clechasseur <- function(number) {
  sounds <- c(Pling = 3, Plang = 5, Plong = 7)
  output <- paste0(names(sounds[number %% sounds == 0]), collapse = '')
  if (nchar(output) != 0) output else as.character(number)
}


raindrops_v0wel <- function(number) {
  sounds <- c("Pling", "Plang", "Plong")
  if (0 %in% (number %% c(3, 5, 7))) {
    paste(sounds[which(number %% c(3, 5, 7) == 0)], collapse = "")
  } else {
    paste(number)
  }
}

# similar to the solutions from my practise day
raindrops_fireproofsocks <- function(number) {
  output = ''
  if (number %% 3 == 0) output = 'Pling'
  if (number %% 5 == 0) output = paste(output, 'Plang', sep='')
  if (number %% 7 == 0) output = paste(output, 'Plong', sep='')
  if (output == '')output = as.character(number)
  return(output)
}
