# Functions to help parse output from census api.

strip_both <- function(x) {
  n <- nchar(x)
  return(substr(x, 2, n - 1))
}


strip_left <- function(x) {
  n <- nchar(x)
  return(substr(x, 2, n))
}


strip_right <- function(x) {
  n <- nchar(x)
  return(substr(x, 1, n - 1))
}


length_15 <- function(x) {

  if (length(x) == 15) {
    x <- x

  } else {
    x <- rep(NA, 15)

  }

  return(x)
}


pad_NA <- function(x) {

  y <- 19 - length(x)
  if (y == 0) {
    x <- x
  } else {
    x <- append(x, rep(NA, y))
  }

  return(x)
}



