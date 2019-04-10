setwd("~/GitHub/MMSS_311_2")

a <- c(1,2,3,4,5)

Mindy <- 12

b <- c(1,2,3,4,5,6)

matrix(b,2,3,TRUE)

matrix(b,2,3)

matrix(1,10,10)

wordvec <- c("THIS", "IS", "A", "VECTOR")

sum_of_three_numbers <- function(x,y,z) {
  x+y+z
}

check <- function(x) {
  if (x<=10) {
    result <- "Yes"
  }
  else if (x>10) {
    result <- "No"
  }
  return(result)
}
check(9)