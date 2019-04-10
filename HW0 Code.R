setwd("~/GitHub/MMSS_311_2")

# Q1a) A vector with the numbers 1-5 in order 
a <- c(1,2,3,4,5)

# Q1b) A scalar named Mindy that takes the value 12
Mindy <- 12

# Q1c) A 2×3 matrix with the numbers 1-6 in order by rows 
b <- c(1,2,3,4,5,6)
matrix(b,2,3,TRUE)

# Q1d) A 2×3 matrix with the numbers 1-6 in order by columns 
matrix(b,2,3)

# Q1e) A 10×10 matrix of 1's 
matrix(1,10,10)

# Q1f)  A vector consisting of the words THIS, IS, A, VECTOR (each word a separate element) 
wordvec <- c("THIS", "IS", "A", "VECTOR")

# Q1g) A function that takes the sum of any three numbers 
sum_of_three_numbers <- function(x,y,z) {
  x+y+z
}

# Q1h) A function that takes one number as input, returns "Yes" if the number is less than or equal to 10 and "No" if the number is greater than 10
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

# Q1i) Generate synthetic data by taking 1,000 draws from a normal distribution with a mean of 10 and a standard deviation of 1. Save these data to an object g.
g <- rnorm(1000,10,1)

# Q1j) Create a separate object called y with 1,000 draws from a normal distribution with a mean of 5 and a standard deviation of 0.5. 
y <- rnorm(1000,5,0.5)

# Q1k)Generate a variable x with 1,000 values, where each value is a mean of 10 samples from g, with replacement. (Hint: use a for loop)

# Q1 Estimate a simple bivariate regression y ??? x and print your results. What do your results show?
reg <- lm(y~x)
print(reg)
