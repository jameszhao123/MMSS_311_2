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
x = NULL
for(i in 1:1000) {
  x [i] <- mean(sample(g, 10, TRUE))
}

# Q1)l Estimate a simple bivariate regression y on x and print your results. What do your results show?
# The results show that the OLS estimator of the coefficient of x is 0.02633, which is very small. This shows that there is only a weak positive correlation between y and x.
reg <- lm(y ~ x)
print(reg)







#Q2a Create an R script ﬁle that sets your working directory and loads the data. 
setwd("~/GitHub/MMSS_311_2")

pums_chicago <- read.csv("pums_chicago.csv")

#2b How many variables are there in the dataset? 
# There are 204 variables (from environment panel)

#2c What is the mean annual income, PINCP in this dataset?
PINCP_mean <- mean(pums_chicago$PINCP, na.rm = TRUE)

#2d Create a new variable in the PUMS dataframe called PINCP_LOG that is equal to the log of annual income. Were NaN values produced? Why?
#NaN values were produced because we cannot take log of 0, which is the value of some annual income observations.
pums_chicago$PINCP_LOG <- log(pums_chicago$PINCP)

#2e Create a new variable GRAD.DUMMY that takes the value “grad” if the respondent has any post-high school education, and “no grad” otherwise. Use the SCHL variable. 
pums_chicago$GRAD.DUMMY <- ifelse(pums_chicago$SCHL > 17, "grad", "no grad")

#2f Drop the variable SERIALNO from the dataset.
pums_chicago$SERIALNO <- NULL

#2g Save your new dataset to a csv ﬁle in the working directory.
write.csv(pums_chicago,'editedPUMS_CHICAGO.csv')

#2h Use the variable ESR, create 5 new dataframes: under 16, employed, unemployed, in the armed forces, and not in the labor force.
under16 <- pums_chicago[pums_chicago$ESR == "NA", ]
employed <- pums_chicago[pums_chicago$ESR %in% c("1", "2"), ]
unemployed <- pums_chicago[pums_chicago$ESR %in% "3", ]
armedforces <- pums_chicago[pums_chicago$ESR %in% c("4", "5"), ]
notinlaborforce <- pums_chicago[pums_chicago$ESR %in% "6", ]

#2i Create a new dataframe that combines employed people and people in the armed forces. 
employed_af <- pums_chicago[pums_chicago$ESR %in% c("1", "2", "4", "5"), ]

#2j In your new employed_af dataframe, keep only the variables AGEP, RAC1P, and PINCP_LOG
new_employed_af <- pums_chicago[c("AGEP", "RAC1P", "PINCP_LOG")]

#2ki Find the mean, median, and 80th percentile of travel time to work, JWMNP 
summary(pums_chicago$JWMNP)
quantile(pums_chicago$JWMNP, probs=0.8, na.rm=TRUE)

#2kii Find the correlation between travel time to work JWMNP and annual wages WAGP 
cor(pums_chicago$JWMNP, pums_chicago$WAGP, use="complete.obs")

#2kiii Make a scatterplot of age and log income.
#2kiv Export this graph to your working directory in pdf format.
pdf("ageonLogincome.pdf")
plot(pums_chicago$AGEP, pums_chicago$PINCP_LOG, main="age on log income")
dev.off()

#2kv Create a crosstab of employment status ESR by race RAC1P
install.packages("gmodels", repos = "http://cran.us.r-project.org")
library("gmodels")
CrossTable(pums_chicago$ESR, pums_chicago$RAC1P)

#2kvi Estimate a linear regression of annual wages WAGP on hours worked per week WKHP 
wagp_on_wkhp <- lm(WAGP ~ WKHP, pums_chicago)

#2kvii Plot the residuals from this regression against the ﬁtted values. What does this show?
#This shows that residuals tend to decrease as the fitted values increase. Furthermore, we can observe that there are only a small number of large deviations for any given fitted value.
wagp_on_wkhp_res <- resid(wagp_on_wkhp)
wagp_on_wkhp_fitted <- fitted(wagp_on_wkhp)
plot(wagp_on_wkhp_fitted, wagp_on_wkhp_res, 
     ylab = "Residuals", xlab="Fitted",
     main = "Residuals on Fitted")

#2li Estimate a linear regression of miles per gallon on weight 
data(mtcars)
car_lm <- lm(mpg ~ wt, mtcars)

#2lii Estimate this regression separately for manual versus automatic transition 
autoData <- mtcars[mtcars$am == "0",]
manualData <- mtcars[mtcars$am == "1",]
autocar_lm <- lm(mpg ~ wt, mtcars)
manualcar_lm <- lm(mpg ~ wt, mtcars)

#2liii Estimate a regression of miles per gallon on the log of horsepower. 
mtcars$log.hp <- log(mtcars$hp)
mpg_on_lg.hp <- lm(mpg ~ log.hp, mtcars)

#2mi Make a scatterplot of weight against miles per gallon. 
#2mii Color the points in your graph according to the transmission of the vehicle. 
#2miii Change the shape of the points to correspond to the number of forward gears in the vehicle.
#2miv Change the x and y labels on the plot to make full words. 
#2mv Change the background of the plot so that the panel background is not gray.
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
ggplot(mtcars)+ geom_point(mapping = aes(x = mpg, y = wt, 
                                         color = mtcars$am, 
                                         shape = mtcars$gear)) + scale_shape_identity() + 
  labs(title = "Weight on Miles per Gallon", x = "Miles per Gallon", y = "Weight") +
  theme(panel.background = element_rect(fill = "lightyellow"))




