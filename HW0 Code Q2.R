#Q2a Create an R script ???le that sets your working directory and loads the data. 
setwd("~/GitHub/MMSS_311_2")

pums_chicago <- read.csv("pums_chicago.csv")

#2b How many variables are there in the dataset? 
# There are 204 variables

#2c What is the mean annual income, PINCP in this dataset? (why wrong?) (why can't save file?)
PINCP_mean <- mean("PINCP", na.rm = TRUE)

#2d (d) Create a new variable in the PUMS dataframe called PINCP_LOG that is equal to the log of annual income. Were NaN values produced? Why?

