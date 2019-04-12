#Q2a Create an R script ???le that sets your working directory and loads the data. 
setwd("~/GitHub/MMSS_311_2")

pums_chicago <- read.csv("pums_chicago.csv")

#2b How many variables are there in the dataset? 
# There are 204 variables

#2c What is the mean annual income, PINCP in this dataset? (why wrong?) (why can't save file?)
PINCP_mean <- mean("PINCP", na.rm = TRUE)

#2d Create a new variable in the PUMS dataframe called PINCP_LOG that is equal to the log of annual income. Were NaN values produced? Why?
#unsure!
#NaN values were produced because we cannot take log of negative numbers.

pums_chicago$PINCP_LOG <- log(pums_chicago$PINCP)

#2e Create a new variable GRAD.DUMMY that takes the value "grad" if the respondent has any post-high school education, and "no grad" otherwise. Use the SCHL variable. 
pums_chicago$GRAD.DUMMY <- ifelse(pums_chicago$SCHL > 12, "grad", "no grad")

#2f Drop the variable SERIALNO from the dataset.
pums_chicago$SERIALNO <- NULL

#2g Save your new dataset to a csv ???le in the working directory.
write.csv(pums_chicago,'editedPUMS_CHICAGO.csv')

#2h Use the variable ESR, create 5 new dataframes: under 16, employed, unemployed, in the armed forces, and not in the labor force. 
