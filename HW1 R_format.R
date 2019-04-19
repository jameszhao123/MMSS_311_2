
setwd("~/GitHub/MMSS_311_2")
sick <- read.csv("sick_data.csv")
sick$RESULT.DUMMY <- ifelse (sick$result == "Positive", "1", "0")
OLS <- lm(RESULT.DUMMY~temp+bp, data = sick)
summary(OLS)
sick$PREDICTED.VALUE <- fitted(OLS)
sick$PREDICTED.OUTCOME <- ifelse(sick$PREDICTED.VALUE >= 0.5, "Positive", "Negative")
#need to figure out how to calculate accuracy
sick$PREDICTION.ACCURACY <- ifelse(sick$PREDICTED.VALUE = sick$result, "1", "0")

#Q1c The equation of the line is -5.7134563 + 0.00628185temp - 0.0082865bp = 0

#Q1d Display the blood pressure and temperature data on a single scatterplot, using either color or shape to distinguish between positive and negative results. Add the line you calculated from the previous step. 
library(ggplot2) 
plot <- ggplot(sick, aes(bp, temp)) 
plot + geom_point()
plot + geom_point(aes(colour = factor(result)))
#how to add equation of line?
#need to run this again for logit

#Q2 RIDGE
widget <- read.csv("widget_data.csv")
plot (widget$y)
install.packages("tidyverse")
install.packages("broom")
install.packages("glmnet")
library(tidyverse)
library(broom)
library(glmnet)

x <- model.matrix(y~., widget)[,-1]

grid = seq(1/100, 100, length = 31)
ridge_mod = glmnet(x, widget$y, alpha = 0, lambda = grid)

#Q2 LASSO
x <- model.matrix(y~., widget)[,-1]

grid = seq(1/100, 100, length = 31)
ridge_mod = glmnet(x, widget$y, alpha = 1, lambda = grid)
