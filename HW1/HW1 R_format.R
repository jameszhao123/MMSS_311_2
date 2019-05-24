
setwd("~/GitHub/MMSS_311_2")

#Q1 OLS
sick <- read.csv("sick_data.csv")
sick$RESULT.DUMMY <- ifelse (sick$result == "Positive", 1, 0)
OLS <- lm(RESULT.DUMMY~temp+bp, data = sick)
summary(OLS)
sick$PREDICTED.VALUE <- fitted(OLS)
sick$PREDICTED.OUTCOME <- ifelse(sick$PREDICTED.VALUE >= 0.5, "Positive", "Negative")
sick$PREDICTED.ACCURACY <- ifelse(sick$PREDICTED.OUTCOME == sick$result, 1, 0)
accuracy.ols <- mean(sick$PREDICTED.ACCURACY)
accuracy.ols
#The OLS regression correctly predicts the results 96.4% of the time.

#Q1c OLS: The equation of the line is -5.7134563 + 0.00628185temp - 0.0082865bp = 0

library(ggplot2) 
ggplot(sick, aes(temp, bp)) + 
  geom_point()+
  geom_point(aes(colour = factor(result)))+
  geom_abline(intercept = -689.1506, slope = 7.580824232)

#Q1 Logit
logit <- glm(RESULT.DUMMY ~ temp+bp, data = sick, family = binomial)
summary(logit)
sick$PREDICTED.VALUE.LOGIT <- fitted(logit)
sick$PREDICTED.OUTCOME.LOGIT <- ifelse(sick$PREDICTED.VALUE.LOGIT >= 0.5, "Positive", "Negative")
sick$PREDICTED.ACCURACY.LOGIT <- ifelse(sick$PREDICTED.OUTCOME.LOGIT == sick$result, 1, 0)
accuracy.logit <- mean(sick$PREDICTED.ACCURACY.LOGIT)
accuracy.logit
#The Logit regression correctly predicts the results 99.2% of the time.

#Q1c Logit: The equation of the line is bp = 6.612235temp - 571.0099.

library(ggplot2) 
ggplot(sick, aes(temp, bp)) + 
  geom_point()+
  geom_point(aes(colour = factor(result)))+
  geom_abline(intercept = -571.0099, slope = 6.612235)


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
useful_ridge_mod <- tidy (ridge_mod)
ggplot(useful_ridge_mod, aes(lambda, estimate)) + geom_line()
cv_ridge_mod <- cv.glmnet(x, widget$y, alpha = 0)$lambda.min
cv_ridge_mod
lambda_min_ridge_mod = glmnet(x, widget$y, alpha = 0, lambda = cv_ridge_mod)
summary (lambda_min_ridge_mod)
#how to find coefficients i get when using this lambda?

#Q2 LASSO
x <- model.matrix(y~., widget)[,-1]

grid = seq(1/100, 100, length = 31)
lasso_mod = glmnet(x, widget$y, alpha = 1, lambda = grid)
useful_lasso_mod <- tidy (lasso_mod)
ggplot(useful_lasso_mod, aes(lambda, estimate)) + geom_line()
cv_lasso_mod <- cv.glmnet(x, widget$y, alpha = 1)$lambda.min
cv_lasso_mod
lambda_min_lasso_mod = glmnet(x, widget$y, alpha = 1, lambda = cv_lasso_mod)
summary(lambda_min_lasso_mod)
#how to find coefficients i get when using this lambda?

#Q3 SVM
pol <- read.csv("pol_data.csv")
install.packages("caret")
install.packages("e1071")
library("caret")
library(e1071)

set.seed(1)
split=2/3
trainIndex <- createDataPartition(pol$group, p=split, list=FALSE)
train <- pol[ trainIndex,]
test <- pol[-trainIndex,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svmfit=svm(group~., data=pol , kernel ="linear", cost=10, scale=FALSE)
summary(svmfit)


#Q3 NB

NBclassfier=naiveBayes(group~pol_margin+col_degree+house_income, data=train)
print(NBclassfier)

printALL=function(model){
  trainPred=predict(model, newdata = train, type = "class")
  trainTable=table(train$group, trainPred)
  testPred=predict(NBclassfier, newdata=test, type="class")
  testTable=table(test$group, testPred)
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NBclassfier)

