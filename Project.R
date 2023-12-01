#DS 6371 Final Project

library(dplyr)
library(ggplot2)

#Reading the files
testData <- read.csv("C:/Users/owola/Documents/MY_COURSES/FALL_2023/DS_6371_Stats_Foundations/Project/house-prices-advanced-regression-techniques/test.csv")
testData

trainData <- read.csv("C:/Users/owola/Documents/MY_COURSES/FALL_2023/DS_6371_Stats_Foundations/Project/house-prices-advanced-regression-techniques/train.csv")
trainData

#Simple linear regression: The explanatory variable is the GrLivArea
#Plotting the graph of Sales price and living area, x = living area, y = sales price

#plotting the linear-linear plot
trainData %>% 
  ggplot(aes(x = GrLivArea, y = SalePrice)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=.95, color="blue") +
  ggtitle("Living Area vs. Sales Price")

#plotting the log-linear plot
trainData %>% 
ggplot(aes(x = GrLivArea, y = log(SalePrice))) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=.95, color="blue") +
  ggtitle("Living Area vs. log of Sales Price")

#plotting the linear-log plot
trainData %>% 
  ggplot(aes(x = log(GrLivArea), y = SalePrice)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=.95, color="blue") +
  ggtitle("log of Living Area vs. Sales Price")

#plotting the log-log plot
trainData %>% 
  ggplot(aes(x = log(GrLivArea), y = log(SalePrice))) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=.95, color="blue") +
  ggtitle("log of Living Area vs. log of Sales Price")

#Looking at each transform plot, we will go ahead with the log-log plot

#Getting the p-value
fit = lm(log(SalePrice) ~ log(GrLivArea), data = trainData)
summary(fit)

#Creating the multiple linear regression
#SalePrice~GrLivArea + FullBath

#Getting the models
model <- lm(SalePrice ~ GrLivArea + FullBath, data = trainData)
trainData$Predicted <- predict(model)

#plotting the linear-linear plot
trainData %>% 
  ggplot(aes(x = Predicted, y = SalePrice)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=.95, color="blue") +
  ggtitle("Living Area vs. Sales Price")