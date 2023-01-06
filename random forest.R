#Data Preprocessing
setwd("C:/Users/cumhu/OneDrive/Desktop/course materials/kaggle/house competition")
library(dplyr)
library(caTools)
library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(leaps)

##Importing the training dataset
dataset = read.csv('train.csv')

#Encoding Categorical Data
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.integer(factor(x, levels = order, exclude = NULL))
  x
}

char <-  c(3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32, 33, 34, 36, 40, 41, 42, 43, 54, 56, 58, 59, 61, 64, 65, 66, 73, 74, 75, 79, 80)
int <- c(1, 2, 4, 5, 18, 19, 20, 21, 27, 35, 37, 38, 39, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 55, 57, 60, 62, 63, 67, 68, 69, 70, 71, 72, 76, 77, 78)

for(x in char){
  dataset[, x] <- encode_ordinal(dataset[, x])
}

for(x in int){
  dataset[is.na(dataset[,x]), x] <- as.integer(mean(dataset[,x], na.rm = TRUE))
}

dataset <- dataset[, -1]

#Splitting the dataset into training set and test set
set.seed(211251341)
split = sample.split(dataset$SalePrice, SplitRatio = 0.8)
training = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

#Feature scaling
#training = as.data.frame(scale(training))
#test = as.data.frame(scale(test))

#Fitting MLR to the training set
regressor = randomForest(x = training[, -80],
                         y = training$SalePrice,
                         ntree = 1000)
summary(regressor)

#Predicting the test set results
y_pred = predict(regressor, newdata = test)

# Visualising the Regression Model results (for higher resolution and smoother curve)
library(ggplot2)
ggplot() +
  geom_point(aes(x = c(1:202), y = test$SalePrice),
             colour = 'red') +
  geom_line(aes(x = c(1:202), y = predict(regressor, newdata = test)),
            colour = 'blue') +
  ggtitle('House Price Prediction') +
  xlab('ID') +
  ylab('Price')

#Making Final Prediction
prediction_dataset <- read.csv('test.csv')

for(x in char){
  prediction_dataset[, x] <- encode_ordinal(prediction_dataset[, x])
}

for(x in int){
  prediction_dataset[is.na(prediction_dataset[,x]), x] <- as.integer(mean(prediction_dataset[,x], na.rm = TRUE))
}

prediction_dataset <- prediction_dataset[, -1]

SalePrice = predict(regressor, newdata = prediction_dataset)
ID = c(1461:2919)
pred = data.frame(ID, SalePrice)

#Exporting results to csv file
write.csv(pred, "C:\\Users\\cumhu\\OneDrive\\Desktop\\course materials\\kaggle\\house competition\\random forest.csv", row.names=F)



