#An Apriori Project: Market Basket Optimization

#Apriori Steps:
##1. Set a minimum support and confidence.
##2. Take all subsets in transactions having higher support than minimum support.
##3. Take all the rules of these subsets having higher confidence than minimum confidence.
##4. Sort the rules by decreasing lift.

#Dataset:
#https://www.kaggle.com/datasets/rupakroy/market-basket-optimization

#Data Preprocessing
#install.packages("arules")
library(arules)
setwd("C:/Users/cumhu/OneDrive/Desktop/course materials/kaggle")
dataset = read.transactions("Market_Basket_Optimisation.csv", sep = ",", rm.duplicates = T)
summary(dataset)

#Training Apriori
rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))

#Visualising the results
inspect(sort(rules, by = "lift")[1:20])
