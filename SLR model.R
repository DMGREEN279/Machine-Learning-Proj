library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)
library(methods)

#running SLR using categories most likely to have effect on saleprice

lr <- lm(SalePrice ~ OverallQual + Neighborhood + GrLivArea + ExterQual, data = comptest)
summary(lr)

#Step 1 - eval metrics funct.

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Step 2 - predict/eval train data
predictions = predict(lr, newdata = ntrain)
eval_metrics(lr, train, predictions, target = 'SalePrice')

# Step 3 - predicting predict/eval test data
predictions = predict(lr, newdata = comptest)
eval_metrics(lr, test, predictions, target = 'SalePrice')