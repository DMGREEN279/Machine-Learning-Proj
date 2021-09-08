library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')

train <-read.csv('C:/Users/dmg27/Desktop/MLproject/train.csv', stringsAsFactors = F)
test <-read.csv('C:/Users/dmg27/Desktop/MLproject/test.csv', stringsAsFactors = F)


dim(train)


#test