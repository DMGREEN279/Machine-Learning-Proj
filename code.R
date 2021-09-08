library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')
library('gridBase')
library('gridExtra')
train <-read.csv('C:/Users/dmg27/Desktop/MLproject/train.csv', stringsAsFactors = F)
test <-read.csv('C:/Users/dmg27/Desktop/MLproject/test.csv', stringsAsFactors = F)


dim(train)


str(train)

dim(test)
str(test)

#how many cols are text data

sum(sapply(train[,1:81], typeof) == "character")

# how many cols are numerical data

sum(sapply(train[,1:81], typeof) == "integer")

summary(train[,sapply(train[,1:81], typeof) == "num"])

test$SalePrice<-rep(NA,1459)
comptest<-bind_rows(train,test)

## lets see what it looks like

str(comptest)

summary(comptest)

#make training set for numerical 
Train_numeric <- names(train)[which(sapply(train, is.numeric))]

#make training set for categorical
Train_char <- names(train)[which(sapply(train, is.character))]
Traincols <- c(Train_char, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')

#data set for numeric
numtrain<-train[Train_numeric]


#data set for categories
cattrain<-train[Train_char]

#visualize
fourbar(cattrain, fun = plotHist, ii = 1:4, ncol = 2)


