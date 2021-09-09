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
library('rlang')
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

fourbar(cattrain, fun = plotHist, ii = 5:8, ncol = 2)

fourbar(cattrain, fun = plotHist, ii = 9:12, ncol = 2)

fourbar(cattrain, fun = plotHist, ii = 13:18, ncol = 2)

fourbar(cattrain, fun = plotHist, ii = 19:22, ncol = 2)

#boxplots for salesprices
ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='blue', linetype='solid', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

#plots for numerical
fourbar(numtrain, fun = plotDen, ii = 2:6, ncol = 2)

fourbar(numtrain, fun = plotDen, ii = 7:12, ncol = 2)

fourbar(numtrain, fun = plotDen, ii = 13:17, ncol = 2)

fourbar(numtrain, fun = plotHist, ii = 18:23, ncol = 2)

row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

summary(train$SalePrice)
quantile(train$SalePrice)

#check skew 
hist(train$SalePrice)

#check outliers 
ggplot(train,aes(y=SalePrice,x=GrLivArea))+geom_point()
summary(train$GrLivArea)

#trim outliers
train <- train[train$GrLivArea<=4000,]

#checking N.A's 

colSums(sapply(train, is.na))

#missing values 
sapply(comptest[,1:80], function(x) sum(is.na(x)))

Colclass <- sapply(names(comptest),function(x){class(comptest[[x]])})
numcol <-names(Colclass[Colclass != "factor"])


#skew for vars.

skew <- sapply(numcol,function(x){skewness(comptest[[x]],na.rm = T)})

#threshold for skew.

skew <- skew[skew > 0.75]

# log(x + 1) xform for skew

for(x in names(skew)) 
{
  comptest[[x]] <- log(comptest[[x]] + 1)
}

#training the model 

train <- comptest[comptest$isTrain==1,]
test <- comptest[comptest$isTrain==0,]
samplesize <- floor(0.75 * nrow(train))

## setting the seed to make the partition reproducible

set.seed(279)
indtrain <- sample(seq_len(nrow(train)), size = samplesize)

ntrain <- train[indtrain, ]
vald <- train[-indtrain, ]
ntrain <- subset(ntrain,select=-c(Id,isTrain))
vald <- subset(vald,select=-c(Id,isTrain))
nrow(ntrain)


nrow(vald)

str(vald)

library(randomForest)

forestmodel <- randomForest(SalePrice~.,
                            data = ntrain)

imp <- importance(forestmodel)
varImpPlot(forestmodel)

bestguess <- predict(forestmodel,test)


RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

a_RMSE <- RMSE(bestguess, vald$SalePrice)
a_RMSE

a_RMSE <- round(a_RMSE, digits = 5)
