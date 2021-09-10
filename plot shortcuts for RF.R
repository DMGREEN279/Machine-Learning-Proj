######plots


## Density with bar

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Density plot function

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

fourbar <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


## categorical

fourbar(cattrain, fun = plotHist, ii = 1:4, ncol = 2)


#density
denplts <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}
#histogram

hstgr <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

#corr function
correlations <- cor(na.omit(numtrain[,-1]))

indi <- sapply(train,function(x) sum(is.na(x)))
missum <- data.frame(index = names(train), Mval=indi)
missum[missum$Missing_Values > 0,]

#combine
test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
comptest <- rbind(train,test)

#pruning
comptest$Alley1 <- as.character(comptest$Alley)
comptest$Alley1[which(is.na(comptest$Alley))] <- "None"
table(comptest$Alley1)

#as factor
comptest$Alley <- as.factor(comptest$Alley1)
comptest <- subset(comptest,select = -Alley1)

#fix lot frontage data gaps
comptest$LotFrontage[which(is.na(comptest$LotFrontage))] <- median(comptest$LotFrontage,na.rm = T)


#dropping N.a......a lot of them

comptest$GarageYrBlt[which(is.na(comptest$GarageYrBlt))] <- 0 

comptest$MasVnrArea[which(is.na(comptest$MasVnrArea))] <- mean(comptest$MasVnrArea,na.rm=T)
comptest$MasVnrType1 <- as.character(comptest$MasVnrType)
comptest$MasVnrType1[which(is.na(comptest$MasVnrType))] <- "None"
comptest$MasVnrType <- as.factor(comptest$MasVnrType1)
comptest <- subset(comptest,select = -MasVnrType1)
table(comptest$MasVnrType)

comptest$FireplaceQu1 <- as.character(comptest$FireplaceQu)
comptest$FireplaceQu1[which(is.na(comptest$FireplaceQu))] <- "None"
comptest$FireplaceQu <- as.factor(comptest$FireplaceQu1)
comptest <- subset(comptest,select = -FireplaceQu1)

comptest$PoolQC1 <- as.character(comptest$PoolQC)
comptest$PoolQC1[which(is.na(comptest$PoolQC))] <- "None"
comptest$PoolQC <- as.factor(comptest$PoolQC1)
comptest <- subset(comptest,select = -PoolQC1)

comptest$Fence1 <- as.character(comptest$Fence)
comptest$Fence1[which(is.na(comptest$Fence))] <- "None"
comptest$Fence <- as.factor(comptest$Fence1)
comptest <- subset(comptest,select = -Fence1)

comptest$MiscFeature1 <- as.character(comptest$MiscFeature)
comptest$MiscFeature1[which(is.na(comptest$MiscFeature))] <- "None"
comptest$MiscFeature <- as.factor(comptest$MiscFeature1)
comptest <- subset(comptest,select = -MiscFeature1)

comptest$GarageQual1 <- as.character(comptest$GarageQual)
comptest$GarageQual1[which(is.na(comptest$GarageQual))] <- "None"
comptest$GarageQual <- as.factor(comptest$GarageQual1)
comptest <- subset(comptest,select = -GarageQual1)

comptest$GarageType1 <- as.character(comptest$GarageType)
comptest$GarageType1[which(is.na(comptest$GarageType))] <- "None"
comptest$GarageType <- as.factor(comptest$GarageType1)
comptest <- subset(comptest,select = -GarageType1)

comptest$GarageFinish1 <- as.character(comptest$GarageFinish)
comptest$GarageFinish1[which(is.na(comptest$GarageFinish))] <- "None"
comptest$GarageFinish <- as.factor(comptest$GarageFinish1)
comptest <- subset(comptest,select = -GarageFinish1)

comptest$GarageCond1 <- as.character(comptest$GarageCond)
comptest$GarageCond1[which(is.na(comptest$GarageCond))] <- "None"
comptest$GarageCond <- as.factor(comptest$GarageCond1)
comptest <- subset(comptest,select = -GarageCond1)

comptest$BsmtQual1 <- as.character(comptest$BsmtQual)
comptest$BsmtQual1[which(is.na(comptest$BsmtQual))] <- "None"
comptest$BsmtQual <- as.factor(comptest$BsmtQual1)
comptest <- subset(comptest,select = -BsmtQual1)

comptest$BsmtCond1 <- as.character(comptest$BsmtCond)
comptest$BsmtCond1[which(is.na(comptest$BsmtCond))] <- "None"
comptest$BsmtCond <- as.factor(comptest$BsmtCond1)
comptest <- subset(comptest,select = -BsmtCond1)

comptest$BsmtExposure1 <- as.character(comptest$BsmtExposure)
comptest$BsmtExposure1[which(is.na(comptest$BsmtExposure))] <- "None"
comptest$BsmtExposure <- as.factor(comptest$BsmtExposure1)
comptest <- subset(comptest,select = -BsmtExposure1)

comptest$BsmtFinType11 <- as.character(comptest$BsmtFinType1)
comptest$BsmtFinType11[which(is.na(comptest$BsmtFinType1))] <- "None"
comptest$BsmtFinType1 <- as.factor(comptest$BsmtFinType11)
comptest <- subset(comptest,select = -BsmtFinType11)

comptest$BsmtFinType21 <- as.character(comptest$BsmtFinType2)
comptest$BsmtFinType21[which(is.na(comptest$BsmtFinType2))] <- "None"
comptest$BsmtFinType2 <- as.factor(comptest$BsmtFinType21)
comptest <- subset(comptest,select = -BsmtFinType21)

comptest$Electrical1 <- as.character(comptest$Electrical)
comptest$Electrical1[which(is.na(comptest$Electrical))] <- "None"
comptest$Electrical <- as.factor(comptest$Electrical1)
comptest <- subset(comptest,select = -Electrical1)

#factoring 

comptest$MSZoning<- factor(comptest$MSZoning)
comptest$Street <- factor(comptest$Street)
comptest$LotShape <-factor(comptest$LotShape )
comptest$LandContour<-factor(comptest$LandContour)
comptest$Utilities<-factor(comptest$Utilities)
comptest$LotConfig<-factor(comptest$LotConfig)
comptest$LandSlope<-factor(comptest$LandSlope)
comptest$Neighborhood<-factor(comptest$Neighborhood)
comptest$Condition1<-factor(comptest$Condition1)
comptest$Condition2<-factor(comptest$Condition2)
comptest$BldgType<-factor(comptest$BldgType)
comptest$HouseStyle<-factor(comptest$HouseStyle)
comptest$RoofStyle<-factor(comptest$RoofStyle)
comptest$RoofMatl<-factor(comptest$RoofMatl)
comptest$Exterior1st<-factor(comptest$Exterior1st)
comptest$Exterior2nd<-factor(comptest$Exterior2nd)
comptest$ExterQual<-factor(comptest$ExterQual)
comptest$ExterCond<-factor(comptest$ExterCond)
comptest$Foundation<-factor(comptest$Foundation)
comptest$Heating<-factor(comptest$Heating)
comptest$HeatingQC<-factor(comptest$HeatingQC)
comptest$CentralAir<-factor(comptest$CentralAir)
comptest$KitchenQual<-factor(comptest$KitchenQual)
comptest$Functional<-factor(comptest$Functional)
comptest$PavedDrive<-factor(comptest$PavedDrive)
comptest$SaleType<-factor(comptest$SaleType)
comptest$SaleCondition<-factor(comptest$SaleCondition)
str(comptest)


#bestguess[which(is.na(bestguess))] <- mean(bestguess,na.rm=T)
#submit <- data.frame(Id=test$Id,SalePrice=bestguess)
#write.csv(submit,file="MachineLearningProjFinalTest.csv",row.names=F)