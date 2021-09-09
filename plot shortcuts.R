plots


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

#dropping N.a......a lot of them
comptest$MasVnrArea[which(is.na(comptest$MasVnrArea))] <- mean(comptest$MasVnrArea,na.rm=T)
comptest$MasVnrType1 <- as.character(comptest$MasVnrType)
comptest$MasVnrType1[which(is.na(comptest$MasVnrType))] <- "None"
comptest$MasVnrType <- as.factor(comptest$MasVnrType1)
comptest <- subset(comptest,select = -MasVnrType1)
table(comptest$MasVnrType)

