library(data.table)
library(caret)
library(ggplot2)
library(doMC)
library(knitr)
library(xtable)
library(randomForest)

registerDoMC(cores = 4)
## point to the project folder
setwd("/Users/bikash/repos/Coursera-Practical-Machine-Learning/project/")

print("Loading Data...")
train <- read.csv('pml-training.csv',header=TRUE,stringsAsFactors = F,na.strings=c('NA','','#DIV/0!'))
test <- read.csv('pml-testing.csv',header=TRUE,stringsAsFactors = F,na.strings=c('NA','','#DIV/0!'))

## drop first 7 column
train<-train[,-seq(1:7)]
test<-test[,-seq(1:7)]

## Cleaning the data: dropping NA value
NA_d <- apply(train,2,function(x) {sum(is.na(x))}) 
train <- train[,which(NA_d == 0)]
NA_d <- apply(test,2,function(x) {sum(is.na(x))}) 
test <- test[,which(NA_d == 0)]


## Variable
VartoInclude <- names(train)
predVar <- predVar[1:52]

set.seed(1234)
## preprocess
class <- which(lapply(train, class) %in% "numeric")

preProc<-preProcess(train[,class])
train1<-predict(preProc,train[,class])
test1<-predict(preProc,test)

## check for values near zero
nzVar <- nearZeroVar(train1, saveMetrics=TRUE)
if (any(nzVar$nzv)) nzVar else message("No variables with near zero variance")
train1 <- train1[,nzVar$nzv==FALSE]
nzVar <- nearZeroVar(test1, saveMetrics=TRUE)
if (any(nzVar$nzv)) nzVar else message("No variables with near zero variance")
test1 <- test1[,nzVar$nzv==FALSE]

## divide data into training and testing data. 80% training data and 20% testing data
data_part <- createDataPartition(y=train$classe, p=0.80, list=FALSE )
training <- data[data_part,]
testing <- data[-data_part,]



RF<-randomForest(train$classe ~.,data = train[,predVar],importance = TRUE)
pred_rf<-predict(RF,test1)

CM<-confusionMatrix(RF,test$classe)
CM$overall
