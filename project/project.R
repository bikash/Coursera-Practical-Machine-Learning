
library(caret)
library(doMC)
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
## divide data into training and testing data. 80% training data and 20% testing data
data_part <- createDataPartition(y=train$classe, p=0.80, list=FALSE )
training <- train[data_part,]
testing <- train[-data_part,]

## preprocess
preProc<-preProcess(training[,-53])
train1<-predict(preProc,training[,-53])
test1<-predict(preProc,testing[,-53])
train1$classe <- training$classe
test1$classe <- testing$classe

## check for values near zero
nzVar <- nearZeroVar(train1, saveMetrics=TRUE)
if (any(nzVar$nzv)) nzVar else message("No variables with near zero variance")
train1 <- train1[,nzVar$nzv==FALSE]
nzVar <- nearZeroVar(test1, saveMetrics=TRUE)
if (any(nzVar$nzv)) nzVar else message("No variables with near zero variance")
test1 <- test1[,nzVar$nzv==FALSE]


## training with full data
RF<-randomForest(as.factor(training$classe) ~.,data = training[,predVar],importance = TRUE)
pred_rf<-predict(RF,testing)

CM<-confusionMatrix(pred_rf,testing$classe)
CM$overall  # Accuracy 0.994

plot(RF,main="Error vs # of trees")

## traning with pre-process data
processRF<-randomForest(as.factor(train1$classe) ~.,data = train1[,predVar],importance = TRUE)
predictRF<-predict(processRF,test1)
CM1 <- confusionMatrix(predictRF,test1$classe)
CM1$overall  # Accuracy 0.9946470

## Accuracy percentage
CM$overall[1]-CM1$overall[1] ##0.000254907 

## final model
finalRF<-randomForest(as.factor(train$classe) ~.,data = train,importance = TRUE)
finalRF.pred<-predict(finalRF,test)
finalRF.pred


##Submission to Coursera
## Write submission files to assignment folder.


pml_write_files = function(x){
  n = length(x)
  path <- "assignment/"
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=file.path(path, filename),quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files( as.character(finalRF.pred))



## convert project rmd file to html file 
# require(knitr) # required for knitting from rmd to md
# require(markdown) # required for md to html 
# knitr::knit2html('project.Rmd')

