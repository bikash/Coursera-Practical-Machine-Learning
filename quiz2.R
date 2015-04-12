## Quiz 2


## Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

## Answer 
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

## Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each 
## of the variables in the data set (you may find the cut2() function in the Hmisc package useful 
## for turning continuous covariates into factors). What do you notice in these plots?
library(Hmisc)
library(plyr)
splitOn <- cut2(training$Age, g = 4)

splitOn <- mapvalues(splitOn, 
                     from = levels(factor(splitOn)), 
                     to = c("red", "blue", "yellow", "green"))


# automatically includes index of samples
plot(training$CompressiveStrength, col = splitOn)

# Answer: There is a step-like pattern in the plot of outcome versus index in 
# the training set that isn't explained by any of the predictor variables so 
# there may be a variable missing.

## Q 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

### Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?

qplot(Superplasticizer, data=training) # OR
ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()

### The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer

## Answer
## There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) 
# they would still all be identical so the distribution would not be symmetric.


## Question 4





