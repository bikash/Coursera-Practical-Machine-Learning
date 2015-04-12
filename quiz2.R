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
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_variables <- grep("^IL", names(training), value = TRUE)
preProc <- preProcess(training[, IL_variables], method = "pca", thresh = 0.9)
preProc

### Answer
## 9

## Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


set.seed(3433)

## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)

## make a subset of these predictors
predictors_IL <- predictors[, IL_str]

# create a new DF of predictors and diagnosis
df <- data.frame(diagnosis, predictors_IL)

# create a training and testing set from this DF
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]

## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)


predictions <- predict(modelFit, newdata = testing)

## get the confusion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)


A1 <- C1$overall[1]

## do similar steps with PCA
modelFit <- train(training$diagnosis ~ ., method = "glm", data = training,
                  preProcess = "pca", 
                  Control = trainControl(preProcOptions = list(thresh = 0.8)))

C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)


