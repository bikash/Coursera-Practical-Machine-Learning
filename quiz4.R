## Quiz 4

# Question 1
# Load the vowel.train and vowel.test data sets:
#   library(ElemStatLearn)
# data(vowel.train)
# data(vowel.test) 
# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. 
# Fit these both with the train() command in the caret package. 
# 
# What are the accuracies for the two approaches on the test data set? What is the accuracy among the test set samples where the two methods agree?

library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           verboseIter=TRUE)

## random Forest
fitRf <- train(y ~ ., data=vowel.train, method="rf")
## Gradient boosting
fitGBM <- train(y ~ x.1, data=vowel.train, method="gbm", trControl = fitControl)

predRf <- predict(fitRf, vowel.test)
predGBM <- predict(fitGBM, vowel.test)

# RF Accuracy: 0.6038961
confusionMatrix(predRf, vowel.test$y)$overall[1]

# GBM Accuracy: 0.530303
confusionMatrix(predGBM, vowel.test$y)$overall[1]

pred <- data.frame(predRf, predGBM, y=vowel.test$y, agree=predRf == predGBM)
head(pred)
accuracy <- sum(predRf[pred$agree] == pred$y[pred$agree]) / sum(pred$agree)
accuracy # Agreement Accuracy: 0.6569579


### Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)


## random Forest
fitRf <- train(diagnosis ~ ., data=training, method="rf")
## gradient boosting machine
fitgbm <- train(diagnosis ~ ., data=training, method="gbm")
## linear discrimental analysis
fitlda <- train(diagnosis ~ ., data=training, method="lda")

## Prediction
predRf <- predict(fitRf, testing)
predGBM <- predict(fitgbm, testing)
predLDA <- predict(fitlda, testing)

## combine
df_combined <- data.frame(predRf, predGBM, predLDA, diagnosis = testing$diagnosis) # training$diagnosis?

fit_combined <- train(diagnosis ~ ., data = df_combined, method = "rf")
predict_final <- predict(fit_combined, newdata = testing)

# confusion matrixes
c1 <- confusionMatrix(predRf, testing$diagnosis) ## 0.7926829 
c2 <- confusionMatrix(predGBM, testing$diagnosis)
c3 <- confusionMatrix(predLDA, testing$diagnosis) ## 0.7682927 
c4 <- confusionMatrix(predict_final, testing$diagnosis)

print(paste(c1$overall[1], c2$overall[1], c3$overall[1], c4$overall[1]))
# Stacked Accuracy: 0.79 is better than random forests and lda 
# and the same as boosting.


## Quesiton 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
# Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).
set.seed(233)
fit <- train(CompressiveStrength ~ ., data=training, method="lasso")
fit

plot.enet(fit$finalModel, xvar="penalty", use.color=T) 
# Cement


# Question 4
## Load the data on the number of visitors to the instructors blog from here: 
# https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv



