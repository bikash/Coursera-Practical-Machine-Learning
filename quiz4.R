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


