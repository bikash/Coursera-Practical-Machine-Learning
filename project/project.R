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
train <- read.csv('pml-training.csv',header=TRUE,stringsAsFactors = F)
test <- read.csv('pml-testing.csv',header=TRUE,stringsAsFactors = F)


