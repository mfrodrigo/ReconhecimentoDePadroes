rm(list = ls())
library(readr)
library(tidyverse) 
library(caret)
library(kernlab)
library(e1071)
data <- read_csv("treino.csv")
y <- data[[41]]
x <- as.matrix(data[2:40])
data <- cbind(x,y)

# N <- 30
# trans <- prcomp(x)
# PC <- predict(trans, x)
# x <- PC[,1:N]

folds = createFolds(y, k = 10)
cv = lapply(folds, function(z) {
  x_train <- x[-z, ]
  y_train <- y[-z]
  x_test <- x[z, ]
  y_test <- y[z]
  classifier <- svm(x =x_train,
                   y=y_train,
                   type = 'nu-classification',
                   kernel = 'radial')
  y_pred <- predict(classifier, newdata =x_test)
  comp <- y_test == y_pred
  success <- comp[comp == TRUE]
  accuracy <- length(success)/length(y_test) 
  return(accuracy)
})

accuracy = mean(as.numeric(cv))

