rm(list = ls())
library(readr)
library(tidyverse) 
library(caret)
library(kernlab)
library(e1071)
library(FSDAM)
library(reticulate)
library(pcaMethods)
data <- read_csv("treino.csv")
y <- data[[41]]
x <- as.matrix(data[2:40])
data <- cbind(x,y)

x <- pca(x, nPcs=39, method="nlpca", maxSteps=50)
aux <- x@completeObs
aux1 <- x
x <- x@completeObs
folds = createFolds(y, k = 10)

cv = lapply(folds, function(z) {
  x_train <- x[-z, ]
  y_train <- y[-z]
  x_test <- x[z, ]
  y_test <- y[z]
  classifier <- svm(x =x_train,
                   y=y_train,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred <- predict(classifier, newdata =x_test)
  comp <- y_test == y_pred
  success <- comp[comp == TRUE]
  accuracy <- length(success)/length(y_test) 
  return(accuracy)
})

accuracy = mean(as.numeric(cv))

