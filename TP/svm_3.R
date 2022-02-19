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

teens <- sum((y=="teens")*1)
twenties <- sum((y=="twenties")*1)
thirties<- sum((y=="thirties")*1)
fourties<- sum((y=="fourties")*1)
fifties<- sum((y=="fifties")*1)

# N <- 15
# trans <- prcomp(x)
# PC <- predict(trans, x)
# x <- PC[,1:N]

folds = createFolds(y, k = 10)

cv = lapply(folds, function(z) {
  x_train <- x[-z, ]
  y_train <- y[-z]
  x_test <- x[z, ]
  y_test <- y[z]
  
  # Grid search
  df <- data.frame(x_train, y_train)
  df$y_train <- as.factor(df$y_train)
  task <- makeClassifTask(data = df, target = "y_train")
  discrete_ps = num_ps = makeParamSet(
    makeNumericParam("C", lower = -10, upper = 10,
                     trafo = function(x) 10^x),
    makeNumericParam("sigma", lower = -10, upper = 10,
                     trafo = function(x) 10^x)
  )
  ctrl = makeTuneControlRandom(maxit = 100L)
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = task,
                   resampling = rdesc,
                   par.set = num_ps,
                   control = ctrl,
                   measures = list(acc, setAggregation(acc, test.sd)),
                   show.info = FALSE)
  C <- res$x$C
  sigma <- res$x$sigma
  print(C, sigma)
  # Treinar a SVM
  svm <- ksvm(x_train,y_train,type='C-bsvc',kernel='rbfdot',
              kpar=list(sigma=sigma),C=C)
  y_pred <- predict(svm, x_test)
  comp <- y_test == y_pred
  success <- comp[comp == TRUE]
  accuracy <- length(success)/length(y_test) 
  return(accuracy)
})

# accuracy <- t(as.data.frame(cv))
# fold1 = cv[[1]]
# y_pred <- as.matrix(fold1[[2]])
# y_test <- as.matrix(fold1[[1]])
# r <- cbind(y_test, y_pred, y_test==y_pred)

