rm(list = ls())
library(caret)
library(kernlab)
library(e1071)
library(readr)
data <- read_csv("treino.csv")
y <- data[[41]]
x <- as.matrix(data[2:40])

N <- 5
trans <- prcomp(x)
PC <- predict(trans, x)
x <- PC[,1:N]

data <- cbind(x,y)
data <- as.data.frame(data)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf_random <- train(y ~ .,
                   data = data,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
