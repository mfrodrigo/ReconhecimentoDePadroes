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

N <- 2
trans <- prcomp(x)
PC <- predict(trans, x)
x <- PC[,1:N]
data <- data.frame(data)

m = tune.svm(y~., data = data,
             tunecontrol=tune.control(cross=10),
             gamma = 2^(-3:-1), cost = 2^(1:2))


