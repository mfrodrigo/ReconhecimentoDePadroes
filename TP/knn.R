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

knn.cross <- tune.knn(x= x,
                      y= y,
                      k = 1:5, 
                      tunecontrol=tune.control(sampling = "cross"),
                      cross=10)



