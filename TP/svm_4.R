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
data <- data.frame(data)

svm_tune <- tune(svm, y~.,
                 data = data,
                 kernel  = "linear",            
                 type    = "C-classification",    
                 ranges  = list(
                cost  = 10^(-1:6), 
                 gamma =  1^(-1:1)
                 )
)