## Cleanup and Data Import
rm(list = ls())
library(RnavGraphImageData)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)
library(data.table)
library(gridExtra)
data( faces )
faces <- t( faces )

#Generating Labels (Code by Profs. Frederico C. and Antonio B.)
y <- NULL
for(i in 1:nrow(faces) )
  {
    y <- c( y, ((i-1) %/% 10) + 1 )
}

# Naming Attributes (Code by Profs. Frederico C. and Antonio B.)
nomeColunas <- NULL
for(i in 1:ncol(faces) )
  {
  nomeColunas <- c(nomeColunas, paste("a", as.character(i), sep=".") )
  }
colnames(faces) <- nomeColunas
rownames(faces) <- NULL

#Normalize Data
facesMean <- mean(faces)
facesNorm <- faces - facesMean

#Covariance Matrix
facesCov <- cov(facesNorm)

#Calculating eigen vectors and values
eigenList <- eigen(facesCov)
eigenValues <- eigenList[[1]]
eigenVectors <- eigenList[[2]]

#Explained Variance
eigenValuesSum = sum(eigenValues)
explainedVariance = eigenValues/eigenValuesSum
cumulativeVariance = cumsum(explainedVariance)

#Finding Optimal Dimensions for 70% Explained Variance
optDim <- which(cumulativeVariance > 0.7)[1]

#Projecting Data on Eigen Vectors
optimalEigVectors <- eigenVectors[,1:optDim]
pcaData <- facesNorm%*%optimalEigVectors

#Putting in labels
pcaData <- cbind(pcaData,y)

#Setting Training Ratio
trainTestRatio <- 0.5

#Setting up results matrix
results <- list()

#Setting seed for reproducible results
set.seed(121)

for(iter in 1:10){
  #Separate training and testing data
  pcaData <- as.data.frame(pcaData)
  sample_size = floor(trainTestRatio*nrow(pcaData))
  picked = sample(seq_len(nrow(pcaData)),size = sample_size)
  trainFaces = pcaData[picked,]
  testFaces = pcaData[-picked,]
  
  trainFaces[,optDim+1] <- as.factor(trainFaces[,optDim+1])
  testFaces[,optDim+1] <- as.factor(testFaces[,optDim+1])
  
  #Exercise is focused on PCA, Bayes is library-implemented
  #Classify
  classifier_cl <- naiveBayes(y ~ ., data = trainFaces)
  
  #Predict Test Data
  y_pred <- predict(classifier_cl, newdata = testFaces)
  
  cm <- table(testFaces$y, y_pred)
  
  #Save results
  results[[length(results)+1]] <- list(confusionMatrix(cm))
}

#Combining Result Accuracies and Extracting Confusion Matrixes
accuracy <- matrix(nrow=0,ncol=0)
for(iter in 1:10){
  accuracy <- c(results[[iter]][[1]][["overall"]][["Accuracy"]],accuracy)
  table <- results[[iter]][[1]]["table"][[1]]
  png(paste("C:/Users/kings/OneDrive/Documents/R/matrix",iter,".png"),height = 25*nrow(table), width = 25*ncol(table))
  p<-tableGrob(table)
  grid.arrange(top = paste("Confusion Matrix ",iter), p)
  dev.off()
}

#Calculate average and stdDev
meanResult <- mean(accuracy)
stdDevResult <- sd(accuracy)

MostraImagem <- function( x )
  {
    rotate <- function(x) t( apply(x, 2, rev) )
    img <- matrix( x, nrow=64 )
    cor <- rev( gray(50:1/50) )
    image( rotate( img ), col=cor )
    }
MostraImagem( faces[284,] )
