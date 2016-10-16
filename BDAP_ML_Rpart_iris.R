library(ISLR)
library(rpart)
library(rpart.plot)
library(C50)
makeDecisionTree <- function(dataset){
  dataset <- iris
  #Shuffle Data - to unorder data
  set.seed(9850)
  random <- runif(nrow(dataset))
  randomDataSet <- dataset[order(random), ]
  
  #Calculate training dataset length
  trainLength <- round(nrow(randomDataSet) * 2/3,digits=0)
  
  #Create training and testing data set 
  trainDataSet <- randomDataSet[1:trainLength, ]
  testDataSet <- randomDataSet[trainLength+1 : nrow(randomDataSet), ]
  
  # calculate decision tree
  
  model <-  createC5.0(trainDataSet) 
  #createrpart(trainset = trainDataSet)
  #createC5.0(trainDataSet)
  summary(model)
  
  #plot train dataset
  rpart.plot(x = model, type = 3, extra = 101, fallen.leaves = T)
  
  #predict for test data set
  predicted <- predict(model, testDataSet, type = "class")
  
  matric <- createConfusionMatrix(testDataSet, predicted)
  matric
}


makeDecisionTree(iris)

attach(iris)

createrpart <- function(trainset){
  rpart(Species ~ . , data = trainset, method = "class")
}


createC5.0 <- function(trainset){
  C5.0( trainset[, -5], trainset[,5])
}

createConfusionMatrix <- function(testDataSet, predicted){
  table(testDataSet[, 5], predicted)  
}