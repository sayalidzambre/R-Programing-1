
library(ISLR)
library(e1071)
iris
set.seed(9850)
random <- runif(nrow(iris))
randomDataSet <- iris[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.75,digits=0)
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]

model <- svm(trainDataSet[,1:4], trainDataSet[,5], cost= 1, gamma = 0.1)
summary(model)
predicted <- predict(model, testDataSet[, 1:4])
confmatrix(testDataSet[,5], predicted)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}