library(ISLR)

library(class)
dataset <- Carseats
attach(dataset)
names(dataset)

dataset[,7] <- as.numeric(dataset[,7])
dataset[,10] <- as.numeric(dataset[,10])

set.seed(9850)
random <- runif(nrow(dataset))
randomDataSet <- dataset[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.8,digits=0)
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]

model <- svm(trainDataSet[,1:10], trainDataSet[,11])
summary(model)
predicted <- predict(model, testDataSet[, 1:10])
confmatrix(testDataSet[,11], predicted)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}