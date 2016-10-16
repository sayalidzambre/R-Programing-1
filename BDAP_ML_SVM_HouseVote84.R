
library(ISLR)
library(e1071)
modHouse=HouseVotes84
modHouse=HouseVotes84
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","j")
}

class(modHouse[,5])
names(modHouse)

modHouse[is.na(modHouse)]="j"

for(j in 2:17){
  modHouse[,j] = as.numeric(modHouse[,j])
}

set.seed(9850)
random <- runif(nrow(modHouse))
randomDataSet <- modHouse[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.75,digits=0)
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]

model <- svm(trainDataSet[,2:17], trainDataSet[,1])
summary(model)
predicted <- predict(model, testDataSet[, 2:17])
confmatrix(testDataSet[,1], predicted)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}