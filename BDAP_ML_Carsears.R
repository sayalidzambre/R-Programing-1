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


model <- knn(trainDataSet[,c(1:10)], testDataSet[,c(1:10)], cl=trainDataSet[,11], k=75)

xtab <- table(testDataSet[,11], model)
library(caret) 
confusionMatrix(xtab)








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
trainLength <- round(nrow(modHouse) * 0.7,digits=0)
trainDataSet <- modHouse[1:trainLength, ]
testDataSet <- modHouse[(trainLength+1) : nrow(randomDataSet), ]


model <- knn(trainDataSet[,2:17], testDataSet[,2:17], cl=trainDataSet[,1], k=7)

xtab <- table(testDataSet[,1], model)
library(caret) 
confusionMatrix(xtab)
