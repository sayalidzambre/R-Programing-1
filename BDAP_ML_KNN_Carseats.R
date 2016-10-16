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






