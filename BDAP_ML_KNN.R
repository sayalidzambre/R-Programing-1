data <- read.csv("C:\\Users\\OWNER\\Desktop\\BDAP\\term 2\\ML02\\KNN.csv")

library(class)

library(ISLR)

iris

set.seed(9850)
random <- runif(nrow(iris))
randomDataSet <- iris[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.5,digits=0)
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]


model <- knn(trainDataSet[,1:4], testDataSet[,1:4], cl=trainDataSet[,5], k=75)

xtab <- table(testDataSet[,5], model)
library(caret) 
confusionMatrix(xtab)
