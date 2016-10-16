library(mlbench)

data(HouseVotes84, package = "mlbench")
HouseVotes84

dataset <- HouseVotes84
#Shuffle Data - to unorder data
set.seed(9850)
random <- runif(nrow(dataset))
randomDataSet <- dataset[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 2/3,digits=0)

#Create training and testing data set 
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]


bayesmodel <- naiveBayes(Class~ . , data = trainDataSet)
predicted <- predict(bayesmodel,testDataSet)
table(testDataSet[,1], predicted)


C50model <- createC5.0(trainDataSet) 
summary(C50model)
predicted <- predict(C50model, testDataSet, type = "class")
matric <- createConfusionMatrix(testDataSet, predicted)
matric



rpartmodel <- createrpart(trainset = trainDataSet)
predicted <- predict(rpartmodel, testDataSet, type = "class")
matric <- createConfusionMatrix(testDataSet, predicted)
matric

ligitmodela <- glm(Class ~ ., data = trainDataSet, family = binomial(link = "logit"))
summary(ligitmodela)
model_pred_probs1 <- predict(ligitmodela, testDataSet)
model_pred_Dir <- rep("democrat", nrow(testDataSet))
model_pred_Dir[model_pred_probs1 > 0.5] = "republican"
table(testDataSet[,1], model_pred_Dir)
library(car)
vif(ligitmodela)


createrpart <- function(trainset){
  rpart(Class ~ . , data = trainset, method = "class")
}


createC5.0 <- function(trainset){
  C5.0( trainset[, -1], trainset[,1])
}


createConfusionMatrix <- function(testDataSet, predicted){
  table(testDataSet[, 1], predicted)  
}