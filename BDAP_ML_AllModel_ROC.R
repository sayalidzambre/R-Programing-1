library(rpart)

moddata <- kyphosis
moddata <- na.omit(moddata)
set.seed(456)
random <- runif(nrow(moddata))
randomDataSet <- moddata[order(random), ]
names(kyphosis)
#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.8,digits=0)

#Create training and testing data set 
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]


#______________ Decision Tree
model <- rpart(Kyphosis ~ ., data = trainDataSet , method = "class")
summary(model)

model_pred_probs <- predict(model, testDataSet, type = "prob")[,2]
# we are reading 2nd column as we want true positive values
model_pred_probs

pred <- prediction(model_pred_probs, testDataSet$Kyphosis)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

# KNN--------------------
library(class)
library(ROCR)
model <- knn(trainDataSet[,c(2:4)], testDataSet[,c(2:4)], 
             cl=trainDataSet[,1], k=3, prob = T)

class(model)
model_pred_probs <- attr(model, "prob")

model_pred_probs = ifelse(model == "absent",  1 - model_pred_probs, model_pred_probs)
model_pred_probs

pred <- prediction(model_pred_probs, testDataSet$Kyphosis)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

#-----SVM

library(e1071)
library(mlbench)
model <- svm(trainDataSet[,2:4], trainDataSet[,1], probability=TRUE, cost = 100, gamma = 1)
summary(model)
model_pred <- predict(model, testDataSet[, 2:4], probability=TRUE)
model_pred_probs <- attr(model_pred, "probabilities")[,2]

pred <- prediction(model_pred_probs, testDataSet$Kyphosis)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

#--------Navies Bayes


model <- naiveBayes(Kyphosis ~ . , data = trainDataSet )
summary(model)
model_pred_probs <- predict(model,testDataSet, type = "raw")[,2]
pred <- prediction(model_pred_probs, testDataSet$Kyphosis)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

