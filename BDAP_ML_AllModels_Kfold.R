library(mlbench)
data(PimaIndiansDiabetes)
modData <- PimaIndiansDiabetes

# Explore Data
library(car)
cor(modData[,-9])
boxplot(modData[, -9])

for(i in 1:8){
  modData[,i] <- remove_outliers(modData[,i])
}
modData <- na.omit(modData)
dim(modData)
rowNum <- 1:nrow(modData)
set.seed(9870)
trainsetrownums <- sample(rowNum, size = 0.8 *nrow(modData))
testsetrownums <- rowNum[-trainsetrownums]

#--------Logistics-------------

model <- glm(diabetes ~ ., data = modData[trainsetrownums,], family = binomial(link = "logit"))
summary(model)

model_pred_probs <- predict(model, modData[testsetrownums,], type = "response" )
model_pred_probs

model_pred_Dir <- rep("neg", length(testsetrownums))
model_pred_Dir[model_pred_probs > 0.5] = "pos"

confmatrix(modData[testsetrownums,9],model_pred_Dir)

library("ROCR")

pred <- prediction(model_pred_probs, modData[testsetrownums,]$diabetes)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")


library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(diabetes ~ ., data = modData, trControl = train_control, method = "glm")
print(model)

#-------------------- Decision Tree------------
model <- rpart(diabetes ~ ., data = modData[trainsetrownums,] , method = "class")
summary(model)

model_pred_probs <- predict(model, modData[testsetrownums,], type = "prob")[,2]
# we are reading 2nd column as we want true positive values
model_pred_probs

model_pred_Dir <- rep("neg", length(testsetrownums))
model_pred_Dir[model_pred_probs > 0.5] = "pos"

confmatrix(modData[testsetrownums,9],model_pred_Dir)

pred <- prediction(model_pred_probs, modData[testsetrownums,]$diabetes)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(diabetes ~ ., data = modData, trControl = train_control, method = "rpart")
print(model)

#-----------------------KNN--------------------------------------

library(class)
model <- knn(modData[trainsetrownums, -9], modData[testsetrownums, -9], 
             cl=modData[trainsetrownums, 9], k=10, prob = T)

class(model)
model_pred_probs <- attr(model, "prob")

model_pred_probs = ifelse(model == "neg",  1 - model_pred_probs, model_pred_probs)
model_pred_probs

pred_vals <- model[1:154]

model_pred_Dir <- rep("pos", length(testsetrownums))
model_pred_Dir[model_pred_probs > 0.5] = "neg"

confmatrix(modData[testsetrownums,9],model_pred_Dir)

pred <- prediction(model_pred_probs, modData[testsetrownums,]$diabetes)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(diabetes ~ ., data = modData, trControl = train_control, method = "knn")
print(model)

#-----SVM ---------------------------------------------

library(e1071)

model <- svm(modData[trainsetrownums, -9], modData[trainsetrownums, 9],
             probability=TRUE)
summary(model)
model_pred <- predict(model, modData[testsetrownums, -9], probability=TRUE)
model_pred_probs <- attr(model_pred, "probabilities")[,1]

model_pred_Dir <- rep("neg", length(testsetrownums))
model_pred_Dir[model_pred_probs > 0.5] = "pos"

confmatrix(modData[testsetrownums,9],model_pred_Dir)

pred <- prediction(model_pred_probs, modData[testsetrownums, 9])
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(diabetes ~ ., data = modData, trControl = train_control, method = "knn")
print(model)

#--------Navies Bayes -------------------------------


model <- naiveBayes(diabetes ~ ., data = modData )
summary(model)
model_pred_probs <- predict(model,modData[testsetrownums, ], type = "raw")[,2]

model_pred_Dir <- rep("neg", length(testsetrownums))
model_pred_Dir[model_pred_probs > 0.5] = "pos"

confmatrix(modData[testsetrownums,9],model_pred_Dir)

pred <- prediction(model_pred_probs, modData[testsetrownums,]$diabetes)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(diabetes ~ ., data = modData, trControl = train_control, method = "knn")
print(model)
#----------------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE) {
  x = modData[,1]
  na.rm = TRUE
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}