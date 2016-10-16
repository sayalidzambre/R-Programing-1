library(rpart)

moddata <- Carseats

set.seed(9850)
random <- runif(nrow(moddata))
randomDataSet <- moddata[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(randomDataSet) * 0.8,digits=0)
trainDataSet <- randomDataSet[1:trainLength, ]
testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]

names(Carseats)

modela <- glm(US ~ Sales+CompPrice+Income+Advertising+Population+Price+Age+Education, data = trainDataSet, family = binomial(link = "logit"))
summary(modela)

library(car)
vif(modela)

model_pred_probs <- predict(modela, testDataSet, type = "response")
model_pred_probs

model_pred_GD <- rep("no", nrow(testDataSet))
model_pred_GD[model_pred_probs > 0.5] = "yes"
model_pred_GD

confmatrix(testDataSet$US, model_pred_GD)
library("ROCR")

pred <- prediction(model_pred_probs, testDataSet$US)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

# Now calculate ROC for Kyphosis

#____________________________________________________________


library(rpart)
library(rpart.plot)
model <- rpart(US ~ Sales+CompPrice+Income+Advertising+Population+Price+Age+Education, data = trainDataSet , method = "class")
summary(model)

model_pred_probs <- predict(model, testDataSet, type = "prob")[,2]
# we are reading 2nd column as we want true positive values
model_pred_probs

pred <- prediction(model_pred_probs, testDataSet$US)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")
