library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)

#model
model <- train(Species ~ ., data = iris, trControl = train_control, method = "knn")
print(model)

model <- train(Kyphosis ~ ., data = kyphosis, trControl = train_control, method = "nb")
print(model)

model <- train(Kyphosis ~ ., data = kyphosis, trControl = train_control, method = "svmLinear")
print(model)

#decision tree
model <- train(Kyphosis ~ ., data = kyphosis, trControl = train_control, method = "rpart")
print(model)

#Carseats
model <- train(US ~ ., data = Carseats, trControl = train_control, method = "nb")
print(model)

model <- train(US ~ ., data = Carseats, trControl = train_control, method = "svmLinear")
print(model)

#decision tree
model <- train(US ~ ., data = Carseats, trControl = train_control, method = "rpart")
print(model)

model <- train(US ~ ., data = Carseats, trControl = train_control, method = "knn")
print(model)

