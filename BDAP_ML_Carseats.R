library(ISLR)

names(Carseats)
train<-Carseats[1:300,]
test<-Carseats[301:400,]
corrplot(cor(Carseats[,-c(7,10,11)]), method = "number")

model <- naiveBayes(US ~ Sales+CompPrice+Income+Advertising+Population+Price+Age+Education , data = train)
summary(model)
predicted <- predict(model,test)
table(test[,11], predicted)
# 87%


model <-  C5.0( train[, -c(7,10,11)], train[,11])
summary(model)
predicted <- predict(model, test, type = "class")
table(test[,11], predicted)

#91%

model <- rpart(US ~ Sales+CompPrice+Income+Advertising+Population+Price+Age+Education , data = train, method = "class")
summary(model)
rpart.plot(x = model, type = 3, extra = 101, fallen.leaves = T)
predicted <- predict(model, test, type = "class")
table(test[,11], predicted)
#91%
