library(ISLR)
str(Smarket)
attach(Smarket)
plot(Volume)

# Split data into testing and training
train<-Smarket[Year<2005,]
test<-Smarket[Year==2005,]
names(Smarket)
library(caret)


library(ggvis)

Smarket %>%ggvis(~Lag1, ~Lag2, fill = ~Direction) %>% layer_points()
Smarket %>%ggvis(~Lag4, ~Lag5, fill = ~Direction) %>% layer_points()
corrplot(cor(Smarket[,-9]))

logit <- glm(Direction ~ Lag1+Lag2+Lag3, family='binomial', data=train)
summary(logit)
test.probs <-predict(logit, test, type='response')
pred.logit <- rep('Down',length(test.probs))
pred.logit[test.probs>=0.5] <- 'Up'
table(pred.logit, test$Direction)



model <- naiveBayes(Direction ~ Lag1+Lag2+Lag3 , data = train)
summary(model)
predicted <- predict(model,test)
table(test[,9], predicted)



model <-  C5.0( train[, -9], train[,9])
summary(model)
predicted <- predict(model, test, type = "class")
table(test[,9], predicted)



model <- rpart(Direction ~ Lag1+Lag2+Lag3 , data = train, method = "class")
summary(model)
rpart.plot(x = model, type = 3, extra = 101, fallen.leaves = T)
predicted <- predict(model, test, type = "class")
table(test[,9], predicted)
