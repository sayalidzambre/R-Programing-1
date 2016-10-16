z <- (0.4-0.5)/(0.8/sqrt(2500))
pnorm(-abs(z))
1 - pnorm(6,4,0.8)



sal <- c(10000,12000,22000,35000, 45000, 80000)
age <- c(25,30,35, 40, 45, 50)
salary <- data.frame(sal,age)
plot(age, sal)
abline(lm(sal~age))

install.packages("ggplot")

library(plotly)
plot_ly(data = salary, x = age.Length, y = sal.Length, mode = "markers",
        color = Species)

cor(age, salary)


age1 <- mean(age)
sal1 <- mean(sal)
agediff <- age - age1
saldiff <- sal- sal1

sum(agediff * agediff)
as.numeric(sum(saldiff * saldiff))

#slope - sum(x-xbar) 
B1 <- sum((age - age1) *(sal - sal1))/sum(agediff * agediff)

#intercept - ybar - B1 xbar

B0 <- sal1 - (B1 * a1)

#model is salary = (-65000) + 2640 (age)
# predict sal for all age in age
predict_sal <- (-65000) + (2640 * age)

error <-  predict_sal - sal
errorsq <- error * error
rss <- sum(errorsq)

rse <- sqrt( rss / (length(age) -2))
# so here on an average I will make error of 9905.5 in calsulating salary in (-65000) + 2640 (age)


#now calculate value for age 42,52, 37
(-65000) + (2640 * c(42, 52, 37))


# home work , if we change B1 and B0 randomaly no value will match least rss -65000 and 2640
(-75000) + (1640 * age)


#--------------------------------------------
x <- c(1,2,4,3,5)
y <- c(1,3,3,2,5)
calculateBSE <- function(x,y){
  x1 <- mean(x)
  y1 <- mean(y)
  xdiff <- x - x1
  ydiff <- y- y1
  
  sum(xdiff * xdiff)
  
  #slope - sum(x-xbar) 
  B1 <- sum((x - x1) *(y - y1))/sum(xdiff * xdiff)
  cat("B1: " , B1)
  #intercept - ybar - B1 xbar
  
  B0 <- y1 - (B1 * x1)
  
  #equation is- 
  cat("relation is - y = " ,B0 , "+" , B1 , "x")
  cat("B0: " , B0)
  #model is salary = (-65000) + 2640 (age)
  # predict sal for all age in age
  predict_y <- B0 + (B1 * x)
  
  error <-  predict_y - y
  errorsq <- error * error
  rss <- sum(errorsq)
  cat("rss: " , rss)
  rse <- sqrt( rss / (length(x) -2))
  paste("rse: " , rse)
}

x <- c(1,2,4,3,5)
y <- c(1,3,3,2,5)
calculateBSE(x,y)

cor(x,y)

calculateSlopevalue <- function(x,y){
  B1 <- (cor(x,y) * (sd(y) / sd(x)))
  paste("B1 : " , B1)
  ybar <- mean(y)
  xbar <- mean(x)
  B0 <- ybar - (B1 * xbar)
  paste("B0 : " , B0)
  paste("y = " , B0 , " + " , B1 ," * x")
}
calculateSlopevalue(age,sal)
calculateSlopevalue(x,y)

summary(lm(sal ~ age))


datat <- read.csv(file.choose(), header = T)
attach(datat)

calculateCoefficient <- function(X, Y){
  plot(X, Y, col = 3)
  model <- lm(Y~X) 
  abline(model, lwd = 4)
  summary(model)
}

calculateCoefficient(age, sal)

calculateCoefficient(X, Y)
calculateCoefficient(x,y)
cor(X, Y)
calculateSlopevalue(X,Y)
calculateBSE(X,Y)

# claculate R2

plot(age, sal, col = 3)
model <- lm(sal ~ age) 
abline(model, lwd = 4)
summary(model)


# Operations on Boston data set for Multivalue regression
library(MASS)
Boston
dim(Boston)
attach(Boston)
names(Boston)

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
plot(lstat , medv)
Boston[lstat == 0 & age == 0, ]

#change in house price with crime rate
lm.fit <- lm(medv ~ crim, data = Boston)
summary(lm.fit)

#change in house price with crime rate and highway connectivity
lm.fit <- lm(medv ~ crim + rad, data = Boston)
summary(lm.fit)

#change in house price with crime rate
lm.fit <- lm(medv ~ crim + rad, data = Boston)
class(summary(lm.fit))

summary(lm.fit)[5][2]

#change in house price with crime rate
lm.fit <- lm(medv ~ lstat + indus, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)


install.packages("ISLR")

library(ISLR)
Carseats
attach(Carseats)

calculateBSE(Income, Sales)

cor(Income, Sales)

Carseats[Income ==0 ,]
summary(lm(Sales ~ Income))

 #Sales and Advertising
plot(Advertising, Sales)

cor(Advertising, Sales)

summary(lm(Sales ~ Advertising))

# SaMes Poppulation

plot(Population, Sales)
cor(Population, Sales)
summary(lm(Sales ~ Population))

plot(CompPrice, Sales)
abline(lm(Sales~CompPrice), col=6)
cor(CompPrice, Sales)
summary(lm(Sales ~ CompPrice))

plot(ShelveLoc, Sales)
abline(lm(Sales~ShelveLoc), col=6)
cor(ShelveLoc, Sales)
summary


plot(Urban, Sales)
abline(lm(Sales~Urban), col=6)
cor(Urban, Sales)
summary(lm(Sales ~ Urban))

# model - y = 7.5 - 0.095 (UrbanYes)
# Urban location sales is lesser than rural by 90 $
# 7.5 -> If my store location is UrbanNo my proce is 7.5
# but P-value of Urban yes =  0.7, which indicates that UrbanYes has no value required
# so model does not exists.


plot(US, Sales)
abline(lm(Sales~US), col=6)
cor(US, Sales)
summary(lm(Sales ~ US))

# model - sales = 6.823 + 1.04*US 
# p- value -> 0.00003 which is < 0.05, means moedl acceptable
# Compare to Out side US my sales will be more than 1.044 in US
# Outside US my sale is 6.82


summary(lm(Sales ~ .-Population-Education-Urban-US,data = Carseats))

# Sales will be increased by 1. ShelveLocGood(B1 = 4.835675), 
  #   2.ShelveLocMedium  1.951993, 
  #   3.Advertising      0.115903
  #   4.CompPrice        0.092571, 
  #   5.Income           0.015785
  #   
#Negative factors are -
  #   1.Price           -0.095319 
  #   2.Age             -0.046128

# Mode --
# Sales = 5.46 + 0.09Comprice + 0.01 Income _ 0.11(Advertising)
    #     -0.09 price + 4.83 shelve_life + 1.95 shelve_median - 0.04 Age
# other than categorical variables sales will increase by their respective Beta for every increase in unit
#For categorical variables -Compare to bad ShelveLoc Medium will increase by 1.95 keeping rest as constant
              # Compare to bad ShelveLocGood will increase by 4.83 keeping rest constant
  


#Question  -  
# salary = f(age, creditScore), R2 = ??
# salary = f(age)  = R2 = ??


data <- read.csv(file.choose(), header = T)
attach(data)
names(data)
summary(lm(salary ~ data$age ))
summary(lm(salary ~ data$age + creditscore ))


# ---------------Carseat Homeworks--------------------
library("ISLR")
Carseats
library("MASS")
Boston

names(Carseats)
# 1. Is there a relationship between advertising budget and sales?
attach(Carseats)
plot(Advertising, Sales)
summary(lm(Sales ~ Advertising))
Carseats[Advertising == 0 ,]
# Analysis - p value is nearning 0 so Advertising is impacting Sales.
# 1. for every increase in adverting budget sales will increase i=by 11.4
# 2. for no expenditure on advertment is done, sales will increase by 67



#--------------------------------------------------
#Confusion Matrix

GD <- read.csv("C:\\Users\\OWNER\\Desktop\\BDAP\\Gold_Deposit.csv", header = T)
dim(GD)
GD_training <- GD[1:44,]
GD_test <- GD[45:54,]

model <- glm(GD_training$GD ~ GD_training$As + GD_training$Sb, data = GD_training, family = binomial(link = "logit"))
summary(model)
dim(GD_test)
model_pred_probs <- predict(model, data=GD_test, type = "response")
model_pred_probs
length(model_pred_probs)
model_pred_GD <- rep("absent", 10)
length(model_pred_GD)
model_pred_GD[model_pred_probs > 0.5] = "present"
length(model_pred_GD)
model_pred_GD






data <- read.csv(file.choose())
