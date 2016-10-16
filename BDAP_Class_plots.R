attach(LungCapData )

boxplot(LungCap ~ Gender)

stripchart(LungCap)

coplot(LungCap ~ Height|Smoke)  ## Combination Plot

pairs(LungCapData)

plot.ts(LungCapData[,1:4], col = 2, main = "plot.ts")

qqnorm(Height)

par(mfrow=c(1, 2))
count <- table(Gender)
box()
pie(count)
box()
pie(table(Smoke), col = c(5,2))

table(Smoke, Gender)
barplot(table(Smoke, Gender), beside = T)

mosaicplot(table(Smoke, Gender))


class(AirPassengers)
class(Boston)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
AirPassengers[]
head(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN=mean))

library(MASS)
Boston
# calculate cor-relation matrix and plot matrix
plotCircleCorelation <- function(dataset){
  M <- cor(dataset)
  library('corrplot') #package corrplot
  corrplot(M, method = "circle") #plot matrix
}
# size of circle gives strenght of corelation
# darker the color stronger relation ship
# blue color is positive corelation
#Red is negative

plotNumberCorelation <- function(dataset){
  M <- cor(dataset)
  library('corrplot') #package corrplot
  corrplot(M, method = "number") #plot matrix
}
M <- cor(Boston)
corrplot(bostonCor, method = "number") #plot matrix
corrplot(M, method = "circle") #plot matrix
corrplot.mixed(M)


# VIF varification - 
attach(Boston)
summary(lm(medv ~ . , Boston))
summary(lm(medv ~ . -rad-tax, Boston))


#VIF calculated - Multicolinearity
install.packages("car")
library(car)
lm.fit <- lm(medv ~ . , data = Boston)
vif(lm.fit)

lm.fit1 <- lm(medv ~ . -rad-tax, data = Boston)
vif(lm.fit1)


# Carseats 
library(ISLR)
attach(Carseats)
lm.fit <- lm(Sales ~ . , Carseats)
corrplot.mixed(cor(Carseats[ ,-c(7, 10, 11)]))
summary(lm.fit)
vif(lm.fit)



lm.fit2 <- lm(Sales ~ . -ShelveLoc-Urban-US, Carseats)
summary(lm.fit2)
vif(lm.fit2)
corrplot.mixed(cor(Carseats[ ,-c(7, 10, 11)]))


#Logistic regression

loan <- read.csv(file.choose(), header = T)
attach(loan)
logr <- glm(Loan.approval ~ loan$Salary + loan$Age, data = loan, family = binomial(link = "logit"))
summary(logr)
summary(logr)

ans <- 1.83 + ( 0.039 * 310 ) - ( 0.38 * 47)
exp(ans) = py /(1- py)

py = (0.019/1.019)

#so 
pn = 1 - py
# py = 0.02 and pn = 0.98

loan[Salary == 310 & Age == 47,]


# Smarket dataset operation fro Logistic regression
library(ISLR)
Smarket
attach(Smarket)

#1.
glm.fir <- glm(Direction ~ Lag1, data = Smarket, family = binomial(link = "logit"))
summary(glm.fir)
#model - Direction = 0.074 - 0.0702 Lag1
# AIC = 1731.2

#2.
glm.fir <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial(link = "logit"))
summary(glm.fir)
#P value is high
# model - Direction = 0.074 - 0.071 Lag1 - 0.044 Lag2
# AIC = 1731.2



#3.
glm.fir <- glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, family = binomial(link = "logit"))
summary(glm.fir)
#P value is high
# model - Direction = 0.074 - 0.071 Lag1 - 0.044 Lag2 + 0.008 Lag3
# AIC = 1731.2


#4.
glm.fir <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4, data = Smarket, family = binomial(link = "logit"))
summary(glm.fir)
#P value is high
# model - Direction = 0.074 - 0.071 Lag1 - 0.044 Lag2 + 0.008 Lag3 + 0.0069 Lag4
# AIC = 1731.2


#3.
glm.fir <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = Smarket, family = binomial(link = "logit"))
summary(glm.fir)
#P value is high
# model - Direction = 0.074 - 0.071 Lag1 - 0.044 Lag2 + 0.008 Lag3 + 0.0069 Lag4 + 0.009 Lag5
# AIC = 1731.2


glm.fit <- glm(Direction ~ ., data = Smarket, family = binomial(link = "logit"))
summary(glm.fit)


glm.fit <- glm(Direction ~ Volume, data = Smarket, family = binomial(link = "logit"))
summary(glm.fit)


glm.fit <- glm(Direction ~ Today, data = Smarket, family = binomial(link = "logit"))
summary(glm.fit)


glm.fit <- glm(Direction ~ .-Today, data = Smarket, family = binomial(link = "logit"))
summary(glm.fit)

# Carseats

attach(Carseats)
names(Carseats)

pval <- c()
name <- c()
aic <- c()
for(var in names(Carseats)){
  v <- eval(parse(text = var))
  glm.fit <- glm(Urban ~ v, data = Carseats, family = binomial(link = "logit"))
  aic <- c(aic,glm.fit$aic)
  name <- c(name, var)
  
}

min(aic)
##### GoldDeposit


goldDeposit <- read.csv(file.choose(), header = T)
attach(goldDeposit)

pval <- c()
name <- c()
aic <- c()
for(var in names(goldDeposit)){
  v <- eval(parse(text = var))
  glm.fit <- glm(Gold.Deposit.present.or.absent. ~ v, data = goldDeposit, family = binomial(link = "logit"))
  aic <- c(aic,glm.fit$aic)
  name <- c(name, var)
  pval <- c(pval,summary(glm.fit))
}

pval


glm.fit <- glm(Gold.Deposit.present.or.absent ~ As.level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)


glm.fit <- glm(Gold.Deposit.present.or.absent ~ Sb.Level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)


glm.fit <- glm(Gold.Deposit.present.or.absent ~ Lineament.Proximity..present.or.absent., data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)


glm.fit <- glm(Gold.Deposit.present.or.absent ~ As.level + Sb.Level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
library(car)

glm.fit <- glm(Gold.Deposit.present.or.absent. ~ 
                 Sb.Level+ Lineament.Proximity..present.or.absent., 
               data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)

glm.fit <- glm(Gold.Deposit.present.or.absent. ~ ., data = goldDeposit, 
               family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)


glm.fit <- glm(Gold.Deposit.present.or.absent. ~ As.level + Lineament.Proximity..present.or.absent., data = goldDeposit, 
               family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


cor(Sb.Level , As.level)



logit(-4.8 + (1.18 * 10 ) + (0.87 * 10))
exp( 15.7 / 16.7)
#------------------- 



glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ As.level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)




glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ Gold.Deposit.present.or.absent., data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ Sb.Level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ Sb.Level+As.level, data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ As.level+ Gold.Deposit.present.or.absent., data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ Sb.Level + Gold.Deposit.present.or.absent., data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)


glm.fit <- glm(Lineament.Proximity..present.or.absent. ~ ., data = goldDeposit, family = binomial(link = "logit"))
vif(glm.fit)
summary(glm.fit)
cor(As.level , Lineament.Proximity..present.or.absent.)