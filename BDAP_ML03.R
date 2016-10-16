


people_walked_in <- as.integer(rnorm(30,25,10))

for24 <- ppois(24, 25)
for27 <- ppois(27, 25)

for27 - for24

# I can add Probability below becaue of independent events 24, 25, 26, 27 are independent Events
dpois(24, 25) + dpois(25, 25) + dpois(26, 25) + dpois(27, 25)



library(rattle)
data <- mtcars
write.csv(data, "C:\\Users\\OWNER\\Desktop\\BDAP\\term 3\\ml103\\mtcars.csv")

rattle()



mu = 3
sig = 0.4

pnorm(3.5, mean=3, sd=0.4)  - pnorm(2.5, mean=3, sd=0.4) 
dnorm(3, 3 , 0.4)


density_standard_norm <- function(x, sigma)
{
  1/(sqrt(2*pi)*sigma)*exp(-0.5*x^2)
}

dnorm(3, 3 , 0.4)

density_standard_norm(3, 0.4)

dnorm(2, mean = 0, sd = 1)


density_standard_norm(2) 


require(fitdistrplus)

set.seed(1)
dat <- rnorm(50,0,1)
data <- rpois(30,25)
f1 <- fitdist(dat,"norm")
f2 <- fitdist(dat,"logis")
f3 <- fitdist(dat,"cauchy")
f4 <- fitdist(data,"pois")


plotdist(dat,"norm",para=list(mean=f1$estimate[1],sd=f1$estimate[2]))
plotdist(dat,"logis",para=list(location=f2$estimate[1],scale=f2$estimate[2]))
plotdist(dat,"cauchy",para=list(location=f3$estimate[1],scale=f3$estimate[2]))
plotdist(dat,"pois",para=list(location=f4$estimate[1]))


vect <- -100:100
sq_vect <- vect ^ 2

cor(vect,sq_vect)

plot(vect, sq_vect)
#-----------------------------------------------------

mu = 300000
singma = 40000
populationSize <- 1000
samplesize <- 100
set.seed(1234)

population <- rnorm(populationSize, mu, singma)

means = c()
for( i in 1:10){
  means <- c(means, mean(sample(population, samplesize, replace = FALSE)))
}

mean(means)
sdn <- sd(means)
meansp<-mean(population)
sdp <- sd(population)
sdp <- (sdp)/sqrt(length(population))

qnorm(0.1, meansp, sdp)
qnorm(0.9, meansp, sdp)



hist(means)
plot(density(means))


xbar = 294387
sigma = 40000
samplesize = 100


left <- qnorm((1-0.95)/2, 294387, (sigma/10))
right <- qnorm(1-((1-0.95)/2), 294387, (sigma/10))
paste(left , " : " , right)


abs(qt(0.05/2, 49))




left <- qnorm((1-0.95)/2, 500000, 50000/10)
right <- qnorm(1-((1-0.95)/2), 500000, 50000/10)





sigma <- 50000    # theoretical standard deviation
mu0   <- 500000   # expected value under H0
mu1   <- 600000   # expected value under H1
alpha <- 0.05  # probability of type I error

# critical value for a level alpha test
crit <- qnorm(1-alpha, mu0, sigma/10)

# power: probability for values > critical value under H1
(pow <- pnorm(crit, mu1, sigma/10))


# probability for type II error: 1 - power
(beta <- 1-pow)


samp <- rnorm(20, 2, 5)

t.test(samp)



# Hypothesis
mtcars
# H0 = 20
# H1 != 20
t.test(mtcars[,1],mu = 20, conf.level = 0.99)



library(hflights)
?hflights

head(hflights)
colnames(hflights)
head(hflights[, 7])
unique(hflights[,7])



hflights$DepDelay
# H0 = 5
t.test(hflights[hflights$Origin == 'IAH',13], mu = 5)
# We reject null hypothesis



AA = na.omit(hflights[hflights$UniqueCarrier == 'AA',13])
UA = na.omit(hflights[hflights$UniqueCarrier == 'UA',13])

AAmean = mean(na.omit(AA))
UAmean = mean(na.omit(UA))

paste( "XD : ", AAmean - UAmean)
sdAA = sqrt((var(AA)/length(AA)) + sqrt(var(UA)/length(UA)))

(AAmean- UAmean -2)/ (sdAA )


t.test(AA, UA, mu =2)


#---------------------------------------------------

iris
head(iris)
plot(iris)



summary(lm(iris$Sepal.Length ~ iris$Petal.Width + iris$Sepal.Width + iris$Petal.Length))

rattle()
#---------------------------------------------------------------
data <- iris
head(data)
corrplot(data)

data <- data[data$Species != 'versicolor',]
train <- data[1:80,]
test <- data[81:100,]
summary(train)

model <- glm(Species ~ Petal.Length, data = train, family = binomial(link = "logit"))
summary(model)

model_pred_probs1 <- c()
model_pred_probs1 <- predict(model, test, type = "response")
model_pred_probs1
length(model_pred_probs1)
# Create true vector
model_pred_GD <- rep("setosa", length( test$Species))
length(model_pred_GD)

model_pred_GD[model_pred_probs1 > 0.7] = "virginica"
length(model_pred_GD)
length(test$Species)
model_pred_GD


confmatrix(test$Species, model_pred_GD)


confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}


#______________ Decision Tree
model <- rpart(Species ~ train$Sepal.Length, data = train , method = "class")
summary(model)

model_pred_probs <- predict(model, testDataSet, type = "prob")[,2]
# we are reading 2nd column as we want true positive values
model_pred_probs

pred <- prediction(model_pred_probs, testDataSet$Kyphosis)
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")






library(leaps)
data.full <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)


library (leaps)
regfit.full=regsubsets (Salary~.,Hitters )
summary(regfit.full)

