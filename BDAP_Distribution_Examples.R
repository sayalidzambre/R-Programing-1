mtcars
names(mtcars)
head(mtcars)
attach(mtcars)
cor(mpg,disp)

dbinom(4, size=12, prob=0.2) 

val<- dbinom(4, size=12, prob=0.2) +
dbinom(3, size=12, prob=0.2) +
dbinom(2, size=12, prob=0.2) +
dbinom(1, size=12, prob=0.2) +
dbinom(0, size=12, prob=0.2) 

val 
pbinom(4, size=12, prob=0.2) 


# Poisons
ppois(16, lambda=12, lower=FALSE) 

#Expon

pexp(2, rate = 1/3)

#Normal



