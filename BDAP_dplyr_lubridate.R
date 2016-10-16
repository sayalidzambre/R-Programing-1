library(dplyr)
library(lubridate)

chicago <- read.csv(file.choose(), header = T)
dim(chicago)
str(chicago)

names(chicago)

subsetData <- select(chicago, city:dptp)
head(subsetData)

s1 <- select(chicago, starts_with("d"))
attach(chicago)
class(pm25tmean2)

chic.f <- filter(chicago, pm25tmean2)

chicago <- arrange(chicago, desc(date))

chicago <- rename(chicago, dewpt = dptp, pm25 = pm25tmean2)


chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))

chicago <- mutate(chicago, year = year(ymd(date)))
head(chicago)

chicago <- read.csv(file.choose(), header = T)
chicago <- mutate(chicago, year = year(mdy(date)))

mutate(chicago, month = month(mdy(date)))




vect <- c(1,2,2,3,4,2,5,3,4,5,10, 12, 14, 14)

discoveries
library("moments")
skewness(discoveries)

plot(density(discoveries))

rnorm(100000)

skewness(rnorm(100000))
sd(rnorm(100000))
kurtosis(rnorm(100000))

pnorm(-2)


dnorm(-4)

plot(density(runif(100000,0, 100)))


qnorm(0.99) * 1000


# avg height = 170 cm and sd = 4 cm, what is probability od height > 180 cm. assume height normaly distributed
(1 - pnorm(180, 170, 4)) * 100

plot(density(pnorm(180, 170, 4)))




# F Distribuion 
# at 95% confidence interval


qf(p = 0.95, df1 = 22, df2 = 19)



library("psych")
data.aov <- read.clipboard(header = T)
attach(data.aov)
boxplot(data.aov)
boxplot(A ~ Stress)

data.result <- aov(formula = data.aov$A ~ data.aov$Stress, data = data.aov)
summary(data.result)

library("psych")
data.gender1 <- read.csv(file.choose(), header = T)
data.gender <- read.clipboard(header =  T)
attach(data.gender1)
data.gender1
score.result <- aov(Score ~ Gender * Age, data = data.gender1)
summary(score.result)


library("dplyr")



