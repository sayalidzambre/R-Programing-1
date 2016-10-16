#x2 + 2x + 3 if x<0
#x+ 3       if 0<=x<2
#x2+4x-7 x>=2

poly_function <- function(xVect){
   retVect <- c()
   for( x in xVect){
     if(x < 0){
       retVect<- c(retVect, (x ^2 ) + (2 * x) + 3)
     }else if(x >= 0 & x < 2){
       retVect<- c(retVect, x + 3)
     }else{
       retVect<- c(retVect, (x ^2 ) + (4 * x) - 7)
     }
   }
   return(retVect)
}

retVect <- poly_function(-10:10)
plot(retVect, col = 2, main = "Function Plot")
#---------------------------------------------------------------------------
#2 - 

primeFunction <- function(xVect){
  primeVect <- c()
  for(num in xVect){
    if(is.integer(num) & num > 0 & isPrime(num)){
        primeVect <- c(primeVect, num)
    }
  }
  return(primeVect)
}
isPrime <- function(num){
  if(num == 1){
    return(F)
  }
  if(num == 2){
    return(T)
  }
  for(i in 2:(num-1)){
    if(num %% i == 0){
      return(F)
    }
  }
  return(T)
}

primeFunction(1:50)

#---------------------------------------------------------------------------

#3-
df <- data.frame(no = c(1:10), var = c('A','B','C','D','E','F','G', 'H','I','J'))

df$include <- logical(10)

df$include[c(4,5)] <- T

df[df$include == T, df$no]
        



# 4.-----------------------------

library(ISLR)
Carseats <- read.csv(file.choose())

splitdf <- function(dataframe, seed){
  if( !is.null(seed))
    set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*3/4))
  trainset <- dataframe[trainindex,]
  testset <- dataframe[-trainindex,]
  list(trainset, testset)
}

splits <- splitdf(Carseats, 100)
str(splits)
lapply(splits,head)

Carseats_training <- splits$trainset
Carseats_test <- splits$testset
nrow(Carseats_test)

# Biulding model
model <- glm(Sales ~ ShelveLoc, data = Carseats_training, family = binomial(link = "logit"))
summary(model)

# Predict values for remaining 20% Data set 
model_pred_probs1 <- c()
model_pred_probs1 <- predict(model, Carseats_testing)
model_pred_probs1
length(model_pred_probs1)

# Create true vector
model_pred_GD <- rep(0, 100)
length(model_pred_GD)

model_pred_GD[model_pred_probs1 > 0.5] = 1
length(model_pred_GD)
model_pred_GD

table(Carseats_seed$Sales, model_pred_GD)

# 5. ----
library(Lahman)
baseball <- read.csv(file.choose(), header = T)
attach(baseball)
names(baseball)
df1 <- baseball[, c("player", "year", "hr", "bb")]

library(dplyr)
library(lubridate)
tempyear <- Sys.Date()

df1 <- mutate(df1, age = year(tempyear) - year)

df2 <- filter(df1, is.na(bb) == F)

df1[6,4] <- NA




#------------------------------------------------------



x <- c(1,1,2,3,NA, 8, 13)
x[ is.na(x) ] <- 5
x


rep(-(1:3), 4)

rep(rep(seq(-1:1),rep(2,3)),2)

x <- seq(2, 20, 3)
x[x>15 | x<6 ]

vector("numeric", length = 5)

# 3. --------------------------------------------------------------

a <- c(2,3,4)
b <- c(1,2)

a%*%b


# replace NA with mean

require(dplyr)
x <- c(1,4,2,NA,6,8,4)
df <- df %>%
  mutate(colname = ifelse(is.na(colname),0,colname))

df <- df %>%
  mutate(colname = ifelse(is.na(colname),0,colname))



mat <- matrix(1:6, 2)
mat[1,2]


x <- c(3,5,1,NA,7,9)

na.action(x,11)

v <- c(3,5,1,4,7,9)
count_odd <- length(v[v %%2 ==1 ])

x[is.na(x)] <- mean(x, na.rm = T)

na.omit(x)