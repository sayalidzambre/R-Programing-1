
library(caret) 
confusionMatrix(xtab)

modHouse=HouseVotes84
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","j")
}

class(modHouse[,5])
names(modHouse)

modHouse[is.na(modHouse)]="j"

for(j in 2:17){
  modHouse[,j] = as.numeric(modHouse[,j])
}


set.seed(9850)
random <- runif(nrow(modHouse))
randomDataSet <- modHouse[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(modHouse) * 0.7,digits=0)
trainDataSet <- modHouse[1:trainLength, ]
testDataSet <- modHouse[(trainLength+1) : nrow(randomDataSet), ]


model <- knn(trainDataSet[,2:17], testDataSet[,2:17], cl=trainDataSet[,1], k=7)

xtab <- table(testDataSet[,1], model)
library(caret) 
confusionMatrix(xtab)
#___________________________Haming Distance______________________________________



modHouse=HouseVotes84
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","j")
}
modHouse[is.na(modHouse)]="j"

for(j in 2:17){
  modHouse[,j] = as.numeric(modHouse[,j])
}


set.seed(9850)
random <- runif(nrow(modHouse))
randomDataSet <- modHouse[order(random), ]

#Calculate training dataset length
trainLength <- round(nrow(modHouse) * 0.7,digits=0)
trainDataSet <- modHouse[1:trainLength, ]
testDataSet <- modHouse[(trainLength+1) : nrow(randomDataSet), ]



# Calculate distance and nearest neighbors
library(e1071)
d <- hamming.distance(modHouse)
NN <- apply(d[testDataSet, trainDataSet], 1, order)

# Predict class membership of the test set
k <- 5
pred <- apply(NN[, 1:k, drop=FALSE], 1, function(nn){
                tab <- table(y[trainDataSet][nn])
                as.integer(names(tab)[which.max(tab)])      # This is a pretty dirty line
              })

# Inspect the results
table(pred, y[testDataSet])
confmatrix(testDataSet[,11], pred)




modHouse=HouseVotes84
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","j")
}
modHouse[is.na(modHouse)]="j"

#Jacard coefficient data
modHouse <- as.matrix(modHouse)
set.seed(9850)
design.set <- sample(nrow(modHouse), 300)
test.set <- setdiff(1:435, design.set)

class(modHouse)
library(e1071)
d <- hamming.distance(modHouse)
dim(d)
# Calculate distance and nearest neighbors
NN <- apply(d[test.set, design.set], 1, order)
class(NN)

# Predict class membership of the test set
k <- 5
pred <- apply(NN[, 2:17, drop=FALSE], 1, function(nn){
  tab <- table(modHouse[design.set][nn])
  as.integer(names(tab)[which.max(tab)])      # This is a pretty dirty line
})

table(pred, modHouse[test.set])