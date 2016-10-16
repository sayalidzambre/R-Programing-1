library(e1071)
library(mlbench)

# for subset of 10 lines and 2 columns
modHouse <- HouseVotes84[1:10, 1:3]

modHouse[is.na(modHouse)]="n"
modHouse <- as.matrix(modHouse)
library(e1071)
d <- hamming.distance(modHouse[,2:3])
temp <- d[10,1:9]
class(temp)
modHouse[which.min(as.vector(d[10,1:9])),1]


# for complete HouseVote dataset

modHouse <- HouseVotes84
modHouse <- na.omit(modHouse)
modHouse <- as.matrix(modHouse)
train.set <- modHouse[1:180,]
test.set <- modHouse[181:232,]
k <- 5 

pred <- c()
for(i in 1:nrow(test.set)){
  t.trainset <- rbind(train.set, test.set[i,])
  t.hd <- hamming.distance(t.trainset[,2:17])
  #getting row for last test data which is appendend in rbind in each loop
  last_hd <- t.hd[nrow(t.trainset),1:(nrow(t.trainset)-1)]
#  calculateMinDistForGivenK(as.vector(last_hd), k)
  
   # calculate min distance node
  min.dist.node <- which.min(as.vector(last_hd))
  #get Class of min.dist.node
  matching.class <- train.set[min.dist.node,1]
  pred <- c(pred,matching.class)
}
confmatrix(test.set[,1], pred)

#-----------------------------------------------------
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}




#------------------------using haming dist function........................
library(mlbench)
library(ROCR)
library(e1071)
#--------- Setting values specific to dataset------------------
data("HouseVotes84")
modHouse <- HouseVotes84
table(modHouse$Class)
# Create levels
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","abstain")
}

modHouse[is.na(modHouse)]="abstain"
modHouse

colNumOfClassvar <- 1
lenofClassLevels <- length(levels(modHouse[, colNumOfClassvar]))
#for k = 5
k <- 5

#---------Uptill this you need code specific to dataset----------------


modHouseMAT <- as.matrix(modHouse) 
rowNum <- 1:nrow(modHouse)

set.seed(9870)
trainsetrownums <- sample(rowNum, size = 0.8 *nrow(modHouse))
testsetrownums <- rowNum[-trainsetrownums]
h.dst <- hamming.distance(as.matrix(modHouse))
#this will give test set in rows and train set in column
h.dst.t <- h.dst[testsetrownums, trainsetrownums, drop = FALSE]

pred <- c()
prob_pred <- matrix(nrow=length(testsetrownums), ncol = lenofClassLevels)

# now for each test data row, will have hamming distance against train data
for(i in 1:length(testsetrownums)){
  
  prob_pred[i, ] <-  table(modHouse[ # to read the value of independent var 
    as.numeric( # as rownames gives string so converting to numeric
      rownames(  # reading rownames of matrix
        as.matrix( # converting to matrix to read row name
          h.dst.t[i # i represents test data's row number
                  , order(h.dst.t[i, ] , decreasing = F)[1:k]]))), colNumOfClassvar] )
  pred <- c(pred, names(which.max(
        sample(  # randomly shuffling if probability of 2 factors is same
        table(modHouse[ # to read the value of independent var 
          as.numeric( # as rownames gives string so converting to numeric
            rownames(  # reading rownames of matrix
              as.matrix( # converting to matrix to read row name
                h.dst.t[i # i represents test data's row number
                        , order(h.dst.t[i, ] , decreasing = F)[1:k]]))), colNumOfClassvar] ))))
                        # for order function refere example from rcran it will give first k min values
              )# end of vector this can be removed if apply function can be applied
                        
}

confmatrix(modHouse[testsetrownums,colNumOfClassvar], pred )

head(prob_pred)


prob_pred <- prob_pred/k
pred <- prediction(prob_pred[,2], modHouse[testsetrownums,colNumOfClassvar])
plot(performance(pred,"tpr","fpr"))
abline(0, 1, lty = 2)
AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ", AUCLog2, "n")


library(caret)
library(klaR)
library(rpart)
library(ISLR)
train_control <- trainControl(method = "loocv", number =  20)
model <- train(Class ~ ., data = modHouse, trControl = train_control, method = "knn")
print(model)

#------------------functions-------------------------------

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}



UCI