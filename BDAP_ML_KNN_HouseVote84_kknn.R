
install.packages("kknn")

#phat for kknn
library(kknn)
library(caret) 
modHouse=HouseVotes84
names(modHouse)
for(j in 2:17){
  levels(modHouse[,j])=c("n","y","abstain")
}

modHouse[is.na(modHouse)]="abstain"
modHouse

housetrain=sample(nrow(HouseVotes84),round(0.7*nrow(HouseVotes84),0))

kknnfit=train.kknn(Class~.,data=modHouse)
kknnfit

kknnmodel=kknn(Class~.,train=modHouse[housetrain,],
               test=modHouse[-housetrain,],kernel="optimal",k=7)

kknnpredClass=kknnmodel$fitted.values

confmatrix(HouseVotes84$Class[-housetrain],kknnpredClass)

kknnphat=(kknnmodel$prob)[,2]
table(kknnpredClass,(kknnphat>=0.5))


confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}