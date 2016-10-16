library(e1071)
library(ISLR)

makeNativeBays <- function(dataset){
  dataset <- iris
  #Shuffle Data - to unorder data
  set.seed(9850)
  random <- runif(nrow(dataset))
  randomDataSet <- dataset[order(random), ]
  
  #Calculate training dataset length
  trainLength <- round(nrow(randomDataSet) * 2/3,digits=0)
  
  #Create training and testing data set 
  trainDataSet <- randomDataSet[1:trainLength, ]
  testDataSet <- randomDataSet[(trainLength+1) : nrow(randomDataSet), ]
  
  model <- naiveBayes(Species~ . , data = trainDataSet)
  predicted <- predict(model,testDataSet)
  table(testDataSet[,5], predicted)
  
  
  model <- naiveBayes(Species~ Sepal.Length + Sepal.Width + Petal.Width , data = trainDataSet)
  predicted <- predict(model,testDataSet)
  table(testDataSet[,5], predicted)
  
  
  
  model <- naiveBayes(Species~ Sepal.Length + Sepal.Width , data = trainDataSet)
  predicted <- predict(model,testDataSet)
  table(testDataSet[,5], predicted)
  
  model <- naiveBayes(Species~ . , data = trainDataSet)
  predicted <- predict(model,testDataSet)
  table(testDataSet[,5], predicted)
}

makeNativeBays(iris)


iris
plot(iris)

library(corrplot)
corrplot(cor(iris[,-c(5)]))
cor(iris[,-c(5)])

library(ggvis)

iris %>%ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>%ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

####  As Sepal.Width