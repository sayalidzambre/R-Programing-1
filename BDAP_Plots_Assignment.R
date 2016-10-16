#1 X2 - 3x + 3 and x2 - 2x + 2
plotGraph <- function(){
  x <- c(1:10)
  y1 <- c()
  y2 <- c()
  for(i in x){
    y1 <- c( y1, (i * i)-(3 * i) + 2)
    y2 <- c( y2, (i * i)-(2 * i) + 3)
  }
  plot(x,y1,type="l",col="red")
  lines(x,y2,col="green")
}
plotGraph()

#2 airquality - histogram

airquality
attach(airquality)

drawHistogram <- function(xname, borderColor, color){
  hist(xname, main = paste("Histogram of ", deparse(substitute(xname))) , 
       xlab = xname, border = borderColor, col = color)  
}
drawHistogram(Ozone, "blue", "green")
drawHistogram(Solar.R, "blue", "yellow")
drawHistogram(Wind, "blue", "red")
drawHistogram(Temp, "blue", c("red","yellow","blue"))

library('corrplot')
corrplot(cor(airquality, method = c("pearson", "kendall", "spearman")), method = "circle")

cov(airquality, use="complete.obs")

#3

library("MASS")
names(painters)
attach(painters)
levels(School)

barplot(table(Composition))

#4. Create a following pie-chart using the data pieVec = (6,1,9,4,3,5,2)
pieVec <- c(6,1,9,4,3,5,2)
lbls <- c("Sunday", "Monday, Tuesday", "Wednesday", "Thrusday", "Friday", "Saturday")
pie(pieVec, labels = lbls, main="Pie Chart of pieVec")
library("plotrix")
pie3D(pieVec,labels=lbls,explode=0.1, main="Pie Chart of pieVec")

#5

attach(mtcars)
names(mtcars)

boxplot(wt~cyl ,data=mtcars, main="Car Milage Data", 
        ylab="Number of Cylinders", xlab="Weight")

#6. Load The BOD data from the R datasets which gives 
#biological oxygen demand (mg/l) vs time (days) in an evaluation 
#of water quality.
attach(BOD)
plot(demand, Time, ylim = c(0, 20), xlab = "Days", ylab = "BOD")


str(airquality)

calculateMeanOfCol <- function(dataFm){
  colNames <- names(dataFm)
  for(col in colNames){
    colWithDataType <-eval(parse(text = col))
    if(is.numeric(colWithDataType) || is.integer(colWithDataType)){
      print(mean(colWithDataType, na.rm = T))
    }
  }
}
calculateMeanOfCol(airquality)

library("swirl")
swirl()
bye()





