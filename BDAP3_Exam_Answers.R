# Sayali Zambre


#1. Write a function which takes a numeric vector as an input and 
#prints out one number per line,
#with its square and cube alongside it.

calculatesquare <- function(numVect){
  numvect <- c(1,4,2,6,7,9)
  for(x in numvect)
  { 
    print( c(x, x^2,x^3))
  }
}
numvect <- c(1,4,2,6,7,9)
calculatesquare(numvect)


#2.

yVector <- sample(0:999, 250, replace = T)
#a. Pick out all the values from yVector which are less than 600. 
#Also print their index positions.

values <- yVector[yVector>600]
position <- which(yVector>600)
matrix(c(values,position ),length(values),2)

#b. How many numbers are divisible by 2.
length(yVector[yVector %% 2 == 0])


#c. Sort the yVector in descending order.
sort(yVector,decreasing = T)

# d. Pick out the elements at index positions 2, 5, 8, 11, ...
yVector[seq(2,250,3)]



# 3 

x <- runif(10000,1,10)

#a. Length of x.
length(x)

#b. Number of values in vector x below 2, above 7 and 
#both below 2 as well as above 7.
length(x[x<2])
length(x[x>7])
length(x[x<2 | x>7]) 

#c. Number of times the value 4 occurs in x.
length(x[x==4])


#d. Print the elements of the vector x from position 5001 to 5010.

x[5001:5010]

#e. Print the smallest value and largest value present in x.
x[which.min(x)]
x[which.max(x)]

# f. Print all the quartiles of x.

quantile(x)

#g. Create another vector which contains all values of x between 4 and 7.
newVector <- x[x>4 & x < 7]

#h. Value closest to 5 in x.
x[which.min(abs(x-5))]

# 4.

A <-matrix(rep(c(10,-10,10),15),nrow = 15,ncol = 3, byrow = TRUE)
dim(A)
A
At <-t(A)
AtA <- At %*% A
AtA
dim(AtA)

# 5.

x <- rnorm(10, 20, 5)
y <- 2.5 * x - 10 + rnorm(10, 0, 9)
x1 <- runif(8, 15, 25)
y1 <- 2.5* x1 - 1.0 + runif(8,-6,6)
x2 <- runif(8,15,25)
y2 <- 2.5* x2 - 1.0 + runif(8,-6,6)

plot.new()
par(mfrow = c(2,2))

#
plot(x, y, main = "y vs x", cex=0.5,pch=1, col=1  )

#
plot(x1, y1, main = "y1 vs x1", cex=0.5,pch=3, col="red")

#
plot(x2, y2, main = "y2 vs x2", cex=0.5, pch=5, col="blue")

#
plot(x, y, pch=5, main = "All 3 plots combined", cex=0.5)
points(x1, y1, pch=3, col="red")
points(x2, y2, pch=5, col="blue")
legend("topleft", legend = c("original", "one", "two"), pch = c(1, 3,5), col= c(1, 2, 4))

# 6.
library(dplyr)
attach(mtcars)
df1 <- mtcars

#a Change the variable name mpg to Miles_Per_Gallon. Save the result as df2.  
head(df1)
df2 <- rename(mtcars,Miles_Per_Gallon = mpg )
df2

#b Using df1, subset all the variables whose mpg is greater than 20. Save the result as df3.  
df3 <- filter(mtcars, mpg>20)
df3

#c. Using df1, subset all the variables whose mpg is greater than 20 and the number of
#cylinders equal to 6. Save the result as df4.  
df4 <- filter(mtcars, mpg>20, cyl ==6)
df4

#d. Add a new variable mpg2 in df1 which contains the square values of mpg. Save the
#result as df5. 
df5<-mutate(mtcars, square_mpg = mpg*mpg)
df5
#e. Add new variables containing squares of mpg, cyl, disp in df1. Also subset rows whose
#mpg is less than 20. Save the resultant output as df6.  

df6<- mtcars %>% mutate(Square_mpg=mpg*mpg, Square_cyl = cyl*cyl, Square_disp=disp*disp) %>% filter(mpg<20)
df6

#Using df1, find the min, max, mean and sd of disp for each gear (i.e. using gear as factor). 
df7<- df1 %>% group_by(gear) %>% summarise(min_disp=min(disp),max_disp=max(disp), mean_disp=mean(disp,na.rm=T), sd_disp=sd(disp,na.rm=T))

df7