

## Practice session -2 

#1. Read hills.txt

#2. Read the file, assigning the result to the object hills
hills <- read.table(file.choose(), sep = " ", header = T)

#3. Examine the object. Note column and row names
names(hills)
#row names - 
rownames(hills)
hills[,1]

#4. Construct a scatterplot matrix How many variables do you think such plots are suitable for?
attach(hills)
names(hills)  
plot(hills$dist, hills$time, main="Scatterplot", 
     xlab="Distance Travelled ", ylab="Time Taken ", pch=19)
pairs(hills)
#5. Make the columns of the hills object available by name
attach(hills)
search()

#6. Construct a scatter plot. The function call in this way 
#means the first argument is the horizontal axis
plot(hills$dist, hills$climb, main = "Climb vs Dist", xlab = "Dist", ylab = "Climb", cex = 0.5)
plot(hills$climb, hills$time, main = "Dist vs Time", xlab = "Climb", ylab = "Time", cex = 0.5)


#7. Interact with the plot to label points - right click to finish
identify(dist,time,row.names(hills))

#8. Compute a linear regression of time against distance
lm(time~dist)

#9. Obtain more information about the regression
summary(lm(time~dist))

#10. Add the least squares regression line - note anonymous function call
plot(dist, time)
abline(lm(time ~ dist))

#11. Obtain some diagnostics plots - note the different arguments to the plot function. 
#Be aware of the prompt in the Console.
plot(lm(time ~ dist))

#12. There are many pre-defined system objects. Display the value of pi - note that this is a reserved word

#13. List objects in current working space
for ( obj in ls() ) { print(get(obj)) }

#14. Display the object ls
ls()

#15. Create a copy of the hills object
library(data.table)
copy_hills <- copy(hills)

#16. List objects in current working space
for ( obj in ls() ) { print(get(obj)) }  

#17. Delete the copy. Note that there is no undelete functionality.
rm(copy_hills)

#18. Finally, quit this session.
q()

#19. Create a vector of coefficients for a quadratic equation, using the sample function.
#Here, we draw a sample of size 3 from ???20, ???19, . . . , 19, 20 with replacement
coeffs <- sample(-20:20,3,replace=T)

#20. Determine the class of the object coeffs.
class(coeffs)

#21. Determine the length of the object coeffs.
length(coeffs)

#22. Determine the names associated with the vector
names(coeffs)

#23. Assign some names. Note the function call occurring on the right hand of the assignment operator.
names(coeffs) <- c("a","b","c")

#24. What is the class of the names of the vector coeffs?
class(names(coeffs))

#25. Prepare to plot the equation, by constructing a regularly spaced vector for 
#the horizontal axis
x <- seq(-7,7,length=200)

#26. Evaluate the quadratic at each point in vector x
y <- coeffs[1]*x^2+coeffs[2]*x+coeffs[3]

#27. Construct the plot plot(x,y) plot(y=y,x=x)Note the different styles of function call.
plot(x,y)
plot(y=y,x=x)

#28. Create a vector of type character, and display the second element
c('a','b','c')[2]

#29. Create a logical vector
vect1 <- c(T, F, T, T)

#30. negate this vector
!vect1

#31. Compute the truth table for logical AND
vect1 & !vect1

#32. Explore arithmetic with logical and numeric
vect1 + c(1:4)

#33. Compute the intersection of {1, 2, . . . , 10} and {5, 6, . . . , 15}
intersect(1:10, 5:15)

#34. Create a factor
fact <- factor(c('a','b','c'))

#35. Examine the representation of the factor
unclass(fact)

#36. Examine the row and column names of the hills object
row.names(hills)
names(hills)

#37. Create data frames. Note row and column names
x1.df <- data.frame(1:10,I(letters[1:10]),factor(letters[1:10]))
x2.df <- data.frame(X1=1:10,X2=I(letters[1:10]),X3=factor(letters[1:10]))

#38. Compute the mean of column X1
mean(x2.df$X1)

#39. Create a matrix
#Examine the default dimension names of the matrix
x.mat <-matrix(1:12,nrow=3,ncol=4)
dimnames(x.mat)

#40. Assign some dimnames to the matrix
dimnames(x.mat) <- list(letters[1:3],letters[1:4])

#41. Combine matrices
xx <- cbind(x.mat,x.mat)
xxx <- rbind(x.mat,x.mat)
rbind(xx,xxx)

#42. Explore indexing
x <- 1:10
names(x) <- letters[x]
x[1:3]
x[c(1,10)]
x[c(-1,-2)]
x[ x > 5]
x[c("a","d")]
x[]
jj1 <- matrix(1:100,ncol=10)
jj1[1:5,]
jj1[1:4,x[x <3]]

#43. Compute sums of the columns of the hills object
lapply(hills,function(x, y) sum)
sapply(hills,function(x, y) sum)

#44. Create a list, and examine elements
lst <- list(a=1:10,b=letters[1:9],b=matrix(1:10,ncol=2))

#45. Element-wise arithmetic with matrices
x.mat <- matrix(1:10,ncol=2)
x.mat+1
x.mat + x.mat

#46. Matrix multiplication
x.mat %*% t(x.mat)

#47. Compute row and column sums of a matrix
apply(x.mat,1,sum)
apply(x.mat,2,sum)

#48. Construct a 2×2 data frame, X say. Experiment with X^(1:K), 
#where K takes values 1:4. How does the recycling rule behave? 
#What happens if you remove the brackets from the command?
x <- data.frame(1:10, LETTERS[1:10])
max.length <- max(sapply(x, length))
lapply(x, rep, length=max.length)

#49. The function system.time returns timings for R operations. 
#Examine the help system about this function. For a 107 × 2 matrix, X, 
#and vector y of length 107/2 compute (a number of times) X ty using 
#matrix multiplication and the function crossproduct. Which is quicker?
x <- matrix(1:214, 107, 2)
y <- c(1:107)

system.time(x)
system.time(y)

#50. Generate a sample of random normal deviates, and a sample of random exponential deviates.

r_double_exp <- function(n=1) {
  X <- log(runif(n))
  Y <- sample(c(-1,1), n, replace=TRUE)
  return(X*Y)
}

my_rnorm <- function(val) {
  finished <- FALSE
  while (!finished) {

    y <- r_double_exp(n = val)
    u <- runif(n = val)
    p_over_Mq <- exp(-y^2/2 + abs(y) - 0.5)
    finished <- (u < p_over_Mq)
  } 
  return(y)
}
my_rnorm(50)

x <- rnorm(50)
y <- rnorm(50,0,1)

#51. Compute some summaries
mean(x)
sqrt(var(x))
cor(x,y)
cor(cbind(x,y))

#52. Try the summary function
summary(x)
summary(cbind(x,y))

#53. Let X ??? N (0, 1) and Y ??? Exp(2). Compute P (X > 1.644) and find q such that P (Y < q) = 0.75.
1 - pnorm(1.644)
qexp(0.75,2)

#54. Use the sample function to obtain a random sample of 10 realisations in a biased coin experiment
sample(c("Head","Tail"), 10, prob=c(0.3,0.7),replace=T)

#55. Load the package MASS and examine the help
help(package="MASS")

#56. Experiment with set.seed 
set.seed(1) 
runif(10) 
set.seed(1) 
runif(10) 
runif(10)

#57. Test to see if sample x is consistent with an Exp(1) distribution using a QQ plot
plot(qexp(ppoints(x),1),sort(x))
abline(0,1)

#58. Compare the two samples with a QQ plot
qqplot(x,y)
abline(0,1)

#59. Compare the two samples with box plots
boxplot(x,y)

#60. A simple alternative to display the two samples in one command
plot(c(x,y),rep(0:1,c(length(x),length(y))),xlab="",ylab="")

#61. Plot a histogram of x and a box plot of y, in the same figure.
par(mfrow=c(2,1))
hist(x)
boxplot(y)

#63
install.packages("mlbench")
library(mlbench)
PimaIndiansDiabetes
data("PimaIndiansDiabetes")

#64
attach(PimaIndiansDiabetes)
plot(triceps,glucose,type="n")
plot(triceps[diabetes=="neg"],glucose[diabetes=="neg"],xlab="Tricep",ylab="glucose")
points(triceps[diabetes=="pos"],glucose[diabetes=="pos"],xlab="Tricep",ylab="glucose",col=2,pch=2)
legend(40,50,c("Diabetes","Healthy"),pch=1:2)

#65 a

View(ChickWeight)
x005 <- ChickWeight[ChickWeight$Chick==34,]
x005
plot(x005$weight,x005$Time)

names(ChickWeight)
#b
x006 <- ChickWeight[ChickWeight$Diet==4,]
x006
boxplot(x = x006$Time)

#c
x007 <- split(x = x006,f = x006$Time)
x007
mweight <- c()
mtime <- c()
for (var in x007) {
  mweight <- c(mweight,mean(var$weight))
  mtime <- c(mtime,var$Time[1])
}
print(mweight)
print(mtime)
plot(mweight,mtime)

#d
x008 <- ChickWeight[ChickWeight$Diet==2,]
x008
x009 <- split(x = x008,f = x008$Time)
x009
mweight01 <- c()
mtime01 <- c()
for (var in x009) {
  mweight01 <- c(mweight01,mean(var$weight))
  mtime01 <- c(mtime01,var$Time[1])
}
print(mweight01)
print(mtime01)
points(x = mweight01,y = mtime01,pch = 3,col = "red")

#e
legend(x = 175,y = 20,legend = c("Group 4","Group 2"),pch = c(1,3),col = c("Black","Red"))

#66
disjoint_func <- function()
{
  a <- sample(1:100, 8, replace = T)
  b <- sample(1:100, 5, replace = T)
  if (length(intersect(a,b)) == 0)
  {
    cat("sets are disjoint ", a)
  }else
  {
    cat("sets are not disjoint ",b)
  }
}
disjoint_func()

#67

calculateroots <- function(a, b, c){
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)
  root <- b %*% b - 4 %*% a %*% c
  if (root > 0){
    temp1 <- sqrt(root)
    root1 = (-b +  temp1) / (2 %*% a) 
    root2 = (-b -  temp1) / (2 %*% a) 
    
    return(c(root1, root2))
  }else if(root == 0){
    
    return()
  }else{
    temp1 <- complex(sqrt(root))
    root1 = (-b +  temp1) / (2 %*% a) 
    root2 = (-b -  temp1) / (2 %*% a)
    return(c(root1, root2))
  }
}
calculateroots(2,6,4)
calculateroots(scan(nmax = 1), scan(nmax = 1), scan(nmax = 1))

x=seq(-5,5,0.1)
plot(x,pnorm(x,0,1),type="l",col="blue")



#68

replaceValue <- function(){
  randam <- rnorm(50,0,1)
  rand <- log(randam, exp(1))
  rand_replace <- ifelse(rand == "NaN", 9999, rand)
  rand_replace
}
replaceValue()

#69
sin_func <- function(){
  rand <- runif(10000, 0, 1)
  y <- c()
  time.1 <- system.time(
          for(x in rand){
            y <- c(y,sin(x))
          }
         )
  time.2 <- system.time(y <- sin(rand))
  class(time.1)
  
}
sin_func()

#70.
comp_interest <- function(P,r,n,t)#Principal, RoI, Compounding Frquency, Time in Years.
{
  A = P*(1+r/n)^(n*t)
  return(A)
}

comp_interest(1000,0.08,12,2)



#71.
Highest_no_index <- function()
{
  print("Enter a vector of any length")
  num <- scan()
  print(class(num))
  highest_no <- which.max(num)
  print("Highest no is at")
  return(highest_no)
}
Highest_no_index()


