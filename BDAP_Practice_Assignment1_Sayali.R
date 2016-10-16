# 1. Assign first five prime numbers to an object named 'prime'.
  prime <- c( 2, 3, 5, 7, 11)
#Coerce object 'prime' to character data type and assign the output to 'character' and then check its class.
  class(prime)
  prime <- as.character(prime)
  class(prime)
#3. Check if elements in 'prime' are > 5 and save the output in 'logical'.
  prime <- as.numeric(prime)
  logical <- prime > 5

#4. Create an object 'inflation' containing 'RBI predicts the inflation 
#     rate to reduce in the coming quarter '. Then replace reduce with moderate.

  inflation <- "RBI predicts the inflation rate to reduce in the coming quarter "
  inflation <- gsub("reduce", "moderate", inflation)
  inflation

#5.Vector named 'vowels' containing all the vowels in English language.
  vowels <- c('a','e','i','o','u')
  
#6. Create a vector 'numbers' of numbers 1 and 2, 10 times each.
  
  numbers <- rep(1:2 , 10)
  
#7. Matrix of dimension 7 x 8, elements being numbers 1-7.
  matrix(1:7, byrow=TRUE, nrow=7, ncol = 8 )
  
#8. Create an array with dimensions 2 x 3 x 4 and elements 1-15.
  arr = array(1:15, dim=c(2,3,4))
  
#9. Clear the workspace.
  rm(list = ls())
  
#  10. Create a data frame of 4 rows consisting of four vectors
#  a. Customer.Id
#  b. Names
#  c. Age
#  d. Default.prob
  Customer.Id <- c()
  Name <- c()
  Age <- c()
  Default.prob <- c()
  
  data.frame(Customer.Id,Name, Age, Default.prob)
  
#11. Download data from file "LungCapData.csv" using read.table ( . ) argument and save it as data1
  data1 <- read.table(file.choose(),header=TRUE, stringsAsFactors=FALSE, sep=",")

#12. Download data using read.clipboard ( . ) and save it as data2
  data2 <- read.delim("clipboard")
 
  library("psych")
  data2 <- read.clipboard(header=TRUE,sep='\t')

  ## All variations of read.clipboard
  read.clipboard(header = TRUE, ...)   #assumes headers and tab or space delimited
  read.clipboard.csv(header=TRUE,sep=',')   #assumes headers and comma delimited
  read.clipboard.tab(header=TRUE,sep='\t')   #assumes headers and tab delimited
  #read in a matrix given the lower off diagonal
  read.clipboard.lower(diag=TRUE,names=FALSE) 
  read.clipboard.upper(diag=TRUE,names=FALSE)
  
  #read in data using a fixed format width (see read.fwf for instructions)
  read.clipboard.fwf(header=FALSE,widths=rep(1,10),...)  
  
#13. Create a vector 'vec1' and 'vec2' with elements 1 to 15 and 115 to 101.  
  vec1 <- c(1:15)
  vec2 <- c(115:101)
  
#14. Create a vector 'vec3' of the sum of the log values of vec1 and vec2 in one argument and print the result.
  vect3 <- log(vec1) + log(vec2)
  print(vect3)
  
#15. Print the 7th element in vec2.
  vec2[7]
  
#16. Create matrix 'mat1' by combining the vec1 and vec2 column wise.
  mat1 <- matrix(c(vec1, vec2),byrow = F, ncol = 2)
  
#17. Change the dimensions of mat1 to 5 x 6 and print mat1.
  mat1 <- matrix(mat1, nrow = 5, ncol = 6)
  
#18. Generate a 5 x 5 matrix 'mat2' with elements 1:5 in the diagonal and other elements being 0.  
  mat2 <- diag(1:5, nrow = 5, ncol = 5)
  
#19. Add another column of elements 6:10 in mat2, making it 5 x 6 matrix.
  mat2 <- cbind(mat2, c(6:10))
  
#20. Print the values of 4th column of mat2.
  mat2[,4] 
  
#21. Find which elements in mat2 are greater than or equal to 5.
  mat2[mat2 > 5]
  #lenth is -  
  length(mat2[mat2 > 5])
  sum(mat2 > 5) 
  
#22. Download and load the "psych" package by using the function
#  a. install.packages (" ")
#  b. library (" ")  
  install.packages("psych")
  library("psych")
  
#23. Download Retail Score data using read.clipboard ( . ) and save it as 'RSC'.
  library("psych")
  RSC <- read.clipboard(header=TRUE,sep='\t')
  
#24. Run describe ( . ) function on the data.
  describe(RSC, na.rm = T, interp = F, skew = T, ranges = T,
           trim = 0.1, type = 3, check = T, fast = NULL)
#25. Take a subset of the creddebt and othedebt column of the data and assign the values to 'credit.debt' and 'other.debt'.  
  attach(RSC)
  credit.debt <- as.numeric(creddebt)
  other.debt <- as.numeric(othdebt)
  
#26. Find the mean and median values of 'credit.debt' and 'other.debt'.
  mean(credit.debt)
  mean(other.debt)
  median(credit.debt)
  median(other.debt)
  
#27. Create a vector 'total.debt' by adding element to element of the two vectors, 'credit.debt' and 'other.debt'.
  total.debt <- credit.debt + other.debt
  
#28. Round of the elements in vector 'total.debt' in multiples of tens.
  total.debt <- 1000 %*% total.debt
  
#29. Paste the elements of the two vectors, 'credit.debt' and 'other.debt' using separator ",".
  paste(c(credit.debt, other.debt), ",")
  
#30. Create authors <- c("Andrie","Joris") lastnames <- c("de Vries","Meys")
#  Create a vector 'Names' whose elements will be "Andrie de Vries" and "Joris Meys"
  authors <- c("Andrie", "Joris")
  lastnames <- c("de Vries", "Meys")
  Names <- paste(authors , lastnames)

    
# 31Create firstnames <- c("Joris", "Carolien", "Koen") lastname <- "Meys" 
#Create a vector 'NAMES' whose elements will have "Jonas" added to all the elements of first names.
  firstnames <- c("Joris", "Carolien", "Koen") 
  lastname <- "Meys"
  Names <- paste(firstnames, "Jonas")
  
  
# 32. Clear your workspace
  rm(list = ls())
  
#33. Load the RetailScoreData file as 'Retail.data'.
  Retail.data = read.csv(file.choose())
  
#34. Create a data.frame 'Retail.3779 with all the observations where 
  #ncust is 3779. Hint : You need to change the data type of the column 'ncust'  
  attach(Retail.data)
  class(ncust)
  Retail.3779 <- as.data.frame(Retail.data[ncust == 3779 ,])
  ## Why to change data type of ncust??
  
#35. Sort the data.frame 'Retail.3779' in the decreasing order of variable 
  #'age' and assign it to Retail.3779.sort. 
  Retail.3779.sort <- Retail.3779[order(-Retail.3779$age),]
  
#36. See how many observations in 'Retail.3779' are employed for more than 10 years.
  length(Retail.3779[Retail.3779$employ > 10,])
  
#37. Find the mean of all observations in 'Retail.data' in variables 
  #'creddebt' and 'othdebt' grouped by 'ncust'.
  aggVales <- aggregate(Retail.data[, c("creddebt","othdebt")], list(Retail.data$ncust), mean)
  
#38. Split the 'Retail.data' using the split functions and assign the 5th data.frame
#  (sublist 5 - [[5]]) to 'Retail.3017'.  
  Retail.3017 <- split( Retail.data , Retail.data$ncust )[5]
  ## ??? data type of Retail.3017 id list
  ## ???? why (sublist 5 - [[5]])??
  
#39. Find summary statistics of the data
  summary(airquality)
  
  
#40. Find the following
#  a. Skewness
#  b. Kurtosis
#  Hint: use the package 'moments'
  library(moments)
  skewness(airquality, na.rm = T)
  kurtosis(airquality, na.rm = T)
  
#41. Draw a histogram of the following data
#  a. Ozone
#  b. Solar.R
#  c. Wind
#  d. Temp
  attach(airquality)
  colors <- c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
  hist(Ozone,  right=FALSE, col = colors , xlab = "Ozone" )
  hist(Solar.R,  right=FALSE, col = colors , xlab = "Solar.R")
  hist(Wind,  right=FALSE, col = colors , xlab = "Wind" )
  hist(Temp,  right=FALSE, col = colors , xlab = "Temp" )
  
  
#42. Find correlation and covariance matrix among the following variables
#  Ozone, Solar.R, Wind & Temp
  mat.cor <- data.frame(x1=rnorm(Ozone), x2=rnorm(Solar.R),
                        x3=rnorm(Wind), x4=rnorm(Temp))
  cor(mat.cor) 
  
  
#  install.packages("corrplot")
#  library('corrplot') 
#  corrplot(mat.cor, method = "circle") 
  
#43. Conduct the anova test for mean and Variance between branch 3,13, 15, 20 and 25  
  branch <- c(3,13, 15, 20, 25)
  ##?? what is anova ? use function aov(branch[1:4] ~ branch[2:5])

  