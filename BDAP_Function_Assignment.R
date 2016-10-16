#1. Create a function in R to compute the compound interest.

calcCompoundInt <- function(principal, rate, years){
  if(principal != 0 && rate != 0)
    finalAmt = principal * ( 1+rate ) ^ years 
  
  return(finalAmt)
}
calcCompoundInt(scan(nmax = 1),scan(nmax = 1),scan(nmax = 1))

#2. Calculate the factorial of a number by creating a function 'calFactorial'.

calFactorial <- function(){
  number <- as.integer(readline("Enter Number whose factorial to be calculated: "))
  ans <- 1
  if(number < 0){
   print("Factorial not calculated.") 
  }else{
    if(number != 0){
      for(i in 1:number){
        ans <- ans * i
      }
    }
    return(ans)
  }
}

calFactorial()

#3

calculateRatio <- function(numVect){
  return((mean(numVect) / sd(numVect)) * sqrt(252))
}
numVect <- c(-171.47, 37.24, 265.20, -393.14, 54.65, -183.08, 116.95, 214.19, 356.28, 300.74, 144.74, -270.43, 243.06, 188.60, 373.49)
calculateRatio(numVect)


#4
calculateValue <- function(n) {
  ans <- 0
  for (s in 1:n) {
    for(r in 1:s){
      ans = ans + ((s *s) / (10 + (4 * r * r * r)))
    }
  }
  return(ans)
}
calculateValue(scan(nmax = 1))

#5 
calculateroots <- function(a, b, c){
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)
  
  temp1 <- sqrt(b %*% b - 4 %*% a %*% c)
  root1 = (-b +  temp1) / (2 %*% a) 
  root2 = (-b -  temp1) / (2 %*% a) 
  
  return(c(root1, root2))
}
calculateroots(2,6,4)
calculateroots(scan(nmax = 1), scan(nmax = 1), scan(nmax = 1))

