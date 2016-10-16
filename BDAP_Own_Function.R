addNumbers <-function(a,b) {
  c <- a + b
  return(c)
}

addNumbers(3,4)


addRange <- function(n){
  ans <- 0
  for (num in n) {
    ans <- ans + num
  }
  return(ans)
}

addRange(10:4)

addNumbersOfSameType <- function(option, range){
  ans <- 0
  if(option == "odd"){
    for(num in range){
      if(num %% 2 != 0){
        ans <- ans + num
      }
    }
  }else{
    
    for(num in range){
      if(num %% 2 == 0){
        ans <- ans + num
      }
    }
  }
  return(ans)
}

addNumbersOfSameType("even", 1:100)


circle <- function(r){
  area <- pi * r * r
  circum <- 2 * pi * r
  return(c(area, circum))
  
}
options(digits = 8)
circle(6)

## F to C Conversion
## c/5 = f-32/9

covertTemperature <- function(case, temp){
  ifelse((case == "F"), return(((temp*9)/5)-32 ), return(((temp-32)/9)*5 ))
}
covertTemperature("F", 100)

## User Input -
readUserInput <- function(){
  x <- readline("What is the value of x?")  
  y <- readline("What is the value of y?")
  print(as.character(x) )
}
readUserInput()

## while 1 - 

while(1){
  x <- readline("What is the value of x?")  
  if(x == 1)
    break
}

## repeat 
i <- -1
repeat {
  i= i + 1
  print(i)
  if(i > 100)
    break
}

##squared
sqr <- seq(1, 100, 2)
squared <- NULL

for(n in 1:50){
  squared[n] <- sqr[n] ^ 2 
}
squared




converttemp<-function(cel)
{
  f<-0
  f<-((9/5)*cel) + 32
  return(f)
}
converttemp(scan())