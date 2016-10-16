myFirstFunction <- function(){
  
  sayali <- c("Sayali", "Dnyandeo", "Zambre", 28)
  exStr <- "Sayali"
    
  print(class(sayali))
  
  print(sayali)
}

firstVectorFunction <- function(){
  vect <- c('a', 'b', 'c', 21)
  print(class(vect))
  print(vect)
}


firstListFunction <- function(){
  lst <- list(c(1, 2, 3), 21.31, sin)
  print(class(lst))
  print(lst)
}

firstMatrixFunction <- function(){
  mtx <- matrix(c('a', 'b', 'c','a','b','c'), nrow = 2, ncol = 3, byrow = TRUE)
  print(mtx)
}

firstDataFunction <- function(){

}



createFirstPie <- function(){
  
  dat <- read.csv('D:/RFolder/repos/R-Programing-1/Test.csv', sep='\t', header=F, col.names=c('inst','freq') );
  dat;
  ##   inst Distance
  ## 1 push  210
  ## 2  mov  270
  ## 3  jmp  150
  ## 4   cp  190
  cols <- rainbow(nrow(dat));
  pie(dat$freq,labels=paste0(round(dat$freq/sum(dat$freq)*100,2),'%'),col=cols);
  legend('bottom',legend=dat$inst,pch='■',ncol=nrow(dat),bty='n',col=cols);
}




