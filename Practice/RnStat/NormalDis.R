# n = 30 and p = 0.7
# Draw normal distribution


normaldist <- function(n,p){
  mu <- n*p
  sig <- sqrt(n*p*(1-p))
  plot(density(rnorm(n,mu, sig)))
}


n = 30
p= 0.7
normaldist(n,p)

binomialFunc <- function(n,p){
  plot(density(rbinom(30,1,0.7)))
  
}
binomialFunc(30,0.7)

val <- c()
for(i in 1:30){
  val[i] = dbinom(i, size=30, prob=0.7) 
}
plot(val)