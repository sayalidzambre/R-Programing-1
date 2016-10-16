# . Create two objects ‘a’ & ‘b’ having values 2 and 7 respectively.
#2. Create an object ‘A’ having value 4 using both types of assignments.
#3. Overwrite object ‘a’ with ‘A’.
#4. Perform the following operations on objects ‘a’ & ‘b’.
#a. Addition.
#b. Subtraction.
#c. Multiplication.
#. Division.


#1. 
a <- 2
b <- 7

#2
A <- 4
4 -> A
A = 4

#3.
a <- A
a

#4
a + b
a - b
a * b
a / b

#5
B <- 8
B ** 2
sqrt(B)

B ** 3
B ** (1/3)

log(B, base = exp(2))
log10(B)
log2(B)
exp(B)

rm(A)
rm(B)


#1.2
#1

x <- c (1, 2, 3, 4, 5) 
2*x

#2
vowels = c("a", "e", "i", "o", "u")


#3
val <- c(0,1)
rep.num <- c(rep(val, 5))

rep.num

# 4

seq.1 <- seq(from = 1, to = 25, by = 2)
length(seq.1)
seq.1[6]
seq.1[-c(3,6,9)]
#5

seq.2 <- seq(from = -3, to = 3, length.out = 10)
seq.2 
seq.2[seq.2 > 1]

arr <- c( 2, 1, 3, 6, 4, 5)
mat.1 <- matrix(arr, nrow = 2, byrow = T, ncol = 3)
mat.1
mat.2 <- t(mat.1)
mat.2

mat.dia <- diag(x = 1:4, 4, 4)
mat.dia
mat.12 <- matrix(1:12, nrow = 4, byrow = T, ncol = 3)
mat.12
mat.dia %*% mat.12 


x <- array(1:24, dim=c(2,3, 4))
x


d = read.table("C:\\Users\\OWNER\\Desktop\\BDAP\\RetailScoreData.txt", 
               sep=";",
               fill=FALSE, 
               strip.white=TRUE)




a <- AirPassengers
a

mean(AirPassengers)
sd(AirPassengers)
quantile(AirPassengers)
length(AirPassengers)
range(AirPassengers)


aitrQ <- airquality
names(airquality)

#Dataset from airQ

mean(subset(aitrQ, )$Ozone, na.rm = T)
mean(subset(aitrQ, )$Solar.R, na.rm = T)
mean(subset(aitrQ, )$Wind, na.rm = T)
mean(subset(aitrQ, )$Temp, na.rm = T)
mean(subset(aitrQ, )$Month, na.rm = T)
mean(aitrQ$Day, na.rm = T)
airQ[, 1]


sum(is.na(aitrQ[, 1]))

summary(aitrQ)

mon <- as.factor(aitrQ$Month)
mon

luncap <- read.csv("C:\\Users\\OWNER\\Desktop\\BDAP\\LungCapData.csv")
attach(luncap)
mean(Age[Gender == "female"])

mean(luncap$Age[luncap$Gender == "male"])

luncap[Gender == "male" & Age > 15,]

femSmoke <- luncap[Gender == "female" & Smoke == "yes" & Caesarean == "yes",]
femSmoke
dim(femSmoke)



moreData <- cbind(luncap, femSmoke)
moreData <- rbind(luncap, femSmoke)
moreData



#1.6 and 1.7


numvec <- c(2,5,8,9,0,6,7,8,4,5,7,11)
charvec <- c("David","James","Sara","Tim","Pierre","Janice","Sara","Priya","Keith",
            "Mark", "Apple", "Sara")
gender <- c("M","M","F","M","M","M","F","F","F","M","M","F")
state <- c("CO","KS","CA","IA","MO","FL","CA","CO","FL","CA","WY","AZ")

nameData <- cbind(numvec, charvec, gender, state)
nameData
dataFrameName = data.frame(numvec, charvec, gender, state) 
dataFrameName

attach(dataFrameName)

dataFrameName[numvec < 5,]
subset.data.frame(numvec < 5)


dataFrameName[charvec == "Sara",]
dataFrameName[numvec == 5, c(2,4)]
dataFrameName[charvec != "Sara" & gender == "F" & numvec > 5, ]

subset(dataFrameName, charvec == "Sara")

person <- read.table("C:\\Users\\OWNER\\Desktop\\BDAP\\File_1.6.txt", sep = " ", header = T)
person

coffee <- c(3, 1, 2, 5, 0, 2, 0, 1, 3, 2)

person <- cbind(person, coffee)
newRow <- c(12, "M",28,4)

person <- rbind(person, newRow)
person[person$Gender != "<NA>", 2 ]

person[c(1,3,8),]




##Mist


vect1 <- c(1:20)
c(20:1)

c(1:20,19:1)

temp <- c(4, 6, 3)
vect2 <- rep(temp,  10)
rep(1:4, c(2,1,2,1))

rep(temp,times=c(10,20,30))

c(rep(4,10), rep(6,10))

#Create a vector of the values of ex
#cos(x) at x = 3, 3.1, 3.2, . . . , 6.

seqVect <- seq(from = 3, to = 6, by = 0.1)
seqVect

(exp(seqVect)* cos(seqVect))


c(0.1 ** seq(3, 36, 3) * 0.2** seq(1, 34, 3))

c(2 ** (seq(1,25,1)) / seq(1,25,1))

sumseq = c(20:100)
sum((sumseq ** 3) + (2*(sumseq ** 2)))


sumseq = c(1:25)

sum(((2**sumseq)/sumseq) + (3**sumseq)/(sumseq ** 2) )
class(ans)
ans


paste("level ", 1:30)
paste("fn ", 1:26)

x.rndm <- sample(0:999,250, replace = F, prob = NULL)
y.rndm <- sample(0:999,250, replace = F, prob = NULL)
y.rndm[2:250] - x.rndm[1:249]


sin(y.rndm[2:250])/cos(x.rndm[1:249])

sum(exp(-x.rndm[1:250])/x.rndm[1:250])

x.rndm[1:248] + 2 * x.rndm[2:249] - x.rndm[3:250]


