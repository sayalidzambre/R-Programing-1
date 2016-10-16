#Create Matrix
#BY DEFAULT by column is True
A <- matrix(c(1, 3, 2, 2, 8, 9), nrow = 2)
A
#BY DEFAULT by column is True
A <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3)
A

#by row values enter
A <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3, byrow = T)
A

#MAtrix addition
A + A


#Matrix sub
A - matrix(c(1, 3, 2, 2, 8, 9), ncol = 3, byrow = T)

#Multiply number to matrix
3 * A

#Matrix multiplication
#when we only do * then each number of Matrix A gets multipled by element element of B
A*A
# modulus multiplication

A%*%A
#Transpose of matrix
Abar <- t(A)
A%*%Abar

#matrix of size 2 x 3 where all values should be 1
matrix(1, nrow = 2, ncol = 3)

#diagonal matrix
diag(c(1,4,8), 3, 3)

diag(c(1,4,2))











