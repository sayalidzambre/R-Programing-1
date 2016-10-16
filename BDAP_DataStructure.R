# Vector  - Same data type in single row array
# Matrix  -  same data type in multiple row
# Array   -  
# List
# DataFrame
# Time Series

# Declaring vector

A  <- c(10, 12, 1)
A
class(A)

B <- c("Sayali", "Zambre")

vect_int <- c(1:88)
vect_color <- c("Red", "Blue", "Green")
vect_logic <- c(T, F, T, F)

vect_int
vect_color
vect_logic

vect_seq_int <- c(1:50) 

vect_seq <- seq(from = 1, to = 40, by = 4)
vect_seq

vect_rep <- rep(1, times = 10)
vect_rep


veect_sep_req <- rep(seq(1, 10 , 3), 2)
veect_sep_req

vect_seq_int[-3]

mat_1 = matrix(1:9 , byrow = T, nrow = 3)
mat_1
class(mat_1)
"2,2"
mat_1[2,2]
"row"
mat_1[2,]
"col"
mat_1[,2]











