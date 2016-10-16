library(mlbench)

head(iris)

boxplot(iris[,-5])

x <- rnorm(1000)
hx <- dnorm(x)

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", 
     main="Comparison of t Distributions")


aov()

#-----------------------------------------

library(neuralnet)

train <- as.data.frame(runif(50, min = 0, max = 100))

train_output <- sqrt(train)
train_data <- cbind(train, train_output)
colnames(train_data) <- c ("Input", "Output")

# Train neural network

net.sqrt <- neuralnet(Output ~ Input, train_data, hidden = 10, threshold = 0.01)
plot (net.sqrt)


test_data <- as.data.frame((1:10)^2)
net_result <- compute(net.sqrt, test_data)

ls(net_result)

print(net_result$net.result)

ans <- cbind(test_data, sqrt(test_data), net_result$net.result)
colnames(ans) <- c("actual","sqrtroot", "pred")
ans
#-------------------------------------------------------------------------


library(neuralnet)

train <- as.data.frame(runif(180, min = 0, max = 1000))

train_output <- (train)^(1/3)
train_data <- cbind(train, train_output)
colnames(train_data) <- c ("Input", "Output")

# Train neural network

net.sqrt <- neuralnet(Output ~ Input, train_data, hidden = 10, threshold = 0.01)
ls(net.sqrt)
plot (net.sqrt)


test_data <- as.data.frame((1:10)^3)
net_result <- compute(net.sqrt, test_data)

ls(net_result)

print(net_result$net.result)

ans <- cbind(test_data, (test_data)^(1/3), net_result$net.result)
colnames(ans) <- c("actual","sqrtroot", "pred")
ans

#----------------------------------------------------


library(neuralnet)

train <- as.data.frame(runif(180, min = 0, max = 100))

train_output <- log10(train)
train_data <- cbind(train, train_output)
colnames(train_data) <- c ("Input", "Output")

# Train neural network

net.sqrt <- neuralnet(Output ~ Input, train_data, hidden = 10, threshold = 0.01)
ls(net.sqrt)
plot (net.sqrt)


test_data <- as.data.frame(1:10)
net_result <- compute(net.sqrt, test_data)

ls(net_result)

print(net_result$net.result)

ans <- cbind(test_data, log10(test_data), net_result$net.result)
colnames(ans) <- c("actual","sqrtroot", "pred")
ans






