

# 1. Read data set
GD <- read.csv("C:\\Users\\OWNER\\Desktop\\BDAP\\Gold_Deposit.csv", header = T)
dim(GD)
# Subset 80 % for creating model
GD_training <- GD[1:44,]
dim(GD_training)

# Subset test data 20 %
GD_test <- GD[45:54,]
dim(GD_test)

#read actual values of Gold Deposite
actual_GD <- GD_test$GD

# Biulding model
modela <- glm(GD ~ As + Sb, data = GD_training, family = binomial(link = "logit"))
summary(modela)
dim(GD_test)
# Predict values for remaining 20% Data set 
model_pred_probs1 <- c()
model_pred_probs1 <- predict(modela, GD_test)
model_pred_probs1
length(model_pred_probs1)
# Create true vector
model_pred_GD <- rep(F, 10)
length(model_pred_GD)

model_pred_GD[model_pred_probs > 0.5] = T
length(model_pred_GD)
model_pred_GD


rm(list = ls())




# Smarket Data

library(ISLR)
Smarket

length(Smarket) * 0.8
# Split data in 80 and 20 %

SM_Model_data <- Smarket[1:1000,]
SM_Test <- Smarket[1001:1250,]
SM_Actual <- SM_Test$Direction

model <- glm(Direction ~ Today, data = SM_Model_data, family = binomial(link = "logit"))
model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag3 + Volume, data = SM_Model_data, family = binomial(link = "logit"))
summary(model)

model_pred_probs <- c()
model_pred_probs <- predict(model, SM_Test)
model_pred_probs

model_pred_Dir <- rep("Down", 250)
model_pred_Dir[model_pred_probs > 0.5] = "Up"

table(SM_Actual, model_pred_Dir)


# Carseat Data 

dim(Carseats)

model_data <- Carseats[1:320,]
test_data <- Carseats[321:400,]

Actual_Urban <- test_data$Urban
Actual_US <- test_data$US

model <- glm(Urban ~ ShelveLoc, data = model_data, family = binomial(link = "logit"))
model <- glm(US ~ ShelveLoc, data = model_data, family = binomial(link = "logit"))
summary(model)

model_pred_probs <- predict(model, test_data)
model_pred_probs

model_pred_Dir <- rep("No", 80)
model_pred_Dir[model_pred_probs > 0.5] = "Yes"

table(Actual_Urban, model_pred_Dir)
table(Actual_US, model_pred_Dir)



LR <- read.csv(file.choose(), header =  T)
dim(LR)

model_data <- LR[1:16,]
test_data <- LR[17:23,]

Actual_approval <- test_data$Loan.approval


model <- glm(Loan.approval ~ Salary + Age, data = model_data, family = binomial(link = "logit"))
model <- glm(Loan.approval ~ Age, data = model_data, family = binomial(link = "logit"))
model <- glm(Loan.approval ~ Salary, data = model_data, family = binomial(link = "logit"))
summary(model)

model_pred_probs <- predict(model, test_data)
model_pred_probs

model_pred_Dir <- rep("N", 7)
model_pred_Dir[model_pred_probs > 0.5] = "Y"

table(Actual_approval, model_pred_Dir)
