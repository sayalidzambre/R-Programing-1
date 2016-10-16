
rootfreq <- c()

nodefreqs <- c(3/7, 4/7)
node_entropy <- -sum(nodefreqs * log2(nodefreqs))

root_entropy <- sum(rootfreq * (nodefreqs * log2(nodefreqs)))

# Entrophy for rpart

library(rpart)
kyphosis

fit <- rpart(Kyphosis~Age + Number + Start, method = "class", data = kyphosis)
plot(fit, uniform = TRUE, main =  "Classification Tree for Kyphosis")
text(fit, use.n = TRUE, all = TRUE, cex = 1)

# party Entrophy

library(party)
readingSkills
attach(RS)
RS <- readingSkills

#add file name in png formate
png(file = "deciion_tree.png")

#Create tree
output.tree <- ctree(nativeSpeaker ~ age + shoeSize + score, data = RS)

# Plot the tree
plot(output.tree)

install.packages("partykit")

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(RWeka)
library(partykit)
data <- read.csv(file = "C:\\Users\\OWNER\\Desktop\\BDAP\\term 2\\ML02\\ComputerPurchased.csv")

#Create tree
j48 <- J48(Computer.bought ~ Age + Income +Student +Credit.rating, 
           data = data, control = Weka_control(), options = NULL)

plot(j48)

#------------------------------------------------------------------------------
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(RWeka)
library(partykit)
png(file = "insurance_deciion_tree.png")
data <- read.csv(file = "C:\\Users\\OWNER\\Desktop\\BDAP\\term 2\\ML02\\Entropy_Sheet.csv")
names(data)
j48 <- J48(Bought ~ IL + MS, 
           data = data, control = Weka_control(), options = NULL)
plot(j48, uniform=TRUE, main="Classification Tree for Chemicals" )

plot(as.party.Weka_tree(j48))

#------------------------------------------------------------------------------
library(rJava)
library(RWeka)
library(partykit)

csc <- CostSensitiveClassifier(Bought ~ IL + MS, data = data, 
 control = Weka_control(`cost-matrix` = matrix(c(0,10, 0, 0, 0, 0, 0, 10, 0), 
 ncol = 3), 
 W = "weka.classifiers.trees.J48", 
 M = TRUE))

plot(as.party.Weka_tree(csc))

library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	

prp(rxAddInheritance(csc))
fancyRpartPlot(rxAddInheritance(csc))