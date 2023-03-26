#setwd("/Users/loki/Documents/FMPH227-MVA-Course-2020/Lecture 2020")
getwd()

library(ISLR)
library(knitr)
library(MASS)
library(class)
library(glmnet)
library(methods)
library(tree)
library(randomForest)
library(ggplot2)

#require(PASWR)
require(ggplot2)
require(xtable)


Heart <- read.csv("Heart.csv", header = TRUE, sep=",")
Heart$AHD <- as.factor(Heart$AHD)
xnams <- c("Age", "Sex", "RestBP", "Chol", "MaxHR")
fmla <- as.formula(paste("AHD ~ ", paste(xnams, collapse= "+")))



###########       REGRESSION TREE EXAMPLE: Boston data set
set.seed (20176)
names(Boston)

trainn = sample(1: nrow(Boston), floor(nrow(Boston)/2))

##fit the tree
tree.boston = tree(medv~.,Boston, subset = trainn)
summary (tree.boston)

###plot the tree
plot(tree.boston)
text(tree.boston,pretty=0,cex=0.63)

tree.boston

##see "help(tree.control)"; can control terminal node size, deviance reduction etc 

####   PREDICT ON TEST SET and get MSE
yhat = predict(tree.boston, newdata = Boston[-trainn,])
mean((yhat - Boston[-trainn, "medv"])^2)

####   PREDICT ON TRAIN SET and get MSE
yhat2 = predict(tree.boston, newdata = Boston[trainn,])
mean((yhat2 - Boston[trainn, "medv"])^2)


###  PLOT PREDICTED VS OBSERVED
plot(Boston[-trainn, "medv"],yhat,ylab="yhat-tree", xlab="y")
abline (0 ,1)

##########

#### PRUNING
cv.boston <- cv.tree(tree.boston) ##10-fold by default

plot(cv.boston$size,cv.boston$dev,type= 'b',cex=0.63)

cv.boston$size[which(cv.boston$dev==min(cv.boston$dev))]

prune.boston = prune.tree(tree.boston,best=5)
#summary(prune.boston)

plot(prune.boston)
text(prune.boston,pretty=0)

#### NO PRUNING

#k is the cost-complexity parameter and K is # of folds
prune.boston0 = prune.tree(tree.boston,k=0)
#summary(prune.boston0)

#plot(prune.boston0)
#text(prune.boston0,pretty=0)

###### To grow Large tree: change defaults for minsize etc
tree2.boston = tree(medv~.,Boston, subset = trainn,control=tree.control(length(trainn),minsize=2,mindev=0))
summary(tree2.boston)
#tree2.boston
#plot(tree2.boston)
#text(tree2.boston,pretty=0,cex=0.1)

cv.boston2 <- cv.tree(tree2.boston) ##10-fold by default
min(cv.boston2$size[which(cv.boston2$dev==min(cv.boston2$dev))])

#plot(cv.boston2$size,cv.boston2$dev,type= 'b',cex=0.63)
#cbind(cv.boston2$size,cv.boston2$dev)
###############################################################
##########  Classification Tree
#### EXAMPLE: HEART DATASEY

tree.heart = tree(fmla,Heart)
summary(tree.heart)
tree.heart
#table(Heart$AHD)
plot(tree.heart)
text(tree.heart,pretty=0)



set.seed(111963)
cv.heart <- cv.tree(tree.heart,FUN = prune.misclass)

#cv.heart


plot(cv.heart$size,cv.heart$dev,type="b",cex=0.63)

cv.heart$size[which(cv.heart$dev==min(cv.heart$dev))]

prune.heart <- prune.misclass(tree.heart,best=4)
prune.heart
#summary(prune.heart)


plot(prune.heart)
text(prune.heart,pretty=0)


#####use all variables
tree.heart2 = tree(AHD~.,Heart)
summary(tree.heart2)


tree.heart2
plot(tree.heart2)
text(tree.heart2,pretty=0,cex=0.5)

set.seed(111963)
cv.heart2 <- cv.tree(tree.heart2,FUN = prune.misclass)
#cv.heart2


plot(cv.heart2$size,cv.heart2$dev,type="b",cex=0.63)

cv.heart2$size[which(cv.heart2$dev==min(cv.heart2$dev))]

prune.heart2 <- prune.misclass(tree.heart2,best=6)
prune.heart2


plot(prune.heart2)
text(prune.heart2,pretty=0)



#######################   BAGGING
set.seed(1111)
bag.boston = randomForest(medv~.,data = Boston, subset = trainn, mtry =dim(Boston)[2]-1, importance = TRUE)
bag.boston

#mean((predict(bag.boston) - Boston[trainn,"medv"])^2) ##OOB estimate??
#mean((predict(bag.boston,newdata=Boston[trainn,]) - Boston[trainn,"medv"])^2) ##training set estimate??


yhat5 = predict(bag.boston, newdata = Boston[-trainn,])
mean((yhat5 - Boston[-trainn, "medv"])^2)


plot(yhat5, Boston[-trainn, "medv"],xlab="yhat-bag", ylab="y")
abline (0 ,1)

############## RANDOM FOREST
set.seed(2021)
rf.boston = randomForest(medv~.,data = Boston, subset = trainn, mtry = 5, importance = TRUE)
rf.boston
 
yhat6 = predict(rf.boston, newdata = Boston[-trainn,])
mean((yhat6 - Boston[-trainn, "medv"])^2)

cbind(importance(bag.boston)[,"%IncMSE"],importance(rf.boston)[,"%IncMSE"])


varImpPlot(bag.boston,cex=0.6)

varImpPlot(rf.boston,cex=0.6)


