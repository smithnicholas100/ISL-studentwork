#Applied 8 
#Nicholas Smith

#8.
install.packages('tree')
library(tree)
library(ISLR)

#(a)
set.seed(1)
trainI=sample(1:nrow(Carseats),nrow(Carseats)/2)
train=Carseats[trainI,]
test=Carseats[-trainI,]

#(b)
carseatTree=tree(Sales~.,data=Carseats,subset=trainI)
summary(carseatTree)
plot(carseatTree)
text(carseatTree,pretty=0)
#There are 16 terminal nodes, which means there are 16 possible training y-hat values for Sales. The observed training values 
#from the y-hat values results in a residual mean deviance of $2.12. That means on average, the observed value is off
#by $2.12 from the value predicted by the model. 
yhat=predict(carseatTree,newdata = test)
testSales=Carseats[-trainI,"Sales"]
mean((yhat-testSales)^2)#MSE~5.38

#(c)
set.seed(1)
cv.carseats=cv.tree(carseatTree)
plot(cv.carseats$size,cv.carseats$dev,type='b') 
#Looks like 6 terminal nodes might be good to avoid overfitting.  However, the absolute best training fit is 
#obtained with 13 terminal nodes 
prune.carseats=prune.tree(carseatTree,best=6)
yhat.prune=predict(prune.carseats,newdata = test)
mean((yhat.prune-testSales)^2)#MSE~5.34 
prune.carseats=prune.tree(carseatTree,best=13)
yhat.prune=predict(prune.carseats,newdata = test)
mean((yhat.prune-testSales)^2)#MSE~5.32
#Pruning appears to be helping the test MSE a little, but not very significantly in this case. 

#(d)
install.packages('randomForest')
library(randomForest)
set.seed(1)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=trainI,mtry=10,importance=TRUE) #10 predictors, default 500 trees
bag.carseats
yhat.bag=predict(bag.carseats,newdata=test)
mean((yhat.bag-testSales)^2) #MSE~3.01
importance(bag.carseats)

#(e)
set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=trainI,mtry=4,importance=TRUE) #m=4 predictors
rf.carseats
yhat.rf=predict(rf.carseats,newdata=test)
mean((yhat.rf-testSales)^2) #MSE~3.31
importance(rf.carseats)
#In this case, have m<p does not appear to be helping the test MSE. It appears to be making the test MSE
#worse. 

#9. 
#(a)
set.seed(1)
trainI=sample(1:nrow(OJ),800)

#(b)
tree.OJ=tree(Purchase~.,data=OJ,subset=trainI)
summary(tree.OJ)
#The training error rate is the misclassification error of 0.1588.  Terminal nodes = 9. 

#(c)
tree.OJ
#The last terminal node reads 7) LoyalCH > 0.764572 261   91.20 CH ( 0.95785 0.04215 ) *
#This means this splits the data where LoyalCH > 0.764572, then it is assigned to the CH category. 261 
#data points were split at this node with a 0.95785 proportion of them being sorted into the CH category, 

#(d)
plot(tree.OJ)
text(tree.OJ,pretty=0)
#The result shows a tree of 9 nodes and 8 splits. The first/most important split is with the LoyalCH predictor. 

#(e)
tree.pred=predict(tree.OJ,OJ[-trainI,],type="class")
table(tree.pred,OJ[-trainI,"Purchase"])
#test error rate
(8+38)/sum(table(tree.pred,OJ[-trainI,"Purchase"]))#~0.1704

#(f) and (g) and (h)
set.seed(1)
cv.OJ=cv.tree(tree.OJ)
plot(cv.OJ$size,cv.OJ$dev,type='b') 
#It appears that we get a good local min at 2 splits and 5 splits

#(i)
prune.OJ=prune.tree(tree.OJ,best=5)
prune.pred=predict(prune.OJ,OJ[-trainI,],type="class")
table(prune.pred,OJ[-trainI,"Purchase"])
#test error rate
(18+34)/sum(table(prune.pred,OJ[-trainI,"Purchase"]))#~0.1926

#(j) The pruned tree has higher training error rates 
summary(prune.OJ)
summary(tree.OJ)

#(k) In this case, the pruned tree still has higher test error rates than the unpruned tree. 


