#Applied #4
#Nicholas Smith

#11. (a)
library('ISLR')
Med = ifelse(Auto$mpg>median(Auto$mpg),"yes","no")
Auto = data.frame(Auto,Med) 
attach(Auto)
#(b)

cor(Auto[,3:7])
#displacement, horsepower, weight, and acceleration seem to correlate >0.5. 

head(Auto)
i = 5#3- 7
boxplot(Auto[,i] ~ Med,
        xlab = "P", ylab = names(Auto)[i], col = c("red"))
#all seemed significant, but weight seems best
#try some transformations on weight log, 1/x, poly(x,2)
par(mfrow=c(2,2))
x = weight
boxplot(x ~ Med,
        xlab = "P", ylab = "weight", col = c("red"))
x = log(weight)
boxplot(x ~ Med,
        xlab = "P", ylab = "log(weight)", col = c("red"))
x = 1/weight
boxplot(x ~ Med,
        xlab = "P", ylab = "1/weight", col = c("red"))
x = poly(weight,2)
boxplot(x ~ Med,
        xlab = "P", ylab = "poly(weight,2)", col = c("red"))
#just plain weight looks good to me
par(mfrow=c(1,1))
counts <- table(Med,cylinders)
barplot(counts,legend = c("below","above"),beside = TRUE)
#each cylinder seems to have a significantly different ratio than the others. 

counts <- table(Med,origin)
barplot(counts,legend = c("below","above"),beside = TRUE)
#the origin seems to be significant between orign 1 vs. origin 2 and origin 3. 

#(c)
set.seed(1)
trainIndex = sample(c(1:dim(Auto)[1]),size =floor(.7*dim(Auto)[1]) ,
                    replace = FALSE)
train = Auto[trainIndex,]
test = Auto[-trainIndex,]

#(d)
library(MASS)
lda.fit = lda(Med~weight, train)  
plot(lda.fit)
lda.pred = predict(lda.fit, test)$class
mytable = table(lda.pred, test[,"Med"])
print(mytable)
#Test error 
mean(lda.pred!=test$Med)

#Let's try using more variables: weight, year, cylinders
lda.fit = lda(Med~weight+year+cylinders, data = traxin)  
plot(lda.fit)
lda.pred = predict(lda.fit, test)$class
mytable = table(lda.pred, test[,"Med"])
print(mytable)
#Test error 
mean(lda.pred!=test$Med)

#Let's try using the full model
lda.fit = lda(Med~weight+year+acceleration+origin+cylinders+displacement+horsepower, data = train)  
plot(lda.fit)
lda.pred = predict(lda.fit, test)$class
mytable = table(lda.pred, test[,"Med"])
print(mytable)
#Test error 
mean(lda.pred!=test$Med)
#The best test error is from lda.fit = lda(Med~weight+year+cylinders, data = train) 

#(e)
qda.fit = qda(Med~weight+year+cylinders, data = train) 
qda.pred = predict(qda.fit, test)$class
mytable = table(qda.pred,test[,"Med"])
print(mytable)
#Test error of QDA
mean(qda.pred!=test$Med)

#13
crim01 = ifelse(Boston$crim>median(Boston$crim), 1, 0)
Boston = data.frame(Boston, crim01)
attach(Boston)
#applied #3 HW found these to be the best predictors in the logistic model rad + indus:rad + rad:tax

#training and testing 
set.seed(1)
trainIndex = sample(c(1:dim(Boston)[1]),size =floor(.7*dim(Boston)[1]) ,
                    replace = FALSE)
train = Boston[trainIndex,]
test = Boston[-trainIndex,]

lda.fit = lda(crim01~rad + indus:rad + rad:tax, train) 
plot(lda.fit)
lda.pred = predict(lda.fit, test)$class
mytable = table(lda.pred, test[,"crim01"])
print(mytable)
#Test error 
test.error.lda = mean(lda.pred!=test$crim01)
test.error.lda

#comparing this to the logistic regression 
logit.model5 = glm(crim01~rad + indus:rad + rad:tax, data = train, family=binomial)

glm.probs=predict(logit.model5,test,type="response")
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$crim01)
test.error.logit = mean(glm.pred != test$crim01)
test.error.logit

#let's try lda one more time without the interactions
lda.fit = lda(crim01~rad + indus + tax, train) 
plot(lda.fit)
lda.pred = predict(lda.fit, test)$class
mytable = table(lda.pred, test[,"crim01"])
print(mytable)
#Test error 
test.error.lda = mean(lda.pred!=test$crim01)
test.error.lda

#Let's just try it with QDA as well
qda.fit = qda(crim01~rad + indus:rad + rad:tax, train) 
qda.pred = predict(qda.fit, test)$class
mytable = table(qda.pred,test[,"crim01"])
print(mytable)
#Test error of QDA
mean(qda.pred!=test$crim01)

#The logistic regression appears to outperform LDA and QDA for the Boston data set. 

