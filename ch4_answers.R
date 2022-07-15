#Nick Smith
#Applied #3
#October 2, 2019

#11 a,b,c,f,g
#(a) 
library(ISLR)
mpg01 = ifelse(Auto$mpg>median(Auto$mpg), 1, 0)
?data.frame
Auto = data.frame(Auto, mpg01)
head(Auto)
attach(Auto)

#(b)
?boxplot
boxplot(cylinders~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
boxplot(displacement~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
boxplot(horsepower~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
boxplot(weight~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
boxplot(acceleration~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
boxplot(year~mpg01, data = Auto, xlab = "Above or below median mpg", ylab =  "cylinders",
        col = c("red"))
pairs(Auto)
#There seems to be a lot of collinearity with cylinders, displacement, horsepower, weight, 
#and inverse acceleration. Displacement or cylinders seem to most statistically distinguish 
#above or below median mpg.  However, I will confirm this in final model.  
#For now, I will use either cylinders and year or displacement and year as the predictors. 
x.cylinders = factor(cylinders)
Auto = data.frame(Auto, x.cylinders)
counts <- table(x.cylinders, mpg01)
head(counts)
barplot(counts,
        xlab="Above Average MPG",beside = TRUE)
#Changed cylinder to categorical and plotted the predictor as a barplot against the mpg01. 

#(c)
set.seed(1)
?sample
train.index = sample(c(1:dim(Auto)[1]), floor(.7*dim(Auto)[1]), replace = FALSE)
train = Auto[train.index,]
test = Auto[-train.index,]

#(f)
?glm
regression.model = glm(mpg01~displacement + year, data = train, family=binomial)
summary(regression.model)
?predict
glm.probs=predict(regression.model,test,type="response")
glm.probs
?rep
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$mpg01)
test.error = mean(glm.pred != test$mpg01)
test.error

#(g)
library(class)
head(train)
test.input = scale(Auto[-trainIndex, c(5,7)])
train.X = scale(train[,c(3,7)])
test.X = scale(test[,c(3,7)])
train.output = train[,10]
test.output = test[,10]

?knn()
k.value = 1
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k1 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k1

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

k.value = 3
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k3 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k3

k.value = 4
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k4 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k4

k.value = 5
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k5 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k5

k.value = 6
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k6 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k6

k.value = 7
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k7 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k7

k.value = 8
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k8 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k8


k.value = 9
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k9 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k9

k.value = 10
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k10 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k10

k.value = 20
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k20 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k20

k.value = 30
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k30 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k30
#K = 7 or K = 9 seem to perform the best on this data set. 

#13 
library(MASS)
set.seed(1)
crim01 = ifelse(Boston$crim>median(Boston$crim), 1, 0)
?data.frame
Boston = data.frame(Boston, crim01)
attach(Boston)
?Boston

pairs(Boston)
head(Boston)
#zn, indus, nox, rm, age, dis, rad, tax, ptratio, black, lstst, medv
#appear collinear: medv, lstat, and rm; and zn, age, and dis. 
boxplot(zn~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "zoned",
        col = c("red"))
boxplot(indus~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "industry",
        col = c("red"))
boxplot(nox~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "nitrogen oxides",
        col = c("red"))
boxplot(age~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "owner occupied < 1940",
        col = c("red"))
boxplot(dis~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "distance to employment",
        col = c("red"))
boxplot(rad~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "radial highway accessibility",
        col = c("red"))
boxplot(tax~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "tax rate",
        col = c("red"))
boxplot(ptratio~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "pupil teacher ratio",
        col = c("red"))
boxplot(lstat~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "lower status",
        col = c("red")) 
boxplot(medv~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "median value of owned homes",
        col = c("red")) 
boxplot(rm~crim01, data = Boston, xlab = "Above or below median crime", ylab =  "average rooms/dwelling",
        col = c("red")) 
?Boston

#indus, nox, age, rad, tax, ptratio, lstat, and medv all appear to be possible statistically sign. predictors. 
#For the collinear predictors, we will use lstat and dis (not age and not medv and not rm). 
#Therefore, we will try lstat, dis, indus, nox, rad, tax, and ptratio
#chas is categroical and needs a barplot
counts <- table(chas, crim01)
barplot(counts, xlab="Above Median Crime Rate", beside = TRUE)
#chas does not appear to predict crim01 with statistical sign. 
df = data.frame(lstat, dis, indus, nox, rad, tax, ptratio, crim01)
head(df)
pairs(df)
#lstat, dis, indus, and nox appear to be collinear.
boxplot(dis~crim01, data = df, xlab = "Above or below median crime", ylab =  "distance to employment",
        col = c("red"))
boxplot(indus~crim01, data = df, xlab = "Above or below median crime", ylab =  "industry",
        col = c("red"))
boxplot(lstat~crim01, data = df, xlab = "Above or below median crime", ylab =  "lower status",
        col = c("red"))
boxplot(nox~crim01, data = df, xlab = "Above or below median crime", ylab =  "nitrogen oxide levels",
        col = c("red"))
#indus appears to be best predictor out of the four . . . with dis as a runner up
#Therefore, I will try indus, dis, rad, tax, and ptratio
df = data.frame(crim, indus, dis, rad, tax, ptratio, crim01)
head(df)
pairs(df)

set.seed(1)
train.index = sample(c(1:dim(df)[1]), floor(.7*dim(df)[1]), replace = FALSE)
train = df[train.index,]
test = df[-train.index,]

logit.model1 = glm(crim01~indus + rad + tax + ptratio, data = train, family=binomial)
summary(logit.model1)
#ptratio is not significant
logit.model2 = glm(crim01~indus + rad + tax, data = train, family=binomial)
summary(logit.model2)
logit.model3 = glm(crim01~(indus + rad + tax)^2, data = train, family=binomial)
summary(logit.model3)
#indus and indus:tax not significant
logit.model4 = glm(crim01~rad + tax + indus:rad + rad:tax, data = train, family=binomial)
summary(logit.model4)
#tax not significant
logit.model5 = glm(crim01~rad + indus:rad + rad:tax, data = train, family=binomial)
summary(logit.model5)
#try log transformation on rad
logit.model6 = glm(crim01~log(rad) + indus:log(rad) + log(rad):tax, data = train, family=binomial)
summary(logit.model6)
logit.model7 = glm(crim01~log(indus) + rad + tax, data = train, family=binomial)
summary(logit.model7)

#will use test data on logit.model2, logit.model5, logit.model6, and logit.model7
?predict
glm.probs=predict(logit.model2,test,type="response")
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$crim01)
test.error = mean(glm.pred != test$crim01)
test.error

glm.probs=predict(logit.model5,test,type="response")
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$crim01)
test.error = mean(glm.pred != test$crim01)
test.error

glm.probs=predict(logit.model6,test,type="response")
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$crim01)
test.error = mean(glm.pred != test$crim01)
test.error

glm.probs=predict(logit.model7,test,type="response")
glm.pred=rep(0, dim(test)[1])
glm.pred[glm.probs >.5]= 1
table(glm.pred,test$crim01)
test.error = mean(glm.pred != test$crim01)
test.error
#model logit.model5 has the best test error rate. \
#logit.model5 uses rad + indus:rad + rad:tax which are composed of the features rad, indus, and tax

names(train)
names(test)

train.X = scale(train[c(2,4,5)])
test.X = scale(test[,c(2,4,5)])
train.output = train[,7]
test.output = test[,7]

?knn()
k.value = 1
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k1 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k1

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

k.value = 3
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k3 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k3

k.value = 4
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k4 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k4

k.value = 5
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k5 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k5

k.value = 6
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k6 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k6

k.value = 7
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k7 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k7

k.value = 8
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k8 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k8


k.value = 9
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k9 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k9

k.value = 10
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k10 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k10

k.value = 20
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k20 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k20

k.value = 30
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k30 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k30
#K-value = 2 resulted in the best test error rate with an impressive 0.05263 which beats the logistic regression 
#error of 0.1579
#lets try a KNN tests with just one feature at a time
names(train)
names(test)

#just indus as feature
train.X = scale(train[c(2)])
test.X = scale(test[,c(2)])
train.output = train[,7]
test.output = test[,7]

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

#just dis as feature
train.X = scale(train[c(3)])
test.X = scale(test[,c(3)])
train.output = train[,7]
test.output = test[,7]

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

names(train)
#just rad as feature 
train.X = scale(train[c(4)])
test.X = scale(test[,c(4)])
train.output = train[,7]
test.output = test[,7]

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

#just tax as feature 
train.X = scale(train[c(5)])
test.X = scale(test[,c(5)])
train.output = train[,7]
test.output = test[,7]

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

#just ptratio as feature 
train.X = scale(train[c(6)])
test.X = scale(test[,c(6)])
train.output = train[,7]
test.output = test[,7]

k.value = 2
knn.pred = knn(train.X,test.X,train.output,k=k.value)
head(knn.pred)
mytable = table(knn.pred,test.output)
mytable
test.error.k2 = 1 - (sum(diag(mytable))/sum(mytable))
test.error.k2

#The test error rate was much better for the KNN test that used the combination of three 
#features: indus, rad, and tax, instead of just one feature at a time. The KNN model with 
#those three features also outperformed the logistic regression model, even after being 
#optimized with interactions.

