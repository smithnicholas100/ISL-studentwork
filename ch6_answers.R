#Nicholas Smith
#Applied 7

#Chapter 6 problem 8
#(a) and (b)
set.seed(1)
x=rnorm(100,0,1)
e=rnorm(100,0,1)
Y=1+2*x+3*x^2+4*x^3+e #response, true model

data=data.frame(x)
for(i in  c(2:10)) {
  data=cbind(data,x^i)
}
df=data.frame(Y,data)
names(df)=c('Y','x','x.2','x.3','x.4','x.5','x.6','x.7','x.8','x.9','x.10')

#(c)
library(leaps)
regfit.full = regsubsets(Y~.-Y,data=df)
reg.summary = summary(regfit.full)
print(reg.summary)

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab = 'Number of variables', 
     ylab='Adjusted R-squared',type = 'l')
points(which.max(reg.summary$adjr2), 
       reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$cp)
plot(reg.summary$cp,xlab = 'Number of variables', 
     ylab='Cp',type = 'l')
points(which.min(reg.summary$cp), 
       reg.summary$cp[which.min(reg.summary$cp)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = 'Number of variables', 
     ylab='BIC',type = 'l')
points(which.min(reg.summary$bic), 
       reg.summary$bic[which.min(reg.summary$bic)], 
       col='red',cex = 2, pch = 20)
#The best model is the model only including the first 3 predictors. Even though, the adjr2 and cp
#show that the best model uses the 4 predictors, the plot shows there is virtually no difference
#between the 3 model and the 4 model.  Also, BIC shows that the 3 model is best. 
reg.model=lm(Y~x+x.2+x.3,data=df)
summary(reg.model) 
#coefficiencts for intercept, x, x.2, and x.3 are ~1.06, 1.98, 2.88, and 4.02 respectively. Very close
#to the true model!

#(d)
regfit.fwd = regsubsets(Y~.-Y,data=df,method= 'forward')
reg.summary = summary(regfit.fwd)
print(reg.summary)

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab = 'Number of variables', 
     ylab='Adjusted R-squared',type = 'l')
points(which.max(reg.summary$adjr2), 
       reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$cp)
plot(reg.summary$cp,xlab = 'Number of variables', 
     ylab='Cp',type = 'l')
points(which.min(reg.summary$cp), 
       reg.summary$cp[which.min(reg.summary$cp)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = 'Number of variables', 
     ylab='BIC',type = 'l')
points(which.min(reg.summary$bic), 
       reg.summary$bic[which.min(reg.summary$bic)], 
       col='red',cex = 2, pch = 20)

regfit.backward = regsubsets(Y~.-Y,data=df,method= 'backward')
reg.summary = summary(regfit.backward)
print(reg.summary)

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab = 'Number of variables', 
     ylab='Adjusted R-squared',type = 'l')
points(which.max(reg.summary$adjr2), 
       reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$cp)
plot(reg.summary$cp,xlab = 'Number of variables', 
     ylab='Cp',type = 'l')
points(which.min(reg.summary$cp), 
       reg.summary$cp[which.min(reg.summary$cp)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = 'Number of variables', 
     ylab='BIC',type = 'l')
points(which.min(reg.summary$bic), 
       reg.summary$bic[which.min(reg.summary$bic)], 
       col='red',cex = 2, pch = 20)
#Foward selects the same features as best, but backward selects different features after the same first 3. 
#Still, both forward and backward show that 3 is best for the same reason given in part (c).

#(e)
library(glmnet)
grid = 10^seq(10,-2,length = 100)#Tuning parameter lambda
set.seed(42)
train = sample(c(1:dim(df)[1]),.7*dim(df)[1],replace = FALSE)
test =(-train)
#Format the data
y = df$Y
x = model.matrix(Y~.,df)#Needed for glmnet
lasso.model = glmnet(x[train,],y[train], alpha = 1, lambda = grid) 
names(lasso.model)
lasso.model$lambda
plot(lasso.model)

set.seed(42)
cv.out = cv.glmnet(x[train,],y[train], alpha = 1)#default k=10
plot(cv.out)
best_lambda = cv.out$lambda.min
best_lambda
lasso.pred = predict(lasso.model,s = best_lambda , newx = x[test,])
mean((lasso.pred-y[test])^2)#MSE

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type = 'coefficients',s=best_lambda)[1:dim(x)[2],]
lasso.coef
#The resulting coefficients are shown.  They are less close to the true model, and contain additional features
#then the true model.  

#(f)
Y=1+7*x^7+e
new.df=data.frame(Y,data)
names(new.df)=c('Y','x','x.2','x.3','x.4','x.5','x.6','x.7','x.8','x.9','x.10')
head(new.df)

regfit.full = regsubsets(Y~.-Y,data=new.df)
reg.summary = summary(regfit.full)
print(reg.summary)

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab = 'Number of variables', 
     ylab='Adjusted R-squared',type = 'l')
points(which.max(reg.summary$adjr2), 
       reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$cp)
plot(reg.summary$cp,xlab = 'Number of variables', 
     ylab='Cp',type = 'l')
points(which.min(reg.summary$cp), 
       reg.summary$cp[which.min(reg.summary$cp)], 
       col='red',cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = 'Number of variables', 
     ylab='BIC',type = 'l')
points(which.min(reg.summary$bic), 
       reg.summary$bic[which.min(reg.summary$bic)], 
       col='red',cex = 2, pch = 20)

reg.model=lm(Y~x.7,data=new.df)
summary(reg.model) 

set.seed(42)
train = sample(c(1:dim(df)[1]),.7*dim(df)[1],replace = FALSE)
test =(-train)
#Format the data
y = new.df$Y
x = model.matrix(Y~.,new.df)#Needed for glmnet
lasso.model = glmnet(x[train,],y[train], alpha = 1, lambda = grid) 
names(lasso.model)
plot(lasso.model)

cv.out = cv.glmnet(x[train,],y[train], alpha = 1)#default k=10
plot(cv.out)
best_lambda = cv.out$lambda.min
best_lambda
lasso.pred = predict(lasso.model,s = best_lambda , newx = x[test,])

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type = 'coefficients',s=best_lambda)[1:dim(x)[2],]
lasso.coef

#Again, the BIC reports the true model (but not the cp or the adjr2), and the linear regression finds coefficients that are very close.
#The lasso also selects the true feature with the cv-best lambda.  However, the coefficients are
#not as good as normal linear regression

#6.9 (a)
library(ISLR)
attach(College)

set.seed(42)
train = sample(c(1:dim(College)[1]),.7*dim(College)[1],replace = FALSE)

#(b)
lm.college = lm(Apps~.-Apps,data=College)
p.college = predict(lm.college, College[-train,-2])
mean((p.college-College$Apps[-train])^2)#test MSE

#(c)
x=model.matrix(Apps~.,College)[,-2]
y=Apps
ridge.model = glmnet(x,y, alpha = 0, lambda = grid) 
names(ridge.model)
plot(ridge.model)

set.seed(42)
cv.out = cv.glmnet(x[train,],y[train], alpha = 0)#default k=10
plot(cv.out)
best_lambda = cv.out$lambda.min
best_lambda
ridge.pred = predict(ridge.model,s = best_lambda , newx = x[-train,])
mean((ridge.pred-y[-train])^2)#MSE

#(d)
lasso.model = glmnet(x,y, alpha = 1, lambda = grid) 
names(lasso.model)
plot(lasso.model)

set.seed(42)
cv.out = cv.glmnet(x[train,],y[train], alpha = 1)#default k=10
plot(cv.out)
best_lambda = cv.out$lambda.min
best_lambda
lasso.pred = predict(lasso.model,s = best_lambda , newx = x[-train,])
mean((lasso.pred-y[-train])^2)#MSE

#(g) The ordinary least squares model actually performed the best on the test validation set. 
#Here is the RMSE:  
sqrt(mean((p.college-College$Apps[-train])^2))
#which compares to the mean of all applications in the original data set: 
mean(Apps)
#Therefore, the RMSE is more than one third of the mean, and I do not consider this very accurate. 
#The ridge had a much higher test MSE than least squares and lasso.  Lasso and least squares 
#compared pretty closely, but least squares took the cake! 




