#HW1 - Applied
#Nicholas Smith

#Chapter 2
#8
#a
college = read.csv("College.csv")
college
#b
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
#c
summary(college)
pairs(college[,1:10])
attach(college)
plot(Private, Outstate)
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50]= "Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(college)
plot(Elite, Outstate)
par(mfrow=c(2,2))
hist(Accept/Apps, 20)
hist(Accept/Apps, 50)
hist(Accept/Apps, 10)
hist(Accept/Apps, 5)
par(mfrow=c(1,2))
plot(Elite, Apps)
plot(Elite, Accept/Apps)
#The two box-plots show that an Elite school seems to generally receive more applications, 
#and accept relatively less applications than a non-Elite school


###########################################################################################
#Chapter 3

#11 
#(a)
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
yuntox=lm(y~x+0)
summary(yuntox)
#the estimated coefficient is very close to the true coefficient of 2.  
#The std. error (SE) is achieved during my call was 0.1065.  That means that there is approximately a 95% probability that
#the true coefficient will be within the confidence interval calculated by the estimated coefficient of 1.9939 (+ or -) 2*0.1065. 
#The t-value is calculated by dividing the estimate by the SE = 1.9939/0.1065 which is 18.73. The t-value will be greater
#the lower the SE is. SE and t-value are inversely proportionate. 
#The p-value is the probability that a random t-value given no correlation between x and y would be greater than the
#calculated t-value.  In this case, we get a probability that of <2e-16 which is extremely insignificant
#and we can safely assert there is a definite correlation/interaction between x and y. 
#Therefore, we reject the null hypothesis because we are certain that the that the coefficient must be very close to 2 and
#is definitely not 0. 

#(b)
xuntoy = lm(x~y+0)
summary(xuntoy)
plot(y,x)
abline(xuntoy)
#We can reject the null hypothesis that the coefficient is zero because there is a significantly low p-value for the estimate
#of a non-zero coefficient.

#(c) a and b results have the same t value

#(d) see other file (addendum) attached with this one for algebraic proof
confirmation1 = function(x,y){
  betahat = sum(x*y) / sum(x**2)
  SE = sqrt(sum((y - x*betahat)**2)/( (length(x)-1)* sum(x**2)  ))
  value = betahat / SE
  return(value)
}

confirmation2 = function(x,y){
  numerator = sqrt(length(x)-1)*sum(x*y)
  denominator = sqrt(sum(x**2)*sum(y**2)-((sum(x*y))**2))
  return(numerator/denominator)
}

confirmation1(x,y) == confirmation2(x,y) #confirmation1 output = confirmation2  output

#(e) see other attached file

#(f) 
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
yuntox=lm(y~x)
xuntoy = lm(x~y)
summary(yuntox)
summary(xuntoy) #comparing the t vale from summary(yuntox) and summary(xuntoy), they are the same


###########################################################################################
#13
#(a)
set.seed(1)
x=rnorm(100)
#(b)
eps=rnorm(100,0,.25)
#(c)
y=-1+0.5*x+eps
yTrue=-1+0.5*x
length(y)
 #B0 is -1 and B1 is 0.5
#(d)
plot(x,y,type="p") #x and y appear to be linear with a y value of -1 corresponding to x-value of 0, and the slope of 1/2
#Also, there appear to be more x-values clustered arount its zero value and diffused less outward. 
#(e) 
model=lm(y~x)
modelTrue=lm(yTrue~x)
summary(model)
summary(modelTrue)
#the estimated slope is 0.49973 very close to actual of 0.5, and the estimated intercept of -1.00942 is close to actual -1
#(f)
abline(model,col= "blue",cex=.5)#Estimated equation
abline(modelTrue,col="black",cex=.5)#population line
legend(-2,legend=c("Population","Estimated"),
       col=c("black","blue"), lty=1, cex=0.8)
#lines(x,y,col="red",type="l",main = "Regression", ylab ="output", xlab="input") why not? 
#(g)
modelPoly=lm(y~x+I(x^2))
summary(model)
summary(modelPoly)
#No, the x^2 coefficient is not statistically significant

#(h)
set.seed(1)
x=rnorm(100)
eps=rnorm(100,0,0.1)
y=-1+0.5*x+eps
plot(x,y,type="p")
modelLowNoise=lm(y~x)
summary(modelLowNoise)
abline(modelLowNoise,col= "blue",cex=.5)#Estimated equation
abline(modelTrue,col="black",cex=.5)#population line
legend(-2,legend=c("Population","Estimated"),
       col=c("black","blue"), lty=1, cex=0.8)
#The least squares line for low noise is much closer to the population line
  
#(i)
set.seed(1)
x=rnorm(100)
eps=rnorm(100,0,5)
y=-1+0.5*x+eps
plot(x,y,type="p")
modelHighNoise=lm(y~x)
summary(modelHighNoise)
abline(modelHighNoise,col= "blue",cex=.5)#Estimated equation
abline(modelTrue,col="black",cex=.5)#population line
legend(-2,legend=c("Population","Estimated"),
       col=c("black","blue"), lty=1, cex=0.8)
#The least squares line for high noise is not as clost to the population line, and infact the summary states that the estimated x coefficient 
#of the least squares line is not signifacnt! 

#(j)
confint(model)
confint(modelLowNoise)
confint(modelHighNoise)
#the confidence interval for both coefficients are tighter where there is less noise, and broader where there is a lot of noise

