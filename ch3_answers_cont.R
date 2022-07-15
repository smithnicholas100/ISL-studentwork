#Nicholas Smith
#Applied #2
#September 23, 2019

#Chapter 3
#9.(a)
library(ISLR) 
attach(Auto)
pairs(Auto)
#b
names(Auto)
cor(Auto[,-9])
#c
modelAll <- lm(mpg~.-name, data = Auto)
summary(modelAll)
#There is a statistically significant relationship between the predictors displacement, weight, year, and origin
#with the response 'mpg'. 
#The positive coefficient with the 'year' variable suggests that mpg increases as time continues. 
#(d)
plot(modelAll)
#The residual plots suggest that 321, 324, and 325 might be outliers. The leverage plot also shows
#point 15 has high leverage
?Auto
modelTest1 <- lm(mpg~displacement + weight + year*origin)
modelTest2 <- lm(mpg~displacement*weight + year + origin)
modelTest3 <- lm(mpg~displacement + weight*year + origin)
modelTest4 <- lm(mpg~(displacement+weight+year+origin)^2)
summary(modelTest1)
summary(modelTest2)
summary(modelTest3)
summary(modelTest4)
#Yes, the three interaction tested one at a time (year:origin, displacement:weight, and weight:year) appear
#to be statistically significant. However, when all the interation are taken all at once with (displacement+weight+year+origin)^2
#then only the interaction displacement:weight appears to be significant. 
x1 = displacement
testDisplacementTrans1 <- lm(mpg~x1)
x1 = sqrt(displacement)
testDisplacementTrans2 <- lm(mpg~x1)
x1 = I(displacement^2)
testDisplacementTrans3 <- lm(mpg~x1)
x1 = log(displacement)
testDisplacementTrans4 <- lm(mpg~x1)
x1 = poly(displacement, 2)
testDisplacementTrans5 <- lm(mpg ~ x1)
anova(testDisplacementTrans1,testDisplacementTrans2)
#fail to reject null hypothesis
anova(testDisplacementTrans1,testDisplacementTrans3)
#fail to reject test 1
anova(testDisplacementTrans1, testDisplacementTrans4)
#fail to reject test 1
anova(testDisplacementTrans1, testDisplacementTrans5)
summary(testDisplacementTrans1)
summary(testDisplacementTrans5)
#Reject test 1, and use x1 = poly(displacement,2) in model
x1 = poly(displacement, 2)
x2 = weight
testWeight1 <- lm(mpg~x2)
x2 = sqrt(weight)
testWeight2 <- lm(mpg~x2)
x2 = I(weight^2)
testWeight3 <- lm(mpg~x2)
x2 = log(weight)
testWeight4 <- lm(mpg~x2)
x2 = poly(weight, 2)
testWeight5 <- lm(mpg~x2)
anova(testWeight1, testWeight2) 
anova(testWeight1,testWeight3)
anova(testWeight1,testWeight4)
anova(testWeight1, testWeight5)
#Reject model 1, and use x2 = poly(weight, 2)
x2 = poly(weight, 2)
x3 = year
testYear1 <- lm(mpg~x3)
x3 = sqrt(year)
testYear2 <- lm(mpg~x3)
x3 = I(year^2)
testYear3 <- lm(mpg~x3)
x3 = log(year)
testYear4 <- lm(mpg~x3)
x3 = poly(year, 2)
testYear5 <- lm(mpg~x3)
anova(testYear1, testYear2) 
anova(testYear1,testYear3)
anova(testYear1,testYear4)
anova(testYear1, testYear5)
#Reject model 1, x3 = poly(year, 2)
x3 = poly(year, 2)
x4 = origin
testorigin1 <- lm(mpg~x4)
x4 = sqrt(origin)
testorigin2 <- lm(mpg~x4)
x4 = I(origin^2)
testorigin3 <- lm(mpg~x4)
x4 = log(origin)
testorigin4 <- lm(mpg~x4)
x4 = poly(origin, 2)
testorigin5 <- lm(mpg~x4)
anova(testorigin1, testorigin2) 
anova(testorigin1,testorigin3)
anova(testorigin1,testorigin4)
anova(testorigin1, testorigin5)
#Reject model 1, x4 = poly(origin, 2)
x4 = poly(origin, 2)
#All of the 2nd-order tranformations tested, one at a time, appeared to significantly fit the data 
#better than the simple linear y=mx+b relationship of model 1.  This was shown using the F-test anova 
#command. The superior fit of the transformed predictors is further confirmed below. 
modelBasic <- lm(mpg~displacement + weight + year + origin)
modelTrans <- lm(mpg~x1 + x2 + x3 + x4)
anova(modelBasic, modelTrans)
summary(modelBasic)
summary(modelTrans)

#10 (a)
?Carseats
attach(Carseats)
head(Carseats)
modelCarseats <- lm(Sales ~ Price + Urban + US)
summary(modelCarseats)

#(b)
#The coefficient for Price represents the factor that calculates how many unit Sales 
#decreases (because the coefficient is negative) compared to every unit increase of Price. 
#The coefficient for Urban (UrbanYes) would be the amount of unit Sales decreased
#(decreased because of the negative) that would occur when there is a 'yes' value for Urban 
#(which registers as a 1).  However, the Urban predictor does not appear to be statistically significant. 
#The coefficient for US (USYes) is the amount of unit Sales increased
#(increased because of the positive sign) that would occur when there is a 'yes' value for US 
#(which registers as a 1).  The US predictor does appear to be statistically significant. 

#(c) See "Applied 2 Addendum_Nick Smith"

#(d)
#The null hypothesis can be rejected for Price and US, because both predictors have a coefficient
#that is statistically signicant a non-zero value. 

#(e)
modelCarseats2 <- lm(Sales ~ Price + US)

#(f) 
summary(modelCarseats)
summary(modelCarseats2)
#Both models have a R-squared value of 0.2393, which means that ~23.9% of the variance for Sales 
#can be explained with the predictors Price and US (not Urban, beceause Urban is not a statistically
#significant predictor)

#(g)
confint(modelCarseats2)

#(h)
par(mfrow=c(2,2))
plot(modelCarseats2)
Carseats[which.max(hatvalues(modelCarseats2)),6]
totalPrice = 0
for (row in 1:nrow(Carseats)) {
  totalPrice = totalPrice + Carseats[row,6]
}
avePrice = totalPrice/dim(Carseats)[1]
avePrice
#There does not appear to be evidence of outliers or "concerning" high leverage in the model. 
#For outliers, no data point had a residual value far greater in absolute value than the rest
#of the data. The average leverage is (1+p)/n = (1+2)/400 = 0.0075.  There is a point that appears
#to have a levereage of ~0.0450 which is ~0.0450/0.0075 = 6 times greater leverage than the average. 
#However, that point has very little residual, so is not too much of a concern. Furthermore, for a 
#data set of 400 data points, it is conceivable to have a wide range of leverage. Realistically, 
#this high leverage point means that there is a data point of a carseat that has either a very low 
#price or a very high price. That is intuitive, because carseats being sold do seem to have a large 
#range of price values. 

#14(a)
set.seed(1)
x1=runif(100)
x2=0.5*x1 + rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
#See "Applied 2 Addendum_Nick Smith" 

#(b)
cor(x1,x2)
par(mfrow=c(1,1))
plot(x1,x2)

#(c)
modelCollinear <- lm(y~x1+x2)
summary(modelCollinear)
#B0-hat is the intercept coefficient of 2.13, which is close to the true intercept of 2. This is 
#statistically significant.  B1-hat is the x1 coefficient and is 1.4396, not too close to the actual
#value of 2. And B2-hat is the x2 coefficient and is 1.0097, and is not close at all to the actual 
#value of 0.3.  
#I can reject the null hypothesis that B1-hat=0 with greater than 95% certainty, 
#because the non-zero value has a p-value of <0.05. 
#I cannot reject the null hypothis that B2-hat=0, because the p-value=0.3754. 

#(d)
modelCollinear2 <- lm(y~x1)
summary(modelCollinear2)
anova(modelCollinear, modelCollinear2)
#The results have a similar R-squared value, and the F-test does not show a significant improvement
#of one model over the other. But the coefficient for x1 is much more statistically
#significant with a p-value <0.0001.  Therefore, I can reject the null hypothesis B1-hat=0. 

#(e)
modelCollinear3 <- lm(y~x2)
summary(modelCollinear3)
anova(modelCollinear3, modelCollinear) 
#The anova command shows that the original model is more statistically significant than the new model. 
#However, the new model shows a very statistically significant coefficient for x2 and I can reject 
#the null hypothesis that the coefficient for x2 = 0. 

#(f)
#The results seemingly contradict one another, because the model y~x1 has a similar fit as y~x1+x2, 
#yet x2 appears to be significant when taken by itself y~x2. 

#(g)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
modelCollinear <- lm(y~x1+x2)
modelCollinear2 <- lm(y~x1)
modelCollinear3 <- lm(y~x2)
summary(modelCollinear)
summary(modelCollinear2)
summary(modelCollinear3)
par(mfrow=c(2,2))
plot(modelCollinear)
rstudent(modelCollinear)[which.max(rstudent(modelCollinear))]
plot(modelCollinear2)
rstudent(modelCollinear2)[which.max(rstudent(modelCollinear2))]
plot(modelCollinear3)
rstudent(modelCollinear3)[which.max(rstudent(modelCollinear3))]
#The model y~x1+x2 now shows that x2 coefficient is statistically significant, and the x1 coefficient
#is not.  The point 101 does appear to be an outlier for the model y~x1, but not for the model y~x1+x2
#and not for the model y~x2. This is determined with the studentized residuals, which 
#generally need to be greater than an absolute value of 3 to be considered an outlier. 
#However, the point 101 still seems to be a high-leverage point for y~x2 of about 0.10 which is 
#~0.10/0.02= 5 times more leverage than the average (even though it is not an outlier in this model).
#For the y~x1+x2 it has ~0.40/0.02= 20 times the leveratee than the average, and this makes the point 
#cross over a "Cook's distance" of 1 (even though the residual by itself is not considered an outlier).
#Interestingly, the point 101 does not register as too much of a high-leverage point (there
#are points with greater leverage) for the y~x1 model (even though it IS an outlier in this model, as 
#far as residuals go).













