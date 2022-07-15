#Application 5
#Nicholas Smith

#Chapter 5
#6 (a)
set.seed(1)
library(ISLR)
library(boot)
head(Default)
attach(Default)

model = glm(default ~ income + balance, family = binomial, data = Default)
summary(model)
#std. error of income coefficient = 4.985e-06, and balance = 2.274e-04

#(b) and (c)
?glm
boot.fn=function(data, index)
  coefficients(glm(default ~ balance + income, family = binomial, data = data, subset = index))
boot(Default, boot.fn, R = 1000)
#t2 is balance with std. error = 2.299e-04 and t3 is income with std. error = 4.866e-06

#(d) the std. error of the glm() model are very similar to the estimated std. error of the boot() function. 
#The std. error calculated by the glm() is also an estimate that makes certain assumptions about the data. 
#So, it is possible the bootstrap etimates are more accurate/reliable than the glm() std. error. 

#7 (a)
attach(Weekly)
model.weekly = glm(Direction ~ Lag1 + Lag2, family = binomial)

#(b)
model.weekly.1 = glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-1,])

#(c)
pred.weekly <- predict(model.weekly.1, newdata = Weekly[1,], type = "response")
Pred <- ifelse(pred.weekly >0.5, "Up", "Down")
Pred == Weekly[1, "Direction"]

count = 1:dim(Weekly)[1]
for(i in count) {
  model.weekly = glm(Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-i,])
  pred.weekly <- predict(model.weekly, newdata = Weekly[i,], type = "response")
  Pred <- ifelse(pred.weekly >0.5, "Up", "Down")
  count[i] = ifelse(Pred == Weekly[i, "Direction"], 1, 0)
}

#(e) the estimated test error by LOOVC
test.error = sum(count)/length(count)
test.error

naive.error = sum(Weekly[, "Direction"] == "Up")/length(count)
naive.error

test.error > naive.error

#The test error is not greater than the naive error.  Therefore, the model is not an 
#accurate model, because it appears that we would have a better chance making the correct prediction
#by simply predicting that every observation would be "Up". 

#9 (a)
library(MASS)
attach(Boston)

u.hat = mean(medv)
#(b) 
stderr.hat = sd(medv)/sqrt(length(medv))
stderr.hat
#The standard error of the mean is the estimated std. dev. of the estimated mean from the true mean. 
#Therefore, there is a std. dev. of magnitude 0.4089 around the mean of 22.5328. 

#(c)
set.seed(1)
boot.fn=function(data, index)
  mean(data[index,'medv'])
boot(Boston, boot.fn, R = 1000)
#the boot gives a std. error of the mean of 0.4107 which is ~ 0.4089 calculated in part (b)

#(d)
low = (22.53281 - (2*0.4106622))
high = (22.53281 + (2*0.4106622)) 
boot.ci = c(low, high)
boot.ci

t.test(Boston$medv)
#the boot.ci is a little wider than the 95 percent confidence interval given by t.test, but very comparable. 
#This makes since because 2*SE is a little more than the closer 95 percent approximation of 1.96*SE

#(e) and (f)
u.med.hat = median(medv)
u.med.hat

set.seed(1)
boot.fn=function(data, index)
  median(data[index,'medv'])
boot(Boston, boot.fn, R = 1000)
#thee SE of the median from the boot function is ~0.3778. This means that the std. dev. of the 
#median values calculated from multiple iid samples is ~0.3778. 

#(g)
u.tenth.hat = quantile(medv,probs = seq(0,1,0.1))[2]
u.tenth.hat

#(h)
set.seed(1)
boot.fn=function(data, index)
  quantile(data[index,'medv'], probs = seq(0,1,0.1))[2]
boot(Boston, boot.fn, R = 1000)
#the estimate of the tenth percentile is the value of medv that 90% of the observations are
#greater than.  The estimated SE of the tenth percentile means that if we take multiple iid 
#samples of equal size, the tenth percentile statistic will have a std. dev. that equals
#the estimated SE (in this case SE estimate ~ 0.4768)





