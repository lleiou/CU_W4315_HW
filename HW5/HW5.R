#It's interesting that all the 5 questions are using the same dataset...

data<-read.table("patient_satisfaction.txt",header=T)
y<-data[,1]
x1<-data[,2]
x2<-data[,3]
x3<-data[,4]


boxplot(x1)
boxplot(x2)
boxplot(x3)


pairs(~x1+x2+x3)

x<-data[,2:4]
x<-as.matrix(x)
fit<-lm(y~x)


library(car)
avPlots(fit,terms = ~.-x2)



resid<-resid(fit)

plot(resid~y)
abline(0,0)

plot(resid~x1)
abline(0,0)

plot(resid~x2)
abline(0,0)

plot(resid~x3)
abline(0,0)


qqnorm(resid)
qqline(resid)








2.

anova(fit)


#MSE<-anova(fit)$"Mean Sq"[2]
#cov<-MSE*solve(t(x)*x)
b<-coef(fit)
b0<-b[1]
b1<-b[2]
b2<-b[3]
b3<-b[4]



vcov<-vcov(fit)


confint(fit, level=0.967)
#it has 3 variables simultaneously, so g = 3.


summary(fit)$r.squared 

predict(lm(y~x1+x2+x3),new =data.frame(x1=35,x2=45,x3=2.2),interval="confidence", level=0.95)
#I don't know why this doesn't work:
#predict(lm(y~x),new =data.frame(t(c(1,2,3))),interval="confidence", level=0.95)
#The error is always the same: 'newdata' had 1 row but variables found have 46 rows ........
#Annoying!!!!!!!!!!!!!!!!!!

predict(lm(y~x1+x2+x3),new =data.frame(x1=35,x2=45,x3=2.2),interval="prediction", level=0.95)



summary(fit)





