#It's interesting that all the 5 questions are using the same dataset...


data<- read.table("patient_satisfaction.txt",header=T)
y<-data[,1]
x1<-data[,2]
x2<-data[,3]
x3<-data[,4]


boxplot(x1)
boxplot(x2)
boxplot(x3)


pairs(~x1+x2+x3)

#c
fit<-lm(y~x1+x2+x3)
#The order of the three variables matters
summary(fit)


#d
#the previous method is a one that uses a package, which proves to be confusing,
#especially when compared with the teacher's method

#library(car)
#avPlots(fit,terms = ~.-x2)

a<-resid(lm(y~x1+x3))
b<-resid(lm(x2~x1+x3))

plot(a~b)

abline(coef(lm(a~b))[1],coef(lm(a~b))[2]) 

coef(lm(a~b))[2]



#e
a<-coef(fit)[1]
b<-coef(fit)[2:4]
y.hat<-a+cbind(x1,x2,x3)%*%b

resid<-resid(fit)

plot(resid~y.hat)
abline(0,0)

plot(resid~x1)
abline(0,0)

plot(resid~x2)
abline(0,0)

plot(resid~x3)
abline(0,0)

#f
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


#Q3
#a
#According to the question, we must fit the model in the order of x1 x2 x3.
anova(lm(y~x2+x1+x3))


anova(lm(y~x1+x2))

#b
anova(lm(y~x1+x2+x3))

qt(0.975,42)
#Pvalue
2*(1-pt(1.897054,42))


#c
qf(0.975,2,42)
#pvalue
1-pf(4.1837,2,42)







#Q4
summary(lm(y~x1+x2))
summary(lm(y~x1+x2+x3))

anova(lm(y~x1))
anova(lm(y~x3+x1))

anova(lm(y~x2))
anova(lm(y~x3+x2))




#Q5

plot(y~x1)

lines(lowess(y~x1))


plot(resid(lm(y~x2))~resid(lm(x1~x2)))

lines(lowess(resid(lm(y~x2))~resid(lm(x1~x2))))

plot(resid(lm(y~x2+x3))~resid(lm(x1~x2+x3)))

lines(lowess(resid(lm(y~x2+x3))~resid(lm(x1~x2+x3))))


cor(y,x1)^2

cor(resid(lm(y~x2)), resid(lm(x1~x2)))^2

cor(resid(lm(y~x2+x3)), resid(lm(x1~x2+x3)))^2

