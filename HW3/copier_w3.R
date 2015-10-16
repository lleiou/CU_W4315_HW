


b0<-coef(lm(y~x))[1]
b1<-coef(lm(y~x))[2]

e<-resid(fit)

#plot Cleveland dot plot
dotchart(x)


#boxplot
boxplot(e)


f<-function(x){b0+b1*x}
y.hat<-f(x)

plot(e~y.hat)
abline(0,0)

plot(e~x)
abline(0,0)



qqnorm(e)
qqline(e)






> data2<-read.table("copiers_full.txt", header=T)
> age<-data2$age
> age


> exp<-data2$experience
> plot(e~age)
> plot(e~exp)





#reorder the dataframe according to y
data1<-data[order(y),]

y1<-data1[,1]
x1<-data1[,2]
fit1<-lm(y1~x1)
summary(fit1)

b10<-coef(fit1)[1]
b11<-coef(fit1)[2]

f2<-function(x1){b10+b11*x1}
e1<-resid(fit)
n1<-length(data1[,2])


plot(e1~c(1:n1))










data2<-read.table("copiers_full.txt",header=T)
age<-data2$age
exp<-data2$experience

plot(e~age)
abline(0,0)

plot(e~exp)
abline(0,0)

