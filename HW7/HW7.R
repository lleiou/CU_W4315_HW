

#Q1A
data<-read.table("job_proficiency.txt", header=T)
boxplot(data$X1)
boxplot(data$X2)
boxplot(data$X3)
boxplot(data$X4)
dataframe<-data.frame(x1=data$X1,x2=data$X2, x3=data$X3, x4=data$X4)
boxplot(dataframe)

#Q1B
pairs(Y~X1+X2+X3+X4, data=data)

#Q1C
fit<-lm(Y~X1+X2+X3+X4, data=data)

#Q2A
step(fit, direction="backward")
fit2<-lm(Y~X1+X3+X4, data=data)
coef(fit2)

#Q2B
fit3<-lm(Y~1, data=data)
step(fit3,scope=~X1+X2+X3+X4, direction="forward")

#Q2C
n<-length(data[,1])
step(fit, direction="backward", k=log(n))
step(fit3,scope=~X1+X2+X3+X4, direction="forward",k=log(n))



#Q31

data<-read.table("SENIC.txt", header=T)
y<-data[,2]
x1<-data[,3]
x2<-data[,4]
x3<-log(data[,5])
x4<-data[,6]
x5<-log(data[,7])
x6<-log(data[,10])
x7<-log(data[,11])
x8<-data[,12]

fit<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8)

library(MASS)
boxcox(fit)

#Q3B
y<-y^(-1)
pairs(y~x1+x2+x3+x4+x5+x6+x7+x8)


#Q3C
fit1<-lm(y~1)
step(fit1, scope=~x1+x2+x3+x4+x5+x6+x7+x8, direction="forward")


#Q3D
step(fit, direction="backward")


#Q3E
fit2<-lm(y~x1+x2+x4+x5+x6+x7)
coef(fit2)

