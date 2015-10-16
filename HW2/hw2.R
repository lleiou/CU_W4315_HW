data<-read.table("copier_maintenance.txt",header=T)
x<-data[,2]
y<-data[,1]
summary(lm(y~x))

b0<-coef(lm(y~x))[1]
b1<-coef(lm(y~x))[2]

anova(lm(y~x))

#95%confidence interval of x1
interval<-confint(lm(y~x), 'x', level=0.95)

#set function of SVM
f<-function(x){b0+b1*x}


xh<-6
#mean service time on 6
f(xh)

n<-length(data[,1])
z1<-(xh-mean(x))^2
z2<-sum((x-mean(x))^2)
mse<-sum((y-f(x))^2)/(n-2)

b0+b1*xh+qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5
b0+b1*xh-qt(0.975, n-2)*(mse*(1+1/n+z1/z2))^0.5

ssr<-sum()
ssto<-sum((y-mean(y))^2)

##################


data<-read.table("crime_rates.txt",header=T)
x<-data[,2]
y<-data[,1]
summary(lm(y~x))


########################


> data<-read.table("SENIC.txt",header=T)

x1<-data[,4]
> x2<-data[,12]
> x3<-data[,6]
> y<-data[,2]


summary(lm(y~x1))

summary(lm(y~x2))

summary(lm(y~x3))




