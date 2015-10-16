data<-read.table("crime_rates.txt",header=T)

x<-data[,2]
y<-data[,1]

hist(x, freq=F, right=F)

fit<-lm(y~x)
e<-resid(fit)

b0=coef(fit)[1]
b1=coef(fit)[2]

hist (e, freq=F, right=F)


f<-function(x){b0+b1*x}
plot(e~f(x))
abline(0,0)

qqnorm(e)
qqline(e)
